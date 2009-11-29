
(require-library chicken-doc)
(use matchable)

(include "cdoc-parser.scm")
(import chicken-doc-parser)

;;; Locking

;; NOT SRFI-18 safe (multiple in-process locks don't block).
(define global-write-lock (make-parameter #f))
(define (acquire-global-write-lock!)
  (when (global-write-lock)
    ;; Not currently recursive.
      (error "Already acquired global write lock"))
  (let ((out (open-output-file (make-pathname (cdoc-base) "lock"))))
    (file-lock/blocking out)
    (global-write-lock out)))
(define (release-global-write-lock!)
  (unless (global-write-lock)
    (error "Releasing unlocked write lock"))
  (close-output-port (global-write-lock))
  (global-write-lock #f))
(define (with-global-write-lock thunk)
  (cond ((global-write-lock)
         (thunk))
        (else    ; FIXME use handle-exceptions
         (acquire-global-write-lock!)
         (handle-exceptions exn (begin
                                  (release-global-write-lock!)
                                  (signal exn))
           (thunk)
           (release-global-write-lock!)))))

;;; Lowlevel

;; PATH: a list or string key path.  TEXT: String to write to text
;; key, or #f to skip.  TYPE: key type (container types are 'unit and 'egg;
;; tag types are 'procedure, 'macro, etc.)  SIGNATURE:
;; Function signature as string; also used for a short description
;; of containers.
(define (write-key path text type sig)
  (let* ((keys (path->keys path))
         (pathname (keys->pathname keys)))
    (with-global-write-lock
     (lambda ()
       (create-directory pathname #t) ;; mkdir -p
       (with-cwd
        pathname
        (lambda ()
          (with-output-to-file (field-filename 'meta)
            (lambda ()
              (for-each (lambda (x)
                          (write x) (newline))
                        `((type ,type)
                          (signature ,sig)
                          ;; (identifier ,id)
                          ))))
          (if text
              (with-output-to-file (field-filename 'text)
                (lambda ()
                  (display text))))))))))

;;; Repo manipulation

(define (create-repository!)
  ;; FIXME: initialization should not occur if the version is wrong
  (when (file-exists? (repo-magic))
    (error "Repository already exists at" (cdoc-base)))
  (create-directory (cdoc-base))
  (create-directory (cdoc-root))
  (with-output-to-file (repo-magic)
    (lambda () (pp `((version . ,repo-version))))))
(define (describe-repository)
;;   (print "Repository information:")
  (pp (cons `(location . ,(cdoc-base))
            (repository-information))))

;;; Hilevel parsing (units, eggs)

(define (write-tags tags tag-body path)
  (for-each (match-lambda ((type sig id)
                      (if id
                          (write-key (append path (list id))
                                     (string-concatenate-reverse
                                      (intersperse tag-body "\n"))
                                     type sig)
;;                           (warning "Skipped writing tag for signature" sig)
                          )))
            (reverse tags)))

;; Open output port to the text key, which is passed to the parser
;; to write a transformed wiki document.  Semi-dumb.
(define (open-output-text path)
  (open-output-file
   (keys+field->pathname (path->keys path) 'text)))

(define (write-eggshell name)
  (write-key (list name) #f 'egg
             (string-append name " egg")))
(define (write-unitshell name id)
  (write-key (list id) #f 'unit name))

(define +wikidir+ "~/scheme/chicken-wiki")
(define +eggdir+ (string-append +wikidir+ "/eggref/4"))
(define +mandir+ (string-append +wikidir+ "/man/4"))
(define (parse-egg name)
  (let ((fn (make-pathname +eggdir+ name))
        (path `(,name)))
    (with-global-write-lock
     (lambda ()
       (write-eggshell name)
       (let ((t (open-output-text path)))
         (parse-and-write-tags/svnwiki fn (lambda (tags body)
                                            (write-tags tags body path))
                                       t)
         (close-output-port t))))))
(define (parse-unit name id)
  (let ((fn (make-pathname +mandir+ name))
        (path (list id)))
    (with-global-write-lock
     (lambda ()
       (write-unitshell name id)
       (let ((t (open-output-text path)))
         (parse-and-write-tags/svnwiki fn (lambda (tags body)
                                            (write-tags tags body path))
                                       t)
         (close-output-port t))))))

(define (refresh-eggs)
  (with-global-write-lock
   (lambda ()
     (for-each (lambda (x) (print x) (parse-egg x)) (directory +eggdir+)))))

;;; ID search cache (write)

(define (write-id-cache!)
  (let ((tmp-fn (string-append (id-cache-filename) ".tmp")))
    (with-output-to-file tmp-fn
      (lambda () (write (hash-table->alist (id-cache)))))
    (rename-file tmp-fn (id-cache-filename))
    (id-cache-mtime (current-seconds)
                    ;; (file-modification-time (id-cache-filename))
                    )))
(define (refresh-id-cache)
  (with-global-write-lock
   (lambda ()
     (with-cwd (cdoc-root)
               (lambda ()
                 (id-cache (make-hash-table eq?))
                 (for-each id-cache-add-directory!
                           (find-files "" directory?))))
;;      (print "Writing ID cache...")
     (write-id-cache!))))

