
(require-library chicken-doc)

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

(define (write-key text type sig id path)
  (and-let* ((key (id->key id)))
    (with-global-write-lock
     (lambda ()
       (with-cwd
        (cdoc-root)
        (lambda ()
          (change-directory (make-pathname path #f))
          (create-directory key)       ;; silently ignore if exists
          (change-directory key)
          (with-output-to-file ",meta"
            (lambda ()
              (for-each (lambda (x)
                          (write x) (newline))
                        `((type ,type)
                          (signature ,sig)
                          (identifier ,id)))))
          (with-output-to-file ",text"
            (lambda ()
              (display text)))))))))

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

;; FIXME: Path is a list of directories (because that's what write-tag expects).
;; This is broken, because write-tag does not escape them
(define (write-tags tags tag-body path)
  (for-each (match-lambda ((type sig id)
                      (if id
                          (write-key (string-concatenate-reverse
                                      (intersperse tag-body "\n"))
                                     type sig id path)
;;                           (warning "Skipped writing tag for signature" sig)
                          )))
            (reverse tags)))

;; This is a hack so we can open an output port to the
;; text key, which is then passed to the parser to write
;; a transformed wiki document.  FIXME: make this less dumb.
(define (open-output-text id path)
  (and-let* ((key (id->key id)))
    (open-output-file (make-pathname
                       (append (list (cdoc-root))
                               path (list key)) ",text"))))

(define (write-eggshell name)
  (write-key "This space intentionally left blank"
             'egg (string-append name " egg") name '(".")))  ;; "." due to change-directory
(define (write-unitshell name id)
  (write-key "This space intentionally left blank"
             'unit name id '(".")))

(define +wikidir+ "~/scheme/chicken-wiki")
(define +eggdir+ (string-append +wikidir+ "/eggref/4"))
(define +mandir+ (string-append +wikidir+ "/man/4"))
(define (parse-egg name)
  (let ((fn (make-pathname +eggdir+ name))
        (path (list name)))   ;; Possible FIXME (see write-tags)
    (with-global-write-lock
     (lambda ()
       (write-eggshell name)
       (let ((t (open-output-text name '())))
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
       (let ((t (open-output-text id '())))
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

