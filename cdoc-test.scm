;;; cdoc-test

(use matchable)
(use regex)
(use srfi-13)
(use posix)

(include "cdoc-parser.scm")
(import chicken-doc-parser)

(define cdoc-root (make-parameter "~/tmp/cdoc/root"))
(define +rx:%escape+ (irregex "[%/,]"))
(define +rx:%unescape+ (irregex "%([0-9a-fA-F][0-9a-fA-F])"))
(define (id->key id)
  (define (escape str)
    (irregex-replace/all +rx:%escape+ str
                         (lambda (m) (sprintf "%~x"
                                         (char->integer
                                          (string-ref (irregex-match-substring m 0) 0))))))
  (let ((str (escape (->string id))))
    (cond ((or (string=? str ".")
               (string=? str ".."))
           (warning "Identifier must not be . or .." str)     ;; ?
           #f)
          (else
           str))))
(define (key->id key)
  (string->symbol
   (irregex-replace/all +rx:%unescape+ key
                        (lambda (m) (string
                                (integer->char
                                 (string->number (irregex-match-substring m 1)
                                                 16)))))))

(define (write-key text type sig id path)
  (and-let* ((key (id->key id)))
    (change-directory (cdoc-root))
    (change-directory (make-pathname path #f))
    (create-directory key)
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
        (display text)))))

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
    (write-eggshell name)
    (let ((t (open-output-text name '())))
      (parse-and-write-tags/svnwiki fn (lambda (tags body)
                                         (write-tags tags body path))
                                    t)
      (close-output-port t))))
(define (parse-unit name id)
  (let ((fn (make-pathname +mandir+ name))
        (path (list id)))
    (write-unitshell name id)
    (let ((t (open-output-text id '())))
      (parse-and-write-tags/svnwiki fn (lambda (tags body)
                                         (write-tags tags body path))
                                    t)
      (close-output-port t))))

;;; hilevel

(use srfi-1)
(define (path->keys path)
  (map id->key (if (pair? path)
                   path
                   (string-split (->string path) "#"))))
(define (describe name)   ;; Test: print ,text and ,meta data for pathname
  (let* ((key (path->keys name))
         (pathname (make-pathname (cons (cdoc-root) key) #f))
         (textfile (make-pathname pathname ",text"))
         (metafile (make-pathname pathname ",meta")))
    (cond ((and (directory? pathname)
                (regular-file? textfile)
                (regular-file? metafile))
           ;; Now embedded in text body; no need to print sig
           ;; (let ((metadata (with-input-from-file metafile read-file)))
           ;;   (printf "~a: ~a\n"
           ;;           (cadr (assq 'type metadata))
           ;;           (cadr (assq 'signature metadata))))
           (with-input-from-file textfile
             (lambda ()
               (for-each-line (lambda (x) (display x) (newline))))))
          (else
           (error "No such identifier" name)))))
;; FIXME: Perhaps abstract key field lookup.  E.g. lookup text, meta fields of keys
(define (signature name)  ;; Return string representing signature
  (let* ((key (path->keys name))
         (pathname (make-pathname (cons (cdoc-root) key) #f))
         (metafile (make-pathname pathname ",meta")))
    (cond ((and (directory? pathname)
                (regular-file? metafile))
           (let ((metadata (with-input-from-file metafile read-file)))
             (cadr (assq 'signature metadata))))
          (else
           (error "No such identifier" name)))))

(define (refresh-eggs)
  (for-each (lambda (x) (print x) (parse-egg x)) (directory +eggdir+)))

;;; searching

;; (change-directory "~/tmp/cdoc/root")
;; (time (find-files "" directory? (lambda (path xs) (if (string=? "find-files" (pathname-file path)) (print path)))))  ;; 1.1 sec
;; (time (define cache (find-files "" directory?)))
;; (time (any (lambda (x) (if (string=? "find-files" (pathname-file x)) (print x) #f)) cache)) ;; 0.070 sec
;; (time (any (lambda (x) (if (string=? "nonexistent" (pathname-file x)) (print x) #f)) cache)) ;; 0.171 sec
;; hash key -> files? (or assq it)

(define key-cache #f)
(define (add! path)
  (let ((id (key->id (pathname-file path)))
        ;; We don't need to save the ID name in the value (since it is in the key)
        (val (map key->id (butlast (string-split path "/")))))
    (hash-table-update!/default key-cache id (lambda (old) (cons val old)) '())))

(define (lookup id)
  (define (lookup/raw id)
    (hash-table-ref/default key-cache id #f))
  ;; reconstruct full path by appending ID
  (cond ((lookup/raw id) => (lambda (path)
                              (map (lambda (x) (append x (list id)))
                                   path)))
        (else '())))
(define (search id)
  (for-each (lambda (x)
              (print ;; (string-intersperse x "#")
               x))
            (lookup id)))

;; FIXME: Does not allow string path, just list path: probably a mistake.
;; E.g. cannot search for chicken#location, but (chicken location) or location ok.
(define (search-and-describe id)
  (let ((entries (lookup id)))
    (cond ((null? entries)
           (void))
          ((null? (cdr entries))
           (print "path: " (car entries))
           (describe (car entries)))
          (else
           (describe-matches entries)))))
(define (search-only id)
  (let ((entries (lookup id)))
    (describe-signatures entries)))
(define (list-keys name)  ;; Test: list keys (directories) under pathname
  (let ((key (path->keys name)))
    (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))
            (directory (make-pathname (cons (cdoc-root) key) #f)))))

;; FIXME: Argument not actually a list path -- could also be a string path.
;; FIXME: Check describe contents of root
;; FIXME: Gross ;)
(define (describe-contents path)
  (for-each (lambda (x) (print (key->id x)
                          "\t\t"
                          (signature (append path
                                             (list (key->id x))))))
            (list-keys path)))
(define (describe-matches paths)
  (print "Found " (length paths) " matches:")
  (describe-signatures paths))
(define (describe-signatures paths)   ; ((srfi-69 hash-table-ref) (synch synch) (posix))
  (for-each (lambda (x) (print x "     " (signature x)))
            paths))

(define (refresh-id-cache)
  (change-directory "~/tmp/cdoc/root")
  (print "Rebuilding ID cache...")
  (set! key-cache (make-hash-table eq?))
  (time (for-each add! (find-files "" directory?)))
  (print "Writing ID cache...")
  (time (with-output-to-file "~/tmp/cdoc/id.idx"
          (lambda () (write (hash-table->alist key-cache)))))) ; .06 s

(define (read-id-cache)
  (set! key-cache
        (with-input-from-file "~/tmp/cdoc/id.idx"
          (lambda () (alist->hash-table (read) eq?))))) ; .06 s

(define (init)
  (read-id-cache)
  (toplevel-command 'desc (lambda () (describe (read)))
                    ",desc ID         Describe identifier ID using chicken-doc")
  (toplevel-command 'doc (lambda () (search-and-describe (read)))
                    ",doc ID          Search and describe identifier ID using chicken-doc"))
