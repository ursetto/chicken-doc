;;; cdoc-test

(use matchable)
(use regex)
(use srfi-13)
(use posix)

(include "cdoc-parser.scm")
(import chicken-doc-parser)

;; Config
(define cdoc-base
  (make-parameter "~/tmp/cdoc"))



(define (cdoc-root)
  (make-pathname (cdoc-base) "root"))
(define +rx:%escape+ (irregex "[%/,.]"))
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
    (with-global-write-lock
     (lambda ()
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
           (display text)))))))

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

;;; hilevel

(use srfi-1)
(define (path->keys path)
  (map id->key (if (or (null? path)
                       (pair? path))
                   path
                   (string-split (->string path) "#"))))
(define (keys->pathname keys)
  (make-pathname (cons (cdoc-root) keys) #f))
(define (field-filename name)
  (string-append "," (->string name)))
(define (keys+field->pathname keys field)  ;; should this take a path instead of keys?
  (make-pathname (keys->pathname keys)
                 (field-filename field)))
(define (cat file)
  (with-input-from-file file
    (lambda ()
      (for-each-line (lambda (x) (display x) (newline))))))

;; Display the "text" field of PATH to current-output-port
(define (describe path)
  (let* ((keys (path->keys path))
         (textfile (keys+field->pathname keys 'text)))
    (cond ((and (file-exists? textfile))
           (cat textfile))   ;; (Signature is embedded in text body)
          (else
           (error "No such identifier" path)))))

;; Return string representing signature of PATH
(define (signature path)
  (let* ((keys (path->keys path))
         (metafile (keys+field->pathname keys 'meta)))
    (cond ((and (file-exists? metafile))
           (let ((meta (with-input-from-file metafile read-file)))
             (cadr (assq 'signature meta))))
          (else
           (error "No such identifier" path)))))

(define (refresh-eggs)
  (with-global-write-lock
   (lambda ()
     (for-each (lambda (x) (print x) (parse-egg x)) (directory +eggdir+)))))

;;; searching

(define key-cache #f)
;; FIXME: 
(define (add! pathname)
  (let ((id (key->id (pathname-file pathname)))
        ;; We don't need to save the ID name in the value (since it is in the key)
        (val (map key->id (butlast (string-split pathname "/\\")))))   ;; hmm
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
;; Return string list of child keys (directories) directly under PATH, or #f
;; if the PATH is invalid.
(define (child-keys path)
  (let* ((keys (path->keys path))
         (dir (keys->pathname keys)))
    (and (directory? dir)
         (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))  ;; Contains hardcoded ,
                 (directory dir)))))

(define (repl-doc-dwim path)
  (cond ((or (null? path)
             (pair? path))
         (describe path))
        (else
         ;; Again, we could use path->keys IF it did not convert strings.
         ;; As is, strings would be double-converted.  However, that is
         ;; dangerous because we do not want to write illegal characters to files.
         ;; Thus we split the ID manually, relying on callee to convert to keys.
         (let ((id-strings (string-split (->string path) "#")))
           (if (or (null? id-strings)
                   (pair? (cdr id-strings)))
               (describe id-strings)
               (search-and-describe (string->symbol (car id-strings))))))))

;; Display the signature of all child keys of PATH, to stdout.
;; NB: if we change path->keys to assume strings inside a path are already keys,
;; we could avoid the key->id->key conversion in SIGNATURE.
(define (describe-children path)
  (for-each (lambda (x) (print (key->id x)
                          "\t\t"
                          (signature (append path
                                             (list (key->id x))))))
            (or (child-keys path)
                (error "No such path" path))))
(define (describe-matches paths)
  (print "Found " (length paths) " matches:")
  (describe-signatures paths))
(define (describe-signatures paths)   ; ((srfi-69 hash-table-ref) (synch synch) (posix))
  (for-each (lambda (x) (print x "     " (signature x)))
            paths))

(define (id-cache-filename)
  (make-pathname (cdoc-base) "id.idx"))
(define (refresh-id-cache)
  (with-global-write-lock
   (lambda ()
     (change-directory (cdoc-root))
     (print "Rebuilding ID cache...")
     (set! key-cache (make-hash-table eq?))
     (time (for-each add! (find-files "" directory?)))
     (print "Writing ID cache...")
     (let ((tmp-fn (make-pathname #f (id-cache-filename) ".tmp")))
       (time (with-output-to-file tmp-fn
               (lambda () (write (hash-table->alist key-cache)))))
       (sleep 10)
       (rename-file tmp-fn (id-cache-filename))))))

(define (read-id-cache)
  (set! key-cache
        (with-input-from-file (id-cache-filename)
          (lambda () (alist->hash-table (read) eq?)))))

(define (init)
  (read-id-cache)
;;   (toplevel-command 'desc (lambda () (describe (read)))
;;                     ",desc ID         Describe identifier ID using chicken-doc")
  (toplevel-command 'doc (lambda () (repl-doc-dwim (read)))
                    ",doc ID          Describe identifier ID using chicken-doc"))


;; NOT SRFI-18 safe
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
         (thunk)
         (release-global-write-lock!))))
