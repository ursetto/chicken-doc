;;; chicken-doc

;; FIXME: Quite a few things are exported for chicken-doc-admin's use only
;; such as id->key.  Furthermore even certain regular things shouldn't
;; be exported to the REPL.

(module chicken-doc
;; Used by chicken-doc command
(verify-repository
 repository-base
 describe-signatures
 search-only
 describe-contents
 describe
 doc-dwim
;; Used additionally by chicken-doc-admin.  Somewhat internal, but exported.
 repository-information repository-root
 repository-magic repository-version
 id-cache id-cache-filename id-cache-mtime id-cache-add-directory!
 path->keys keys->pathname field-filename keys+field->pathname
 )

(import scheme chicken)
(use matchable regex srfi-13 posix data-structures srfi-69 extras files utils)
(import irregex)
(import (only csi toplevel-command))

;;; Config

(define repository-base
  (make-parameter #f))

(define (locate-repository)
  (or (getenv "CHICKEN_DOC_REPOSITORY")
      (make-pathname (chicken-home) "chicken-doc")))

;; Hmm--should we set this on module load?
(repository-base (locate-repository))

;;; Lowlevel

(define (repository-root)
  (make-pathname (repository-base) "root"))

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

(define (path->keys path)
  (map id->key (if (or (null? path)
                       (pair? path))
                   path
                   (string-split (->string path) "#"))))
(define (keys->pathname keys)
  (make-pathname (cons (repository-root) keys) #f))
(define (field-filename name)
  (string-append "," (->string name)))
(define (pathname+field->pathname pathname field)
  (make-pathname pathname (field-filename field)))
(define (keys+field->pathname keys field)  ;; should this take a path instead of keys?
  (pathname+field->pathname (keys->pathname keys) field))

;;; Access

;; Return string list of child keys (directories) directly under PATH, or #f
;; if the PATH is invalid.
(use srfi-1) ; filter
(define (child-keys path)
  (let* ((keys (path->keys path))
         (dir (keys->pathname keys)))
    (and (directory? dir)
         (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))  ;; Contains hardcoded ,
                 (directory dir)))))

;; Return string representing signature of PATH
(define (signature path)
  (let* ((keys (path->keys path))
         (pathname (keys->pathname keys))
         (metafile (pathname+field->pathname pathname 'meta)))
    (cond ((file-exists? metafile)
           (let ((meta (with-input-from-file metafile read-file)))
             (cadr (assq 'signature meta))))
          ((directory? pathname)
           ;; write-keys may create intermediate container directories
           ;; without metadata, so handle this specially.
           "")
          (else
           (error "No such identifier" path)))))

;;; Describe

;; Display file to stdout
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

;; Display the signature of all child keys of PATH, to stdout.
;; NB: if we change path->keys to assume strings inside a path are already keys,
;; we could avoid the key->id->key conversion in SIGNATURE.
(define (describe-contents path)
  (for-each (lambda (x) (print (key->id x)
                          "\t\t"
                          (signature (append path
                                             (list (key->id x))))))
            (or (child-keys path)
                (error "No such path" path))))

(define (describe-signatures paths)   ; ((srfi-69 hash-table-ref) (synch synch) (posix))
  (for-each (lambda (x) (print x "     " (signature x)))
            paths))
(define (describe-matches paths)
  (print "Found " (length paths) " matches:")
  (describe-signatures paths))

;;; ID search cache

(define id-cache
  (make-parameter #f))
(define (id-cache-filename)
  (make-pathname (repository-base) "id.idx"))
(define id-cache-mtime
  (make-parameter 0))
(define (id-cache-add-directory! pathname)
  (let ((id (key->id (pathname-file pathname)))
        ;; We don't save the ID name in the value (since it is in the key)
        (val (map key->id (butlast (string-split pathname "/\\")))))   ;; hmm
    (hash-table-update!/default (id-cache) id (lambda (old) (cons val old)) '())))
(define (read-id-cache!)
  (id-cache
   (call-with-input-file (id-cache-filename)
     (lambda (in)
       (id-cache-mtime (file-modification-time (port->fileno in)))
       (alist->hash-table (read in) eq?)))))
(define (validate-id-cache!)
  (when (< (id-cache-mtime)
           (file-modification-time (id-cache-filename)))
    (read-id-cache!)))
(define (invalidate-id-cache!)
  (id-cache-mtime 0))

;;; ID search

(define (lookup id)
  (define (lookup/raw id)
    (hash-table-ref/default (id-cache) id #f))
  (validate-id-cache!)
  (cond ((lookup/raw id) => (lambda (path)
                              ;; reconstruct full path by appending ID
                              (map (lambda (x) (append x (list id)))
                                   path)))
        (else '())))

;; (define (search id)
;;   (for-each (lambda (x)
;;               (print ;; (string-intersperse x "#")
;;                x))
;;             (lookup id)))

(define (search-and-describe id)
  (let ((entries (lookup id)))
    (cond ((null? entries)
           (error "No such identifier" id))
          ((null? (cdr entries))
           (print "path: " (car entries))
           (describe (car entries)))
          (else
           (describe-matches entries)))))
(define (search-only id)
  (let ((entries (lookup id)))
    (describe-signatures entries)))
(define (search-and-describe-contents id)
  (let ((entries (lookup id)))
    (cond ((null? entries)
           (void))
          ((null? (cdr entries))
           (print "path: " (car entries))
           (describe-contents (car entries)))
          (else
           (describe-matches entries)))))

(define (doc-dwim path)
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

;;; Repository

(define repository-version 1)
(define repository-information (make-parameter '()))
(define (repository-magic)
  (make-pathname (repository-base) ".chicken-doc-repo"))
(define (verify-repository)
  (and (file-exists? (repository-magic))
       (let ((repo-info (with-input-from-file (repository-magic) read)))
         (repository-information repo-info)
         (let ((version (or (alist-ref 'version repo-info) 0)))
           (cond ((= version repository-version))
                 (else (fprintf (current-error-port) "Invalid repo version number ~a\n" version)
                       #f))))))
(define (set-chicken-doc-repository! x)
  (invalidate-id-cache!)
  (repository-base x)
  (unless (verify-repository)
    (warning "No chicken-doc repository found at" (repository-base))))

;;; REPL

(define repl-doc-dwim doc-dwim)
(define repl-toc-dwim            ;; FIXME: ignore # paths for now
  (lambda (path)
    (cond ((or (null? path)
               (pair? path))
           (describe-contents path))
          (else
           (search-and-describe-contents path)))))

(when (feature? 'csi)
  ;; Warning -- will execute if called from a script.
  ;; We really only want this to execute at the REPL.
  (set-chicken-doc-repository! (repository-base) ;; (locate-repository)
                          )
  (toplevel-command 'doc (lambda () (repl-doc-dwim (read)))
                    ",doc PATH         Describe identifier or path with chicken-doc")
  (toplevel-command 'toc (lambda () (repl-toc-dwim (read)))
                    ;; TOC should look up if this is a relative path
                    ",toc PATH         List contents of path"))

)  ;; end module

