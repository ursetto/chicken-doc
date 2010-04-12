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
;; Node API
 lookup-node
 node-signature
 node-type
;; Other API
 decompose-qualified-path
;; Parameters
 wrap-column
 )

(import scheme chicken)
(use matchable regex srfi-13 posix data-structures srfi-69 extras files utils srfi-1)
(import irregex)
(import (only csi toplevel-command))

;;; Config

(define wrap-column
  (make-parameter 0))
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

(define +rx:%escape+ (irregex "[%/,.*<>?!: ]"))
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
  (map id->key path))
(define (keys->pathname keys)
  (make-pathname (cons (repository-root) keys) #f))
(define (field-filename name)
  (string-append "," (->string name)))
(define (pathname+field->pathname pathname field)
  (make-pathname pathname (field-filename field)))
(define (keys+field->pathname keys field)  ;; should this take a path instead of keys?
  (pathname+field->pathname (keys->pathname keys) field))

;; Turn pathspec (a path list or path string) into a path or id.
;; Path lists pass through.  Qualified path strings (contains #) become
;; path lists.  Unqualified path strings become ids (symbols).  An empty
;; path string becomes () -- i.e. toplevel.
(define (decompose-pathspec pathspec)
  (if (pair? pathspec)
      pathspec
      (let ((p (decompose-qualified-path pathspec)))
        (cond ((null? p) p)
              ((null? (cdr p)) (string->symbol (car p)))
              (else p)))))
(define (decompose-qualified-path str)
  (string-split (if (symbol? str) (symbol->string str) str)
                "#"))

;;; Access

(define-record-type chicken-doc-node
  (make-node path id md)
  node?
  (path node-path)            ; includes ID
  (id node-id)
  (md node-md))

;; Return string list of child keys (directories) directly under PATH, or #f
;; if the PATH is invalid.

(define (node-children node)
  (define (path-child-keys path)
    (let* ((keys (path->keys path))
           (dir (keys->pathname keys)))
      (and (directory? dir)
           (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))  ;; Contains hardcoded ,
                   (directory dir)))))
  (let ((path (node-path node)))
    (map (lambda (k)
           (lookup-node (append path (list (key->id k)))))
         (path-child-keys path))))

;; Obtain metadata alist at PATH.  Valid node without metadata record
;; returns '().  Invalid node throws error.
(define (read-path-metadata path)
  (let* ((keys (path->keys path))
         (pathname (keys->pathname keys))
         (metafile (pathname+field->pathname pathname 'meta)))
    (cond ((file-exists? metafile)
           (read-file metafile))
          ((directory? pathname)
           ;; write-keys may create intermediate container directories
           ;; without metadata, so handle this specially.
           '())
          (else
           (error "No such identifier" path)))))

(define (node-metadata-field node field)
  (cond ((assq field (node-metadata node))
         => cadr)
        (else #f)))

(define node-metadata node-md)    ; Alternatively, load metadata as needed.

;; Return node record at PATH or throw error if the record does
;; not exist (implicitly in read-path-metadata).
(define (lookup-node path)
  (let ((id (if (null? path)
                ""   ; TOC
                (last path))))
    (make-node path id (read-path-metadata path))))

;; Return string representing signature of PATH.  If no signature, return "".
(define (node-signature node)
  (or (node-metadata-field node 'signature)
      ""))

;; Return symbol representing type of PATH, or 'unknown.
(define (node-type node)
  (let ((key (node-metadata-field node 'type)))
    (if key
        (if (string? key)
            (string->symbol key)
            key)
        'unknown)))

;;; Describe

;; Utility procedure (dropped in Chicken >= 4.3.2)
(define (for-each-line proc #!optional (port (current-input-port)))
  (do ((line (read-line port) (read-line port)))
      ((eof-object? line))
    (proc line)))

;; Display file to stdout
(define (cat file)
  (with-input-from-file file
    (lambda ()
      (for-each-line (lambda (x) (display x) (newline))))))

;; Display the "text" field of NODE to current-output-port.  Even if
;; NODE is a valid node, that doesn't mean it has text contents.
(include "svnwiki-sxml-text.scm")
(define (describe node)
  (let ((path (node-path node)))
    (let* ((keys (path->keys path))
           (file (keys+field->pathname keys 'sxml)))
      (cond ((and (file-exists? file))
             (display-sxml-as-text (read-file file)
                                   (wrap-column)))
            (else
             (error "No such identifier" path))))))

;; Display the signature of all child keys of PATH, to stdout.
;; NB: if we change path->keys to assume strings inside a path are already keys,
;; we could avoid the key->id->key conversion in SIGNATURE.
(define (maximum-string-length strs)
  (reduce max 0 (map string-length strs)))
(define (padding len s)
  (make-string (max 0 (- len (string-length s)))
               #\space))
(define (describe-contents node)
  (let ((kids (node-children node)))
    (let* ((strids (map ->string (map node-id kids)))
           (len (maximum-string-length strids)))
      (for-each (lambda (n s) (print s (padding len s)
                                "  "
                                (node-signature n)))
                kids strids))))

(define (describe-signatures nodes)
  (let* ((strpaths (map ->string (map node-path nodes)))
         (len (maximum-string-length strpaths)))
    (for-each (lambda (n s) (print s (padding len s)
                              "  "
                              (node-signature n)))
              nodes strpaths)))

(define (describe-matches nodes)
  (print "Found " (length nodes) " matches:")
  (describe-signatures nodes))

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

;; Returns list of nodes matching identifier ID.
;; ID may be a symbol or string.
(define (match-nodes id)
  (define (lookup id)
    (hash-table-ref/default (id-cache) id '()))
  (validate-id-cache!)
  (let ((id (if (string? id) (string->symbol id) id)))
    (map (lambda (x)
           (lookup-node (append x (list id))))
         (lookup id))))

;; (define (search id)
;;   (for-each (lambda (x)
;;               (print ;; (string-intersperse x "#")
;;                x))
;;             (lookup id)))

(define (search-and-describe id)
  (let ((nodes (match-nodes id)))
    (cond ((null? nodes)
           (error "No such identifier" id))
          ((null? (cdr nodes))
           (print "path: " (node-path (car nodes)))
           (describe (car nodes)))
          (else
           (describe-matches nodes)))))
(define (search-only id)
  (let ((nodes (match-nodes id)))
    (describe-signatures nodes)))
(define (search-and-describe-contents id)
  (let ((nodes (match-nodes id)))
    (cond ((null? nodes)
           (void))
          ((null? (cdr nodes))
           (print "path: " (car nodes))
           (describe-contents (car nodes)))
          (else
           (describe-matches nodes)))))

(define (doc-dwim pathspec)
  (let ((p (decompose-pathspec pathspec)))
    (if (or (pair? p) (null? p))
        (describe (lookup-node p))
        (search-and-describe p))))

;;; Repository

(define repository-version 2)
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
  (lambda (pathspec)
    (let ((p (decompose-pathspec pathspec)))
      (cond ((or (null? p)
                 (pair? p))
             (describe-contents (lookup-node p)))
            (else
             (search-and-describe-contents p))))))

(when (feature? 'csi)
  ;; Warning -- will execute if called from a script.
  ;; We really only want this to execute at the REPL.
  (set-chicken-doc-repository! (repository-base) ;; (locate-repository)
                          )
  (toplevel-command 'doc (lambda () (repl-doc-dwim (read)))
                    ",doc PATHSPEC     Describe identifier or path with chicken-doc")
  (toplevel-command 'toc (lambda () (repl-toc-dwim (read)))
                    ;; TOC should look up if this is a relative path
                    ",toc PATHSPEC     List contents of path"))

)  ;; end module

