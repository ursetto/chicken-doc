;;; chicken-doc

(include "chicken-doc-text.scm") ; local module

(module chicken-doc
*
;; Used by chicken-doc command
#;
(verify-repository
 open-repository close-repository locate-repository current-repository
 repository-base
 describe-signatures
 search-only
 describe-contents
 describe
 doc-dwim
;; Used additionally by chicken-doc-admin.  Somewhat internal, but exported.
 repository-information repository-root
 repository-magic +repository-version+
 repository-id-cache set-repository-id-cache!
 path->keys keys->pathname field-filename keys+field->pathname key->id
 make-id-cache id-cache-filename
;; Node API
 lookup-node
 match-nodes
 node-signature
 node-type
 node-sxml
 node-path
 node-children
 node-child-ids         ; experimental
;; Other API
 decompose-qualified-path
;; Parameters
 wrap-column
 )

(import scheme chicken)
(use matchable regex srfi-13 posix data-structures srfi-69 extras files utils srfi-1)
(import irregex)
(import (only csi toplevel-command))
(import chicken-doc-text)

;;; Config

(define wrap-column
  (make-parameter 76))   ; 0 or #f for no wrapping

;;; Lowlevel

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
  (make-pathname (cons (repository-root (current-repository)) keys) #f))
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

(define (path-child-keys path)
  (let* ((keys (path->keys path))
         (dir (keys->pathname keys)))
    (and (directory? dir)
         (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))  ;; Contains hardcoded ,
                 (directory dir)))))

(define (node-children node)
  (let ((path (node-path node)))
    (map (lambda (k)
           (lookup-node (append path (list (key->id k)))))
         (path-child-keys path))))

;; Shouldn't be necessary -- normally you should use node-children --
;; but currently a node lookup populates the node with metadata,
;; which wastes some time if you only need ids.  Ideally metadata
;; would be loaded lazily or lookup speed would be faster.
(define (node-child-ids node)
  (map key->id (path-child-keys (node-path node))))

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
    (make-node path id
#;
               (let* ((keys (path->keys path))
                     (pathname (keys->pathname keys)))
                 (cond ((directory? pathname)
                        #f)
                       (else
                        (error "no such node" path))))


                 (read-path-metadata path))))

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

(define (node-sxml node)
  (let* ((keys (path->keys (node-path node)))
         (file (keys+field->pathname keys 'sxml)))
    (and (file-exists? file)
         (read-file file))))

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
(define (describe node)
  (cond ((node-sxml node)
         => (lambda (doc)
              (write-sxml-as-text doc (wrap-column))))
        (else
         (error "No such identifier"
                (sprintf "~a" (node-path node))))))

;; Display the signature of all child keys of PATH, to stdout.
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

;; Cache is unique to repository but is shared between
;; threads holding the same repository object.
(define-record-type id-cache
  (make-id-cache table mtime filename)
  id-cache?
  (table id-cache-table)
  (mtime id-cache-mtime)
  (filename id-cache-filename))

(define (make-invalid-id-cache repo-base)
  (make-id-cache #f 0
                 (make-pathname repo-base "id.idx")))

(define (current-id-cache)  ; access current id cache hash table; legacy
  (repository-id-cache (current-repository)))
(define (id-cache-ref c id)
  (hash-table-ref/default (id-cache-table c) id '()))
(define (id-cache-keys c)
  (hash-table-keys (id-cache-table c)))

;; Validate and update the shared id cache in the current repository.
(define (validate-id-cache!)
  (define (read-id-cache! r c)
    (define (read-id-cache c)
      (let ((fn (id-cache-filename c)))
        (call-with-input-file fn
          (lambda (in)
            (make-id-cache
             (alist->hash-table (read in) eq?)
             (file-modification-time (port->fileno in))
             fn)))))
    (set-repository-id-cache! r (read-id-cache c)))

  ;; We don't currently lock id-cache validations with a mutex.
  ;; All that (should) happen is that when the cache is (rarely)
  ;; updated, if two threads validate at the same time both will
  ;; read the entire cache in.
  (let* ((r (current-repository))
         (c (repository-id-cache r)))
    (when (< (id-cache-mtime c)
             (file-modification-time (id-cache-filename c)))
      (read-id-cache! r c))))

;; Not currently needed.  Also not tested and not thread-safe
;; (define (invalidate-id-cache!)
;;   (set-repository-id-cache! (current-repository) (make-invalid-id-cache)))

;;; ID search

;; Returns list of nodes matching identifier ID.
;; ID may be a symbol or string.
(define (match-nodes/id id)
  (define (lookup id)
    (id-cache-ref (current-id-cache) id))
  (validate-id-cache!)
  (let ((id (if (string? id) (string->symbol id) id)))
    (map (lambda (x)
           (lookup-node (append x (list id))))
         (lookup id))))

;; Returns list of nodes whose identifiers
;; match regex RE.
(define (match-nodes/re re)
  (define (cache-keys)
    (id-cache-keys (current-id-cache)))
  (let ((rx (irregex re)))
    (validate-id-cache!)
    (let ((keys (sort (map symbol->string (cache-keys))
                      string<?)))
      (append-map (lambda (id)
                    (match-nodes id))
                  (filter-map (lambda (k)
                                (and (string-search rx k) k))
                              keys)))))

;;(define ids (sort (flatten (hash-table-fold t (lambda (k v s) (cons (map (lambda (x) (string-intersperse (map ->string (append x (list k))) " ")) v) s)) '())) string<?))
;;(let ((rx (irregex "o.O"))) (filter-map (lambda (k) (and (string-search rx k) k)) ids))



;; ,t (validate-id-cache!)
;;    0.123 seconds elapsed
;;    0.024 seconds in (major) GC
;;    47942 mutations
;;        3 minor GCs
;;        5 major GCs
;; after id cache loaded, disk cache warm
;; ,t (match-nodes (irregex "posix"))
;;    0.065 seconds elapsed                (0.06 - 0.10 sec)
;;        0 seconds in (major) GC
;;    68054 mutations
;;      832 minor GCs
;;        0 major GCs

;; Return list of nodes whose identifiers match
;; symbol, string or re.
(define (match-nodes idre)
  (if (or (irregex? idre) (regexp? idre))
      (match-nodes/re idre)
      (match-nodes/id idre)))

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

(define +repository-version+ 2)

;; The repository object is a new concept (formerly all fields
;; were global parameters) so our API does not expect a
;; repository object to be passed in.  Therefore, we make
;; current-repository a global parameter.

(define-record-type chicken-doc-repository
  (make-repository base root magic info id-cache)
  repository?
  (base repository-base)
  (root repository-root)
  (magic repository-magic)
  (info repository-information)
  (id-cache repository-id-cache set-repository-id-cache!))

;; Current repository for node lookup API.
(define current-repository (make-parameter #f))

;; Return standard location of repository.  Does not
;; guarantee it exists.
(define (locate-repository)
  (or (getenv "CHICKEN_DOC_REPOSITORY")
      (make-pathname (chicken-home) "chicken-doc")))

;; Open the repository found in the standard location
;; and set the current repository for the thread.
(define (verify-repository)  ; legacy name; should be changed
  (current-repository
   (open-repository
    (locate-repository))))

;; Open repository and return new repository object or
;; throw error if nonexistent or format failure.
(define (open-repository base)
  (let ((magic (make-pathname base ".chicken-doc-repo")))
    (if (file-exists? magic)
        (let ((info (with-input-from-file magic read)))
          (let ((version (or (alist-ref 'version info) 0)))
            (cond ((= version +repository-version+)
                   (let ((r (make-repository base
                                             (make-pathname base "root")
                                             magic
                                             info
                                             (make-invalid-id-cache base))))
                     (set-finalizer! r close-repository)
                     r))
                  (else (error "Invalid repo version number ~a, expected ~a\n"
                                version +repository-version+)))))
        (error "No chicken-doc repository found at " base))))
(define (close-repository r)
  (void))

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
(define repl-wtf
  (lambda (re)
    (search-only (irregex re))))

(when (feature? 'csi)
  ;; Warning -- will execute if called from a script.
  ;; We really only want this to execute at the REPL.
  (verify-repository)
  
  (toplevel-command 'wtf (lambda () (repl-wtf (string-trim-both
                                          (read-line))))
                    ",wtf RE           Regex search with chicken-doc (\"where to find\")")
  (toplevel-command 'toc (lambda () (repl-toc-dwim (read)))
                    ;; TOC should look up if this is a relative path
                    ",toc PATHSPEC     List contents of path with chicken-doc")
  (toplevel-command 'doc (lambda () (repl-doc-dwim (read)))
                    ",doc PATHSPEC     Describe identifier or path with chicken-doc"))

)  ;; end module

