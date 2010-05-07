;;; chicken-doc

(include "chicken-doc-text.scm") ; local module

(module chicken-doc
;; Used by chicken-doc command
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
 make-repository-placeholder
 repository-modification-time
;; Node API
 lookup-node
 match-nodes
 match-node-paths/re
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
  (%make-node path id md)
  node?
  (path node-path)            ; includes ID
  (id node-id)
  (md node-md))

(define (make-node path id)
  (%make-node path id
              (delay (read-path-metadata path))))

;; Return string list of child keys (directories) directly under PATH, or #f
;; if the PATH is invalid.

(define (path-child-keys path)
  (let* ((keys (path->keys path))
         (dir (keys->pathname keys)))
    (and (directory? dir)
         (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))  ;; Contains hardcoded ,
                 (sort (directory dir) string<?)))))

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

(define (node-metadata node)
  (force (node-md node)))         ;  load metadata as needed

;; Return node record at PATH or throw error if the record does
;; not exist (implicitly in read-path-metadata).
(define (lookup-node path)
  (let ((id (if (null? path)
                ""   ; TOC
                (last path))))
    ;; Note that, if metadata is delayed, our API requires that
    ;; the node be checked for existence here.  If instead nodes
    ;; were not required to exist, and a manual existence check
    ;; were possible, we could avoid the directory touch when
    ;; merely matching against nodes.
    (let* ((keys (path->keys path))   ; FIXME!! path->keys is noticeably slow [*]
           (pathname (keys->pathname keys)))
      (or (directory? pathname)
          (error "no such node" path)))          ; now required
    (make-node path id)))
; [*] ,t (let loop ((n 100000)) (if (= n 0) 'done (begin (path->keys '(abc def ghi)) (loop (- n 1)))))  -> 1.66 seconds elapsed, 17 major GCs, 1.2M mutations
; [*] ,t (let loop ((n 100000)) (if (= n 0) 'done (begin (path->keys '(abc d.f ghi)) (loop (- n 1)))))  -> 2.76 seconds, 40 GCs, 1.6M mutations
; [*] ,t (let loop ((n 100000)) (if (= n 0) 'done (begin (path->keys '("abc" "def" "ghi")) (loop (- n 1))))) -> 0.876 seconds, 6 major GCs, 1.2M mutations
;; uri-encode-string takes 5x as long as id->key, skip it

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

#|
(define (node-modification-time node)
  ;; hypothetical function returning the last update time of a node.
  ;; The most accurate result is probably obtained by returning the
  ;; mtime of the ,meta file.  Clients may also want to know when
  ;; children have been updated (or at least when the child IDs change).
  ;; It may be sufficient to check the parent directory mtime or
  ;; the max of that and ,meta since rename, add and delete of child
  ;; directories will change parent mtime.  Parent mtime is usually
  ;; not affected when ,meta or ,sxml file is overwritten, though.
  )
  
|#

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
  (%make-id-cache table mtime filename
                  ids ; id string vector
                  paths ; path string list
                  )
  id-cache?
  (table id-cache-table)
  (mtime id-cache-mtime)
  (filename id-cache-filename)
  (ids %id-cache-ids)
  (paths %id-cache-paths))

;; Delayed construction of id string list and paths is legal
;; because cache updates are disallowed.
(define (make-id-cache table mtime filename)
  (%make-id-cache table mtime filename
                  (delay (list->vector
                          (sort (map symbol->string (hash-table-keys table))
                                string<?)))
                  (delay (sort
                          (flatten
                           (hash-table-fold
                            table
                            (lambda (k v s)
                              (cons
                               (map (lambda (x)
                                      (string-intersperse
                                       (map ->string (append x (list k))) " "))
                                    v)
                               s))
                            '()))
                          string<?))))

(define (make-invalid-id-cache repo-base)
  (make-id-cache #f 0
                 (make-pathname repo-base "id.idx")))

(define (current-id-cache)  ; access current id cache hash table; legacy
  (repository-id-cache (current-repository)))
(define (id-cache-ref c id)
  (hash-table-ref/default (id-cache-table c) id '()))
(define (id-cache-keys c)
  (hash-table-keys (id-cache-table c)))

;; Validate and update the shared id cache in repository R.
(define (validate-id-cache! r)
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
  (let* ((c (repository-id-cache r)))
    (when (< (id-cache-mtime c)
             (file-modification-time (id-cache-filename c)))
      (read-id-cache! r c))))

;; Not currently needed.  Also not tested and not thread-safe
;; (define (invalidate-id-cache!)
;;   (set-repository-id-cache! (current-repository) (make-invalid-id-cache)))

;; Return a list of sorted IDs as strings, suitable for regex node matching.
;; Construction is lazy because it is not that cheap.
(define (id-cache-ids c)
  (force (%id-cache-ids c)))
;; This one's pretty expensive (time and space wise).
(define (id-cache-paths c)
  (force (%id-cache-paths c)))

;;; ID search

;; Returns list of nodes matching identifier ID.
;; ID may be a symbol or string.
(define (match-nodes/id id)
  (define (lookup id)
    (id-cache-ref (current-id-cache) id))
  (validate-id-cache! (current-repository))
  (let ((id (if (string? id) (string->symbol id) id)))
    (map (lambda (x)
           (lookup-node (append x (list id))))
         (lookup id))))

(define (vector-filter-map f v)
  ;; filter-map vector V to list.  this is here because
  ;; we converted the id-cache-ids to a vector.
  (let ((len (vector-length v)))
    (let lp ((i 0) (L '()))
      (if (fx>= i len)
          (reverse L)
          (lp (fx+ i 1)
              (cond ((f i (vector-ref v i))
                     => (lambda (x) (cons x L)))
                    (else
                     L)))))))

;; Returns list of nodes whose identifiers
;; match regex RE.
(define (match-nodes/re re)
  (let ((rx (irregex re)))
    (validate-id-cache! (current-repository))
    (append-map (lambda (id)
                  (match-nodes id))
                (vector-filter-map (lambda (i k)   ; was filter-map
                                     (and (string-search rx k) k))
                                   (id-cache-ids (current-id-cache))))))

;; Match against full node paths with RE.
(define (match-node-paths/re re)
  (let ((rx (irregex re)))
    (validate-id-cache! (current-repository))
    (map (lambda (path)
           (lookup-node (string-split path))) ; stupid resplit
         (filter-map (lambda (k)
                       (and (string-search rx k) k))
                     (id-cache-paths (current-id-cache))))))

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
;; after id-cache-ids cache
;; ,t (match-nodes (irregex "posix"))
;;    0.036 seconds elapsed
;;        0 seconds in (major) GC
;;     9642 mutations
;;       83 minor GCs
;;        0 major GCs
;; ,t (match-nodes (irregex "."))
;;    1.978 seconds elapsed           ; actually about 10-15 seconds on disk
;;    0.057 seconds in (major) GC
;;   147205 mutations
;;      404 minor GCs
;;        4 major GCs
;; time chicken-doc -m . >/dev/null    ; presuming totally warm disk cache
;; real    0m0.960s
;; ,t (match-nodes (irregex "."))
;;    0.321 seconds                    ; if metadata read is delayed, but dir checked
;;    0.133 seconds                    ; if metadata read delayed and dir not checked
;;    0.250 seconds                    ; if metadata read delayed and dir not checked, but path->pathname still computed



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

;; Internal; make a fake repository object containing
;; all the fields a valid object would have.
(define (make-repository-placeholder base)
  (make-repository base
                   (make-pathname base "root")
                   (make-pathname base ".chicken-doc-repo")
                   `((version . ,+repository-version+))
                   (make-invalid-id-cache base)))

;; Open repository and return new repository object or
;; throw error if nonexistent or format failure.
(define (open-repository base)
  (let ((rp (make-repository-placeholder base)))
    (let ((magic (repository-magic rp)))
      (if (file-exists? magic)
          (let ((info (with-input-from-file magic read)))
            (let ((version (or (alist-ref 'version info) 0)))
              (cond ((= version +repository-version+)
                     (let ((r (make-repository (repository-base rp)
                                               (repository-root rp)
                                               magic
                                               info
                                               (repository-id-cache rp))))
                       (set-finalizer! r close-repository)
                       r))
                    (else (error "Invalid repo version number ~a, expected ~a\n"
                                 version +repository-version+)))))
          (error "No chicken-doc repository found at " base)))))
(define (close-repository r)
  (void))

;; Last modification time of entire repository.  We just use the mtime
;; of the id cache, as update operations do not modify any global timestamp.
;; This means stale data may be returned until the cache is refreshed.
(define (repository-modification-time r)
  (validate-id-cache! r)  ;; may be wasteful, but we need the current mtime
  (id-cache-mtime (repository-id-cache r)))

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

