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
 repository-information repository-root open-repository*
 repository-magic +repository-version+
 repository-id-cache set-repository-id-cache!
 path->keys keys->pathname field-filename keys+field->pathname key->id
 make-id-cache id-cache-filename id-cache-table validate-id-cache!
 make-repository-placeholder
 repository-modification-time
;; Node API
 lookup-node
 match-nodes
 match-node-paths/re
 match-ids/prefix
 match-paths/prefix
 node-signature
 node-type
 node-sxml
 node-path
 node-id
 node-timestamp
 node-children
 node-child
 node-child-ids
 node-definition-ids    ;experimental
 node-definition-id?    ;experimental
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

;; Split path STR at #.  However, keep any #+ prefix in the first segment.
;; In other words, sys#foo#bar -> ("sys" "foo" "bar")
;; but ##sys#foo#bar -> ("##sys" "foo" "bar")
;; and #u8 -> ("#u8").  Allows # read syntax and internal namespaces.
(define (decompose-qualified-path path)
  (let ((str (if (symbol? path) (symbol->string path) path)))
    (cond ((string=? str "") '())  ;; string-skip returns #f for ""
          ((string-skip str #\#)
           => (lambda (i)
                (if (= i 0)
                    (string-split str "#")
                    (let ((S (string-split (substring str i) "#")))
                      (cons (string-append (substring str 0 i)
                                           (car S))
                            (cdr S))))))
          (else str)   ;; all #s
          )))

;;; Access

(define-record-type chicken-doc-node
  (%make-node path id md pathname definfo)
  node?
  (path node-path)            ; includes ID
  (id node-id)
  (md node-md)
  (pathname node-pathname)    ; internal; cached node pathname
  (definfo %node-definfo)    ; internal; node definitions record
  )

(define (node-definfo n)
  (force (%node-definfo n)))

(define-record-type chicken-doc-node-definfo
  (make-node-definfo index start pathname)
  node-definfo?
  (index node-definfo-index)
  (start node-definfo-start)
  (pathname node-definfo-pathname))

(define (make-empty-node-definfo)
  (make-node-definfo #f 0 #f))
(define (node-definfo-keys D)
  (let ((I (node-definfo-index D)))
    (if I
        (hash-table-keys (node-definfo-index D))
        '())))
(define (node-definfo-offset D id)
  (car (hash-table-ref/default (node-definfo-index D) id '(#f))))
(define (node-definfo-sxml D id)
  (cond ((node-definfo-offset D id)
         => (lambda (o)
              (let ((pos (+ o (node-definfo-start D))))
                (call-with-input-file*
                 (node-definfo-pathname D)
                 (lambda (p)
                   (set-file-position! p pos seek/set)
                   (read p))))))
        (else #f)))

(define (make-node path id pathname)
  (%make-node path id
              (delay (read-path-metadata pathname))
              pathname
              (delay (read-definfo pathname))))

;; Return string list of child keys (directories) directly under PATH, or #f
;; if the PATH is invalid.  FIXME: might return '() if PATH indicates a
;; definition node (or if that is otherwise indicated).

(define (node-child-keys node)
  (let ((dir (node-pathname node)))
    (if (directory? dir)
        (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))  ;; Contains hardcoded ,
                (sort (directory dir) string<?))
        '())))

(define (node-children node)   ;; FIXME: inefficient for definition children.
  (map (lambda (id) (node-child node id))
       (node-child-ids node)))

;; Returns child node of NODE with id ID, or #f if not found.
(define (node-child node id)
  (let ((path (node-path node))
        (pathname (node-pathname node)))
    (let ((child-path (append path (list id)))
          (idstr (->string id)))
      (if (node-definition-id? node idstr)
          (make-definition-node node child-path idstr)
          (let ((child-pathname (make-pathname pathname (id->key id))))  ;; otherwise regular node
            (and (directory? child-pathname)
                 (make-node child-path id child-pathname)))))))

;; Shortcut if you only need identifiers for node children.
;; Might be faster than node-children.
(define (node-child-ids node)
  (append (map key->id (node-child-keys node))
          (node-definition-ids node)))

;; (define (node-definition-ids node)
;;   (let next-def ((defs (node-definitions node))
;;                  (ids '()))
;;     (if (null? defs)
;;         ids
;;         (let next-sig ((sigs (cdadr (car defs))) (sigids '()))
;;           (if (null? sigs)
;;               (next-def (cdr defs)
;;                         (append sigids ids))
;;               (let* ((x (car sigs)) (type (car x)) (sig (cadr x)) (alist (cddr x)))
;;                 (next-sig
;;                  (cdr sigs)
;;                  (cons
;;                   (cond ((assq 'id alist) => cadr) ;; Check for pre-parsed ID.
;;                         (else
;;                          (error 'node-definition-ids "preparsed ID unavailable")))
;;                   sigids))))))))

(define (node-definition-ids node)
  (sort (node-definfo-keys (node-definfo node))
        string<?))
(define (node-definition-id? node id)
  (and-let* ((D (node-definfo node))       ;; check if this node is in the definition index
             (I (node-definfo-index D)))
    (hash-table-exists? I (->string id))))

(define (make-definition-node parent path id)
  (define (find-sig def id)
    (let ((sigs (cdadr def)))
      (find (lambda (s) (cond ((assq 'id (cddr s))
                          => (lambda (idc) (equal? id (->string (cadr idc)))))
                         (else #f)))
            sigs)))
  (define (get-definition-sxml parent id)
    (node-definfo-sxml (node-definfo parent) id))
  (define (definition-sxml->metadata sxml parent id)
    (let ((s (find-sig sxml id)))
      (if s
          (let ((type (car s)) (signature (cadr s)))
            `((type ,type)
              (signature ,signature)
              (timestamp ,(node-timestamp parent))
              (sxml ,sxml)))
          (error 'definition-sxml->metadata "no match for id in signature" id))))
  (%make-node path id
              (definition-sxml->metadata (get-definition-sxml parent id) parent id)
              (pathname+field->pathname (node-pathname parent) 'defs) ;; tmp -- non-dir indicates def node
              (make-empty-node-definfo)))

;; Obtain metadata alist at node at PATHNAME.  Valid node without metadata record
;; returns '().  Invalid node throws error.
(define (read-path-metadata pathname)
  (let ((metafile (pathname+field->pathname pathname 'meta)))
    (cond ((file-exists? metafile)
           (read-file metafile))
          ((directory? pathname)
           ;; write-keys may create intermediate container directories
           ;; without metadata, so handle this specially.
           '())
          (else
           (error "No such metadata pathname" metafile) ;; internal error
           ))))

(define (call-with-input-file* file proc)
  (let ((p (open-input-file file)))
    (handle-exceptions exn (begin (close-input-port p)
                                  (signal exn))
      (let ((rc (proc p)))
        (close-input-port p)
        rc))))
(define (read-definfo pathname)
  (let ((deffile (pathname+field->pathname pathname 'defs)))
    (cond ((file-exists? deffile)
           (call-with-input-file* deffile
             (lambda (p)
               (let ((index (read p)))
                 (unless (and (pair? index)
                              (eq? (car index) 'index))
                   (error "Invalid file format in definition index"))
                 (make-node-definfo (alist->hash-table (cdr index) string=?)
                                    (file-position p) deffile)))))
          (else
           (make-empty-node-definfo)))))

(define (node-metadata-field node field)
  (cond ((assq field (node-metadata node))
         => cadr)
        (else #f)))

(define (node-metadata node)
  (force (node-md node)))         ;  load metadata as needed

(define (node-definitions node)
  (force (node-metadata-field node 'defs)))

;; Return node record at PATH or throw error if the record does
;; not exist.  It would be acceptable to return #f on failure,
;; like node-child does; the only reason we don't is to
;; signal to the caller which part of the path lookup failed,
;; which is perhaps not that useful.
(define (lookup-node path)
  (define (make-root-node)
    (make-node '() "" (keys->pathname '())))
  (let loop ((node (make-root-node))
             (P path))
    (if (null? P)
        node
        (loop (or (node-child node (car P))
                  (error 'lookup-node "node path not found"
                         `(,@(node-path node) ,(car P))))
              (cdr P)))))

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

;; Return SXML document for node.  If small enough, it may be packed
;; into the metadata; otherwise read it from the filesystem.
(define (node-sxml node)
  (or (node-metadata-field node 'sxml)  ;; FIXME? Returns #f on (sxml #f)
      (let* ((file (pathname+field->pathname (node-pathname node)
                                             'sxml)))
        (and (file-exists? file)
             (with-input-from-file file read)))))

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

;; Return timestamp of a node in seconds since UNIX epoch,
;; or #f if no timestamp was available. (Should we return 0 in that case?)
(define (node-timestamp node)
  (node-metadata-field node 'timestamp))

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
                  paths ; path string vector
                  )
  id-cache?
  (table id-cache-table)
  (mtime id-cache-mtime)
  (filename id-cache-filename)
  (ids %id-cache-ids)
  (paths %id-cache-paths))

;; Delayed construction of id string list and paths is legal
;; because cache updates are disallowed.  Note that any
;; change to the id cache on disk will result in revalidation
;; and full recomputation of the delayed constructors.
(define (make-id-cache table mtime filename)
  (%make-id-cache table mtime filename
                  (delay (list->vector
                          (sort (map ->string (hash-table-keys table))
                                ;; ->string not symbol->string to workaround WRITE bug for integer symbols
                                string<?)))
                  (delay (list->vector
                          (sort
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
                           string<?)))))

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
(define (vector-copy v #!optional (start 0) (end (vector-length v)) (fill (void)))
  ;; SRFI-43 vector-copy.  Why is vector-lib's implementation so verbose?
  (let ((len (vector-length v)))
    (when (> start end) (error 'vector-copy "start > end" start end))
    (when (< start 0)   (error 'vector-copy "start < 0" start))
    (when (> start len) (error 'copy-vec "start > len" start len))
    (let ((c (make-vector (- end start))))
      (let ((end (min end len)))
        (let loop ((vi start)
                   (ci 0))
          (cond ((< vi end)
                 (vector-set! c ci (vector-ref v vi))
                 (loop (+ vi 1) (+ ci 1)))
                (else c)))))))


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
         (vector-filter-map (lambda (i k)
                              (and (string-search rx k) k))
                            (id-cache-paths (current-id-cache))))))

;; Search for "nearest" VAL in vector V at start or end of a range.
;; START? is a boolean indicating whether this is the start or end of
;; the range.  INCLUSIVE? is a boolean indicating whether VAL itself
;; should be included in the results.  CMP is a procedure of two
;; arguments x y which returns < 0 if x < y, 0 if x = y, or > 0 if x >
;; y.  Returns a vector index of the nearest value; in "start" mode
;; this is inclusive, in "end" mode it is exclusive.
(define (binary-search-nearest v val cmp start? inclusive?)
  (let ((len (vector-length v)))
    (let lp ((L 0)
             (R len))
      (let ((M (fx/ (fx+ R L) 2)))
        (let ((item (vector-ref v M)))
          ;;(printf "item: ~a L: ~a M: ~a R: ~a\n" item L M R)
          (let ((dir (cmp val item)))
            (cond ((fx= dir 0)
                   (if inclusive?
                       (if start? M (fx+ M 1))
                       (if start? (fx+ M 1) M)))
                  ((fx< dir 0)
                   (if (fx> M L)
                       (lp L M)
                       M))           ; not sure this can happen
                  (else
                   (if (fx< M (- R 1))
                       (lp M R)
                       (fx+ M 1))))))))))

;; Return a vector (??) of identifier name strings or full path
;; strings which match the prefix STR.
(define match-ids/prefix)     ; probably not the best name
(define match-paths/prefix)

(let ()
  (define (strcmp x y)
    (cond ((string<? x y) -1)
          ((string=? x y) 0)
          (else 1)))
  ;; finds strings in v in range [str1,str2) and returns indices [start . end)
  (define (binary-search-range v str1 str2)
    (cons (binary-search-nearest v str1 strcmp #t #t)
          (binary-search-nearest v str2 strcmp #f #f)))
  (define (next-string str)
    ;; ASSUMING BYTE SEMANTICS!
    ;; Note that src char #\xff will fail and wrap around.
    (let ((len (string-length str))
          (new (string-copy str)))
      (string-set! new (- len 1)
                   (integer->char (+ 1 (char->integer
                                        (string-ref str (- len 1))))))
      new))
  (define (match-vector v str limit)
    (if (= 0 (string-length str))
        '#()
        (match (binary-search-range v str (next-string str))
               ((start . end)
                (if limit
                    (vector-copy v start (min (+ start limit) end))
                    (vector-copy v start end))))))

  (set! match-ids/prefix
        (lambda (str #!optional (limit #f))
          (validate-id-cache! (current-repository))
          (match-vector (id-cache-ids (current-id-cache))
                        str limit)))
  (set! match-paths/prefix
        (lambda (str #!optional (limit #f))
          (validate-id-cache! (current-repository))          
          (match-vector (id-cache-paths (current-id-cache))
                        str limit))))

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

(define +repository-version+ 3)

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
  (or (get-environment-variable "CHICKEN_DOC_REPOSITORY")
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
;; throw error if nonexistent or unknown format.  May accept
;; some old repository formats, so this repo MUST only be
;; passed to procedures which explicitly handle old formats.
;; Currently, that is only destroy-repository!.
(define (open-repository* base)
  (let ((rp (make-repository-placeholder base)))
    (let ((magic (repository-magic rp)))
      (if (file-exists? magic)
          (let ((info (with-input-from-file magic read)))
            (let ((r (make-repository (repository-base rp)
                                      (repository-root rp)
                                      magic
                                      info
                                      (repository-id-cache rp))))
              (set-finalizer! r close-repository)
              r))
          (error "No chicken-doc repository found at" base)))))
;; Open repository like open-repository*, but only permit the
;; current repository format.  Unless otherwise stated, procedures can only
;; handle the current format, and rely on the check happening at open time.
(define (open-repository base)
  (let ((r (open-repository* base)))
    (let ((version (or (alist-ref 'version (repository-information r)) 0)))
      (cond ((= version +repository-version+)
             r)
            (else (error (sprintf "Invalid repository version number ~a, expected ~a\n"
                                          version +repository-version+)))))))
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

