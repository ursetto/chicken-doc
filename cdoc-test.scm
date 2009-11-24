;;; cdoc-test

(use matchable)
(use regex)
(use srfi-13)
(use posix)

(define +identifier-tags+
  (list "procedure" "macro" "read" "parameter"
        "record" "string" "class" "method" "constant"))

;; Match tag lines of the form "<procedure>(abc def)</procedure>"
(define +rx:tag+
  (irregex `(: bol
               "<" (submatch (or ,@+identifier-tags+)) ">"
               (submatch (?? (+ any)))
               "</" (backref 1) ">"
               (? "<br>")
               eol)))
(define +rx:section+
  (irregex '(: bol
               (submatch (** 2 5 "="))
               (* space)
               (submatch (+ any))
               eol)))

;; Convert signature (usually a list or bare identifier) into an identifier
;; At the moment, this just means taking the car of a list if it's a list,
;; and if it cannot be read as a scheme expression, fail
(define (signature->identifier sig type)
  (condition-case
   (let ((L (with-input-from-string sig read)))
     (cond ((pair? L) (car L))
           (else sig)))
   ((exn)
    (warning "Could not parse signature" sig)
    #f)))

(define (tag-line line)
  (match (string-search +rx:tag+ line)
         ((_ type signature)
          (list type signature (signature->identifier signature type)))
         (#f #f)))
(define (section-line line)
  (match (string-search +rx:section+ line)
         ((_ num title)
          (list num title))
         (#f #f)))

;; (define (write-tags tags tag-body)
;;    (printf "tags: ~S\n" tags)
;;    (printf "tag-body: ~A\n" (string-concatenate-reverse
;;                              (intersperse tag-body "\n"))))
(define (write-tags tags tag-body path)
  (for-each (match-lambda ((type sig id)
                      (if id
                          (write-key (string-concatenate-reverse
                                      (intersperse tag-body "\n"))
                                     type sig id path)
;;                           (warning "Skipped writing tag for signature" sig)
                          )))
            (reverse tags)))

;; FIXME: Path is a list of directories (because that's what write-tag expects).
;; This is broken, because write-tag does not escape them
(define (check-all fn path)
  (with-input-from-file fn
    (lambda ()
      (let loop ((line (read-line))
                 (section 1)
                 (tags '())
                 (tag-body '())
                 (where 'section))
        (define (tag?) (pair? tags))
        (cond ((eof-object? line)
               (when (tag?)
                 (write-tags tags tag-body path))
               #f)
              ((tag-line line) =>
               (match-lambda ((type sig id)
                         ;; NB Tag signatures are formatted and saved directly in
                         ;; the tag body.  This preserves context (all grouped signatures
                         ;; will appear when one is referenced), especially important
                         ;; for identifiers with multiple valid signatures.
                         (cond ((eq? where 'tag-header)
                                (loop (read-line) section (cons (list type sig id)
                                                                   tags)
                                      (cons (sprintf "~a: ~a" type sig) tag-body)
                                      where))
                               (else
                                (when (tag?)
                                  (write-tags tags tag-body path))
                                (loop (read-line) section (cons (list type sig id)
                                                                   '())
                                      (cons (sprintf "~a: ~a" type sig) '())
                                      'tag-header))))))
              ((section-line line) =>
               (match-lambda ((num title)
                         ;; (print "section: " num " title: " title)
                         (cond ((tag?)
                                (write-tags tags tag-body path)
                                (loop (read-line) section '() '() 'section))
                               (else
                                (loop (read-line) section tags tag-body 'section))))))
              (else
               (if (tag?)
                   (loop (read-line) section tags (cons line tag-body) 'line)
                   (loop (read-line) section tags tag-body 'line)))
              )))))


;; (check-all "~/scheme/chicken-wiki/eggref/4/sql-de-lite" (list "sql-de-lite"))
;; (check-all "~/scheme/cdoc/sql-de-lite.wiki")
;; (check-all "~/scheme/chicken-wiki/man/4/Unit posix" (list "posix"))
;; (signature->identifier "(prepared-cache-size n" 'procedure)
;; (parse-unit "Non-standard macros and special forms" "chicken")
;; (parse-unit "Locations" "chicken")


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

(define (write-eggshell name)
  (write-key "This space intentionally left blank"
             'egg (string-append name " egg") name '(".")))
(define (write-unitshell name id)
  (write-key "This space intentionally left blank"
             'unit name id '(".")))

(define +wikidir+ "~/scheme/chicken-wiki")
(define +eggdir+ (string-append +wikidir+ "/eggref/4"))
(define +mandir+ (string-append +wikidir+ "/man/4"))
(define (parse-egg name)
  (let ((fn (make-pathname +eggdir+ name))
        (path (list name)))
    (write-eggshell name)
    (check-all fn path)))
(define (parse-unit name id)
  (let ((fn (make-pathname +mandir+ name))
        (path (list id)))
    (write-unitshell name id)
    (check-all fn path)))


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
