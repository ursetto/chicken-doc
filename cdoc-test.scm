;;; cdoc-test

(use matchable)
(use irregex)

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
                          (warning "Skipped writing tag for signature" sig))))
            (reverse tags)))

(define (check-all fn path)
  (with-input-from-file fn
    (lambda ()
      (let loop ((line (read-line))
                 (section 1)
                 (tag? #f)   ; remove in favor of (pair? tags)
                 (tags '())
                 (tag-body '())
                 (where 'section))
        (cond ((eof-object? line)
               (when tag?
                 (write-tags tags tag-body path))
               #f)
              ((tag-line line) =>
               ;; FIXME: Existing tag should be terminated unless the last line
               ;; was also a tag.
               (match-lambda ((type sig id)
                         ;;                          (print "type: " type " sig: " sig
                         ;;                                 " id: " id)
                         (cond ((eq? where 'tag-header)
                                (loop (read-line) section #t (cons (list type sig id)
                                                                   tags)
                                      tag-body
                                      where))
                               (else
                                (when tag?
                                  (write-tags tags tag-body path))
                                (loop (read-line) section #t (cons (list type sig id)
                                                                   '())
                                      '()
                                      'tag-header))))))
              ((section-line line) =>
               (match-lambda ((num title)
                         ;; (print "section: " num " title: " title)
                         (cond (tag?
                                (write-tags tags tag-body path)
                                (loop (read-line) section #f '() '() 'section))
                               (else
                                (loop (read-line) section tag? tags tag-body 'section))))))
              (else
               (if tag?
                   (loop (read-line) section tag? tags (cons line tag-body) 'line)
                   (loop (read-line) section tag? tags tag-body 'line)))
              )))))


;; (check-line "<procedure>(abc def)</procedure>")
;; (check-all "~/scheme/chicken-wiki/eggref/4/sql-de-lite" (list "sql-de-lite"))
;; (check-all "~/scheme/cdoc/sql-de-lite.wiki")
;; (check-all "~/scheme/chicken-wiki/man/4/Unit posix" (list "posix"))
;; (signature->identifier "(prepared-cache-size n" 'procedure)

(define cdoc-root (make-parameter "~/tmp/cdoc/root"))
(define +rx:%escape+ (irregex "[%/,]"))
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
             'egg name name '(".")))
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
(define (list-keys name)  ;; Test: list keys (directories) under pathname
  (filter (lambda (x) (not (eqv? (string-ref x 0) #\,)))
          (directory (make-pathname (list (cdoc-root) name) #f))))
(define (describe name)   ;; Test: print ,text and ,meta data for pathname
  (let* ((name (if (pair? name) name (list name)))
         (name (map ->string name))
         (pathname (make-pathname (cons (cdoc-root) name) #f))
         (textfile (make-pathname pathname ",text"))
         (metafile (make-pathname pathname ",meta")))
    (cond ((and (directory? pathname)
                (regular-file? textfile)
                (regular-file? metafile))
           (let ((metadata (with-input-from-file metafile read-file)))
             (printf "~a: ~a\n"
                     (cadr (assq 'type metadata))
                     (cadr (assq 'signature metadata)))
             (with-input-from-file textfile
               (lambda ()
                 (for-each-line (lambda (x) (display x) (newline)))))))
          (else
           (error "No such identifier" name)))))
(define (refresh-eggs)
  (for-each (lambda (x) (print x) (parse-egg x)) (directory +eggdir+)))

