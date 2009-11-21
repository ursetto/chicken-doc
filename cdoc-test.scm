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

(define (check-all fn)
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
                 (printf "tags: ~S\n" tags)
                 (printf "tag-body: ~A\n" (string-concatenate-reverse
                                           (intersperse tag-body "\n"))))
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
                                      (cons line tag-body)
                                      where))
                               (else
                                (when tag?
                                  (printf "tags: ~S\n" tags)
                                  (printf "tag-body: ~A\n" (string-concatenate-reverse
                                                            (intersperse tag-body "\n"))))
                                (loop (read-line) section #t (cons (list type sig id)
                                                                   '())
                                      (cons line '())
                                      'tag-header))))))
              ((section-line line) =>
               (match-lambda ((num title)
                         ;; (print "section: " num " title: " title)
                         (cond (tag?
                                (printf "tags: ~S\n" tags)
                                (printf "tag-body: ~A\n" (string-concatenate-reverse
                                                          (intersperse tag-body "\n")))
                                (loop (read-line) section #f '() '() 'section))
                               (else
                                (loop (read-line) section tag? tags tag-body 'section))))))
              (else
               (if tag?
                   (loop (read-line) section tag? tags (cons line tag-body) 'line)
                   (loop (read-line) section tag? tags tag-body 'line)))
              )))))


;; (check-line "<procedure>(abc def)</procedure>")
;; (check-all "~/scheme/chicken-wiki/eggref/4/sql-de-lite")
;; (check-all "~/scheme/cdoc/sql-de-lite.wiki")
;; (check-all "~/scheme/chicken-wiki/man/4/Unit posix")
;; (signature->identifier-string "(prepared-cache-size n" 'procedure)
