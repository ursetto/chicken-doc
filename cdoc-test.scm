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
                 (section 1))
        (cond ((eof-object? line)
               #f)
              ((tag-line line) =>
               (match-lambda ((type sig id)
                         (print "type: " type " sig: " sig
                                " id: " id)
                         (loop (read-line) section))))
              ((section-line line) =>
               (match-lambda ((num title)
                         (print "section: " num " title: " title)
                         (loop (read-line) section))))
              (else
               (loop (read-line) section))
              )))))


;; (check-line "<procedure>(abc def)</procedure>")
;; (check-all "~/scheme/chicken-wiki/eggref/4/sql-de-lite")
;; (check-all "~/scheme/chicken-wiki/man/4/Unit posix")
;; (signature->identifier-string "(prepared-cache-size n" 'procedure)
