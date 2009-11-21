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

;; Convert signature (usually a list or bare identifier) into an identifier
;; At the moment, this just means taking the car of a list if it's a list,
;; and if it cannot be read as a scheme expression, fail
(define (signature->identifier-string sig type)
  (condition-case
   (let ((L (with-input-from-string sig read)))
     (cond ((pair? L) (car L))
           (else sig)))
   ((exn)
    (warning "Could not parse signature" sig)
    #f)))

(define (check-line line)
  (match (string-search +rx:tag+ line)
         ((_ type signature)
          (print "type: " type " sig: " signature
                 " id: " (signature->identifier-string signature type)))
         (#f #f)))

(define (check-all fn)
  (with-input-from-file fn
    (lambda ()
      (let loop ((line (read-line)))
        (cond ((eof-object? line)
               #f)
              (else
               (check-line line)
               (loop (read-line))))))))


;; (check-line "<procedure>(abc def)</procedure>")
;; (check-all "~/scheme/chicken-wiki/eggref/4/sql-de-lite")
;; (check-all "~/scheme/chicken-wiki/man/4/Unit posix")
;; (signature->identifier-string "(prepared-cache-size n" 'procedure)
