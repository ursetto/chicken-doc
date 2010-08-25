(module chicken-doc-text (write-sxml-as-text)

(import scheme chicken)
(use fmt)
(use sxml-transforms)
(use matchable)
(use data-structures srfi-13 ports)
(require-library srfi-1)
(import (only srfi-1 filter-map reduce make-list))

(define +identifier-tags+
  (map string->symbol
       (list "procedure" "macro" "read" "parameter"
             "record" "string" "class" "method" "constant")))

(define walk pre-post-order)

;; sxml-transforms does not allow us to pass state around so we
;; use parameters and preorder traversal; it also does not let
;; us obtain the current stylesheet bindings, so we must approximate
;; them with a letrec
(define (make-text-stylesheet doc #!key (wrap 78))
  (define (flatten-frags frags)
    (with-output-to-string (lambda () (SRV:send-reply frags))))
  (define (indent-and-wrap-with-bullet indent wrap prefix items)
    ;; we could have caller combine indent and prefix into prefix
    (define (split-first-line s)
      (cond ((string-index s #\newline)
             => (lambda (i) (cons (substring s 0 i)
                             (substring s (+ i 1)))))
            (else (cons s #f))))
    (let* ((prefix (string-append (make-string indent #\space) prefix)))
      (if (not wrap)
          `(,prefix ,items #\newline)
          (match
           ;; split required to extract indented lists in items ("rest") -- rethink this?
           (split-first-line (flatten-frags items))
           ((line1 . rest)
            `(,(fmt #f (with-width
                        wrap
                        (columnar (string-length prefix) (dsp prefix)
                                  (wrap-lines line1))))
              ,rest))))))
  (define (extract-dl-items dl)  ; returns ( (term . defs) ...)
    (let loop ((dl dl)
               (L '())
               (dt #f)    ; the unknown term
               (dd '()))
      (if (null? dl)
          (if dt
              (let ((L (cons (cons dt (reverse dd)) L))) ; remaining dt
                (reverse L))
              '())
          (match (car dl)
                 (('dt . term)
                  (if dt
                      (loop (cdr dl) (cons (cons dt (reverse dd)) L) term '())
                      (loop (cdr dl) L term '())))    ; skip until first dt
                 (('dd . def)
                  (loop (cdr dl) L dt (cons def dd)))))))
  (define (extract-table-items table cell-ss)
    ;; returns ( (td td ...) (td td ...) )
    ;; with TD flattened into strings and wrapped
    (let ((cell-ss `((@ *preorder* . ,drop-tag)
                     . ,cell-ss)))
      (filter-map (match-lambda (('tr . tds)
                            (filter-map
                             (match-lambda (('td . body)
                                       (flatten-frags (pre-post-order body cell-ss)))
                                      ;; we don't pass the "th" identity back, so we can't
                                      ;; do further processing, such as centering
                                      (('th . body)
                                       (string-upcase
                                        (flatten-frags (pre-post-order body cell-ss))))
                                      (else #f))
                             tds))
                           (else #f))   ; usu. whitespace and (@ ...)
                  table)))
  ;; special formatter for table top/bottom
  (define (fill char) (lambda (st) ((cat (make-string (fmt-width st) char)) st)))
  ;(define (fill char) (lambda (st) ((pad-char char (pad/both (fmt-width st))) st)))
  (define (drop-tag . x) '())
                           
  (let* ((wrap (and wrap (not (zero? wrap)) (max wrap 0)))
         (list-indent (make-parameter 2))
         (hr-glyph (if wrap (make-string wrap #\-) "--------")))
    (letrec
        ((default-elts
          `((*text* . ,(lambda (tag text) text))
            (*default* . ,(lambda (tag . body) (warning "dropped" (cons tag body)) '()))))
         (inline-elts
          `((b . ,(lambda (tag . body) `("_" ,body "_")))
            (i . ,(lambda (tag . body) `("/" ,body "/")))
            (tt . ,(lambda (tag . body) `("`" ,body "`")))
            (sub . ,(lambda (tag . body) body))
            (sup . ,(lambda (tag . body) body))
            (big . ,(lambda (tag . body) body))
            (small . ,(lambda (tag . body) body))

            (link . ,(lambda (tag href #!optional (desc #f))
                       (if desc
                           `(,desc #\space #\( ,href #\))
                           href)))
            (int-link . ,(lambda (tag href #!optional (desc #f))
                           (or desc
                               href ;; `(#\[ ,href #\])
                               )))))
         (block-elts
          `((section . ,(lambda (tag level name . body)
                          `(#\newline
                            ,(case level
                               ((2) "==")
                               ((3) "===")
                               ((4) "====")
                               ((5) "=====")
                               ((6) "======")
                               (else "======"))
                            " " ,name #\newline ,body)))
            ,@(let ((parse-LIs
                     (lambda (items prefix)
                       (let ((index 0))
                         `(#\newline
                           ,(pre-post-order
                             items
                             `((li *preorder* .
                                   ,(lambda (tag . items)
                                      (set! index (+ index 1)) ; grr
                                      (let ((p (prefix index))
                                            (i (list-indent)))
                                        (indent-and-wrap-with-bullet
                                         i wrap p
                                         (parameterize ((list-indent (+ i (string-length p))))
                                           ;; NB the transformer won't correctly handle
                                           ;; block elements other than nested lists
                                           (pre-post-order items ss)))))))))))))
                `((ul *preorder* .
                      ,(lambda (tag . items)
                         (parse-LIs items (lambda (i) "* "))))
                  (ol *preorder* .
                      ,(lambda (tag . items)
                         (parse-LIs items (lambda (i) (string-append
                                                 (number->string i) ". ")))))))

            (dl *preorder* .
                ,(lambda (tag . items)
                   `(#\newline
                     ,(map (match-lambda
                            ((term . defs)
                             (let ((prefix (string-append
                                            "- " (flatten-frags (pre-post-order term inline-ss))
                                            ": ")))
                                       (indent-and-wrap-with-bullet
                                        (list-indent) wrap prefix
                                        ; FIXME: multiple defs should be displayed separately
                                        (pre-post-order defs inline-ss)))))
                           (extract-dl-items items)))))
            
            (dl ((dt . ,(lambda (tag . body)
                          `(#\newline "- " ,body ": ")))
                 (dd . ,(lambda (tag . body)
                          body)))
                . ,(lambda (tag . body)
                     body))
            
            (p . ,(lambda (tag . body)
                    (let ((str (flatten-frags body)))  ; FIXME remove if no wrap
                      `(#\newline
                        ,(if wrap
                             (fmt #f (with-width wrap (wrap-lines str)))
                             (list str #\newline)))))) ; need extra NL if no wrap-lines
            (pre . ,(lambda (tag . body)
                      `(#\newline "    "  ; dumb
                        ,(string-intersperse (string-split (flatten-frags body) "\n" #t)
                                             "\n    ")
                        #\newline ; hmm
                        )
                      ))
            (highlight . ,(lambda (tag lang . body)
                            ;; use PRE output for now; ignore LANG
                            `(#\newline "    " ; dumb
                              ,(string-intersperse (string-split (flatten-frags body) "\n" #t)
                                                   "\n    ")
                              #\newline ; hmm
                              )
                            ))

            (def ((sig *preorder*
                       . ,(lambda (tag . sigs)
                            (list (map (lambda (s)
                                         (match s
                                                ((type sig . alist)
                                                 ;; don't bother filtering by valid types
                                                 `(#\newline "-- " ,type ": "
                                                   ,(walk sig inline-ss)))))
                                       sigs)
                                  #\newline))))
                 . ,(lambda (tag . body) body))

            
            (hr .
                ,(lambda (tag)
                   `(#\newline ,hr-glyph #\newline)))

            ;; (fmt #f (columnar "| " (wrap-lines col1) " | " (wrap-lines col2)
            ;;                   " | " (wrap-lines col3) " |"))
            
            (table *preorder* .
                   ,(lambda (tag . elts)
                      ;; Using columnar essentially requires that wrapping is enabled,
                      ;; so if not, set it to 76 just for tables. *FIXME*
                      (let* ((wrap (if (or (not wrap) (zero? wrap)) 76 wrap))
                             (rows (extract-table-items elts inline-ss))
                             (ncol (reduce max 0 (map length rows)))
                             (sep (fmt #f (with-width
                                           wrap
                                           (apply columnar
                                                  `(" +-"
                                                    ,@(intersperse
                                                       (make-list ncol (fill #\-))
                                                       "-+-")
                                                    "-+"))))))
                        (list
                         #\newline
                         ;;sep
                         (map (lambda (row)
                                ;; first pad row to uniform number of columns
                                (let* ((len (length row))
                                       (row (if (> ncol len)
                                                (append row (make-list (- ncol len) ""))
                                                row)))
                                  (list
                                   sep
                                   (fmt #f (with-width
                                            wrap
                                            (apply columnar
                                                   `(" | "
                                                     ,@(intersperse
                                                        (map wrap-lines row) " | ")
                                                     " |")))))))
                              rows)
                         sep))))

            (blockquote
             . ,(lambda (tag . body)
                  (let ((str (flatten-frags body)))
                    `(#\newline
                      ,(if wrap
                           (fmt #f (with-width wrap (columnar "  > " (wrap-lines str))))
                           (list "  > " str #\newline))))))

            ;; (examples (example (expr ...) (result ...)) ...) => (pre ...)
            ;; Some extraneous NLs are deleted, not all; newline output is crappy
            ;; (init ...) clause ignored
            (examples *preorder*
                      . ,(lambda (tag . body)
                           (pre-post-order
                            body
                            `((example *preorder*
                                       . ,(lambda (tag . body)
                                            (pre-post-order
                                             `(pre .
                                                   ,(pre-post-order
                                                     body
                                                     `((init *preorder*
                                                             . ,(lambda (tag . body)
                                                                  `(,body #\newline)))
                                                       (expr *preorder*
                                                             . ,(lambda (tag . body)
                                                                  body))
                                                       (result *preorder*
                                                               . ,(lambda (tag . body)
                                                                    `("\n; Result: " ,body)))
                                                       (*default* . ,drop-tag))))
                                             ss)))
                              (*default* . ,drop-tag)))))
            
            (tags *preorder* . ,drop-tag)
            (toc *preorder* . ,drop-tag)))

         (ss `(,@block-elts
               ,@inline-elts
               ,@default-elts))
         (inline-ss `(,@inline-elts
                      ,@default-elts)))

      ss)))

(define (write-sxml-as-text doc wrap-col)
  (SRV:send-reply
   (pre-post-order doc
                   (make-text-stylesheet doc wrap: wrap-col))))

)
