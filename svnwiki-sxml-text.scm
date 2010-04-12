(use fmt)
(use sxml-transforms)
(use matchable)
(use ports) ;with-output-to-string

(define +identifier-tags+
  (map string->symbol
       (list "procedure" "macro" "read" "parameter"
             "record" "string" "class" "method" "constant")))

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
            ;; Explicit with-width works around a bug(?) where the
            ;; second column does not expand to fill the available
            ;; width.
            (if (string=? "" line1)
                `(,prefix ,items #\newline)   ; avoid fmt BUG: error on (wrap-lines "")
                (let ((plen (string-length prefix)))
                  `(,(fmt #f (columnar plen (dsp prefix)
                                       (with-width
                                        (- wrap plen)
                                        (wrap-lines line1))))
                    ,rest))))))))
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
                           
  (let ((wrap (and wrap (not (zero? wrap)) (max wrap 0)))
        (list-indent (make-parameter 0))
        (drop-tag (lambda x '())))
    (letrec
        ((ss 
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
                                            "- " (flatten-frags (pre-post-order term ss))
                                            ": ")))
                                       (indent-and-wrap-with-bullet
                                        2 wrap prefix
                                        ; FIXME: multiple defs should be displayed separately
                                        (pre-post-order defs ss)))))
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
                             (if (string=? str "")
                                 '() ; work around for bug in (wrap-lines "")
                                 (fmt #f (with-width wrap (wrap-lines str))))
                             (list str #\newline)))))) ; need extra NL if no wrap-lines
            (pre . ,(lambda (tag . body)
                      `(#\newline "    "  ; dumb
                        ,(string-intersperse (string-split (flatten-frags body) "\n" #t)
                                             "\n    ")
                        #\newline ; hmm
                        )
                      ))
            (script . ,(lambda (tag lang . body)
                         ;; use PRE output for now; ignore LANG
                         `(#\newline "    "  ; dumb
                           ,(string-intersperse (string-split (flatten-frags body) "\n" #t)
                                                "\n    ")
                           #\newline ; hmm
                           )
                         ))

            (def ((sig ,(let ((sign (lambda (tag sig) `(#\newline "-- " ,tag ": " ,sig))))
                          (map (lambda (x) `(,x . ,sign))
                               +identifier-tags+))
                       . ,(lambda (tag . body) (list body #\newline))))
                 . ,(lambda (tag . body) body))
            (tags . ,drop-tag)
            (toc . ,drop-tag)
            
;;; inline elts
            (b . ,(lambda (tag . body) `("_" ,body "_")))
            (i . ,(lambda (tag . body) `("/" ,body "/")))
            (tt . ,(lambda (tag . body) `("`" ,body "`")))

            (link . ,(lambda (tag href #!optional (desc #f))
                       (if desc
                           `(,desc #\space #\( ,href #\))
                           href)))
            (int-link . ,(lambda (tag href #!optional (desc #f))
                           (or desc
                               href ;; `(#\[ ,href #\])
                               )))
;;; defaults
            
            (*text* . ,(lambda (tag text) text))
            (*default* . ,(lambda (tag . body) (warning "dropped" tag) '()))
            )))
      ss)))

(define (display-sxml-as-text doc wrap-col)
  (SRV:send-reply
   (pre-post-order doc
                   (make-text-stylesheet doc wrap: wrap-col))))
