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
  (let ((wrap (and wrap (max wrap 0)))
        (list-depth (make-parameter 0))
        (flatten (lambda (frags) (with-output-to-string (lambda () (SRV:send-reply frags)))))
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
            (ul *preorder* .
                ,(lambda (tag . items)
                   (parameterize ((list-depth (+ (list-depth) 1)))
                     `(#\newline
                       ,(pre-post-order
                         items
                         (let* ((prefix (string-append
                                         (make-string (* 2 (- (list-depth) 1))
                                                      #\space)
                                         "* "))
                                (split-first-line ; return (line1 . rest) or (line1 . #f)
                                 (lambda (s)
                                   (cond ((string-index s #\newline)
                                          => (lambda (i) (cons (substring s 0 i)
                                                          (substring s (+ i 1)))))
                                         (else (cons s #f)))))
                                )
                           `((li
                              . ,(lambda (tag . items)
                                   (if (not wrap)
                                       `(,prefix ,items #\newline)
                                       (match
                                        (split-first-line (flatten items))
                                        ((line1 . rest)
                                         ;; Explicit with-width works around a bug(?)
                                         ;; where the second column does not expand to
                                         ;; fill the available width.
                                         (let ((plen (string-length prefix)))
                                           `(,(fmt #f (columnar plen (dsp prefix)
                                                                (with-width
                                                                 (- wrap plen)
                                                                 (wrap-lines line1))))
                                             ,rest))))
)))
                             . ,ss ; actually, other block elts should be disallowed in UL
                             )))))))
            (ol *preorder* .
                ,(lambda (tag . items)
                   (parameterize ((list-depth (+ (list-depth) 1)))
                     (pre-post-order items
                                     `((li . ,(lambda (tag . items)
                                                `(#\newline
                                                  ,(make-string (* 2 (- (list-depth) 1))
                                                                #\space)
                                                  "1. "
                                                  ,items)))
                                       . ,ss)))))
            (dl ((dt . ,(lambda (tag . body)
                          `(#\newline "- " ,body ": ")))
                 (dd . ,(lambda (tag . body)
                          body)))
                . ,(lambda (tag . body)
                     body))
            
            (p . ,(lambda (tag . body)
                    (let ((str (flatten body)))  ; FIXME remove if no wrap
                      `(#\newline
                        ,(if wrap
                             (if (string=? str "")
                                 '() ; work around for bug in (wrap-lines "")
                                 (fmt #f (with-width wrap (wrap-lines str))))
                             (list str #\newline)))))) ; need extra NL if no wrap-lines
            (pre . ,(lambda (tag . body)
                      `(#\newline "    "  ; dumb
                        ,(string-intersperse (string-split (flatten body) "\n" #t)
                                             "\n    ")
                        #\newline ; hmm
                        )
                      ))
            (script . ,(lambda (tag lang . body)
                         ;; use PRE output for now; ignore LANG
                         `(#\newline "    "  ; dumb
                           ,(string-intersperse (string-split (flatten body) "\n" #t)
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
            (b . ,(lambda (tag . body) `("*" ,body "*")))
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

(use posix)
(define (wrap-at)
  (let-values (((rows cols) (terminal-size (current-input-port))))
    (if (= cols 0)
        78
        (inexact->exact (truncate (* cols 0.95))))))

(define (display-sxml-as-text doc)
  (SRV:send-reply
   (pre-post-order doc
                   (make-text-stylesheet doc wrap: (wrap-at)))))
