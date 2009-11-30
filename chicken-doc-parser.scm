(module chicken-doc-parser (parse-and-write-tags/svnwiki)

(import scheme chicken)
(use regex) (import irregex)
(use matchable)
(use extras)
(use ports)

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
(define +rx:code+
  (irregex '(: "{{" (submatch (+ (~ "}")))
               "}}")))
(define +rx:link+
  (irregex '(: "[[" (submatch (+ (~ "]|")))
               "|"
               (submatch (+ (~ "]|")))
               "]]")))
(define +rx:nondescript-link+
  (irregex '(: "[[" (submatch (+ (~ "]")))
               "]]")))
(define +rx:wiki-command+
  (irregex '(: bos "[[" (submatch (+ (~ "]:|")))
               ":"
               (submatch (* (~ "]:|")))
               "]]")))
(define +rx:enscript-either+
  (irregex "</?enscript[^>]*>"))
(define +rx:td-end+ (irregex '(or "</td>" "</th>")))
(define +rx:tr-begin+ (irregex "<tr>"))
(define +rx:deltable+
  (irregex '(or "<td>" "<th>" "</tr>"
                (: "<" (? "/") "table" (* (~ ">")) ">"))))

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

;; Read and parse svnwiki format text file at pathname FN and
;; write all tag groups using (WRITE-TAGS tags tag-body), where TAGS
;; is a list of (type signature identifier) records, and TAG-BODY
;; is a string containing the tag body for this tag group.  Additionally,
;; a transformed wiki document will be written to port PARSED-OUT.
;; (This API must change if the parsing gets any more complex.  We
;;  would probably just return an SXML document.)
(define (parse-and-write-tags/svnwiki fn write-tags parsed-out)
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
                 (write-tags tags tag-body))
               (void))
              ((tag-line line) =>
               (match-lambda ((type sig id)
                         ;; NB Tag signatures are formatted and saved directly in
                         ;; the tag body.  This preserves context (all grouped signatures
                         ;; will appear when one is referenced), especially important
                         ;; for identifiers with multiple valid signatures.
                         (let ((pretty-sig (sprintf "~a: ~a" type sig)))
                           (display pretty-sig parsed-out)
                           (newline parsed-out)
                           (cond ((eq? where 'tag-header)
                                  (loop (read-line) section (cons (list type sig id)
                                                                  tags)
                                        (cons pretty-sig tag-body)
                                        where))
                                 (else
                                  (when (tag?)
                                    (write-tags tags tag-body))
                                  (loop (read-line) section (cons (list type sig id)
                                                                  '())
                                        (cons pretty-sig '())
                                        'tag-header)))))))
              ((section-line line) =>
               (match-lambda ((num title)
                         (display line parsed-out)
                         (newline parsed-out)
                         (cond ((tag?)
                                (write-tags tags tag-body)
                                (loop (read-line) section '() '() 'section))
                               (else
                                (loop (read-line) section tags tag-body 'section))))))
              (else
               ;; Q&D replacements; won't work across line break, but that is rare
               (let* ((line (irregex-replace/all +rx:code+ line "`" 1 "`"))
                      (line (irregex-replace/all +rx:link+ line 2 " (" 1 ")"))
                      (line (irregex-replace/all +rx:wiki-command+ line ""))
                      (line (irregex-replace/all +rx:nondescript-link+ line 1))
                      (line (irregex-replace/all +rx:enscript-either+ line ""))
                      (line (irregex-replace/all +rx:td-end+ line "\t| "))
                      (line (irregex-replace/all +rx:tr-begin+ line "| "))
                      (line (irregex-replace/all +rx:deltable+ line "")))
                 (display line parsed-out)
                 (newline parsed-out)
                 (if (tag?)
                     (loop (read-line) section tags (cons line tag-body) 'line)
                     (loop (read-line) section tags tag-body 'line))))
              )))))


;; (check-all "~/scheme/chicken-wiki/eggref/4/sql-de-lite" (list "sql-de-lite"))
;; (check-all "~/scheme/cdoc/sql-de-lite.wiki")
;; (check-all "~/scheme/chicken-wiki/man/4/Unit posix" (list "posix"))
;; (signature->identifier "(prepared-cache-size n" 'procedure)
;; (parse-unit "Non-standard macros and special forms" "chicken")
;; (parse-unit "Locations" "chicken")


)
