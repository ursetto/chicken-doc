#!/usr/bin/env csi4 -script

(require-library chicken-doc)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " (program-name) " [-s|-f|-c|-i] path")
      (print "       " (program-name) " key | path")
      (print "  -s path        Show signature")
      (print "  -c path        Show table of contents")
      (print "  -i path        Show documentation")
      (print "  -f key         Search for key and show signature(s)")
      (print "  key            Search for key and show documentation")
      (print "  path           Show documentation for absolute path")
      (exit 1))))

;; (init)

(when (null? (command-line-arguments))
  (usage))

(unless (verify-repository)
  (fprintf (current-error-port) "No repository found at ~a\n"
           (repository-base))
  (exit 1))

(let ((o (car (command-line-arguments))))
  (cond ((string=? o "-s")
         ;; 
	 (describe-signatures (list (map string->symbol (cdr (command-line-arguments))))))
        ((string=? o "-f")
         ;; Is this useful?  Basically, identifier search on signatures, showing path
         ;; I wonder if we need the signature, or just the path
	 (search-only (string->symbol (cadr (command-line-arguments)))))
        ((string=? o "-c")
         (describe-contents (map string->symbol (cdr (command-line-arguments)))))
        ((string=? o "-i")
         (describe (map string->symbol (cdr (command-line-arguments)))))
        (else
         (let ((ids (map string->symbol (command-line-arguments))))
           (if (null? (cdr ids))
               (doc-dwim (car ids))
               (doc-dwim ids))))))




;;
