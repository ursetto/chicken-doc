#!/usr/bin/env csi4 -script

(require-library chicken-doc)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " (program-name) " -s|-c|-i path")
      (print "       " (program-name) " -f key")
      (print "       " (program-name) " key | path")
      (print "  -s path        Show signature")
      (print "  -c path        Show table of contents (child keys)")
      (print "  -i path        Show documentation")
      (print "  -f key         Show all matching paths for key")
      (print "where KEY is a single identifier and PATH is one or")
      (print "more keys comprising a path from the documentation root,")
      (print "separated by spaces or the # character.")
      (print)
      (print "When no option is given, guess the user's intent.  With")
      (print "a single key, find the key (as with -f) and show its")
      (print "documentation (as with -i) or show all matching paths")
      (print "if multiple matches exist.  If more than one key is")
      (print "provided, show documentation on the path (as with -i).")
      (print)
      (print "Examples:")
      (print "  -f open/rdonly           # Show matches for open/rdonly")
      (print "  -s posix open/rdonly     # Show signature of open/rdonly in Unit posix")
      (print "  -s 9p open/rdonly        # Show signature of open/rdonly in 9p egg")
      (print "  -i 9p#open/rdonly        # Show documentation for same")
      (print "  -c posix                 # Show TOC for Unit posix")
      (print "  use                      # Show doc for \"use\" in chicken core")
      (print "  posix                    # Show doc for Unit posix")
      (print "  open/rdonly              # Show matches for open/rdonly")
      (print "  posix open/rdonly        # Show doc for open/rdonly in Unit posix")
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
