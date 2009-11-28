#!/usr/bin/env csi4 -script

(require-library chicken-doc-admin)

(define (usage)
  (print "usage: " (program-name) " [-r]")
  (print "  -r        regenerate indices")
  (print "  -l        list repository information")
  (print "  -i        initialize repository non-destructively")
  (exit))

(when (null? (command-line-arguments))
  (usage))

(let ((o (car (command-line-arguments))))
  (cond ((string=? o "-i")
         (create-repository!))
        (else
         (unless (verify-repository)
           (fprintf (current-error-port)
                    "No repository found at ~a\nUse -i to initialize\n" (cdoc-base))
           (exit 1))
         (cond ((string=? o "-r")
                (print "Rebuilding ID cache...")
                (refresh-id-cache))
               ((string=? o "-l")
                (describe-repository))
               (else
                (usage))))))
