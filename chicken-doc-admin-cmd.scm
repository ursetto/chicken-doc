#!/usr/bin/env csi4 -script

(require-library chicken-doc)

(define (usage)
  (print "usage: " (program-name) " [-r]")
  (print "  -r        regenerate indices")
  (exit))

(when (null? (command-line-arguments))
  (usage))

(let ((o (car (command-line-arguments))))
  (cond ((string=? o "-r")
         (refresh-id-cache))
        (else
         (usage))))
