(require-library chicken-doc)
(import chicken-doc (only chicken-doc-text chicken-doc-ansi-colors))
(require-library posix)
(import (only posix with-output-to-pipe setenv))
(use regex) (import irregex)

;;; Usage

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " (program-name) " -s|-c|-i path")
      (print "       " (program-name) " -f id")
      (print "       " (program-name) " -m re")
      (print "       " (program-name) " id | path")
      (print "   -s path        Show signature")
      (print "   -c path        Show table of contents (child IDs)")
      (print "   -i path        Show documentation")
      (print "   -f id          Show all matching paths for ID")
      (print " where ID is a single identifier and PATH is zero or")
      (print " more IDs comprising a path from the documentation root,")
      (print " separated by spaces or the # character.")
      (print)
      (print "   -m re          Show all matching paths for RE")
      (print " where RE is a POSIX regular expression.  Similar to -f.")
      (print)
      (print "When no option is given, guess the user's intent.  With")
      (print "a single ID, find the ID (as with -f) and show its")
      (print "documentation (as with -i) or show all matching paths")
      (print "if multiple matches exist.  If more than one ID is")
      (print "provided, show documentation on the path (as with -i).")
      (print)
      (print "Examples:")
      (print "  -f open/rdonly           # Show matches for open/rdonly")
      (print "  -s posix open/rdonly     # Show signature of open/rdonly in Unit posix")
      (print "  -s 9p open/rdonly        # Show signature of open/rdonly in 9p egg")
      (print "  -i 9p#open/rdonly        # Show documentation for same")
      (print "  -c posix                 # Show TOC for Unit posix")
      (print "  -c                       # Show toplevel TOC")
      (print "  -m call-                 # Show identifiers containing call-")
      (print "  -m -file$                # Show identifiers ending in -file")
      (print "  use                      # Show doc for \"use\" in chicken core")
      (print "  posix                    # Show doc for Unit posix")
      (print "  open/rdonly              # Show matches for open/rdonly")
      (print "  posix open/rdonly        # Show doc for open/rdonly in Unit posix")
      (exit 1))))

;;; Pager

(define *default-pager*
  (case (software-type)
    ((windows) "more /s")
    ((unix) "less")
    (else "")))
(define (with-output-to-pager thunk)
  (cond ((get-environment-variable "EMACS")
         (thunk))  ; Don't page in emacs subprocess.
        ((not (terminal-port? (current-output-port)))
         (thunk))  ; Don't page if stdout is not a TTY.
        (else
         (unless (get-environment-variable "LESS")
           (setenv "LESS" "FRXis"))  ; Default 'less' options
         (let ((pager (or (get-environment-variable "CHICKEN_DOC_PAGER")
                          (get-environment-variable "PAGER")
                          *default-pager*
                          "")))
           (if (or (string=? pager "")
                   (string=? pager "cat"))
               (thunk)
               ;; with-output-to-pipe does not close pipe on exception, borking tty
               (let ((pipe (open-output-pipe pager))
                     (rv #f))
                 (handle-exceptions exn (begin (close-output-pipe pipe)
                                               (signal exn))
                   ;; Can't reliably detect if pipe open fails.
                   (set! rv (with-output-to-port pipe thunk)))
                 (close-output-pipe pipe)
                 rv))))))

;;; Wrapping

(define (determine-wrap-column)
  (define (safe-terminal-size p)
    (handle-exceptions e (values 0 0)
      (terminal-size p)))
  (cond ((get-environment-variable "CHICKEN_DOC_WRAP")
         => string->number)
        (else
         (let-values (((rows cols) (safe-terminal-size
                                    (current-input-port))))
           (if (= cols 0)
               76       ; (* 80 0.95)
               (inexact->exact (truncate (* cols 0.95))))))))

;;; Colors

;; Called prior to setting up pager so terminal-port? does not take it into account;
;; if env var set, we assume your pager supports ANSI escape sequences as in less -R.
(define (ansi-colors?)
  (and (get-environment-variable "CHICKEN_DOC_COLORS")
       (terminal-port? (current-output-port))
       (let ((term (get-environment-variable "TERM")))
         (and term (not (equal? term "dumb"))))))       ;; emacs==dumb

;;; Helpers

;; Special lookup for command-line.  Treat args as a standard path list
;; -but- if only one argument is provided, try to decompose it as a
;; qualified path string.
(define (lookup args)
  (define (normalize-path p)
    (cond ((null? p) p)
          ((null? (cdr p))
           (decompose-qualified-path (car p)))
          (else p)))
  (lookup-node (normalize-path args)))

;;; Main

(when (null? (command-line-arguments))
  (usage))

(verify-repository)

(wrap-column (determine-wrap-column))
(chicken-doc-ansi-colors (ansi-colors?))
(chicken-doc-warnings
 (get-environment-variable "CHICKEN_DOC_WARNINGS"))

(let ((o (car (command-line-arguments))))
  (cond ((null? o)
         (usage))
        ((or (string=? o "-h")
             (string=? o "--help")
             (string=? o "-?"))
         (usage))
        (else
         (with-output-to-pager
          (lambda ()
            (cond ((string=? o "-s")
                   (describe-signatures (list (lookup (cdr (command-line-arguments))))))
                  ((string=? o "-f")
                   ;; Is this useful?  Identifier search ("find") on signatures, showing path.
                   ;; I wonder if we need the signature, or just the path.
                   (search-only (cadr (command-line-arguments))))
                  ((string=? o "-m")
                   ;; Not doing search-and-describe because when there are zero
                   ;; matches, that will throw an error
                   (search-only (irregex (cadr (command-line-arguments)))))
                  ((string=? o "-c")
                   (describe-contents (lookup (cdr (command-line-arguments)))))
                  ((string=? o "-i")
                   ;; FIXME: decompose-pathspec required here but won't work yet.
                   (describe (lookup (cdr (command-line-arguments)))))
                  (else
                   (let ((ids (command-line-arguments)))
                     (if (null? (cdr ids))
                         (doc-dwim (car ids))
                         (doc-dwim ids))))))))))
