
;;; A proof-of-concept alternative user completion command (adapted
;;; from ess-complete-object-name) for use with ESS, making use of the
;;; rcompgen package rather than ESS's built-in completion mechanism.
;;; Including this in your .emacs file should be sufficient (see usage
;;; note below).  The R package 'rcompgen' must be installed.

(require 'ess-site)

(defun alt-ess-complete-object-name ()

  (interactive)
  (ess-make-buffer-current)
  (let* ((comint-completion-addsuffix nil)
	 (beg-of-line (save-excursion (comint-bol) (point)))
	 (end-of-line (point-at-eol))
	 (line-buffer (buffer-substring beg-of-line end-of-line))
	 (token-string ;; setup, including computation of the token
	  (progn 
	    (ess-command (format "rcompgen:::.assignLinebuffer('%s')\n" line-buffer))
	    (ess-command (format "rcompgen:::.assignEnd(%d)\n" (- (point) beg-of-line)))
	    (car (ess-get-words-from-vector "rcompgen:::.guessTokenFromLine()\n"))))
	 (possible-completions ;; compute and retrieve possible completions
	  (progn 
	    (ess-command "rcompgen:::.completeToken()\n")
	    (ess-get-words-from-vector "rcompgen:::.retrieveCompletions()\n"))))
    (or (comint-dynamic-simple-complete token-string possible-completions) 'none)))

;; This completion function can now be invoked as 
;; M-x alt-ess-complete-object-name

;; However, it is more common to associate completion with the TAB
;; key, which can be done as follows:

(define-key inferior-ess-mode-map "\t" 'alt-ess-complete-object-name)

;; Unlike ESS, this does not revert to filename completion when inside
;; quotes (mostly because I haven't figured out how to do that).  You
;; can still do filename completion with M-TAB (note that many window
;; managers will catch Alt-TAB for window navigation, so you might
;; need to use Esc-TAB for this).

