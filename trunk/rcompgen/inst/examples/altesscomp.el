
;;; This is a proof-of-concept alternative user completion command
;;; (adapted from ess-complete-object-name) for use with ESS, making
;;; use of the rcompgen package rather than ESS's built-in completion
;;; mechanism.  Including this in your .emacs file should be
;;; sufficient (see usage note below).  The R package 'rcompgen' must
;;; be installed.

(require 'ess-site)

;;; There are two ways to proceed.

;;; (1) You can completely replace 'ess-complete-object-name', in
;;; which case you should keep the following function definition, but
;;; change the name 'alt-ess-complete-object-name' to
;;; 'ess-complete-object-name'.  The advantage to this approach is
;;; that you retain all the other ESS goodies like filename
;;; completion, C-c C-TAB completion in code buffers, etc.  The
;;; downside is that you won't be able to use the original
;;; 'ess-complete-object-name' any more.

;;; (2) Define the new function with a different name, and use
;;; whichever one you want (this is what will happen if you use this
;;; file as-is):

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

;;; This completion function can now be invoked as 
;;; M-x alt-ess-complete-object-name

;;; If you want to associate completion with the TAB key, that can be
;;; done as follows:

;; (define-key inferior-ess-mode-map "\t" 'alt-ess-complete-object-name)

