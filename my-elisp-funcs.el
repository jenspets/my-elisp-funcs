;;; my-elisp-funcs --- A set of helper functions;

;;; Commentary:
;;; This is helper functions to help my Emacs workflow

;;; Code:

(defun my-show-header ()
  "Show a header with information in some major modes.  To enable, use (add-hook 'buffer-list-update-hook 'my-show-header) in init file."
  ;; (when (and (or (eq major-mode 'org-mode)
  ;; (eq major-mode 'python-mode))
  (when (and (or (eq major-mode 'org-mode)
		 (derived-mode-p 'prog-mode))
	     (not (derived-mode-p 'epresent-mode))  ; Don't want header-line when presenting
	     (not (and (boundp 'gptel-mode) gptel-mode)))  ; Needed for org-mode in gptel; it uses the header-line for status information
    (setq header-line-format
	  '(:eval
	    (list
	     "["
	     (buffer-name)
	     "]> "
	     (which-function))))))


(defun today (&optional latex-format)
  "Insert today's date in ISO format.  If LATEX-FORMAT is set, add double hyphen."
  (interactive)
  (unless latex-format (setq latex-format nil))
  (if latex-format
      (insert (format-time-string "%Y--%m--%d"))
    (insert (format-time-string "%Y-%m-%d"))))

;; (global-set-key "\C-c." 'today)

(defun linedance (begin end)
  "Break up sentences in buffer between BEGIN and END into one line.
Used for Latex code to transform one line with several sentences to one
sentence at each line."
  (interactive "r")
  (if (use-region-p)
      (let (res)
	(dolist (line (split-string (buffer-substring begin end) "\\. ") res)
	  (setq res (if (and (> (length (string-trim line)) 0)
			     (not (or (string= (substring (string-trim line) -1 nil) ".")
				      (string= (substring (string-trim line) -1 nil) ":"))))
			(concat res (string-trim line) ".\n")
		      (concat res (string-trim line)))))
	(kill-region begin end)
	(insert res))))

;; (global-set-key "\C-ce" 'linedance)
    
(provide 'my-elisp-funcs)
;;; my-elisp-funcs.el ends here
