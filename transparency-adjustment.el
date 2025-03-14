;;; transparency-adjustment --- Adjusting transparency

;;; Commentary:
;;; Functions for adjusting transparency on the fly.  Works with X, not tested for Wayland.
;;;  Set transparency for X, ideas from https://www.emacswiki.org/emacs/TransparentEmacs

;;; Code:

(setq jps-frame-transparency 85 ; Default value for frame transpaency
      jps-current-transparency-value 85
      jps-transparency-step 5)

(set-frame-parameter nil 'alpha-background jps-frame-transparency)
; The following code makes emacsclient only start in terminal mode:
; (add-to-list 'default-frame-alist '(alpha-background . jps-frame-transparency))
; TODO: Fix default transparency for emacsclient

;; This does also not work, emacsclient just crashes with it...
;; (add-hook 'after-make-frame-functions 'jps-set-transparency)
;; (defun jps-set-transparency ()
;;   "Set default parameters after creating a new window.  Fixes a bug when running as emacsserver."
;;   (set-frame-parameter nil 'alpha-backround jps-frame-transparency)
;;   (add-to-list 'default-frame-alist '(alpha-background . jps-frame-transparency)))

(defun toggle-transparency ()
  "Toggle transparency settings for frame."
  (interactive)
  (let ((alpha-background (frame-parameter nil 'alpha-background)))
    (set-frame-parameter
     nil 'alpha-background
     (if (eql alpha-background 100) jps-current-transparency-value 100))))

(defun set-default-transparency ()
  "Set transparency settings to default value."
  (interactive)
  (setq jps-current-transparency-value jps-frame-transparency)
  (set-frame-parameter nil 'alpha-background jps-current-transparency-value))

(defun adjust-transparency (adj)
  "Adjust transparency by ADJ step.  If value is increaed to 100 or more, the toggle will reset to default value."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-background)))
    (setq jps-current-transparency-value (cond ((>= alpha 0) (min (+ alpha adj) 100))
					       ((< alpha 0) (max (+ alpha adj) 0)))))
  (set-frame-parameter nil 'alpha-background jps-current-transparency-value)
  (if (eql jps-current-transparency-value 100)
      (setq jps-current-transparency-value jps-frame-transparency)
    nil))

(defun increase-transparency ()
  "Increase transparency with step size."
  (interactive)
  (adjust-transparency jps-transparency-step))

(defun decrease-transparency ()
  "Decrease tsransparency with step size."
  (interactive)
  (adjust-transparency (- jps-transparency-step)))
  
(define-prefix-command 't-map)
(global-set-key (kbd "C-c t") 't-map)
(define-key t-map (kbd "t") 'toggle-transparency)
(define-key t-map (kbd "d") 'set-default-transparency)
(define-key t-map (kbd "+") 'increase-transparency)
(define-key t-map (kbd "-") 'decrease-transparency)

(provide 'transparency-adjustment)
;;; transparency-adjustment.el ends here
