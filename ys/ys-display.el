;;; ys-display.el
;;;
;;; Commentary:
;;;
;;; Configuration related to visual display, such as themes, scrolling,
;;; mode-lines, etc...
;;;
;;; Code:


(use-package emacs ; Note we reuse the package 'emacs' here
  :init
  ;; Initial UI configuration
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq use-file-dialog nil
        inhibit-startup-screen t
        initial-scratch-message nil)
  (defun display-startup-echo-area-message () ; get rid of default text in minibuffer
    (message ""))
  (pixel-scroll-precision-mode t)
  ;; Show filename in frame
  (setq-default frame-title-format "%b (%f)")

  ;; Font
  (set-face-attribute 'default nil
                      :font "Berkeley Mono"
                      ;; If we're using retina bump up the height
                      :height (if *sys/mac* 130 100))

  ;; Set the Size of the frame
  (set-frame-size (selected-frame) 100 45 nil)
  (set-frame-parameter (selected-frame) 'top 0)

  ;; Configure the built-in window dividers
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode))

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-challenger-deep t))

;; Nicer Modeline
(use-package doom-modeline
  :ensure t ; install if not exist
  :init (doom-modeline-mode 1))

;; Cool icons
(use-package nerd-icons)

;; nya-nya-nya-n-nya-nya-nya-nyan
(use-package nyan-mode
  :init
  (nyan-mode))

(provide 'ys-display)

;;; ys-display.el ends here
