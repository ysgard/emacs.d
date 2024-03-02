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
  ;(pixel-scroll-precision-mode t) ; good-scroll should be better
  (set-face-attribute 'default nil
                      :font "Berkeley Mono"
                      ;; If we're using retina bump up the height
                      :height (if (eq system-type 'darwin) 130 100)))


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

;; Window Dividers
(use-package emacs
  :config
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  :init
  (window-divider-mode))


;; (use-package good-scoll
;;   :commands good-scroll-mode
;;   :custom
;;   (good-scroll-duration 0.2)
;;   (good-scroll-point-jump 4)
;;   :init
;;   ;; Bind page-up and page-down to good-scroll
;;   (global-set-key [next] #'good-scroll-up-full-screen)
;;   (global-set-key [prior] #'good-scroll-down-full-screen))


(provide 'ys-display)

;;; ys-display.el ends here
