;;; ys-keybinds.el
;;;
;;; Commentary:
;;;
;;; Keybinds are all put in one place here, because otherwise I
;;; forget where I set them. Easier to organize too.
;;;
;;; Code: This, is code

;; Provide hints as to available keys

(use-package emacs
  :init
  ;; Map the correct keybindings for macOS
  (when *sys/mac*
    (setq mac-command-modifier 'super
          mac-option-modifier 'meta
          max-control-modifier 'control)))


(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after 0.5s instead of 1s
  :config
  (which-key-mode))

(provide 'ys-keybinds)

;;; ys-keybinds.el ends here
