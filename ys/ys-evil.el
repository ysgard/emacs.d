;;; ys-evil.el
;;;
;;; Commentary:
;;;
;;; Configuration for evil-mode
;;;
;;; Code:

;; Become EVIL
(use-package evil
  :after emacs
  :demand ; No lazy loading here
  :config
  (evil-mode 1)
  ;; Set nice colors for state
  (setq evil-emacs-state-cursor '("purple" box)
        evil-normal-state-cursor '("yellow" box)
        evil-visual-state-cursor '("orange" box)
        evil-insert-state-cursor '("yellow" bar)
        evil-replace-state-cursor '("blue" bar)
        evil-operator-state-cursor '("blue" hollow))
  :init
  ;; Set initial states for certain modes
  ;;(evil-set-initial-state 'ibuffer-mode 'normal)
  ;;(evil-set-initial-state 'dired-mode 'emacs)
  ;;(evil-set-initial-state 'treemacs-mode 'emacs))
   )

;; Evil collection
(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))

(provide 'ys-evil)

;;; ys-evil.el ends here
