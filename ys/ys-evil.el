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
  ;; (loop for (mode . state)
  ;;       in '((inferior-emacs-lisp-mode . emacs)
  ;;            (nrepl-mode . insert)
  ;;            (comint-mode . normal)
  ;;            (shell-mode . emacs)
  ;;            (git-commit-mode . insert)
  ;;            (term-mode . emacs)
  ;;            (help-mode . emacs)
  ;;            (grep-mode . emacs)
  ;;            (bc-menu-mode . emacs)
  ;;            (magit-branch-manager-mode . emacs)
  ;;            (dired-mode . normal)
  ;;            (wdired-mode . normal)
  ;;            (treemacs-mode . emacs))
  ;;       do (evil-set-initial-state mode state))
  ;; Define `,,' as ESC
  ;; (when (require 'key-chord nil 'noerror)
  ;;   (key-chord-define evil-normal-state-map ",," 'evil-force-normal-state)
  ;;   (key-chord-define evil-visual-state-map ",," 'evil-change-to-previous-state)
  ;;   (key-chord-define evil-insert-state-map ",," 'evil-normal-state)
  ;;   (key-chord-define evil-replace-state-map ",," 'evil-normal-state)))
  )

;; Evil collection
(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))

(provide 'ys-evil)

;;; ys-evil.el ends here
