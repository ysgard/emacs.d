;;; ys-editor.el --- text editing config
;;;
;;; Commentary:
;;;
;;; Generic text editing configuration.
;;;
;;; Code:

(use-package emacs
  :init
  ;; Use spaces, but configure tab-width for modes that use tabs
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (define-key global-map (kbd "RET") 'newline-and-indent)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq require-final-newline t) ; Always append newline before EOF

  ;; The unwashed masses have spoken - no double spaces
  (setq sentence-end-double-space nil)

  (delete-selection-mode t) ; typed text replaces selection
  (transient-mark-mode t) ; highlight marked regions

  ;; Turn on columns and highlight current line
  (setq column-number-mode t)
  (global-display-line-numbers-mode t))

;; Show hex as the color they represent
(use-package rainbow-mode
  :hook (prog-mode text-mode))

;; Highlight TODO items
 (use-package hl-todo
   :custom
   ;; Better hl-todo colors, taken from spacemacs
   (hl-todo-keyword-faces '(("TODO" . "#dc752f")
                            ("NEXT" . "#dc752f")
                            ("THEM" . "#2d9574")
                            ("PROG" . "#4f97d7")
                            ("OKAY" . "#4f97d7")
                            ("DONT" . "#f2241f")
                            ("FAIL" . "#f2241f")
                            ("DONE" . "#86dc2f")
                            ("NOTE" . "#b1951d")
                            ("KLUDGE" . "#b1951d")
                            ("HACK" . "#b1951d")
                            ("TEMP" . "#b1951d")
                            ("QUESTION" . "#b1951d")
                            ("HOLD" . "#dc752f")
                            ("FIXME" . "#dc752f")
                            ("XXX+" . "#dc752f")))
   :hook
   ((prog-mode text-mode) . global-hl-todo-mode))



(provide 'ys-editor)

;;; ys-editor.el ends here
