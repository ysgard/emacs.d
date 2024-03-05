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

(provide 'ys-editor)

;;; ys-editor.el ends here
