;;; ys-completion.el --- grab bag of completion
;;;
;;; Commentary:
;;;
;;; I expect the packages to change frequently until I finally find something
;;; I like. Probably company-mode.
;;;
;;; Code:

;; Vertico
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t)) ; enable cycling between first and last

;; Persist history

(provide 'ys-completion)

;;; ys-completion.el ends here
