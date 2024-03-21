;;; ys-completion.el --- grab bag of completion
;;;
;;; Commentary:
;;;
;;; I expect the packages to change frequently until I finally find something
;;; I like. Probably company-mode.
;;;
;;; Code:

(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
        ("RET" . nil)
        ("[return]" . nil)
        ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init (setq company-backends '(company-capf
                                 company-elisp
                                 company-cmake
                                 company-yasnippet
                                 company-files
                                 company-keywords
                                 company-etags
                                 company-gtags
                                 company-ispell)))

;; company-box is a frontend for company
(use-package company-box
  :after company
  :diminish company-box-mode
  :custom
  (company-box-show-single-candidate t)
  (company-box-frame-behavior 'point)
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-max-candidates 10)
  (company-box-icon-right-margin 0.5)
  :hook
  (company-mode . company-box-mode))

;; Sort candidates for company
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

(provide 'ys-completion)

;;; ys-completion.el ends here
