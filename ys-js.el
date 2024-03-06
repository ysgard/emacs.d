;;; ys-js.el --- Web and Javascript development
;;;
;;; Commentary:
;;;
;;; Set up web and js-specific packages in here,
;;; along with Typescript and other JS-adjacent stuff.
;;;
;;; Code:


(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))


(provide 'ys-js)

;;; ys-js.el ends here
