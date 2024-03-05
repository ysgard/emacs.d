;;; ys-lsp.el --- syntax and code completion
;;;
;;; Commentary:
;;;
;;; Code syntax parsing and autocompletion that isn't language-specific.
;;;
;;; Code:


;;; Tree-sitter (doesn't seem to work currently, needs fixing
(use-package emacs
  :init
  (setq treesit-language-source-alist
        '((bash "git@github.com:tree-sitter/tree-sitter-bash")
          (cmake "git@github.com:tree-sitter/tree-sitter-cmake")
          (css "git@github.com:tree-sitter/tree-sitter-css")
          (elisp "git@github.com:tree-sitter/tree-sitter-elisp")
          (go "git@github.com:tree-sitter/tree-sitter-go")
          (html "git@github.com:tree-sitter/tree-sitter-html")
          (javascript "git@github.com:tree-sitter/tree-sitter-javascript" "master" "src")
          (json "git@github.com:tree-sitter/tree-sitter-json")
          (make "git@github.com:tree-sitter/tree-sitter-make")
          (markdown "git@github.com:tree-sitter/tree-sitter-markdown")
          (python "git@github.com:tree-sitter/tree-sitter-python")
          (regex "git@github.com:tree-sitter/tree-sitter-regex")
          (ruby "git@github.com:tree-sitter/tree-sitter-ruby")
          (toml "git@github.com:tree-sitter/tree-sitter-toml")
          (tsx "git@github.com:tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "git@github.com:tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "git@github.com:tree-sitter/tree-sitter-yaml"))))


(provide 'ys-lsp)

;;; ys-lsp.el ends here
