;;; init.el
;;;
;;; Commentary: Entry point for our Emacs config
;;; Code:

;; Import some utility functions
(add-to-list 'load-path "~/.emacs.d/ys")
(require 'ys-lib)

;;; Package management
(require 'ys-package)

;;; Base
(require 'ys-base)

;;; Editing
(require 'ys-editor)

;; Keybinds
;; We load this a bit early so other packages can take advantage
;; of :general in their package loads.
(require 'ys-keybinds)

;; Become EVIL
(require 'ys-evil)

;;; Terminal configs
(require 'ys-terminal)

;;; Display and Themes
(require 'ys-display)

;;; File navigation and sidebars
(require 'ys-navigation)

;; Projects
(require 'ys-project)

;; LSP and friends
(require 'ys-lsp)

;; Web dev
(require 'ys-js)

;; Everything else
(require 'ys-misc)

;; Completion framework(s)
(require 'ys-completion)

;;; Dashboard
(require 'ys-dashboard)

;;; init.el ends here
