;;; init.el
;;;
;;; Commentary: Entry point for our Emacs config
;;; Code:

;;; UI

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
;; Set default width to 100 cols
(add-to-list 'initial-frame-alist '(width . 100))

;; Import some utility functions
(add-to-list 'load-path "~/.emacs.d/ys")
(require 'ys-lib)

;;; Package management
(require 'ys-package)

;;; Garbage collection

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))


;;; Base


;; Set sane defaults
(use-package emacs ; Wrap emacs-specific settings in a use-package block
  :init
  ;; Some basic info
  (setq user-full-name "Jan Van Uytven")
  (setq user-mail-address "ysgard@ysgard.net")

  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message () ; get rid of default text in minibuffer
    (message ""))
  (defalias 'yes-or-no-p 'y-or-n-p) ; Prefer y/n to yes/no
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Don't use ESC as a modifier

  ;; UTF-8 everywhere
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
	coding-system-for-read 'utf-8
	coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; Deal with backup and temporary files
  (setq make-backup-files nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

  ;; Make sure buffers match file contents when file changes
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

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

  ;; Show filename in frame
  (setq-default frame-title-format "%b (%f)")

  ;; Map the correct keybindings for macOS
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq max-control-modifier 'control))

  ;; Make sure we turn off evil keybinding before we start evil,
  ;; As we use general instead
  (setq evil-want-keybinding nil)

  ;; Turn on columns and highlight current line
  (setq column-number-mode t)
  (global-display-line-numbers-mode t)

  ;; Miscellanea
  (setq x-underline-at-descent-line t
        use-dialog-box nil
        ring-bell-function 'ys/flash-mode-line)
  (show-paren-mode t)

  ;; Only display the warning buffer if we encounter an error
  (setq warning-minimum-level :error)

  ;; Unset SSH_AUTH_SOCK on MacOS to fix built-in git fetches
  ;; https://emacs.stackexchange.com/questions/30874/magit-how-to-use-ssh-key-rather-than-being-prompted-for-password
  (if (eq system-type 'darwin) (setenv "SSH_AUTH_SOCK" nil))

  ;; We use ido for fuzzy finding
  (setq ido-enable-flex-matching t ; Match characters if we can't match substring
        ido-enable-prefix nil      ; Finds matches that aren't prefixes
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ido-max-prospects 10
        ido-create-new-buffer 'always
        ido-use-virtual-buffers t)
  ;; Show more commonly used file extensions first
  (setq ido-file-extensions-order '(".tf" ".rb" ".md" ".txt"))
  (ido-mode t)
  ;; Add a hook to `~' sends us to homedir with ido-find-file
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-file-completion-map
                          (kbd "~")
                          (lambda ()
                            (interactive)
                            (if (looking-back "/")
                                (insert "~/")
                              (call-interactively 'self-insert-command)))))))




;; Keybinds
;; We load this a bit early so other packages can take advantage
;; of :general in their package loads.
(require 'ys-keybinds)
;; Become EVIL
(require 'ys-evil)
;;; Terminal configs
(require 'ys-terminal)


;;; Theming


(use-package emacs ; Note we reuse the package 'emacs' here
  :init
  (pixel-scroll-precision-mode t)
  (set-face-attribute 'default nil
                      :font "Berkeley Mono"
                      ;; If we're using retina bump up the height
                      :height (if (eq system-type 'darwin) 130 100)))


(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-challenger-deep t))

;; Nicer Modeline
(use-package doom-modeline
  :ensure t ; install if not exist
  :init (doom-modeline-mode 1))

;; Cool icons
(use-package nerd-icons)

;; nya-nya-nya-n-nya-nya-nya-nyan
(use-package nyan-mode
  :init
  (nyan-mode))





;;; Tree-sitter
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


;;; Packages

;; Dired
(require 'dired)
;; Reuse dired buffers
(use-package dired-single
  :config
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer))

;; Dired project drawer
(use-package dired-sidebar
  :after dired-subtree
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :general
  (leader-keys
    :keymap 'dired-mode-map
    "w o" '(other-window :which-key "other"))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__"
        dired-sidebar-theme 'nerd
        dired-sidebar-use-term-integration t
        dired-sidebar-use-custom-font nil
        dired-sidebar-should-follow-file nil
        dired-sidebar-use-one-instance t))

;; Have a nice sidebar for buffers too
(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font nil))

;; Projects
(use-package projectile
  :demand
  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "p b" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(projectile-switch-project :which-key "switch project")
    "p a" '(projectile-add-known-project :which-key "add project")
    "p r" '(projectile-remove-known-project :which-key "remove project"))
  :init
  (projectile-mode +1))

;; Make sure our path matches the system's
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Fuzzy finding
;; Ido everywhere!
(use-package ido-completing-read+
  :config (ido-ubiquitous-mode t))

;; Smex provides an ido-like interface to M-x
(use-package smex
  :config (smex-initialize)
  :general
  (leader-keys
    "x" '(smex :which-key "execute")))

(use-package ido-vertical-mode :config (ido-vertical-mode))

(use-package flx-ido
  :config
  (flx-ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        gc-cons-threshold 20000000))


;; Magit
(use-package magit)

;; Git gutter
(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; Treemacs
(use-package treemacs
  :general
  (leader-keys
    "t" '(treemacs :which-key "treemacs")))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit
  :after (treemacs magit))

;; Terraform
(use-package terraform-mode)



;;; init.el ends here
