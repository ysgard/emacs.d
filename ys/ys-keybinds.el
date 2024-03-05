;;; ys-keybinds.el
;;;
;;; Commentary:
;;;
;;; Keybinds are all put in one place here, because otherwise I
;;; forget where I set them. Easier to organize too.
;;;
;;; Code:

;; Provide hints as to available keys

(use-package emacs
  :init
  ;; Don't use ESC as a modifier
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Map the correct keybindings for macOS
  (when *sys/mac*
    (setq mac-command-modifier 'super
          mac-option-modifier 'meta
          max-control-modifier 'control))

  ;; Make sure we turn off evil keybinding before we start evil,
  ;; As we use general instead
  (setq evil-want-keybinding nil))


(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after 0.5s instead of 1s
  :config
  (which-key-mode))

;; General makes defining keybinds very easy
(use-package general
  :demand
  :config
  (general-evil-setup)
  ;; Define space as our leader key
  (general-create-definer leader-keys
                          :states '(normal insert visual emacs)
                          :keymaps 'override
                          :prefix "SPC"
                          :global-prefix "C-SPC")

  (leader-keys

   ;;"a" '(:ignore t :which-key "")
   ;;"a <escape>" '(keyboard-escape-quit :which-key t)

   ;; Buffer
   "b" '(:ignore t :which-key "buffer")
   ;; Don't show an error because SPC b ESC is undefined, just abort
   "b <escape>" '(keyboard-escape-quit :which-key t)
   "b n" '(ys/next-user-buffer :which-key "next")
   "b p" '(ys/previous-user-buffer :which-key "previous")
   "b k" '(kill-current-buffer :which-key "kill")
   "b b" '(ido-switch-buffer :which-key "list")
   "b s" '(ibuffer-sidebar-toggle-sidebar :which-key "sidebar")

    ;; Config
   "c" '(:ignore t :which-key "config")
   "c <escape>" '(keyboard-escape-quit :which-key t)
   "c r" '(restart-emacs :which-key "restart emacs")
   "c i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init")
   "c l" '(ab/enable-line-numbers :which-key "relative line numbers")

   ;; Dired
   "d" '(:ignore t :which-key "dired")
   "d <escape>" '(keyboard-escape-quit :which-key t)
   "d d" '(dired-jump :which-key "dired")
   "d s" '(dired-sidebar-toggle-sidebar :which-key "sidebar")

   ;; File
   "f" '(:ignore t :which-key "file")
   "f <escape>" '(keyboard-escape-quit :which-key t)
   "f n" '(ys/new-empty-buffer :which-key "new")
   "f f" '(ido-find-file :which-key "open")
   "f s" '(save-buffer :which-key "save")
   "f r" '(rename-visited-file :which-key "rename")
   "f d" '(ys/delete-this-file :which-key "delete")
   "f q" '(save-buffers-kill-emacs :which-key "quit")

   ;; Magit
   "g" '(:ignore t :which-key "git")
   "g <escape>" '(keyboard-escape-quit :which-key t)
   "g g" '(magit-status :which-key "status")
   "g l" '(magit-log :which-key "log")
   ;(general-nmap "<escape>" #'transient-quit-one)

   ;; p - Projectile
   ;; See `ys-projects.el'

   ;; Formatting
   "m" '(:ignore t :which-key "format")
   "m <escape>" '(keyboard-escape-quit :which-key t)
   "m /" '(ys/toggle-comment :which-key "comment/uncomment")
   "m e" '(ys/replace-last-sexp :which-key "evaluate sexp")

   ;; search, replace
   "q" '(:ignore t :which-key "query")
   "q <escape>" '(keyboard-escape-quit :which-key t)
   "q r" '(query-replace-regexp :which-key "find and replace")

   ;; Shells and executions
   "s" '(:ignore t :which-key "shells")
   "s <escape>" '(keyboard-escape-quit :which-key t)
   "s `" '(ys/eshell-here :which-key "eshell")
   "s x" '(ys/eshell-x :which-key "exit eshell")
   "s t" '(shell-pop :which-key "pop a shell")

   ;; windows
   "w" '(:ignore t :which-key "windows")
   "w <escape>" '(keyboard-escape-quit :which-key t)
   "w o" '(other-window :which-key "other")
   "w /" '(split-window-below :which-key "vertical split")
   "w %" '(split-window-right :which-key "horizontal split")
   "w d" '(delete-window :which-key "delete")
))

(provide 'ys-keybinds)

;;; ys-keybinds.el ends here
