;;; ys-base.el --- base Emacs config
;;;
;;; Commentary:
;;;
;;; Configuration of base features of Emacs.
;;;
;;; Code:


(use-package emacs
  :init
  (setq user-full-name "Jan Van Uytven"
        user-mail-address "ysgard@ysgard.net")

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

  ;; Only display the warning buffer if we encounter an error
  (setq warning-minimum-level :error)

  ;; Miscellanea
  (setq x-underline-at-descent-line t
        use-dialog-box nil
        ring-bell-function 'ys/flash-mode-line)
  (show-paren-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p) ; Prefer y/n to yes/no

  ;; Unset SSH_AUTH_SOCK on MacOS to fix built-in git fetches
  ;; https://emacs.stackexchange.com/questions/30874/magit-how-to-use-ssh-key-rather-than-being-prompted-for-password
  (when *sys/mac* (setenv "SSH_AUTH_SOCK" nil)))


(provide 'ys-base)

;;; ys-base.el ends here
