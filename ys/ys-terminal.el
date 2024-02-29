;;; ys-terminal.el
;;;
;;; Commentary:
;;;
;;; Configuration for various shells
;;;
;;; Code:

;; Use vterm instead of ansi-term for best integration with the shell.
;; Note - should add the following to ~/.zshrc
;;
;; vterm_printf() {
;;     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
;;         # Tell tmux to pass the escape sequences through
;;         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
;;     elif [ "${TERM%%-*}" = "screen" ]; then
;;         # GNU screen (screen, screen-256color, screen-256color-bce)
;;         printf "\eP\e]%s\007\e\\" "$1"
;;     else
;;         printf "\e]%s\e\\" "$1"
;;     fi
;; }
(use-package vterm)

;; Create a pop-up shell for the directory of the file being edited
(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell))))
        shell-pop-term-shell "/bin/zsh")
  ;; Need to do this manually for some reason
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(provide 'ys-terminal)
;;; ys-terminal.el ends here
