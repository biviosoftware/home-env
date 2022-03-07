; Copyright (c) 2022 bivio Software, Inc.  All rights reserved.
(provide 'b-evil)

(require 'evil-escape)
(require 'evil)
(defun b-do-evil-init ()
    ;; b-do-evil-init will be run when evil-mode is turned on and
    ;; off. evil-escape-mode needs to be turned on/off with it.
    (evil-escape-mode (if evil-mode 1 -1))
    (setq-default evil-escape-key-sequence "jj")
    (setq-default evil-escape-delay 0.2)
    (evil-set-initial-state 'shell-mode 'normal)
    (define-key evil-normal-state-map (kbd "gF") 'b-ffap-with-line)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    ;; Also in visual mode
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
    )
(add-hook 'evil-mode-hook 'b-do-evil-init)
