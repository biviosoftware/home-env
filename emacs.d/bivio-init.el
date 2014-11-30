(provide 'bivio-init)
(require 'b-python)
(require 'b-perl)
(require 'b-hours)
(require 'sgml-mode)
(require 'compile)
(require 'cc-mode)
(require 'comint)
(require 'vc)
(require 'diff-mode)
(require 'markdown-mode)
(require 'yaml-mode)
(require 'css-mode)

(global-font-lock-mode t)
(transient-mark-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq
 next-line-add-newlines nil
 case-replace t
 case-fold-search t
 visible-bell t
 highlight-nonselected-windows nil
 term-term-name "eterm"
 ring-bell-function 'ignore
 truncate-partial-width-windows nil
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 large-file-warning-threshold 100000000
 history-delete-duplicates t
 indent-line-function 'indent-relative-maybe
 dabbrev-case-fold-search t
 dabbrev-case-replace nil
 dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
 font-lock-maximum-decoration t
)

;; Perl
(eval-after-load
 "compile"
 '(setq compilation-error-regexp-alist
	(append
	 '((".*at \\([^ ]+\\) line \\([0-9]+\\)\\.?\n" 1 2))
	 compilation-error-regexp-alist)))

(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-Mode))
(add-hook 'espresso-mode-hook
	  '(lambda ()
	     (setq espresso-indent-level 2)
	     (setq espresso-expr-indent-offset 0)
	     (define-key espresso-mode-map "\C-c\C-m" 'compile)))

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 2)))

(add-hook
 'shell-mode-hook
 (lambda (&optional arg)
   (setq
    shell-dirstack-query "command dirs"
    explicit-shell-file-name "/bin/bash")
   (define-key shell-mode-map "\C-cc" 'dirs)))

(setq comint-password-prompt-regexp
      (concat "[pP]assphrase: \\|[pP]assword: \\|"
	      comint-password-prompt-regexp))
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

(defun gosmacs-previous-window ()
  "Select the window above or to the left of the window now selected.
From the window at the upper left corner, select the one at the lower right."
  (interactive)
  (select-window (previous-window)))

(defun gosmacs-next-window ()
  "Select the window below or to the right of the window now selected.
From the window at the lower right corner, select the one at the upper left."
  (interactive)
  (select-window (next-window)))

(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun scroll-one-col-right (&optional arg)
  "Scroll the selected window right (text to the right) one column (or N cols)."
  (interactive "p")
  (scroll-right (or arg 1)))

(defun scroll-one-col-left (&optional arg)
  "Scroll the selected window left (text to the left) one column (or N cols)."
  (interactive "p")
  (scroll-left (or arg 1)))

(defvar shift-width 4
  "*The number of columns idented by shift-right or shift-left")

		   
(defun indent-by-shift (arg)
  "Change column by ARG shift-widths."
  (if (listp arg) (setq arg (car arg)))
  (if (= arg 0)
    nil
    (skip-chars-forward " \t")
    (if (> arg 0)
	; Shift always
      (indent-to-column (* (/ (+ (current-column) (* shift-width arg))
				shift-width) shift-width))
	; Negative shift first find spaces backwards
      (let ((opoint (point)))
	(beginning-of-line)
	(let ((pos (point)))
	  (goto-char opoint)
	  (if (re-search-backward "[ \t]*" pos t)
	      (progn
		(skip-chars-forward " \t")
		(backward-delete-char-untabify
		 (- (current-column)
		    (* (/ (max (- (+ (current-column)
				     (- shift-width 1))
				  (* shift-width 1)) ;arg
			       0) shift-width)
		       shift-width))))
	    ; failed to find spaces before point
	    (beginning-of-line)))))))
  
(defun shift-right (&optional arg)
  "Shift to the right by shift-width spaces."
  (interactive "p")
  (indent-by-shift (or arg 1)))

(defun shift-left (&optional arg)
  "Shift to the left by shift-width spaces."
  (interactive "p")
  (indent-by-shift (- (or arg 1))))

(defun join-line (&optional arg)
  (interactive "p")
  (let ((count (max (or arg 1) 1)))   ; Don't allow negative args
    (while (> count 0)
      (end-of-line)
      (delete-char 1)  ; If end of buffer, then will get a signal
      (fixup-whitespace)
      (setq count (- count 1)))))

(defun backward-upcase-word (&optional arg)
  (interactive "p")
  (upcase-word (- (or arg 1))))
(defun backward-downcase-word (&optional arg)
  (interactive "p")
  (downcase-word (- (or arg 1))))
(defun backward-capitalize-word (&optional arg)
  (interactive "p")
  (capitalize-word (- (or arg 1))))

(defun debug-mode ()
  "Toggles the current value of debug-on-error."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error is %s" debug-on-error))

(define-key global-map "\177" 'backward-delete-char-untabify)

(define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)
(global-set-key [(control _)] 'advertised-undo)

;;;; Control keys
(define-key text-mode-map "\C-i" 'shift-right)
(define-key global-map "\C-m" 'newline-and-indent)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key global-map "\C-z" 'scroll-one-line-up)
(define-key global-map "\C-_" 'advertised-undo)

;;;; Meta keys
(define-key global-map "\M-c" 'backward-capitalize-word)
(define-key global-map "\M-h" 'backward-kill-word)
(define-key global-map "\M-i" 'shift-left)
(define-key global-map "\M-l" 'backward-downcase-word)
(define-key global-map "\M-p" 'mark-paragraph)
(define-key global-map "\M-u" 'backward-upcase-word)
(define-key global-map "\M-z" 'scroll-one-line-down)
(define-key global-map "\M-'" 'dabbrev-expand)
(define-key global-map "\M-C-'" 'dabbrev-completion)
(define-key global-map "\M-/" 'abbrev-prefix-mark)
(define-key global-map "\M-%" 'query-replace-regexp)

;;;; Control-x keys
(define-key ctl-x-map "l" 'goto-line)
(define-key ctl-x-map "n" 'gosmacs-next-window)
(define-key ctl-x-map "p" 'gosmacs-previous-window)

;;;; Control-x Control keys
(define-key ctl-x-map "\C-d" 'delete-window)
(define-key ctl-x-map "c" 'center-line)
(define-key ctl-x-map "\C-j" 'join-line)
(define-key ctl-x-map "\C-l" 'sort-lines)
(define-key ctl-x-map "\C-m" 'shell)
(define-key ctl-x-map "\C-n" 'insert-buffer)
(define-key ctl-x-map "\C-u" nil); DELETE
(define-key ctl-x-map "\C-z" 'shrink-window)

(defun b-face-under-cursor (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added-face ((t (:foreground "blue" :weight bold))))
 '(diff-changed-face ((t (:foreground "magenta" :weight extra-bold))))
 '(diff-context-face ((((class color) (background dark)) (:foreground "black"))))
 '(diff-file-header-face ((t nil)))
 '(diff-header-face ((t nil)))
 '(diff-removed-face ((t (:foreground "red" :weight bold))))
 '(sh-heredoc ((t (:foreground "green"))))
 '(minibuffer-prompt ((((background dark)) nil))))
