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
(require 'shell)
(ignore-errors (require 'java-mode))

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
(blink-cursor-mode -1)
(set-cursor-color "red")
(setq-default indent-tabs-mode nil)

(if (fboundp 'ansi-color-for-comint-mode-off)
    (ansi-color-for-comint-mode-off))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(add-to-list 'compilation-error-regexp-alist
	     '(".*at \\([^ ]+\\) line \\([0-9]+\\)\\.?\n" 1 2))
(if (file-readable-p "/bin/bash")
    (progn
      (setq
       shell-file-name "/bin/bash"
       shell-command-switch "-lc")
      (add-hook
       'shell-mode-hook
       (lambda (&optional arg)
         (setq
          shell-dirstack-query "command dirs"
          explicit-shell-file-name shell-file-name)
         (add-hook 'window-configuration-change-hook 'b-comint-fix-window-size nil t)
         (define-key shell-mode-map "\C-cc" 'dirs)))))


(add-to-list
 'auto-mode-alist '("\\.\\(bview\\|bconf\\|btest\\|bunit\\|t\\|pl\\|PL\\|pm\\)$"  . cperl-mode))

(define-key java-mode-map "\C-c\C-m" 'compile)

(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-Mode))
(add-hook 'espresso-mode-hook
	  '(lambda ()
	     (setq espresso-indent-level 2)
	     (setq espresso-expr-indent-offset 0)
	     (define-key espresso-mode-map "\C-c\C-m" 'compile)
	     (set (make-local-variable 'compile-command)
		  (setq compile-command
			(concat
			 (if (string-match "/test/" (buffer-file-name))
			     "mocha "
			     "node ")
			 (file-name-nondirectory buffer-file-name))))))

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'find-file-hook
	  (lambda ()
	    (if (string-match "/src/biviosoftware/boldchat/" (buffer-file-name))
		(set (make-local-variable 'tab-width) 4))))

(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 2)))

(defun b-comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

(setq comint-password-prompt-regexp
      (concat "[pP]assphrase: \\|[pP]assword: \\|"
	      comint-password-prompt-regexp))
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

(if (or (getenv "BIVIO_HTTPD_PORT")
	(file-exists-p "~/bconf")
	(file-exists-p "~/bconf.d"))
    (let
	;; Turn off tracing which may be on when debugging
	((res
	  (shell-command-to-string
	   (concat "env BCONF="
		   (or (getenv "BIVIO_DEFAULT_BCONF") "")
		   " bivio release list_projects_el --trace= 2>&1"))))
      (if (string-match "^(setq" res)
	  (eval (read res))
	(error "Error calling \"bivio release list_projects_el\": %s" res))
      (setq b-copyright-owner-history
	    (mapcar #'(lambda (lst) (caddr lst)) b-perl-projects)
	    b-perl-release-scope-history
	    (mapcar #'(lambda (lst) (car lst)) b-perl-projects)
	    b-copyright-owner (car b-copyright-owner-history)
	    b-perl-release-scope (car b-perl-release-scope-history))))

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
  (let ((count (max (or arg 1) 1)))	  ; Don't allow negative args
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


 ;; '(font-lock-comment-face ((t (:foreground "red"))))
 ;; '(font-lock-doc-face ((t (:foreground "red"))))
 ;; '(font-lock-string-face ((t (:foreground "magenta"))))
 ;; '(font-lock-keyword-face ((t (:foreground "blue"))))
 ;; '(font-lock-builtin-face ((t (:foreground "blue"))))
 ;; '(font-lock-function-name-face ((t (:foreground "green"))))
 ;; '(font-lock-type-face ((t (:foreground "cyan"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "green"))))

(custom-set-faces
 ;;; (what-cursor-position) \C-u \C-x = verbose details about face under cursor
 ;;; (list-colors-display)  colors available to emacs
 ;;; (describe-face) explains what faces are shown
 '(font-lock-comment-face ((t (:foreground "magenta"))))
 '(font-lock-doc-face ((t (:foreground "magenta"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-builtin-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-variable-name-face ((t (:foreground "blue"))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(sh-heredoc ((t (:foreground "red"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(diff-added-face ((t (:foreground "blue" :weight bold))))
 '(diff-changed-face ((t (:foreground "magenta" :weight extra-bold))))
 '(diff-context-face ((((class color) (background dark)) (:foreground "black"))))
 '(diff-file-header-face ((t nil)))
 '(diff-header-face ((t nil)))
 '(diff-removed-face ((t (:foreground "red" :weight bold))))
 '(minibuffer-prompt ((((background dark)) nil))))

(if (functionp 'server-force-delete)
    (server-force-delete))
(ignore-errors (server-start))
