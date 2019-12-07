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
(require 'mmm-mode)
(require 'uniquify)
(require 'xterm-color)
;;; remove after emacs 23 is gone
(ignore-errors (require 'web-mode))
(ignore-errors (require 'espresso))
(ignore-errors (require 'java-mode))

(global-font-lock-mode t)
(transient-mark-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq
 case-fold-search t
 case-replace t
 completion-auto-help 'lazy
 completion-ignore-case t
 dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
 dabbrev-case-fold-search t
 dabbrev-case-replace nil
 font-lock-maximum-decoration t
 highlight-nonselected-windows nil
 history-delete-duplicates t
 indent-line-function 'indent-relative-maybe
 large-file-warning-threshold 100000000
 next-line-add-newlines nil
 pcomplete-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ring-bell-function 'ignore
 term-term-name "eterm"
 truncate-partial-width-windows nil
 visible-bell t
 )
(blink-cursor-mode -1)
(set-cursor-color "red")
(setq-default indent-tabs-mode nil)
(setq uniquify-buffer-name-style 'forward)

(if (fboundp 'ansi-color-for-comint-mode-off)
    (ansi-color-for-comint-mode-off))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;;; Hack to check if running in vagrant, and need to add "redraw"
;;; See: http://emacs.stackexchange.com/questions/9512/why-does-the-buffer-get-garbled
(if (and
     (file-accessible-directory-p "/vagrant")
     (not (string-match-p
           "^1\n"
           (ignore-errors
             (shell-command-to-string "grep -c '^core id[[:space:]]*:' /proc/cpuinfo")))))
    (add-hook 'isearch-update-post-hook 'redraw-display))

(add-to-list 'compilation-error-regexp-alist
	     '(".*at \\([^ ]+\\) line \\([0-9]+\\)\\.?\n" 1 2))
(if (file-readable-p "/bin/bash")
    (progn
      (setq
       shell-file-name "/bin/bash"
       shell-command-switch "-lc")
      (require 'bash-completion)
      (bash-completion-setup)
      (add-hook
       'shell-mode-hook
       (lambda (&optional arg)
         (setq
          shell-dirstack-query "command dirs"
          explicit-shell-file-name shell-file-name)
         (add-hook 'window-configuration-change-hook 'b-comint-fix-window-size nil t)
         (define-key shell-mode-map "\C-cc" 'dirs)))))

;;; https://github.com/syl20bnr/spacemacs/issues/6820#issuecomment-239665146
;;; keep same window for starting shells or listing buffers
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*Buffer List\\*" display-buffer--same-window-action) display-buffer-alist)


(defalias 'perl-mode 'cperl-mode)
(add-to-list
 'auto-mode-alist '("\\.\\(bview\\|bconf\\|btest\\|bunit\\|t\\|pl\\|PL\\|pm\\)\\'" . cperl-mode))
; interpreter-mode-alist takes priority over auto-mode-alist
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

(define-key java-mode-map "\C-c\C-m" 'compile)

(if (fboundp 'web-mode)
    (progn
      (setq web-mode-code-indent-offset 4)
      (setq web-mode-css-indent-offset 4)
      (setq web-mode-markup-indent-offset 4)
      (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))))


(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(add-to-list 'auto-mode-alist '("\\.\\(sls\\|yml\\)$" . yaml-mode))

;(defvar bivio-delete-trailing-whitespace t)
;(setq bivio-delete-trailing-whitespace nil)
(add-hook 'find-file-hook
	  (lambda ()
            (let ((case-fold-search nil)
                  (bn (or buffer-file-name "")))
              (if (string-match-p "/Radia/\\|/SRW/" bn)
                  (set (make-local-variable 'tab-width) 4))
              (if (if (boundp 'bivio-delete-trailing-whitespace)
                      bivio-delete-trailing-whitespace
                    (not (string-match-p "/[Ww]iki/\\w+$\\|/Radia/\\|/SRW/" bn)))
                  (add-hook 'write-contents-functions
                            (lambda()
                              (save-excursion
                                (delete-trailing-whitespace))))))))
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
      (concat "\\s-[pP]assphrase: \\|\\s-[pP]assword: \\|"
	      comint-password-prompt-regexp))
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

(if (and (getenv "BCONF")
         (or (getenv "BIVIO_HTTPD_PORT")
             (file-exists-p "~/bconf")
             (file-exists-p "~/bconf.d")))
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

(defun bivio-git-push ()
  "Calls git push in the current directory"
  (interactive)
  (message "Pushing...")
  (shell-command "git push"))

(define-key global-map "\177" 'backward-delete-char-untabify)

(define-key minibuffer-local-completion-map " " 'minibuffer-complete-word)

;;;; Control keys
(define-key text-mode-map "\C-i" 'shift-right)
(define-key global-map "\C-m" 'newline-and-indent)
(define-key global-map "\C-r" 'isearch-backward-regexp)
(define-key global-map "\C-s" 'isearch-forward-regexp)
(define-key global-map "\C-z" 'scroll-one-line-up)

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


;;;; VC keys
(define-key vc-prefix-map "p" 'bivio-git-push)

 ;; '(font-lock-comment-face ((t (:foreground "red"))))
 ;; '(font-lock-doc-face ((t (:foreground "red"))))
 ;; '(font-lock-string-face ((t (:foreground "magenta"))))
 ;; '(font-lock-keyword-face ((t (:foreground "blue"))))
 ;; '(font-lock-builtin-face ((t (:foreground "blue"))))
 ;; '(font-lock-function-name-face ((t (:foreground "green"))))
 ;; '(font-lock-type-face ((t (:foreground "cyan"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "green"))))

;; Allow rpm spec *.include files to set rpm
(custom-set-variables
 '(safe-local-variable-values (quote ((eval sh-set-shell "rpm" nil nil)))))

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
 '(cperl-hash-face ((t (:foreground "black"))))
 '(cperl-array-face ((t (:foreground "black"))))
 '(sh-heredoc ((t (:foreground "magenta"))))
 '(font-lock-string-face ((t (:foreground "magenta"))))
 '(diff-added-face ((t (:foreground "blue" :weight bold))))
 '(diff-changed-face ((t (:foreground "magenta" :weight extra-bold))))
 '(diff-context-face ((((class color) (background dark)) (:foreground "black"))))
 '(diff-file-header-face ((t nil)))
 '(diff-header-face ((t nil)))
 '(diff-removed-face ((t (:foreground "red" :weight bold))))
 '(minibuffer-prompt ((((background dark)) nil)))
 '(mmm-default-submode-face ((t (:foreground "red" :background nil))))
 '(mmm-init-submode-face ((t (:background nil))))
 '(mmm-cleanup-submode-face ((t (:background nil))))
 '(mmm-declaration-submode-face ((t (:background nil))))
 '(mmm-comment-submode-face ((t (:background nil))))
 '(mmm-output-submode-face ((t (:background nil))))
 '(mmm-special-submode-face ((t (:background nil))))
 '(mmm-code-submode-face ((t (:background nil))))
 '(web-mode-html-tag-face ((t (:foreground "blue"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "blue"))))
 '(web-mode-html-attr-name-face ((t (:foreground "cyan"))))
 '(web-mode-html-attr-value-face ((t (:foreground "magenta"))))
 '(rst-level-1 ((t nil)))
 )

(if (functionp 'server-force-delete)
    (server-force-delete))
(ignore-errors (server-start))
