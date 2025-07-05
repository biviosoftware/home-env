(provide 'bivio-init)
(require 'b-evil)
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
(require 'flymake-yamllint)
(require 'css-mode)
(require 'shell)
(require 'mmm-mode)
(require 'uniquify)
(require 'web-mode)

(global-font-lock-mode t)
(transient-mark-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(defvar bivio-redraw-after-isearch t "whether to call after redraw-display (only if in vagrant)")
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
(defun bivio-isearch-update-post-hook ()
  (if bivio-redraw-after-isearch
      (redraw-display)))
(if (and
     (file-accessible-directory-p "/vagrant")
     (not (string-match-p
           "^1\n"
           (ignore-errors
             (shell-command-to-string "grep -c '^core id[[:space:]]*:' /proc/cpuinfo")))))
    (add-hook 'isearch-update-post-hook 'bivio-isearch-update-post-hook))

(add-to-list 'compilation-error-regexp-alist
	     '(".*at \\([^ ]+\\) line \\([0-9]+\\)\\.?\n" 1 2))
(defun bivio-shell-mode-hook (&optional arg)
  (setq
   shell-dirstack-query "command dirs"
   explicit-shell-file-name shell-file-name)
  (add-hook 'window-configuration-change-hook 'b-comint-fix-window-size nil t)
;TODO(robnagler) Something useful when working on large shell buffers
;            ;; Disable font-locking in this buffer to improve performance
;            (font-lock-mode -1)
;            ;; Prevent font-locking from being re-enabled in this buffer
;            (make-local-variable 'font-lock-function)
;            (setq font-lock-function (lambda (_) nil))
;TODO(robnagler) this doesn't work for me, but would be useful
;            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (define-key shell-mode-map "\C-cc" 'dirs))

(if (file-readable-p "/bin/bash")
    (progn
      (setq
       shell-file-name "/bin/bash"
       shell-command-switch "-lc")
      (require 'bash-completion)
      (bash-completion-setup)
      (add-hook 'shell-mode-hook 'bivio-shell-mode-hook)))

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

(let ((suffixes
       '("as[cp]x" "[agj]sp" "djhtml" "erb" "html?" "json" "jsx?" "mustache"
          "phtml" "tpl\\.php" "tsx?" "vue"))
      (indent 4))
  (let ((pattern (concat "\\.\\(" (string-join suffixes "\\|") "\\)\\'")))
    (setq web-mode-code-indent-offset indent)
    (setq web-mode-css-indent-offset indent)
    (setq web-mode-markup-indent-offset indent)
    (add-to-list 'auto-mode-alist `(,pattern . web-mode))))

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.\\(sls\\|yml\\)$" . yaml-mode))

(setq flymake-yamllint-arguments
      (list "--config-data"
            "extends: default
rules:
  brackets:
    min-spaces-inside: 0
    max-spaces-inside: 1
  comments:
    require-starting-space: false
  comments-indentation: disable
  document-start: disable
  line-length: disable
  truthy: disable"))
(add-hook 'yaml-mode-hook 'flymake-yamllint-setup)
(add-hook 'yaml-mode-hook
          (lambda ()
            (flymake-mode)
            (flymake-yamllint-setup)))
(setq flymake-start-on-flymake-mode t)
(setq flymake-no-changes-timeout 0.3)

(defvar bivio-not-delete-trailing-whitespace-re "/[Ww]iki/\\w+$\\|/Radia/\\|/SRW/\\|/rshellweg/src/\\|\\.[Ii][Ii][Ff]$"
  "matches files to not delete trailing whitespace on save")
(defvar bivio-delete-trailing-whitespace t "whether to delete trailing whitespace on save")
(make-variable-buffer-local 'bivio-delete-trailing-whitespace)
(defun bivio-write-contents-hook ()
  "calls delete-trailing-whitespace in excursion"
  (save-excursion
    (delete-trailing-whitespace)))
(defun bivio-find-file-hook ()
  "Controls whether to delete trailing whitespace and tab-width"
  (let ((case-fold-search nil)
        (bn (or buffer-file-name "")))
    (if (string-match-p "/Radia/\\|/SRW/" bn)
        (set (make-local-variable 'tab-width) 4))
    (if (and bivio-delete-trailing-whitespace
             (not (string-match-p bivio-not-delete-trailing-whitespace-re bn)))
        (add-hook 'write-contents-functions 'bivio-write-contents-hook))))

(add-hook 'find-file-hook 'bivio-find-file-hook)
(defun bivio-css-mode-hook ()
  "set css-indent-offset to 2"
  (setq css-indent-offset 2))
(add-hook 'css-mode-hook 'bivio-css-mode-hook)

(defun b-comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))


(unless (fboundp 'comint-clear-buffer)
  (defun bivio-comint-clear-buffer ()
    "Clear the comint buffer. Remove when we are on emacs >= 25"
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))
  (define-key shell-mode-map "\C-c\M-o" 'bivio-comint-clear-buffer))

(defun b-ffap-with-line ()
  "Go to file under cursor possibly with line number."
  (interactive)
  (require 'ffap)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (if fname
	(let ((line (save-excursion
		      (goto-char (point-at-eol))
		      (and (re-search-backward "\\(?::\\|line \\)\\([0-9]+\\)"
					       (line-beginning-position)
					       t)
			   (string-to-number (match-string 1))))))
	  (with-no-warnings (find-file-at-point fname))
	  (when line
	    (goto-char (point-min))
	    (forward-line (1- line))))
      (user-error "File does not exist"))))

(setq comint-password-prompt-regexp
      (concat "\\s-[pP]assphrase: \\|\\s-[pP]assword: \\|"
	      comint-password-prompt-regexp))
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt)

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

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
  (shell-command "gpush"))

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
(define-key ctl-x-map "g" 'b-ffap-with-line)


;;;; VC keys
(define-key vc-prefix-map "p" 'bivio-git-push)

;; Allow rpm spec *.include files to set rpm
(custom-set-variables
 '(safe-local-variable-values (quote ((eval sh-set-shell "rpm" nil nil)))))

(if (functionp 'server-force-delete)
    (server-force-delete))
(ignore-errors (server-start))
