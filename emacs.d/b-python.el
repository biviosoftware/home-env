; Copyright (c) 2014 bivio Software, Inc.  All rights reserved.
(provide 'b-python)
(require 'python)

(defvar b-python-copyright-owner "RadiaSoft LLC."
  "*Who owns the copyrights for templates")

(defvar b-python-license "http://www.apache.org/licenses/LICENSE-2.0.html"
  "*What license")

(defun b-python-compile-command ()
  "Returns a string compile-command."
  (concat
   (cond
    ((string-equal "pytest" (b-python-file-type)) "pykern test ")
    (t "python "))
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "")))

(defun b-python-copyright nil
  "Returns a copyright statement for this year."
  (format "Copyright (c) %s %s  All Rights Reserved."
	  (format-time-string "%Y" (current-time))
	  b-python-copyright-owner))

; don't double indent on a continuation
(setq-default python-indent-def-block-scale 1)

(defun b-python-indent-context-docstring (orig-fun &rest args)
  "Keep indent inside docstrings to non-empty previous line.
See http://stackoverflow.com/a/32059968/3075806 for explanation."
  (let ((res (apply orig-fun args)))
    (pcase res
      (`(:inside-string . ,start)
       `(:inside-string . ,(save-excursion
                             (re-search-backward "[^[:space:]\n]")
                             (back-to-indentation)
                             (point))))
      (_ res))))
(if (boundp 'advice-add)
    (advice-add 'python-indent-context :around #'b-python-indent-context-docstring))

(defun b-python-file-type nil
  "Returns module or pytest"
  (cond
   ((not (buffer-file-name)) "module")
   ((string-match "\\(/test_[^/]+\\|_test\\)\\.py$" (buffer-file-name)) "pytest")
   ((string-match "\\.py$" (buffer-file-name)) "module")
   (t nil)))

(defun b-python-maybe-pykern-fmt-run ()
  (when (string-match
         (concat "^" (expand-file-name "src/radiasoft" "~") ".*")
         (buffer-file-name))
    (add-hook 'after-save-hook #'b-python-pykern-fmt-run
              nil t)))

(defun b-python-sirepo-service-restart (service)
  "Start sirepo service in project root"
  (interactive)
  (let
      ((prev-shell nil)
       (prev-buffer (current-buffer))
       (dir (b-python-project-root))
       (service-buffer-name (concat "*" service "-sirepo*"))
       (shell-buffer-name "*shell*")
       (kill-buffer-query-functions nil))
    (progn
      (message "Restarting sirepo server")
;TODO(robnagler): Keep existing window just like compile command does
      (if (get-buffer service-buffer-name)
	  (progn
	    (set-buffer (get-buffer service-buffer-name))
            (kill-buffer (get-buffer service-buffer-name))
	    (set-buffer prev-buffer)))
      (if (get-buffer shell-buffer-name)
	  (progn
	    (set-buffer shell-buffer-name)
	    (setq prev-shell (rename-buffer (generate-new-buffer-name "tmp")))
	    (set-buffer prev-buffer)))
      (shell)
      (set-buffer (get-buffer shell-buffer-name))
      (rename-buffer service-buffer-name)
      (accept-process-output (get-buffer-process (current-buffer)))
      (set-buffer (get-buffer service-buffer-name))
      (insert "cd " dir " && sirepo service " service "; stty -echo")
      (comint-send-input)
      (if prev-shell
	  (progn
	    (set-buffer (get-buffer prev-shell))
	    (rename-buffer shell-buffer-name))))
    (switch-to-buffer prev-buffer)))

(defun b-python-server-restart nil
  "Start manage.py in the current python root"
  (interactive)
  (if (string-match "/sirepo/" (buffer-file-name))
      (b-python-sirepo-service-restart "http")))


(defun b-python-project-root (&optional d)
  "Find project's root"
  (let
      ((pwd (or d (directory-file-name
		   (file-name-directory
		    (or (buffer-file-name) (error "buffer has no name")))))))
    (if (string-equal pwd "/")
	(error (concat "not in python directory=" pwd))
      (if (or (file-exists-p (concat pwd "/pyproject.toml"))
          (file-exists-p (concat pwd "/setup.py")))
	  pwd
	(b-python-project-root
	 (directory-file-name (file-name-directory pwd)))))))

(defun b-python-pykern-fmt-run ()
  (shell-command (concat "pykern fmt run "
                         (buffer-file-name)))
  (minibuffer-message "Running pykern fmt")
  (revert-buffer :ignore-auto :noconfirm :preserve-modes))

(defun b-python-template nil
  "inserts template according to file name"
  (interactive)
  (funcall
   (intern-soft
    (concat "b-python-template-" (b-python-file-type)))))

(defun b-python-template-module (&optional no-imports)
  "inserts a template for a python module."
  (goto-char (point-min))
  (insert
   (concat "\"\"\"?

:copyright: " (b-python-copyright) "
:license: " b-python-license "
\"\"\"
" (if no-imports ""
"from pykern.pkcollections import PKDict
from pykern.pkdebug import pkdc, pkdlog, pkdp
")))
  (goto-char (point-min))
  (re-search-forward "\\?"))


(defun b-python-template-pytest nil
  "inserts a template for a pytest."
  (progn
    (goto-char (point-min))
    (b-python-template-module t)))

(add-hook 'python-mode-hook
          '(lambda ()
             (set (make-local-variable 'compile-command)
                  (b-python-compile-command))
             (abbrev-mode)
             (b-python-maybe-pykern-fmt-run)))

(progn
  (define-key python-mode-map "\C-c\C-m" 'compile)
  (define-key python-mode-map "\C-ch" 'b-python-server-restart)
  (define-key python-mode-map "\C-ct" 'b-python-template))
