; Copyright (c) 2014 bivio Software, Inc.  All rights reserved.
(provide 'b-python)
(require 'python)
(load-library "mmm-rst-python")

(defvar b-python-copyright-owner "Bivio Software, Inc."
  "*Who owns the copyrights for templates")

(defvar b-python-license "http://www.apache.org/licenses/LICENSE-2.0.html"
  "*What license")

(defun b-python-compile-command ()
  "Returns a string compile-command."
  (concat
   (cond
    ((string-equal "pytest" (b-python-file-type)) "py.test ")
    (t "python "))
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "")))

(defun b-python-copyright nil
  "Returns a copyright statement for this year."
  (format "Copyright (c) %s %s  All Rights Reserved."
	  (format-time-string "%Y" (current-time))
	  b-python-copyright-owner))

(defun b-python-file-type nil
  "Returns module or pytest"
  (cond
   ((not (buffer-file-name)) "module")
   ((string-match "\\(/test_[^/]+\\|_test\\)\\.py$" (buffer-file-name)) "pytest")
   ((string-match "\\.py$" (buffer-file-name)) "module")
   (t nil)))

(defun b-python-flask-restart nil
  "Start manage.py in the current python root"
  (interactive)
  (let
      ((prev-shell nil)
       (prev-buffer (current-buffer))
       (dir (b-python-project-root)))
    (progn
      (message "Restarting flask")
;TODO(robnagler): Keep existing window just like compile command does
      (if (get-buffer "*flask*")
	  (progn
	    (set-buffer (get-buffer "*flask*"))
	    (kill-buffer (get-buffer "*flask*"))
	    (set-buffer prev-buffer)))
      (if (get-buffer "*shell*")
	  (progn
	    (set-buffer "*shell*")
	    (setq prev-shell (rename-buffer (generate-new-buffer-name "tmp")))
	    (set-buffer prev-buffer)))
      (shell)
      (set-buffer (get-buffer "*shell*"))
      (rename-buffer "*flask*")
      (accept-process-output (get-buffer-process (current-buffer)))
      (set-buffer (get-buffer "*flask*"))
      (insert "cd " dir " && python manage.py runserver --host 0.0.0.0 --port 8000 --debug")
      (comint-send-input)
      (if prev-shell
	  (progn
	    (set-buffer (get-buffer prev-shell))
	    (rename-buffer "*shell*"))))
    (switch-to-buffer prev-buffer)))

(defun b-python-project-root (&optional d)
  "Find project's root"
  (let
      ((pwd (or d (directory-file-name
		   (file-name-directory
		    (or (buffer-file-name) (error "buffer has no name")))))))
    (if (string-equal pwd "/")
	(error "not in python directory")
      (if (file-exists-p (concat pwd "/setup.py"))
	  pwd
	(b-python-project-root
	 (directory-file-name (file-name-directory pwd)))))))

(defun b-python-template nil
  "inserts template according to file name"
  (interactive)
  (funcall
   (intern-soft
    (concat "b-python-template-" (b-python-file-type)))))

(defun b-python-template-module nil
  "inserts a template for a python module."
  (goto-char (point-min))
  (insert "# -*- coding: utf-8 -*-
u\"\"\"?

:copyright: " (b-python-copyright) "
:license: " b-python-license "
\"\"\"
from __future__ import absolute_import, division, print_function
from pykern.pkdebug import pkdc, pkdp
")
  (goto-char (point-min))
  (re-search-forward "\\?"))


(defun b-python-template-pytest nil
  "inserts a template for a pytest."
  (progn
    (goto-char (point-min))
    (insert "import pytest
")
    (b-python-template-module)))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'compile-command)
		  (b-python-compile-command))
             (mmm-mode)))

(progn
  (define-key python-mode-map "\C-c\C-m" 'compile)
  (define-key python-mode-map "\C-ch" 'b-python-flask-restart)
  (define-key python-mode-map "\C-ct" 'b-python-template))
