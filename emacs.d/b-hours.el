;;;  Copyright (c) 2007 bivio Software, Inc.  All rights reserved.
;;; 
;;;  Visit http://www.bivio.biz.
;;;  
;;;  This library is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU Lesser General Public License as
;;;  published by the Free Software Foundation; either version 2.1 of the
;;;  License, or (at your option) any later version.
;;;  
;;;  This library is distributed in the hope that it will be useful, but
;;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;  Lesser General Public License for more details.
;;;  
;;;  You should have received a copy of the GNU Lesser General Public
;;;  License along with this library;  If not, you may get a copy from:
;;;  http://www.opensource.org/licenses/lgpl-license.html
;;;  
;;;  $Id: b-hours.el,v 1.5 2007/11/25 14:40:55 nagler Exp $
;;;
(provide 'b-hours)
(require 'b-perl)
(defvar b-hours-project
  nil
  "*Most recent project")
(defvar b-hours-description-history
  '("Email" "Administrivia" "Accounting" "Sysadmin")
  "*Description history")

(defun b-hours-add ()
  "Insert hours into ~/hours.txt"
  (interactive)
  (let
      ((project
	(completing-read "Project: "
			 (mapcar
			  (lambda (p)
			    (list (car p) 1))
			  b-perl-projects)
			 nil
			 t
			 (or
			  (car (b-perl-which-project
				(or buffer-file-name "")))
			  (car b-hours-project))
			 'b-hours-project
			 nil))
       (hours (float (string-to-number (read-string "Hours: "))))
       (desc
	(let
	    ((minibuffer-local-completion-map
	      (copy-keymap minibuffer-local-completion-map)))
	  (define-key minibuffer-local-completion-map " " 'self-insert-command)
	  (completing-read "Description: "
			 b-hours-description-history
			 nil
			 nil
			 nil
			 'b-hours-description-history
			 nil)))
       (curr (current-buffer)))
    (save-excursion
      (if (<= (length desc) 0)
	  (error "must specify a description"))
      (find-file (format-time-string "~/artisans/hours/%Y%m.txt"))
      (goto-char (point-min))
      (re-search-forward (concat "^" project "$"))
      (forward-char)
      (insert
       (format "%8s% 7.1f\n"
	       (format-time-string "%m/%d/%y")
	       hours))
      (let
	  ((start (point))
	   (fill-column 60))
	(insert (concat desc "\n"))
	(fill-region start (point))
	(indent-rigidly start (point) 16)
	(goto-char start))
      (delete-char -1)
      (fixup-whitespace)
      (save-buffer)
      (switch-to-buffer curr))))
(define-key global-map "\C-x," 'b-hours-add)
