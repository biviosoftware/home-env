;;;  Copyright (c) 2004-2007 bivio Software, Inc.  All rights reserved.
;;;
;;;  Visit http://www.bivio.biz for more info.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Perl (cperl-mode)
;;;
(provide 'b-perl)
(require 'cperl-mode)
(require 'cl)
(require 'thingatpt)
(defconst b-perl-src-re "\\(.*/\\(src[^/]*\\)/\\)perl/")
(defvar b-perl-projects
  '(("YourPackage" "project-prefix" "Your Company, Inc."))
  "*The list of project atrributes: '((\"PerlPackage\" \"prefix\" \"Your Company, Inc.\")).  Generally output from bivio Release list_projects_el")
(defvar b-copyright-owner "YOUR COMPANY"
  "*The name of the copyright owner")
(defvar b-copyright-owner-history
  '("YOUR COMPANY")
  "*List of possible copyright owners")
(defvar b-perl-refactoring
  nil
  "*Most recent refactoring")
(defvar b-perl-refactoring-history
  nil
  "*A list of recent refactorings")

(defvar b-perl-insert-variable
  '(
    "Type.Amount"
    "Type.DateTime"
    "Type.FilePath"
  )
  "*A list of recent variables")

(defun b-perl ()
  "b-perl provides configuration and functions to format your Perl according
to the bOP coding style guide.  b-perl is built on cperl-mode, which it
installs as a replacement for perl-mode.

The following key bindings are defined when you load this file:

key             binding
---             -------
C-c b           b-perl-call-super
C-c h           b-perl-httpd-restart
C-c f           b-perl-find-module
C-c l           b-perl-btest-last-log
C-c p           b-perl-man-builtin
C-c r		b-perl-refactor-rename-method
C-c t		b-perl-template
C-c s		b-perl-insert-method
C-c v		b-perl-insert-variable
C-c u           b-perl-insert-import
C-c C-e		next-error
C-c C-m		compile
C-c C-r         b-perl-refactor
C-c e           b-perl-bunit-next-error-buf

You can also define the following variables:

The b-copyright-owner will be inserted when you b-perl-template-pm
or b-perl-template-:

    (setq b-copyright-owner \"My Company, Inc.\")
    (setq b-copyright-owner-history
      '(\"My Company, Inc.\"
	\"Some Other Client, Inc.\"))
"
  (interactive)
  (describe-function 'b-perl))

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (cperl-set-style "PerlStyle")
	    (setq cperl-continued-statement-offset 4)
	    (setq cperl-merge-trailing-else nil)
	    (setq cperl-tab-always-indent nil)
	    (setq fill-column 79)
	    (set (make-local-variable 'compile-command)
		 (b-perl-compile-command))
	    (if (>= (string-to-number cperl-version) 5.0)
		(progn
		  (setq cperl-close-paren-offset -4)
		  (setq cperl-brace-offset 0)
		  (setq cperl-indent-parens-as-block t)
		  (set (make-local-variable 'dabbrev-abbrev-char-regexp)
		       "\\sw\\|\\s_\\|:")))
	    (if (>= (string-to-number cperl-version) 6.0)
		(progn
		  (setq cperl-continued-brace-offset -4)
		  (setq cperl-indent-parens-as-block t)))
	    ))

(defun b-perl-call-super nil
  "Generates a call to SUPER::<current sub>"
  (interactive)
  (let
      ((start (point)))
    (insert
     "return shift->SUPER::"
     (save-excursion
       (re-search-backward "^sub ")
       (forward-word 2)
       (thing-at-point 'b-perl-word))
     "(@_);")
    (cperl-indent-region start (point))))


(defun b-perl-find-module nil
  "Finds file of module at point"
  (interactive)
  (find-file-other-window
   (b-perl-file-name-from-module
    (shell-command-to-string
     (concat
      (b-perl-env)
      "bivio class qualified_name "
      (thing-at-point 'b-perl-module)
      " --TRACE= 2>&1")))))

(defun b-perl-man-builtin nil
  "Shows perlfunc man page entry for function at point"
  (interactive)
  (let ((builtin (read-string "Perl builtin: " (thing-at-point 'b-perl-word))))
    (man "perlfunc")
    (save-excursion
      (other-window 1)
      (goto-char (point-min))
      (re-search-forward
       (concat "^       "  builtin  "\\( \\|$\\)"))))
  (other-window -1))

(defun b-perl-btest-last-log (arg)
  "Finds last log for the btest in the buffer"
  (interactive "P")
  (let
      ((tag (and
	     arg
	     (read-string "Case tag: " (thing-at-point 'b-perl-word)))))
    (dired-other-window
     (concat
      "log/"
      (file-name-sans-extension
       (file-name-nondirectory (buffer-file-name)))))
    (dired-revert)
    (if tag
	(progn
	  (goto-char (point-min))
	  (re-search-forward (concat "-" tag "$")))
      (progn
	(goto-char (point-max))
	(forward-line -2)
	(end-of-line)))
    (dired-find-file)))

(defun b-perl-project-prefix nil
  "Returns short code for project"
  (nth 1 (b-perl-which-project (buffer-file-name))))

(defun b-perl-httpd-restart nil
  "Start an b-httpd in the current project's root"
  (interactive)
  (let
      ((project (b-perl-project-prefix))
       (prev-shell nil)
       (prev-buffer (current-buffer))
       (env (b-perl-env)))
    (progn
      (message "Restarting b-httpd")
;TODO: Keep existing window just like compile command does
      (if (get-buffer "*httpd*")
	  (progn
	    (set-buffer (get-buffer "*httpd*"))
	    (kill-buffer (get-buffer "*httpd*"))
	    (set-buffer prev-buffer)))
      (if (get-buffer "*shell*")
	  (progn
	    (set-buffer "*shell*")
	    (setq prev-shell (rename-buffer (generate-new-buffer-name "tmp")))
	    (set-buffer prev-buffer)))
      (shell)
      (set-buffer (get-buffer "*shell*"))
      (rename-buffer "*httpd*")
      (accept-process-output (get-buffer-process (current-buffer)))
      (set-buffer (get-buffer "*httpd*"))
      (insert env " bivio httpd run")
      (comint-send-input)
      (if prev-shell
	  (progn
	    (set-buffer (get-buffer prev-shell))
	    (rename-buffer "*shell*"))))
    (switch-to-buffer prev-buffer)))

(defun b-perl-do-insert-variable (name value &optional is-our)
  (goto-char (point-min))
  (re-search-forward "^\\(our\\|my\\)(")
  (re-search-forward "^$")
  (if name
      (insert
       (if is-our "our" "my")
       "($_"
       (upcase name)
       ")"
       (if value (concat " = " value) "")
       ";\n")
    (if value (insert value))))

(defun b-perl-do-insert-method (name body type)
    (goto-char (point-max))
    (re-search-backward "^1;$")
    (let ((found nil)
	  (last (point)))
      (catch 'loop
	(while (re-search-backward "^sub \\([_a-zA-Z0-9]+\\)" nil t)
	  (setq found (match-string 1))
	  (cond
	   ((eq 'CONSTANT type) (if (and (string-match "^[A-Z]" found)
					 (string< found name))
				    (throw 'loop (goto-char last))))
	   ((eq 'PRIVATE type) (if (or (not (string-match "^_" found))
				       (string< found name))
				    (throw 'loop (goto-char last))))
	   (t (if (or (and (string-match "^[a-z]" found)
			   (string< found name))
		      (string-match "^[A-Z]" found))
		  (throw 'loop (goto-char last)))))
	  (setq last (point)))))
    (if name
	(progn
	  (insert "sub " name " {\n}\n\n")
	  (forward-char -4)))
    (if body
	(insert body))
    (if (re-search-backward "@@" nil t)
	(delete-char 2)))


(defun b-perl-do-insert-import (module)
  "Insert import into import list"
  (if (not module)
      (setq module (read-string "module (class) name: ")))
  (goto-char (point-min))
  (re-search-forward "^use ")
  (re-search-forward "^$")
  (insert "b_use('" module "');\n"))

(defconst b-perl-word-chars "A-Za-z0-9_:")
(put 'b-perl-word 'end-op
     (lambda () (skip-chars-forward b-perl-word-chars)))
(put 'b-perl-word 'beginning-op
     (lambda () (skip-chars-backward b-perl-word-chars)))

(defconst b-perl-module-chars (concat b-perl-word-chars "."))
(put 'b-perl-module 'end-op
     (lambda () (skip-chars-forward b-perl-module-chars)))
(put 'b-perl-module 'beginning-op
     (lambda () (skip-chars-backward b-perl-module-chars)))

(defun b-perl-insert-use ()
  "Add word at point as import or prompt"
  (interactive)
  (let
      ((module (thing-at-point 'b-perl-word)))
    (b-perl-do-insert-import (if (string-match "\\:\\:" module) module nil))))

(defun b-perl-insert-method (&optional name body type)
  "insert a perl method (sub) in lexographically correct location.
If the method begins with '_' it will be inserted in private subroutines
section.

Handles look up to see if there's a b-perl-insert-<name> function that
handles special method names.

You may include a qualifier in the name, e.g.

   a some_abstract
   c some_constant
   f some_factory
   p _some_private
   s some_static

Otherwise, inserts in the methods section."
  (interactive)
  (let
      ((special nil)
       (qualifier ?!)
       (case-fold-search nil))
    (if (not name)
	(setq name (read-string "method (sub) name: ")))
    (if (> (length (split-string name)) 1)
	(progn
	  (setq qualifier (string-to-char (nth 0 (split-string name))))
	  (setq name (nth 1 (split-string name)))))
    (if qualifier (message (char-to-string qualifier) "HEY"))
    (if (not type)
	(progn
	  (setq type
		(cl-case qualifier
		  (?a 'ABSTRACT)
		  (?c (setq name (upcase name)) 'CONSTANT)
		  (?f 'FACTORY)
		  (?p 'PRIVATE)
		  (?s 'STATIC)
		  (?! (cond
		      ((string-match "^new" name) 'FACTORY)
		      ((string-match "^_" name) 'PRIVATE)
		      ((string-match "^[A-Z]" name) 'CONSTANT)
		      (t 'SIMPLE)))
		  (t (error "bad qualifier"))))))
    (if body
	(b-perl-do-insert-method name body type)
      (funcall (or (intern-soft (concat "b-perl-insert-method-" name))
		   (intern-soft
		    (concat "b-perl-insert-method-"
			    (downcase (symbol-name type)))))
	       name
	       type))))

(defun b-perl-insert-method-constant (name type)
  (b-perl-do-insert-method name "
    return @@;"
			   type))

(defun b-perl-insert-method-USAGE (name type)
  (b-perl-do-insert-method name (concat "
    return <<'EOF';
usage: bivio "
    (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
    " [options] command [args..]
commands
  @@ --
EOF")
			   'CONSTANT))

(defun b-perl-insert-method-private (name type)
  (b-perl-do-insert-method name "
    my(@@) = @_;
    return;"
			   type))

(defun b-perl-insert-method-factory (name type)
  (b-perl-do-insert-method
   name
   (concat "
    my($self) = shift->SUPER::" name "(@_);
    $self->[$_IDI] = {
        @@
    };
    return $self;")
   type))

(defun b-perl-insert-method-static (name type)
  (b-perl-do-insert-method name "
    my($proto) = @_;
    @@
    return;"
			   type))

(defun b-perl-insert-method-abstract (name type)
  (b-perl-do-insert-method name "
    my($self@@) = @_;
    die('abstract method called');
    # DOES NOT RETURN"
			   type))

(defun b-perl-insert-method-simple (name type)
  (b-perl-do-insert-method name "
    my($self) = @_;
    @@
    return;"
			   type))

(defun b-perl-insert-method-internal_post_load_row (name type)
  (b-perl-do-insert-method name "
    my($self, $row) = @_;
    return 0
	unless shift->SUPER::internal_post_load_row(@_);
    @@
    return 1;"
			   type))

(defun b-perl-insert-method-internal_pre_execute (name type)
  (b-perl-do-insert-method name "
    my($self) = @_;
    my(@res) = shift->SUPER::internal_pre_execute(@_);
    @@
    return @res;"
			   type))

(defun b-perl-insert-method-handle_config (name type)
  (b-perl-do-insert-method name "
    my(undef, $cfg) = @_;
    $_CFG = $cfg;
    return;"
			   type)
  (b-perl-do-insert-variable nil "b_use('IO.Config')->register(my $_CFG = {
    @@<required> => b_use('IO.Config')->REQUIRED,
    b_use('IO.Config')->NAMED => {
        <optional> => <default>,
    },
});"))

(defun b-perl-insert-method-_trace (name type)
  (save-excursion
    (b-perl-do-insert-variable "TRACE" nil t)
    (b-perl-do-insert-import "IO.Trace")))

(defun b-perl-insert-method-handle_die (name type)
  (b-perl-do-insert-method name "
    my($proto, $die) = @_;
    # @@
    return;"
			   type))


(defun b-perl-insert-method-internal_initialize (name type)
  (b-perl-do-insert-method name "
    my($self) = @_;
    return $self->merge_initialize_info($self->SUPER::internal_initialize, {
        version => 1,
        @@
    });"
			   type))

(defun b-perl-insert-method-execute (name type)
  (b-perl-do-insert-method name "
    my($proto, $req) = @_;
    @@
    return 0;"
			   type))

(defun b-perl-insert-method-render (name type)
  (b-perl-do-insert-method name "
    my($self, $source, $buffer) = @_;
    $$buffer .= @@;
    return;"
			   type))


(defun b-perl-insert-variable ()
  "insert a global variable, which always begin with \"$_\", but you may
leave this off when entering the variable name at the prompt.

If the variable name matches one of the following special cases,
the variable will be initialized as defined:

   my($_A) = b_use('Type.Amount');
   my($_D) = b_use('Type.Date');
   my($_DT) = b_use('Type.DateTime');
   my($_IDI) = __PACKAGE__->instance_data_index;
   my($_LQ) = b_use('SQL.ListQuery');
or
   Type.DateTime will render my($_DT) = b_use('Type.DateTime');
"
  (interactive)
  (let
      ((name (read-string "global variable name: "
			  nil 'b-perl-insert-variable)))
    (if (string-match "^\\$?_\\(.*\\)" name)
	(setq name (match-string 1)))
    (let
	((special (intern-soft
		   (concat "b-perl-insert-variable-" (downcase name)))))
      (if special
	  (funcall special)
	(unless (b-perl-do-insert-class-variable name)
	  (b-perl-do-insert-variable (upcase name) "")
	  (re-search-backward ";"))))))

(defun b-perl-do-insert-class-variable (name)
  (if (string-match "^.+\\.\\(.+\\)$" name)
      (progn
	(b-perl-do-insert-variable
	 (let ((base (match-string 1 name))
	       (case-fold-search nil))
	   (apply 'concat (mapcar (lambda (a)
				    (let ((s (string a)))
				      (if (string-match "[A-Z]" s)
					  s)))
				  (string-to-list base))))
	 (concat "b_use('" name "')"))
	t)))

(defun b-perl-insert-variable-a ()
  (b-perl-do-insert-variable "A" "b_use('Type.Amount')"))

(defun b-perl-insert-variable-d ()
  (b-perl-do-insert-variable "D" "b_use('Type.Date')"))

(defun b-perl-insert-variable-dt ()
  (b-perl-do-insert-variable "DT" "b_use('Type.DateTime')"))

(defun b-perl-insert-variable-idi ()
  (b-perl-do-insert-variable "IDI" "__PACKAGE__->instance_data_index"))

(defun b-perl-insert-variable-lq ()
  (b-perl-do-insert-variable "LQ" "b_use('SQL.ListQuery')"))

(defun b-perl-module-from-file-name (name)
  "find the /perl/ part and use all names after as package components.  Replaces / with ::."
  (setq name (file-name-sans-extension name))
  (if (string-match "/\\(perl/\\)" name)
      (mapconcat
       (lambda (a) a)
       (split-string (substring name (match-end 1)) "/")
       "::")
    ""))

(defun b-perl-file-name-from-module (module)
  "Module may be Foo.Bar or Foo::Bar.  Uses class maps"
  (concat
   (or (and (string-match b-perl-src-re buffer-file-name)
	    (match-string 0 buffer-file-name))
       (error "Not in src/perl directory"))
   (mapconcat
    (lambda (a) a)
    (split-string module "[::\n\r]" t)
    "/")
   ".pm"))

(defun b-perl-file-name-from-package (package)
  "Assumes we are in a perl file and we are now finding a module relative to the current file"
  (concat
   (or (and (string-match b-perl-src-re buffer-file-name)
	    (match-string 0 buffer-file-name))
       (error "Not in src/perl directory"))
   (mapconcat
    (lambda (a) a)
    (split-string package "::")
    "/")
   ".pm"))

(defun b-perl-template nil
  "inserts template according to file name"
  (interactive)
  (funcall
   (intern-soft
    (concat "b-perl-template-" (or (file-name-extension buffer-file-name) "")))))

(defun b-perl-template-btest nil
  "inserts a template for an acceptance test"
  (interactive)
  (b-set-copyright-owner)
  (insert "# " (b-copyright) "
test_setup('" (car (b-perl-which-project (buffer-file-name))) "');
"))

(defun b-perl-template-bunit nil
  "inserts a template for a unit test"
  (interactive)
  (b-set-copyright-owner)
  (insert "# " (b-copyright) "
")
  ;;; template-table elements: (file-suffix template &optional post-function)
  (let* ((template-table '(("List" "ListModel();
[
    load_all => [
        [] => undef,
    ],
];
")
                 ("Form" "FormModel();
[
    error_case({
        'Model1.field1' => 'value1',
        'Model1.field2' => 'bogus',
    } => {
        'Model1.field2' => 'SOME_ERROR',
    }),
    simple_case({
        'Model1.field1' => 'value1',
        'Model1.field2' => 'value2',
    } => {
        field1 => 'value1',
        field2 => 'value2',
    }),
    # non-simple case (e.g. multiple models)
    [{
        'Model1.field1' => 'fixedvalue1',
        'Model2.field5' => 'value2',
    }] => [{
        'Model.Model1' => {
            field1 => 'fixedvalue1',
        },
        'Model.Model2' => {
            field5 => 'value2',
        },
    }],
];
")
                 ("Type" "Type();
[
    from_literal => [
        '' => UNDEF(),

    ],
];
" (lambda ()
    (re-search-backward "],")
    (previous-line)))))

         ;;; parse filename for which template to apply and use
         ;;; the last item in template-table if none match.
         (basename (file-name-sans-extension
                (file-name-nondirectory (buffer-file-name))))

	 (filetype
	  (or (first (member
	      (progn (string-match "\\(....\\)[0-9]*$" basename)
                      (match-string 1 basename))
		 (mapcar 'car template-table)))
              (first (first (last template-table)))))

         (template (cdr (assoc filetype template-table))))

    (insert (pop template))
    (if template (funcall (pop template)))
))

(defun b-perl-template-bview nil
  "inserts a template for a bview"
  (interactive)
  (b-set-copyright-owner)
  (insert "# " (b-copyright) "
view_parent('base');
view_put(
    base_content => Prose('put-content-here'),
);
")
    (re-search-backward "put-content-here"))

(defun b-perl-template-pm nil
  "inserts a template for a perl module which may extend an existing
class."
  (interactive)
  (let*
      ((name (read-string
	      "module (package) name: "
	      (b-perl-module-from-file-name (buffer-file-name))))
       (extends (or
		 (read-string "extends module: "
			      (cond
			       ((string-match "::Type::" name)
				"Bivio.Type")
			       ((string-match "::Model::.*List$" name)
				"Biz.ListModel")
			       ((string-match "::Model::.*ListForm$" name)
				"Biz.ListFormModel")
			       ((string-match "::Model::.*Form$" name)
				"Biz.FormModel")
			       ((string-match "::Model::" name)
				"Biz.PropertyModel")
			       ((string-match "::Action::" name)
				"Biz.Action")
			       ((string-match "::Delegate::" name)
				"Bivio.Delegate")
			       ((string-match "::Util::" name)
				"Bivio.ShellUtil")
			       ((string-match "::Facade::" name)
				"UI.FacadeBase")
			       ((string-match "::View::" name)
				"View.Base")
			       (t (b-perl-module-from-file-name
				   (file-name-directory (buffer-file-name))))))
		 "")))
    (b-set-copyright-owner)
    (insert "# " (b-copyright) "
package " name ";
use strict;
use Bivio::Base"
            (if (and (stringp extends)
		     (not (or
			   (string= extends "")
			   (string= extends "Bivio::"))))
		(concat " '" extends "'")
	      " 'Bivio::UNIVERSAL'")
";\n"
            (if (string-match "::View::\\|Widget::" name)
		"b_use('UI.ViewLanguageAUTOLOAD');\n"
	      "b_use('IO.ClassLoaderAUTOLOAD');\n")
	    "
our($VERSION) = sprintf('%d.%02d', q$Revision" ": 0.0$ =~ /\\d+/g);
"
            (if (string= extends "Type.Enum")
		"__PACKAGE__->compile([
    UNKNOWN => 0,
]);
"
	      "")
"
1;
")))

(defun b-copyright nil
  "Returns a copyright statement for this year. See also b-set-copyright-owner"
  (format "Copyright (c) %s %s  All Rights Reserved."
	  (format-time-string "%Y" (current-time))
	  b-copyright-owner))
(defun b-set-copyright-owner nil
  "Sets the value of b-copy-owner variable"
  (setq b-copyright-owner
	(read-string "Copright owner: "
		     b-copyright-owner
		     '(b-copyright-owner-history . 1))))

(defun b-perl-which-project (filename)
  "Returns project list from b-perl-projects based on filename"
  (cl-labels
      ((regexp (regexp string)
	       (if (string-match regexp string)
		   (cl-labels
		       ((get-substrings
			 (s mdata)
			 (if (null mdata) nil
			   (cons (substring s (car mdata)
					    (cadr mdata))
				 (get-substrings s
						 (cddr mdata))))))
		     (get-substrings string (match-data)))
		 nil)))
    (assoc
     (nth 3 (regexp (concat b-perl-src-re "\\([^/]*\\)") filename))
     b-perl-projects)))

(defun b-perl-env ()
  "Returns appropriate env BCONF="
  (let
      ((project (b-perl-which-project (buffer-file-name)))
       (src-tree (and
		  (string-match b-perl-src-re buffer-file-name)
		  (match-string 2 buffer-file-name))))
    (if (null project)
	""
      (shell-command-to-string
       (concat
	"b-env -env "
	(nth 1 project)
	" "
	(nth 0 project)
	" "
	(or src-tree ""))))))

(defun b-perl-compile-command ()
  "Returns a string compile-command.  Appends 'env BCONF=...' for test files."
  (concat
   (b-perl-env)
   (cond
    ((string-match "\\.btest$" buffer-file-name) "bivio test acceptance ")
    ((string-match "\\.\\(bunit\\|t\\)$" buffer-file-name) "bivio test unit ")
    (t "perl -w "))
   (file-name-nondirectory buffer-file-name)))

(defun b-perl-bunit-next-error-buf (name)
  "Creates name from point to mark"
  (let
      ((value (buffer-substring (mark) (point)))
       (fn (concat ".b-perl-bunit-next-error-buf-" name)))
    (save-excursion
      (progn
	(let
	    ((b (get-buffer fn)))
	  (if b
	      (progn
		(set-buffer b)
		(if (buffer-modified-p)
		    (basic-save-buffer))
		(kill-buffer b))))
	(if (file-exists-p fn)
	    (delete-file fn))
	(let
	    ((buf (set-buffer (find-file-noselect fn t))))
	  (progn
	    (insert value)
	    (basic-save-buffer)
	    (highlight-changes-mode 'active)
	    buf))))))

(defun b-perl-bunit-next-error-compare (buf-a buf-b)
  "compare two buffers"
  (let
      ((file-a (buffer-file-name buf-a))
       (file-b (buffer-file-name buf-b))
       change-info
       change-a
       a-start
       a-end
       len-a
       change-b
       b-start
       b-end
       len-b)
    (progn
      (setq change-info
	    (hilit-chg-get-diff-info buf-a file-a buf-b file-b))
      (set-buffer buf-a)
      (setq change-a (car change-info))
      (setq change-b (car (cdr change-info)))
      (hilit-chg-make-list)
      (while change-a
	(setq a-start (nth 0 (car change-a)))
	(setq a-end (nth 1 (car change-a)))
	(setq b-start (nth 0 (car change-b)))
	(setq b-end (nth 1 (car change-b)))
	(setq len-a (- a-end a-start))
	(setq len-b (- b-end b-start))
	(set-buffer buf-a)
	(hilit-chg-set-face-on-change a-start a-end len-b nil)
	(with-current-buffer buf-b
	  (hilit-chg-set-face-on-change b-start b-end len-a nil))
	(setq change-a (cdr change-a))
	(setq change-b (cdr change-b)))
      (set-buffer buf-b)
      (basic-save-buffer)
      (set-buffer buf-a)
      (basic-save-buffer)
      (delete-file file-a)
      (delete-file file-b))))

(defun b-perl-bunit-next-error ()
  "Moves to the next bunit error in the *compilation* buffer"
  (interactive)
  (let
      ((prev-buffer (current-buffer)))
    (progn
      (set-buffer (get-buffer "*compilation*"))
      (if (mark t)
	  (goto-char (mark t)))
      (re-search-forward "not ok [0-9]")
      (re-search-forward "):")
      (forward-char 1)
      (set-mark (point))
      (re-search-forward " != ")
      (let
	  ((expect (b-perl-bunit-next-error-buf "expect")))
	(progn
	  (set-mark (point))
	  (re-search-forward "FAILED\\|ok [0-9]")
	  (beginning-of-line)
	  (b-perl-bunit-next-error-compare
	   expect (b-perl-bunit-next-error-buf "actual")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refactorings
;;;
(defun b-perl-refactor ()
  "Prompts for refactoring function and calls"
  (interactive)
  (funcall
   (intern-soft (concat "b-perl-refactor-"
			(completing-read "Refactoring: "
					 ; need to add symbols automatically
					 '(("view-to-shortcut" 1)
					   ("rename-method" 1)
					   ("class-loader-autoload" 1)
					   ("use" 1))
					 nil
					 t
					 nil
					 'b-perl-refactoring
					 nil)))))

(defun b-perl-refactor-class-loader-autoload ()
  "Renames the method and updates documentation and references to
old name within documentation and body method, but not rest of module."
  (save-excursion
    (b-perl-do-insert-import "IO.ClassLoaderAUTOLOAD")))

(defun b-perl-refactor-rename-method ()
  "Renames the method and updates documentation and references to
old name within documentation and body method, but not rest of module."
  (interactive)
  (let
      ((old (read-string "OLD method (sub) name: "))
       (new (read-string "NEW method (sub) name: "))
       (case-fold-search nil)
       (end nil)
       (start nil)
       (is-private nil)
       (body nil))
    (goto-char (point-min))
    (re-search-forward (concat "^sub " old " {"))
    (beginning-of-line)
    (setq start (point))
    (re-search-forward "^}\n\n")
    (while (re-search-backward (concat "\\b" old "\\b") start t)
      (replace-match new t t)
      (forward-word -1))
    (re-search-backward "^sub ")
    (kill-line)
    (setq start (point))
    (re-search-forward "^}")
    (backward-char 2)
    (re-search-backward "\\S_")
    (setq body (buffer-substring start (+ (point) 1)))
    (forward-line)
    (re-search-forward "\n+")
    (delete-char (- start (point)))
    (b-perl-insert-method new body)
    (save-excursion
      (goto-char (point-min))
      (query-replace old new t))))

(defun b-perl-refactor-use ()
  "Replace __PACKAGE__->use, Bivio::Type->get_instance and Bivio::Biz::Model->get_instance"
  (interactive)
  (save-excursion
    (let
       ((case-fold-search nil))
      (mapcar (lambda (op)
		(let
		    ((pat (nth 0 op))
		     (rep (nth 1 op)))
		  (goto-char (point-min))
		  (while (search-forward pat nil t)
		    (replace-match rep nil nil))))
	      '(("Bivio::Type->get_instance('" "b_use('Type.")
		("Bivio::Biz::Model->get_instance('" "b_use('Model.")
		("__PACKAGE__->use('Bivio::UI::HTML::" "b_use('UIHTML.")
		("__PACKAGE__->use(" "b_use(")
		("$self->use(" "b_use("))))))


;;
;; Keys
;;

(progn
    (define-key cperl-mode-map "\C-c;" 'comment-or-uncomment-region)
    (define-key cperl-mode-map "\C-cb" 'b-perl-call-super)
    (define-key cperl-mode-map "\C-ce" 'b-perl-bunit-next-error)
    (define-key cperl-mode-map "\C-cf" 'b-perl-find-module)
    (define-key cperl-mode-map "\C-ch" 'b-perl-httpd-restart)
    (define-key cperl-mode-map "\C-cl" 'b-perl-btest-last-log)
    (define-key cperl-mode-map "\C-cp" 'b-perl-man-builtin)
    (define-key cperl-mode-map "\C-cr" 'b-perl-refactor-rename-method)
    (define-key cperl-mode-map "\C-cs" 'b-perl-insert-method)
    (define-key cperl-mode-map "\C-ct" 'b-perl-template)
    (define-key cperl-mode-map "\C-cu" 'b-perl-insert-use)
    (define-key cperl-mode-map "\C-cv" 'b-perl-insert-variable)
    (define-key cperl-mode-map "\C-c\C-m" 'compile)
    (define-key cperl-mode-map "\C-c\C-e" 'next-error)
    (define-key cperl-mode-map "\C-c\C-r" 'b-perl-refactor))
