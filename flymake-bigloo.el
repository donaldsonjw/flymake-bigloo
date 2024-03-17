;;;;  -*- lexical-binding: t; -*-
;;;; Copyright(c) 2011 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of flymake-bigloo.
;;;;
;;;;     flymake-bigloo is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     flymake-bigloo is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU General Public
;;;;     License along with flymake-bigloo.  If not, see
;;;;     <http://www.gnu.org/licenses/>.
;;;; Copyright(c) 2011, 2024 Joseph Donaldson(donaldsonjw@yahoo.com) 

(require 'flymake)
(require 'subr-x)

(defcustom flymake-bigloo-error-regexps
  '("File \"\\(?1:[^\"]+\\)\", line \\(?2:[0-9]+\\), character \\([0-9]+\\):[\012]\\(?4:\\(.+[\012]\\)+\\)"
    "\\(?:\*\*\* \\(?4:ERROR:\\(.+[\012]\\)\\{2\\}\\)\\)")
  "The list of bigloo error regexps"
  :type '(repeat (string))
  :group 'flymake-bigloo)

(defcustom flymake-bigloo-command 'flymake-bigloo-use-special-make-target
  "Command used by the `flymake-bigloo' backend.
A list of strings or a symbol naming a function that produces one
such list when called with no arguments in the buffer where the
variable `flymake-mode' is active.

The command should invoke the Bigloo compiler that checks the
syntax of a scheme program passed to it and prints the result
on its standard output."
  :type '(choice
          (symbol :tag "Function")
          (repeat :tag "Command(s)" string))
  :group 'flymake-bigloo)


(defcustom flymake-bigloo-buildfiles
  '("Makefile" "makefile" "GNUmakefile" "Makefile.inc")
  "The list of buildfile (makefile) names to search"
  :type '(repeat (string))
  :group 'flymake-bigloo)


(defcustom flymake-bigloo-buildfile-target 
    "check-syntax" 
  "The name of the flycheck check syntax target"
  :type 'string
  :group 'flymake-bigloo)


(defun flymake-bigloo-get-regexp ()
  "create a single regexp including all defined error regexps"
  (string-join (mapcar (lambda (r) (format "\\(?:%s\\)" r)) flymake-bigloo-error-regexps) "\\|"))
 

(defun flymake-bigloo-file-contains-check-syntax-target-p (filename)
  "does the file contain a check-syntax target?"
  (save-excursion
    (let* ((buffer (find-file filename))
(result (search-forward flymake-bigloo-buildfile-target nil t)))
      (kill-buffer buffer)
      result)))

(defun parent-dir (path)
  "obtain the parent directory of the given path, if it
exists"
   (if (string= path "/")
       nil
     (file-name-directory (directory-file-name path))))


(defun flymake-bigloo-find-buildfile-dir (start-dir)
  "Search from start-dir looking for sutiable makefile containing a flycheck
check-syntax target. If one is found return its location, otherwise return
nil"
  (catch 'found
    (let ((curr-dir start-dir))
      (while curr-dir
	(dolist (name flymake-bigloo-buildfiles)
	  (let ((result (locate-dominating-file curr-dir name)))
	    (when (and result
		       (flymake-bigloo-file-contains-check-syntax-target-p
			(concat (file-name-as-directory result) name)))
	      (throw 'found result))))
	(setq curr-dir (parent-dir curr-dir)))
	nil)))

(defun trim-common-prefix (prefix str)
  (string-match prefix str)
  (substring str (match-end 0)))


(defun flymake-bigloo-use-special-make-target ()
  "Command for checking a file via a check-syntax Make target."
  (let* ((source-dir (expand-file-name (file-name-directory buffer-file-name)))
         (buildfile-dir (expand-file-name (flymake-bigloo-find-buildfile-dir source-dir)))
         (target (trim-common-prefix buildfile-dir buffer-file-name)))
    (unless (executable-find "make")
      (error "Make not found"))
    (unless buildfile-dir
      (error "check-syntax target not found"))
    `("make"
      "-C"
      ,buildfile-dir 
      ,flymake-bigloo-buildfile-target 
    ,(format "CHK_SOURCES=%s" target))))


(defun flymake-bigloo-get-error-msgs ()
  "return a list of errors  contained in output"
  (let ((regexp (flymake-bigloo-get-regexp))
	(err-msgs nil))
    (while (search-forward-regexp regexp nil t)
      (setq err-msgs (cons (match-string 0) err-msgs)))
    err-msgs))


(defun flymake-bigloo-get-errors (source err-msgs)
  (let ((errs nil))
    (dolist (msg err-msgs errs)
      (catch 'exit 
	(dolist (regexp flymake-bigloo-error-regexps)
     	  (let ((match-start (string-match regexp msg)))
     	    (when match-start
	      (let* ((line (let ((lm (match-string 2 msg)))
                             (if lm (string-to-number lm) 0)))
		     (message (match-string 4 msg))
                     (region (flymake-diag-region source line)))
                (message "line: %s" line)
     		(setq errs
                      (cons (flymake-make-diagnostic source
                                                     (car region)
                                                     (cdr region)
                                                     :error message)
                                 errs))
		(throw 'exit nil)))))))))

(defun flymake-bigloo--make-diagnostics (source)
  "Parse bigloo compilation messages in current buffer.
Return a list of Flymake diagnostic objects for the source buffer
SOURCE."
  (flymake-bigloo-get-errors source (flymake-bigloo-get-error-msgs)))

(defvar-local flymake-bigloo--proc nil "Internal variable for `flymake-bigloo'.")

;;;###autoload
(defun flymake-bigloo (report-fn &rest _args)
  "Flymake backend for Bigloo scheme compiler. This backend uses
either the makefile targe indicated by `flymake-bigloo-buildfile-target'
or simply bigloo -syntax to check bigloo source files.
REPORT-FN is Flymake's callback"
  (when (process-live-p flymake-bigloo--proc)
    (kill-process flymake-bigloo--proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       flymake-bigloo--proc
       (make-process
        :name "bigloo-flymake"
        :buffer (generate-new-buffer "*bigloo-flymake*")
        :command (if (symbolp flymake-bigloo-command)
                     (funcall flymake-bigloo-command)
                   flymake-bigloo-command)
        :noquery t :connection-type 'pipe
        :sentinel
        (lambda (p _ev)
          (unwind-protect
              (when (eq 'exit (process-status p))
                (when (with-current-buffer source (eq p flymake-bigloo--proc))
                  (with-current-buffer (process-buffer p)
                    (goto-char (point-min))
                    (let ((diags
                           (flymake-bigloo--make-diagnostics source)))
                      (if (or diags (zerop (process-exit-status p)))
                          (funcall report-fn diags)
                        ;; non-zero exit with no diags is cause
                        ;; for alarm
                        (funcall report-fn
                                 :panic :explanation
                                 (buffer-substring
                                  (point-min) (progn (goto-char (point-min))
                                                     (line-end-position)))))))))
            (unless (process-live-p p)
              ;; (display-buffer (process-buffer p)) ; uncomment to debug
              (kill-buffer (process-buffer p)))))))
      (process-send-region flymake-bigloo--proc (point-min) (point-max))
      (process-send-eof flymake-bigloo--proc))))

(defun bigloo-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'flymake-bigloo nil t)
  (flymake-mode))

(add-hook 'bee-mode-hook 'bigloo-setup-flymake-backend)

(provide 'flymake-bigloo)
;;; flymake-bigloo.el ends here

