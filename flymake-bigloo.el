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
;;;; Copyright(c) 2011 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of flymake-bigloo.
;;;;
;;;;     flymake-bigloo is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     flymake-bigloo is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU General Public
;;;;     License along with Pthread-extra.  If not, see
;;;;     <http://www.gnu.org/licenses/>.

(provide 'flymake-bigloo) 
(require 'flymake) 

(defcustom flymake-bigloo-buildfile-list '("GNUmakefile" "makefile"  "Makefile")
  "*The list of buildfile(makefile) names to search for"
  :group 'flymake-bigloo
  :type '(repeat (string)))

(defcustom flymake-bigloo-buildfile-target "check-syntax"
  "*The name of the flymake check syntax target"
  :group 'flymake-bigloo
  :type 'string)

;;;; utility procedures
(defun strip-final-slash (path)
  "strip the final slash if one exists"
  (if (char-equal (aref path (- (length path) 1))
		   ?/)
      (substring path 0 (- (length path) 1))
    path))

(defun parent-dir (path)
  (file-name-directory (strip-final-slash path)))


(defun flymake-bigloo-find-buildfile-dir (start-dir)
  "Search from start-dir looking for sutiable makefile containing a flymake
   check-syntax target. If one is found return its location, otherwise return
   nil" 
  (catch 'found 
    (dolist (name *flymake-bigloo-buildfile-list*)
      (let ((result (locate-dominating-file start-dir name)))
	(cond ((and result 
		    (file-contains-flymake-target-p 
		     (concat (file-name-as-directory result) name)))
	       (throw 'found result))
	      (result
	       (let ((result2 (flymake-bigloo-find-buildfile-dir 
			       (parent-dir result))))
		 (when result2 (throw 'found result2))))
	      (t
	       nil))))))

(defun file-contains-flymake-target-p (filename)
  "does the file contain a check-syntax target?"
  (save-excursion 
    (let* ((buffer (find-file filename))
	   (result (search-forward *flymake-bigloo-buildfile-target* nil t)))
      (kill-buffer buffer)
      result)))



(defun flymake-bigloo-init ()
  ;;; first use ude-root-directory if available
  ;;; if not, search for a Makefile
  ;;; else attempt stand-alone
  (let* ((source-file-name buffer-file-name)
	 (source-dir (file-name-directory buffer-file-name)) 
	 (buildfile-dir (flymake-bigloo-find-buildfile-dir source-dir)))
    (setq flymake-base-dir buildfile-dir)
    (let* ((temp-source-file-name  
	    (flymake-init-create-temp-buffer-copy 
	     'flymake-create-temp-inplace)))
      (if buildfile-dir
	  (flymake-get-syntax-check-program-args 
	   temp-source-file-name buildfile-dir
	   t t
	   'flymake-get-make-cmdline)
	(let ((local-file (file-relative-name 
			   temp-source-file-name 
			   (file-name-directory buffer-file-name)))) 
	  (list "bigloo" (list "-init" local-file)))))))
      
   
(defun flymake-bigloo-cleanup ()
    
  (let ((ast-file (concat (file-name-sans-extension 
			   flymake-temp-source-file-name) 
			  ".ast"))) 
    (flymake-safe-delete-file ast-file) 
    (flymake-safe-delete-file flymake-temp-source-file-name) 
    (setq flymake-last-change-time nil)))


(setq flymake-allowed-file-name-masks 
          (cons '(".+\\.scm$" 
                          flymake-bigloo-init 
                          flymake-bigloo-cleanup 
                          flymake-get-real-file-name) 
                        flymake-allowed-file-name-masks)) 


(add-hook 'bee-mode-hook (lambda () 
			   (setq flymake-split-error/warning-message-pattern 
				 "[\r\n][\r\n]")))
 
(add-hook 'find-file-hook 'flymake-find-file-hook) 


(setq flymake-err-line-patterns 
      (cons '("File \"\\([^\"]+\\)\", line \\([0-9]+\\), character \\([0-9]+\\):[\012]\\(\\(.+[\012]\\)+\\)" 1 2 3 4)
	    (cons '("\\(\*\*\* ERROR:\\(.+[\012]\\)+\\)" nil nil nil 1)
		       flymake-err-line-patterns)))