;;; pyconf.el --- Set up python execution configurations like dap-mode ones
;;
;; Copyright (C) 2022 Andrew Favia
;; Author: Andrew Favia <drewlinguistics01 at gmail dot com>
;; Version: 0.1
;; Package-Requires ((pyvenv "20211014.707") (emacs "27.1"))
;; Keywords: processes, python
;; URL: https://github.com/andcarnivorous/pyconf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This small package provides a class and functions to set up python execution configurations.
;; It is possible to configure the python execution command, virtualenv to load and use for the run,
;; environment variables, parameters to pass and the directory where to execute the python file.
;;
;; Usage:
;;
;; You can use the `pyconf-add-config' function passing a `pyconf-config' object
;; You can then call `pyconf-start' to execute one of your configurations
;; Example:
;; (pyconf-add-config (pyconf-config :name "test1"
;; 				  :pyconf-exec-command "python3"
;; 				  :pyconf-file-to-exec "~/test/test.py"
;; 				  :pyconf-path-to-exec "~/test/"
;; 				  :pyconf-params "--verbose"
;; 				  :pyconf-venv "~/test/.venv/"
;; 				  :pyconf-env-vars '("TEST5=5")))
;;
;; Add multiple configurations:
;; (pyconf-add-configurations `(
;; 			     ,(pyconf-config :name "test3"
;; 							       :pyconf-exec-command "python3 -i"
;; 							       :pyconf-file-to-exec "~/test/test.py"
;; 							       :pyconf-path-to-exec "~/test/"
;; 							       :pyconf-params ""
;; 							       :pyconf-venv "~/test/.venv/"
;; 							       :pyconf-env-vars '("TEST5=5"))
;; 			     ,(pyconf-config :name "test2"
;; 							       :pyconf-exec-command "python3 -i"
;; 							       :pyconf-file-to-exec "~/test/test.py"
;; 							       :pyconf-path-to-exec "~/test/"
;; 							       :pyconf-params ""
;; 							       :pyconf-venv "~/test/.venv/"
;; 							       :pyconf-env-vars '("TEST5=5"))
;; 			     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;
;;; Code:

(require 'eieio)
(require 'subr-x)
(require 'pyvenv)

(defvar pyconf-config-list '())

(defun pyconf-run-python-proc (command-s path-to-file exec-dir &optional params venv env-vars)
  "Execute COMMAND-S pointing to PATH-TO-FILE, setting the `default-directory' to EXEC-DIR if provided and pass the PARAMS given.  Load with `pyvenv' the VENV virtualenv if provided and set the ENV-VARS if provided."
  (let ((venv (or venv ""))
	(params (or params ""))
	(env-vars (or env-vars ""))
	(cached-venv nil)
	(built-env-vars ""))
    (if venv
	(progn
	  (if pyvenv-virtual-env
	      (setq cached-venv pyvenv-virtual-env))
	  (pyvenv-deactivate)
	  (pyvenv-activate venv)))
    (if (> (length env-vars) 0)
	(setq built-env-vars (string-join env-vars " ")))
    (let ((default-directory exec-dir))
      (async-shell-command (string-join
			    (list built-env-vars command-s path-to-file params) " ") (format "*PyConf %s*" path-to-file)))
    (if cached-venv
	(progn
	  (pyvenv-deactivate)
	  (pyvenv-activate cached-venv))
      (pyvenv-deactivate))))

(defclass pyconf-config ()
  ((name :initarg :name
	 :type string
	 :custom string)
   (pyconf-exec-command :initarg :pyconf-exec-command
			:type string
			:custom string)
   (pyconf-file-to-exec :initarg :pyconf-file-to-exec
			:type string
			:custom string)
   (pyconf-path-to-exec :initarg :pyconf-path-to-exec
			:type string
			:custom string
			:initform ".")
   (pyconf-params :initarg :pyconf-params
		  :type string
		  :custom string)
   (pyconf-venv :initarg :pyconf-venv
		:type string
		:custom string
		:initform "")
   (pyconf-env-vars :initarg :pyconf-env-vars
		    :type list
		    :custom list
		    :initform '())))

(defun pyconf-execute-config (pyconf-config-obj)
  "Pass to the PYCONF-RUN-PYTHON-PROC all the values from PYCONF-CONFIG-OBJ."
  (pyconf-run-python-proc (slot-value pyconf-config-obj 'pyconf-exec-command)
			  (slot-value pyconf-config-obj 'pyconf-file-to-exec)
			  (slot-value pyconf-config-obj 'pyconf-path-to-exec)
			  (slot-value pyconf-config-obj 'pyconf-params)
			  (slot-value pyconf-config-obj 'pyconf-venv)
			  (slot-value pyconf-config-obj 'pyconf-env-vars)))

(defun pyconf-start (choice)
  "Prompt to choose one CHOICE configuration and then execute it."
  (interactive
   (let ((completion-ignore-case  t))
     (list (completing-read "Choose Config: " pyconf-config-list nil t))))
  (pyconf-execute-config (cdr (assoc choice pyconf-config-list)))
  choice)

(defun pyconf-add-config (config-listp)
  "Add CONFIG-LISTP configuration to PYCONF-CONFIG-LIST."
  (let ((config-name (slot-value config-listp 'name)))
    (add-to-list 'pyconf-config-list `(,(slot-value config-listp 'name) . ,config-listp))))

(defun pyconf-add-configurations (configurations-list)
  "Add a list of configurations from CONFIGURATIONS-LIST to the PYCONF-CONFIG-LIST."
  (dolist (configuration-item configurations-list)
    (pyconf-add-config configuration-item)))

(provide 'pyconf)

;;; pyconf.el ends here
