;;; pyconf.el --- Set up python execution configurations like dap-mode ones
;;
;; Copyright (C) 2022 Andrew Favia
;; Author: Andrew Favia <drewlinguistics01 at gmail dot com>
;; Version: 0.2.0
;; Package-Requires: ((pyvenv "1.21") (emacs "28.1") (transient "0.3.7") (pyenv-mode "0.1.0"))
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
;;                                   :pyconf-exec-command "python3"
;;                                   :pyconf-file-to-exec "~/test/test.py"
;;                                   :pyconf-path-to-exec "~/test/"
;;                                   :pyconf-params "--verbose"
;;                                   :pyconf-venv "~/test/.venv/"
;;                                   :pyconf-env-vars '("TEST5=5")))
;;
;; Add multiple configurations:
;; (pyconf-add-configurations `(,(pyconf-config :name "test3"
;;                                              :pyconf-exec-command "python3 -i"
;;                                              :pyconf-file-to-exec "~/test/test.py"
;;                                              :pyconf-path-to-exec "~/test/"
;;                                              :pyconf-params ""
;;                                              :pyconf-venv "~/test/.venv/"
;;                                              :pyconf-env-vars '("TEST5=5"))
;;                              ,(pyconf-config :name "test2"
;;                                              :pyconf-exec-command "python3 -i"
;;                                              :pyconf-file-to-exec "~/test/test.py"
;;                                              :pyconf-path-to-exec "~/test/"
;;                                              :pyconf-params ""
;;                                              :pyconf-venv "~/test/.venv/"
;;                                              :pyconf-env-vars '("TEST5=5"))))
;; Bootstrap projects:
;;
;; You can use `pyconf-bootstrap-pyproject' to choose a directory where to create a new pyenv
;; virtualenv (it will have the name of the directory) and optionally install dependencies
;; using poetry.  The function will also create a .dir-locals file (refer to Emacs' docs)
;; that will set the pyenv version to the one of the project/directory you chose everytime
;; you open a file in the directory or any of its subdirectories.
;;; Code:

(require 'eieio)
(require 'subr-x)
(require 'pyvenv)
(require 'transient)
(require 'pyenv-mode)
(require 'python)

(defvar pyconf-config-list '())
(defconst pyconf-errors-buffer-name "*PYCONF ERRORS*")

(defun pyconf--remote-exec-dir (exec-dir)
  "Build the remote docker path using the EXEC-DIR to pass to tramp."
  (if (string-prefix-p "/docker:" exec-dir) t nil))

(defun pyconf--send-shell-to-remote (path)
  "Send the current buffer to the remote given PATH."
  (save-excursion
    (pyconf--on-remote path))
  (with-current-buffer "*Python*"
    (while (not python-shell--first-prompt-received)
      (accept-process-output (get-buffer-process (current-buffer)))))
  (python-shell-send-buffer))

(defun pyconf-run-python-proc (command-s path-to-file exec-dir &optional params venv pyenv-version env-vars)
  "Execute COMMAND-S pointing to PATH-TO-FILE.
Set the `default-directory' to EXEC-DIR if provided and pass the
PARAMS given.  Load with `pyvenv' the VENV virtualenv if provided,
othersiwe if PYENV-VERSION is provided use `pyenv'.
finally, set the ENV-VARS if provided."
  (let ((venv (pyconf--is-empty-string venv))
        (params (pyconf--is-empty-string params))
        (env-vars (or env-vars nil))
        (pyenv-version (pyconf--is-empty-string pyenv-version))
        (cached-venv nil)
        (built-env-vars nil))
    (when (or venv pyenv-version)
      (if (or pyvenv-virtual-env (pyenv-mode-version))
          (setq cached-venv (or pyvenv-virtual-env (pyenv-mode-version))))
      (pyconf-venv-or-pyenv venv pyenv-version))
    (if (> (length env-vars) 0)
        (setq built-env-vars (string-join env-vars " ")))
    (if (pyconf--remote-exec-dir exec-dir)
          (pyconf--send-shell-to-remote exec-dir)
      (let ((default-directory exec-dir))
        (pyconf--run-shell-command built-env-vars command-s path-to-file params))
      (if cached-venv
          (pyconf-venv-or-pyenv venv pyenv-version)
        (pyvenv-deactivate)))))

(defun pyconf--run-shell-command (built-env-vars command-s path-to-file params)
  "Run given COMMAND-S preceded by BUILT-ENV-VARS and followed by PATH-TO-FILE and PARAMS."
      (async-shell-command
       (string-join (list built-env-vars command-s path-to-file params) " ") (format "*PyConf %s*" path-to-file)))

(defun pyconf-venv-or-pyenv (venv pyenv-version)
  "Check if only one of VENV or PYENV-VERSION is nil and activate."
  (cond ((and venv pyenv-version)
         (error "Pyconf Configuration cannot have both a pyenv and a venv reference"))
        ((not venv)
         (pyconf-pyenv-activate pyenv-version))
        ((not pyenv-version)
         (pyconf-venv-activate venv))
        ((and (not venv) (not pyenv-version))
         (message "no venv or pyenv passed"))))

(defun pyconf-pyenv-activate (pyenv-version)
  "Unset any active pyenv version and then set it to PYENV-VERSION."
  (pyenv-mode-unset)
  (pyenv-mode-set pyenv-version))

(defun pyconf-venv-activate (venv)
  "Activate given VENV after deactivating any eventual active one."
  (pyvenv-deactivate)
  (pyvenv-activate venv))

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
   (pyconf-pyenv :initarg :pyconf-pyenv
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
                          (slot-value pyconf-config-obj 'pyconf-pyenv)
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

(defun pyconf--eval-env-vars (vars-list)
 "Evaluate elisp code stored in a string VARS-LIST."
  (if (= (length vars-list) 0)
      '()
    (eval (car (read-from-string vars-list)))))

(transient-define-suffix pyconf-transient-save (&optional args)
  "Save a pyconf configuration given the necessary parameters."
  :key "s"
  :description "save"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let ((config-name (or (transient-arg-value "--name=" args) (buffer-name)))
        (config-command (or (transient-arg-value "--command=" args) "python"))
        (config-file-path (or (transient-arg-value "--file-path=" args) (buffer-file-name)))
        (config-exec-path (or (transient-arg-value "--path=" args) (file-name-directory buffer-file-name)))
        (config-params (or (transient-arg-value "--params=" args) ""))
        (config-venv (or (transient-arg-value "--venv=" args) ""))
        (config-pyenv (or (transient-arg-value "--pyenv=" args) ""))
        (config-env-vars (or (pyconf--eval-env-vars (transient-arg-value "--env-vars=" args)) '())))
    (pyconf-add-configurations (list (pyconf-config :name config-name
                                                 :pyconf-exec-command config-command
                                                 :pyconf-file-to-exec config-file-path
                                                 :pyconf-path-to-exec config-exec-path
                                                 :pyconf-params config-params
                                                 :pyconf-venv config-venv
                                                 :pyconf-pyenv config-pyenv
                                                 :pyconf-env-vars config-env-vars)))))

(transient-define-suffix pyconf-transient-execute (&optional args)
  "Execute a non-persistent pyconf configuration given the necessary parameters."
  :key "x"
  :description "execute"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let ((config-name (or (transient-arg-value "--name=" args) (buffer-name)))
        (config-command (or (transient-arg-value "--command=" args) "python"))
        (config-file-path (or (transient-arg-value "--file-path=" args) (buffer-file-name)))
        (config-exec-path (or (transient-arg-value "--path=" args) (file-name-directory buffer-file-name)))
        (config-params (or (transient-arg-value "--params=" args) ""))
        (config-venv (or (transient-arg-value "--venv=" args) ""))
        (config-pyenv (or (transient-arg-value "--pyenv=" args) ""))
        (config-env-vars (or (pyconf--eval-env-vars (transient-arg-value "--env-vars=" args)) '())))
    (pyconf-execute-config (pyconf-config :name config-name
                                           :pyconf-exec-command config-command
                                           :pyconf-file-to-exec config-file-path
                                           :pyconf-path-to-exec config-exec-path
                                           :pyconf-params config-params
                                           :pyconf-venv config-venv
                                           :pyconf-pyenv config-pyenv
                                           :pyconf-env-vars config-env-vars))))

(defun pyconf-prefix-init (obj)
  "Load dynamically default values and set OBJ value slot.
Refer to
https://stackoverflow.com/questions/28196228/emacs-how-to-get-directory-of-current-buffer"
  (oset obj value `(,(format "--name=%s" (buffer-name))
                    "--command=python"
                    ,(format "--file-path=%s" (buffer-file-name))
                    ,(format "--path=%s" (file-name-directory buffer-file-name)))))

(defun pyconf--read-pyenv ()
  "Wrapper around pyvenv venv list."
  (pyvenv-virtualenv-list))

(transient-define-argument pyconf-pyenv--pyenv ()
  "Argument to select pyenv version to use for execution."
  :class 'transient-option
  :shortarg "-P"
  :description "Pyenv version"
  :always-read t
  :argument "--pyenv="
  :choices (lambda (_prompt _initial _history) (read-directory-name _prompt)))

(transient-define-argument pyconf-venv--venv ()
  "Virtualenv argument."
  :class 'transient-option
  :shortarg "-v"
  :description "virtualenv"
  :always-read t
  :argument "--venv=")

(transient-define-argument pyconf-params--params ()
  "Parameters argument."
  :class 'transient-option
  :shortarg "--params"
  :description "parameters"
  :always-read t
  :argument "--params=")

(transient-define-argument pyconf-envvars--env-vars ()
  "Environment Variables Argument."
  :class 'transient-option
  :shortarg "-e"
  :description "envvars"
  :always-read t
  :argument "--env-vars=")

(transient-define-argument pyconf-name--name ()
  "Name execution argument."
  :class 'transient-option
  :shortarg "-n"
  :description "name"
  :always-read t
  :argument "--name=")

(transient-define-argument pyconf-command--command ()
  "Command executable argument."
  :class 'transient-option
  :shortarg "-c"
  :description "command"
  :always-read t
  :argument "--command=")

(transient-define-argument pyconf-filepath--file-path ()
  "File Path Argument."
  :class 'transient-option
  :shortarg "-f"
  :description "file path"
  :always-read t
  :argument "--file-path="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))

(transient-define-argument pyconf-path--path ()
  "Execution Path Argument."
  :class 'transient-option
  :shortarg "-p"
  :description "execution path (local, docker)"
  :always-read t
  :argument "--path="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))

(transient-define-prefix pyconf-menu ()
  "PyConf transient interface."
  :init-value 'pyconf-prefix-init
  ["Arguments"
   (pyconf-name--name)
   (pyconf-command--command)
   (pyconf-filepath--file-path)
   (pyconf-path--path)
   (pyconf-venv--venv)
   (pyconf-params--params)
   (pyconf-envvars--env-vars)
   (pyconf-pyenv--pyenv)
   ]
  ["Actions"
   [(pyconf-transient-save)
    (pyconf-transient-execute)]])

(defun pyconf--is-empty-string (input-string)
  "Check return INPUT-STRING if it is not empty."
  (or (if (not (string= "" input-string))
          input-string
        nil)))

(defun pyconf--split-vars-string (vars-string)
  "Split a VARS-STRING containing environment variables comma separated into a list."
  (split-string vars-string ","))

(defun pyconf--use-req-file (req-file)
  "Prompt user on whether to use the requirements REQ-FILE."
  (interactive
   (list (yes-or-no-p "Use requirements file?")))
    req-file)

(defun pyconf-bootstrap-prefix-init (obj)
  "Load dynamically default values and set OBJ value slot.
Refer to
https://stackoverflow.com/questions/28196228/emacs-how-to-get-directory-of-current-buffer"
  (oset obj value `(,(format "--dir=%s" (file-name-directory buffer-file-name))
                    ,(format "--requirements=%s%s" (file-name-directory buffer-file-name) "requirements.txt"))))

(transient-define-prefix pyconf-bootstrap-menu ()
  "PyConf transient interface."
  :init-value 'pyconf-bootstrap-prefix-init
  ["Arguments"
   ("p" "use pyenv" "--use-pyenv")
   ("d" "project directory" "--dir=")
   (pyconf-pyenv--pyenv)
   ("r" "use requirement(venv)" "--requirements=")
   ("y" "use poetry" "--use-poetry")
   ]
  ["Actions"
   [("b" pyconf-transient-bootstrap)]])

(transient-define-suffix pyconf-transient-bootstrap (&optional args)
  "Execute a non-persistent pyconf configuration given the necessary parameters."
  :key "b"
  :description "bootstrap python project"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let ((use-pyenv (or (transient-arg-value "--use-pyenv" args) nil))
        (package-dir (or (transient-arg-value "--dir=" args) nil))
        (pyenv-version (or (transient-arg-value "--pyenv-config" args) nil))
        (req-file (or (transient-arg-value "--requirements" args) nil))
        (use-poetry (or (transient-arg-value "--use-poetry" args) nil)))
    (if (and use-poetry req-file)
        (message "You cannot use both a requirement file and poetry!")
      (pyconf--start-bootstrap use-pyenv pyenv-version use-poetry package-dir req-file))))

(defun pyconf-bootstrap-pyproject (target-dir pyenv-version use-pyenv use-poetry)
  "Create a new venv with given PYENV-VERSION in TARGET-DIR.
If USE-POETRY, it will install all dependencies in the TARGET-DIR.
It will also create a dir-locals that switches pyenv in python mode."
  (interactive (let ((completion-ignore-case t))
		 (list (read-directory-name "DDir: ")
		       (completing-read "Choose Config: " (pyvenv-virtualenv-list) nil t)
                       (yes-or-no-p "Use Pyenv (No uses venv)?")
		       (yes-or-no-p "Use Poetry (No uses pip)?"))))
  (let* ((package-dir (file-truename target-dir))
	 (use-short-answers t)
         (req-file (if use-poetry
                       nil
                     (call-interactively 'pyconf--use-req-file))))
    (pyconf--start-bootstrap use-pyenv pyenv-version use-poetry package-dir req-file)))

(defun pyconf--start-bootstrap (use-pyenv pyenv-version use-poetry package-dir req-file )
  "Prepare the environment by creating the dir-locals and installing dependencies.
How dependencies are installed and how the venv in
managed depends on the values of the parameters passed.
USE-PYENV is a bool, PYENV-VERSION is the pyenv version
you want to create the new venv with.
USE-POETRY is a bool, if true the other methods like pyenv
and requirements file will be ignored.
PACKAGE-DIR is the current project/python package directory.
REQ-FILE is whether to use a requirements file to install dependencies."
  (let* ((venv-name (file-name-base (directory-file-name (file-truename package-dir))))
         (out-contents (format "((nil . ((eval . (pyconf--switch-pyvenv \"%s\")))))" venv-name))
         (out-file (expand-file-name ".dir-locals.el" package-dir)))
    (if use-pyenv
        (pyconf-create-pyenv venv-name pyenv-version)
      (pyconf-create-venv package-dir))
    (when use-poetry (pyconf-install-with-poetry package-dir))
    (if req-file
        (pyconf-install-requirements package-dir))
    (when (and (not req-file) (not use-poetry))
      (pyconf-install-with-pip package-dir))
    (with-temp-file out-file (insert out-contents))))

(defun pyconf-install-requirements (package-dir)
  "Install the dependencies using the requirements file in PACKAGE-DIR."
  (let (default-directory package-dir))
  (shell-command "pip install -r requirements.txt" "*pyconf pip install requirements*" "*PYCONF ERRORS"))

(defun pyconf-create-venv (target-dir)
  "Create the virtual environment using python's venv in TARGET-DIR."
  (let ((default-directory target-dir))
    (shell-command "python -m venv .venv" "*pyconf create venv*" pyconf-errors-buffer-name)
    (pyvenv-activate   (format "%s/.venv" target-dir))))
  
(defun pyconf-create-pyenv (name version)
  "Create a new pyenv virtualenv with given NAME using given pyenv VERSION."
  (if (not (member name (pyvenv-virtualenv-list)))
      (pyconf--exec-pyenv-create version name))
  (pyvenv-deactivate) ;; it seems workon alone does not always work
  (pyvenv-workon name))

(defun pyconf--exec-pyenv-create (version name)
  "Create new pyenv virtualenv with NAME using python VERSION."
  (call-process-shell-command
       (string-join (list "pyenv virtualenv" version name) " ") nil 0))

(defvar pyconf-bootstrap-packages nil
  "List of packages you want to install when boostrapping a project.  Nil by default.")

(defun pyconf-install-with-poetry (target-dir)
  "Install python dependencies in TARGET-DIR using poetry."
  (let ((default-directory target-dir))
    (shell-command "pip install poetry" "*pyconf poetry install*")
    (shell-command "poetry install" "*pyconf poetry install*" pyconf-errors-buffer-name)
    (if pyconf-bootstrap-packages
        (shell-command (format "poetry add %s" (string-join pyconf-bootstrap-packages " ")) "*pyconf poetry add extra*" pyconf-errors-buffer-name))))

(defun pyconf-install-with-pip (target-dir)
  "Install python dependencies in TARGET-DIR using poetry."
  (let ((default-directory target-dir))
    (shell-command "pip install -e ." "*pyconf pip install project*" pyconf-errors-buffer-name)
    (if pyconf-bootstrap-packages
        (shell-command (format "pip install %s" (string-join pyconf-bootstrap-packages " ")) "*pyconf pip install extra*" pyconf-errors-buffer-name))))

(defun pyconf--switch-pyvenv (environment)
  "Switch a virtualenvironment to ENVIRONMENT."
  (pyvenv-deactivate) ;; it seems workon alone does not always work
  (pyvenv-workon environment))

(defun pyconf--build-remote-path (remote-type container-id path)
  "Build the remote path given the REMOTE-TYPE to the CONTAINER-ID PATH."
  (cond ((string= remote-type "docker")
         (format "/%s:%s:%s" remote-type container-id path))))

(defun pyconf--on-remote (path)
  "Start a Python interpreter on the remote PATH."
  (let ((default-directory path))
    (without-yes-or-no (run-python))))

(defun pyconf-on-remote (container-id)
  "Provided the CONTAINER-ID, send the current buffer code to it."
  (interactive (list (completing-read "Provide container id please: " nil t)))
  (message (pyconf--build-remote-path "docker" container-id "/"))
  (save-excursion
    (pyconf--on-remote (pyconf--build-remote-path "docker" container-id "/")))
  (with-current-buffer "*Python*"
    (while (not python-shell--first-prompt-received)
      (accept-process-output (get-buffer-process (current-buffer)))))
  (python-shell-send-buffer))

; taken from https://stackoverflow.com/a/59509250
(defmacro without-yes-or-no (&rest body)
  "Override `yes-or-no-p' & `y-or-n-p', not to prompt for input and return t by using the BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    ,@body))

(provide 'pyconf)
;; TODO implement path autocomplete for path options in transient.
;; TODO for this one: pyconf-bootstrap-menu
;;; pyconf.el ends here
