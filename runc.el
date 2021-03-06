;;; runc.el --- Run commands quickly                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <vitor@archiscool>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'eieio)
(require 'transient)

;; Vars
(defvar runc-default-runner nil "An instace of `runc-i-runner` used to run the commands.")
(defvar runc-process-buffer-max-line-length 1000 "Maximum line length for a process buffer.")

;; Base data
(cl-defstruct runc-runnable
  "Defines a runnable command."
  (program nil :type 'string :documentation "A string containing the program to run.")
  (default-args nil :type 'list :documentation "The default args to be passed to program.")
  (base-args nil :type 'list :documentation "List of arguments that will ALWAYS be passed to program.")
  (directory nil :type 'string :documentation "The default directory to use for running `program`.")
  (name nil :type 'string :documentation "A name to identify this runnable."))


;; Methods for runnable
(defun runc--default-dir (runnable)
  "Returns the default directory to use for a runnable."
  (or (runc-runnable-directory runnable)
      default-directory))

(defun runc--buffer-name (runnable)
  "Returns the buffer name to use for a runnable."
  (let ((name (runc-runnable-name runnable)))
    (cond
     (name (s-concat "*" name "*"))
     (:else "*Runnable*"))))

(defun runc--args-for-run (runnable args)
  (-concat
   (runc-runnable-base-args runnable)
   (or args (runc-runnable-default-args runnable))))

(defun runc--compile-command (runnable &optional args)
  "Returns a string for `compile-command` to run a runnable with `args`.
   If passed, `args` must be a list of string arguments."
  (let* ((command (runc-runnable-program runnable))
         (args* (--map (shell-quote-argument it) (runc--args-for-run runnable args)))
         (compile-command (s-join " " (apply #'list command args*))))
    compile-command))

(defun runc--initialize-process-buffer (runnable &optional args)
  "Initializes the buffer for a process."
  (let ((buffname (runc--buffer-name runnable)))
    (when (get-buffer buffname)
      (kill-buffer buffname))
    (let ((buff (get-buffer-create buffname)))
      (with-current-buffer buff
        (insert "------------------------------------------------------------")
        (insert "\nStart: ")
        (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y" (current-time)))
        (insert "\nProgram: ")
        (prin1 (runc-runnable-program runnable) buff)
        (insert "\nArgs: ")
        (prin1 (runc--args-for-run runnable args) buff)
        (insert "\nDirectory: ")
        (prin1 (runc--default-dir runnable) buff)
        (insert "\n------------------------------------------------------------")
        (insert "\n")
        (compilation-mode))
      buff)))

;; Truncate long lines filter
(defun runc--long-line-re ()
  (s-concat "^.\\{" (int-to-string runc-process-buffer-max-line-length) "\\}.*$"))

(defun runc--truncate-large-lines-buffer-end (start)
  "Truncates all large lines from `start` to buffer end."
  (save-excursion
    (goto-char start)
    (goto-char (line-beginning-position))
    (while (re-search-forward (runc--long-line-re) nil t)
      (replace-match "[[TRUNCATED LINE]]")))
  (save-excursion
    (goto-char start)
    (goto-char (line-beginning-position))
    (while (re-search-forward "^\\[\\[TRUNCATED LINE\\]\\].*$" nil t)
      (replace-match "[[TRUNCATED LINE]]"))))

(defun runc--truncate-large-lines-compilation-filter (proc string)
  "This fn is a copy of `compilation-filter`, but it forces all lines to have at most
runc-process-buffer-max-line-length."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (pos (copy-marker (point) t))
            (min (point-min-marker))
	    (max (copy-marker (point-max) t))
	    (compilation-filter-start (marker-position (process-mark proc))))
        (unwind-protect
            (progn
	      (widen)
	      (goto-char compilation-filter-start)
              (insert string)
              (runc--truncate-large-lines-buffer-end compilation-filter-start)
              (unless comint-inhibit-carriage-motion
                (comint-carriage-motion (process-mark proc) (point)))
              (set-marker (process-mark proc) (point))
              (compilation--ensure-parse (point))
              (run-hooks 'compilation-filter-hook))
	  (goto-char pos)
          (narrow-to-region min max)
	  (set-marker pos nil)
	  (set-marker min nil)
	  (set-marker max nil))))))

;; Transient extensions
(defclass runc-transient-shortarg (transient-option) ()
  "Infix equal to `transient-option`, but that returns a value as (list argument value)
instead of (concat argument value). Usefull for sending commands to runc.")

(cl-defmethod transient-infix-value ((obj runc-transient-shortarg))
  (when-let ((value (oref obj value)))
    (list (oref obj argument) value)))

;; Runners
(defclass runc-i-runner ()
  ()
  "Interface class for a runner (that runs a runnable)"
  :abstract t)

(cl-defmethod runc--runner-run ((runner runc-i-runner) runnable &optional args)
  "Runs a runnable using a runner. `args` is an optional list of arguments for the cmd."
  (error "Missing implementation"))


(defclass runc-simple-runner (runc-i-runner)
  ()
  "Simple runner that starts a process and switches to compilation-mode.")

(cl-defmethod runc--runner-run ((runner runc-simple-runner) runnable &optional args)
  (let* ((default-directory (runc--default-dir runnable))
         (buffname (runc--buffer-name runnable))
         (program (runc-runnable-program runnable))
         (args* (runc--args-for-run runnable args))
         (buff (runc--initialize-process-buffer runnable args))
         (process (apply #'start-process buffname buffname program args*)))
    (set-process-filter process #'runc--truncate-large-lines-compilation-filter)
    (display-buffer buffname)))


(defclass runc-compilation-runner (runc-i-runner)
  ()
  "Runner based on Compilation mode, calling `compile` interactively.")

(cl-defmethod runc--runner-run ((runner runc-compilation-runner) runnable &optional args)
  (let* ((default-directory (runc--default-dir runnable))
         (compilation-buffer-name-function (-const (runc--buffer-name runnable)))
         (compile-command (runc--compile-command runnable args)))
    (call-interactively #'compile)))

(defclass runc-comint-runner (runc-i-runner)
  ()
  "Runner based on Comint mode.")

(cl-defmethod runc--runner-run ((runner runc-comint-runner) runnable &optional args)
  (let* ((default-directory (runc--default-dir runnable))
         (buffname (runc--buffer-name runnable))
         (program (runc-runnable-program runnable))
         (args* (runc--args-for-run runnable args))
         (buff  (runc--initialize-process-buffer runnable)))
    (with-current-buffer buff
      (setq buffer-read-only nil))
    (apply #'make-comint-in-buffer buffname buff program nil args*)
    (display-buffer buff)))

;; API
(defun runc-run (runnable &optional args runner)
  "Runs a runnable."
  (let ((runner* (or runner runc-default-runner (runc-simple-runner))))
    (runc--runner-run runner* runnable args)))

(cl-defmacro runc-def (defname &key program directory name default-args base-args transient default-runner)
  "Defines a command.

`defname` - The base name for the functions that will call the command.
`program` - The program to execute.
`directory` - Optional default directory that will be set before executing the program.
`name` - A name identifying this command.
`default-args` - Optional default arguments for the program (if no areguments are passed at runtime)
`base-args` - Optional base arguments, which are always given to `command` before any runtime args.
`transient` - Optional. If present, the value for this key must be a list containing `transient` groups, just like the vectors given to `transient-define-prefix`.
`default-runner` - Optional. If specified, must be an instace of `runc-i-runner`. This is the default runner used to execute the command.

   This macro will define:

1. A function called `defname`, which accepts optional args and runs the command.
2. A `runnable` instance called `defname/runnable`, containing the runnable for this command.

   If `transient` is defined, this macro will further define:

3. A transient prefix called `defname/transient`, that can be used to call the function with arguments specified by the transient groups.
4. A function called `defname/run-from-transient`, which can be used as a suffix for the transient.

   As an example, the following will generate a fn to call the ls function, as well as a transiento with the principal flags that can be given.

(runc-def ls-command
  :name \"LsCommand\"
  :program \"ls\"
  :directory (expand-file-name \"~\")
  :transient ([\"Options\" (\"l\" \"Print in a list\" \"-l\")
                         (\"a\" \"Print hidden files\" \"-a\")
                         (\"h\" \"Human readable\" \"-h\")]
              [\"Actions\" (\"r\" \"Run\" ls-command/run-from-transient)]))
"
  (let ((runnable-symbol (-> defname symbol-name (s-concat "/runnable") intern))
        (transient-symbol (-> defname symbol-name (s-concat "/transient") intern))
        (run-from-transient-symbol (-> defname symbol-name (s-concat "/run-from-transient") intern))
        (has-transient? (not (equal (length transient) 0))))
    (list
     `progn
     `(setq ,runnable-symbol
            (make-runc-runnable :program ,program
                                :directory ,directory
                                :name ,name
                                :default-args ,default-args
                                :base-args ,base-args))
     `(defun ,defname (&optional args runner)
        ,(s-concat "Runs the runnable with name `" name "`.")
        (interactive)
        (runc-run ,runnable-symbol args (or runner ,default-runner)))
     
     (when has-transient?
       `(defun ,run-from-transient-symbol (args)
          (interactive (list (transient-args ',transient-symbol)))
          (,defname (-flatten args))))

     (when has-transient?
       `(transient-define-prefix ,transient-symbol ()
          ,name
          ,@transient)))))

(put 'runc-def 'lisp-indent-function 1)

(provide 'runc)
;;; runc.el ends here
