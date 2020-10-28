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
(require 'cl-lib)

;;; Code:


;; Vars
(defvar runc-default-runner nil "An instace of `runc-i-runner` used to run the commands.")


;; Base data
(cl-defstruct runc-runnable
  "Defines a runnable command."
  (program nil :type 'string :documentation "A string containing the program to run.")
  (default-args nil :type 'list :documentation "The default args to be passed to program.")
  (directory nil :type 'string :documentation "The default directory to use for running `program`.")
  (name nil :type 'string :documentation "A name to identify this runnable."))


;; Helpers
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


;; Runners
(defclass runc-i-runner ()
  ()
  "Interface class for a runner (that runs a runnable)"
  :abstract t)

(cl-defmethod runc--runner-run ((runner runc-i-runner) runnable)
  "Runs a runnable using a runner."
  (error "Missing implementation"))


(defclass runc-simple-runner (runc-i-runner)
  ()
  "Simple runner using *process* and a new filter.")

(cl-defmethod runc--runner-run ((runner runc-simple-runner) runnable)
  (let* ((default-directory (runc--default-dir runnable))
         (buffname (runc--buffer-name runnable))
         (program (runc-runnable-program runnable))
         (args (runc-runnable-default-args runnable))
         (_ (when (get-buffer buffname)
              (kill-buffer buffname)))
         (process (apply #'start-process buffname buffname program args)))
    (set-process-filter process #'compilation-filter)
    (with-current-buffer buffname
      (compilation-mode))
    (display-buffer buffname)))


(defclass runc-compilation-runner (runc-i-runner)
  ()
  "Runner based on Compilation mode, calling `compile` interactively.")

(cl-defmethod runc--runner-run ((runner runc-compilation-runner) runnable)
  (let* ((default-directory (runc--default-dir runnable))
         (compilation-buffer-name-function (-const (runc--buffer-name runnable)))
         (command (runc-runnable-program runnable))
         (args (--map (shell-quote-argument it) (runc-runnable-default-args runnable)))
         (compile-command (s-join " " (apply #'list command args))))
    (call-interactively #'compile)))

;; API
(defun runc-run (runnable &optional runner)
  "Runs a runnable."
  (let ((runner* (or runner runc-default-runner (runc-simple-runner))))
    (runc--runner-run runner* runnable)))

(cl-defmacro runc-def (defname &key program directory name)
  "Defines a runnable and a function to run it."
  (let ((runnable-symbol (-> defname symbol-name (s-concat "-runnable") intern)))
    `(progn
       (setq ,runnable-symbol
             (make-runc-runnable :program ,program :directory ,directory :name ,name))
       (defun ,defname ()
         ,(s-concat "Runs the runnable with name `" name "`.")
         (interactive)
         (runc-run ,runnable-symbol)))))

(put 'runc-def 'lisp-indent-function 1)

(provide 'runc)
;;; runc.el ends here
