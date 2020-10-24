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


;; Base data
(cl-defstruct runc-runnable
  "Defines a runnable command."
  (command nil :type 'string :documentation "A string containing the command to run.")
  (directory nil :type 'string :documentation "The default directory to use for running `command`.")
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
     :else "*Runnable*")))


;; API
(defun runc-run (runnable)
  "Runs a runnable."
  (let ((default-directory (runc--default-dir runnable))
        (compilation-buffer-name-function (-const (runc--buffer-name runnable)))
        (compile-command (runc-runnable-command runnable)))
    (call-interactively #'compile)))

(cl-defmacro runc-def (defname &key command directory name)
  "Defines a runnable and a function to run it."
  (let ((runnable-symbol (-> defname symbol-name (s-concat "-runnable") intern)))
    `(progn
       (setq ,runnable-symbol
             (make-runc-runnable :command ,command :directory ,directory :name ,name))
       (defun ,defname ()
         ,(s-concat "Runs the command with name `" name "`.")
         (interactive)
         (runc-run ,runnable-symbol)))))

(put 'runc-def 'lisp-indent-function 1)

(provide 'runc)
;;; runc.el ends here
