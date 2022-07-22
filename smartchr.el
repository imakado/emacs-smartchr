;;; smartchr.el ---  emacs version of smartchr.vim  -*- lexical-binding: t -*-

;; Copyright (c) 2009 by IMAKADO.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; URL: https://github.com/imakado/emacs-smartchr
;; Prefix: smartchr
;; Package-Requires: ((emacs "24.3"))
;; LICENSE: GPL-2.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Thanks to k1LoW for original idea.

;;; Commentary:

;; (global-set-key (kbd "=") (smartchr '(" = " " == " " === ")))

;; substitute `!!' with cursor
;; (global-set-key (kbd "{")
;;              (smartchr '("{ `!!' }" "{ \"`!!'\" }" "{")))


;;; TODO:
;; Error with head version of auto-complete.el
;; reported by k1LoW


;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(defgroup smartchr nil
  "Support input several candidates with a single key."
  :group 'smartchr)

(defcustom smartchr-template-cursor-re (rx "`!!'")
  "Regular expression to be replaced with the cursor after expanded template string."
  :type  'string
  :group 'smartchr)

(defcustom smartchr-execute-indent-command-after-expand nil
  "Controls indentation of after text expansions."
  :type 'boolean
  :group 'smartchr)

(defcustom smartchr-indent-function 'smartchr-indent-function
  "Function to perform indentation."
  :type 'function
  :group 'smartchr)

(defcustom smartchr-region-text-re (rx "___")
  "Regular expression replaced with the cursor or selected region after expanded template string."
  :type  'string
  :group 'smartchr)

(defun smartchr-should-do-p  ()
  (not (minibufferp)))

(defun smartchr-indent-function (start-marker end-marker)
  (indent-region (marker-position start-marker)
                 (marker-position end-marker)))

(defun smartchr-indent-function-each-line (start-marker end-marker)
  (save-excursion
    (goto-char (marker-position start-marker))
    (move-beginning-of-line 1)
    (call-interactively 'indent-according-to-mode)
    (cl-loop while (<= (point) (marker-position end-marker))
             do (progn (call-interactively 'indent-according-to-mode)
                       (move-beginning-of-line 2)))))

(cl-defstruct (smartchr-struct
               (:constructor smartchr-make-struct
                             (&key cleanup-fn insert-fn region-start region-end region-text
                                   template)))
  (region-start nil)
  (region-end nil)
  (region-text nil)
  (marker-start nil)
  (marker-end nil)
  template
  cleanup-fn insert-fn)

(defvar smartchr-disabled nil
  "Non nil means smartchr enabled.")

(defun smartchr (&rest list-of-string)
  "Make an interactive command to support input several LIST-OF-STRING candidates."
  (let ((list-of-string (if (consp (car-safe list-of-string))
                            (car-safe list-of-string)
                          list-of-string)))
    (let* ((last-struct nil)
           (count 0)
           (region-text "")
           (start-point nil)
           (smartchr-structs (mapcar 'smartchr-parse list-of-string))
           (marker-start nil)
           (marker-end nil)
           (first-time-p nil)
           )
      (lambda (&optional arg)
        (interactive "p")
        
        ;; marker
        ;; xxx ___
        ;; region
        (cond ((region-active-p)
               (setq region-text (or (buffer-substring-no-properties (region-beginning)
                                                                     (region-end))
                                     ""))
               
               (smartchr-delete-region-quietly (region-beginning)
                                               (region-end)))
              (t ;; (setq region-text "")
               ))
        
        (if (or smartchr-disabled
                (not (smartchr-should-do-p)))
            (self-insert-command arg)
          (cond
           ((eq this-command real-last-command)
            (cl-incf count)
            (setq first-time-p nil))
           (t
            (setq first-time-p t)
            
            (setq count 0)))
          ;; init first-time-p 
          (when first-time-p
            (setq start-point (point))
            
            (setq marker-start (set-marker (make-marker) (point))
                  marker-end (set-marker (make-marker) (point)))
            (set-marker-insertion-type marker-start nil)
            (set-marker-insertion-type marker-end t)
            
            (unless (region-active-p)
              (setq region-text ""))
            )
          

          (when (>= count (length smartchr-structs))
            (setq count 0))
          ;; cleanup -> insert
          (let ((struct (nth count smartchr-structs)))
            (cl-assert (smartchr-struct-p struct))
            (when (eq this-command real-last-command)
              (cl-assert (smartchr-struct-p last-struct))
              (funcall (smartchr-struct-cleanup-fn last-struct) marker-start marker-end)
              )
            (setq last-struct struct)

            (funcall (smartchr-struct-insert-fn struct) marker-start marker-end)
            
            (smartchr-subst-cursor-text
             struct region-text start-point (marker-position marker-end))
            (smartchr-subst-region-text
             struct region-text start-point (marker-position marker-end))


            (when smartchr-execute-indent-command-after-expand
              (funcall smartchr-indent-function
                       marker-start
                       marker-end))
            ))))))

(defun smartchr-delete-region-quietly (s e)
  (cl-flet ((message (&rest args) nil))
    (with-output-to-string
      (delete-region s e))))

(defun smartchr-subst-cursor-text (struct region-text start-point end-point)
  (let ((p nil))
    (save-excursion 
      (goto-char start-point)
      (when (re-search-forward smartchr-template-cursor-re end-point t)
        (setq p (match-beginning 0))
        (replace-match "")))
    (when p
      (goto-char p))))

(defun smartchr-subst-region-text (struct region-text start-point end-point)
  (let ((p nil))
    (save-excursion 
      (goto-char start-point)
      (when (re-search-forward smartchr-region-text-re end-point t)
        (setq p (match-beginning 0))
        (replace-match region-text)))
    (when p
      (goto-char p))))

(defun smartchr-parse (template)
  "Return smartchr-struct by TEMPLATE."
  (let* ((rs nil)
         (re nil)
         (region-text "")
         (template template))
    (cond
     ((smartchr-struct-p template)
      template)
     ((functionp template)
      (let ((str-or-struct (funcall template)))
        (cond
         ((smartchr-struct-p str-or-struct)
          str-or-struct)
         ((stringp str-or-struct)
          (smartchr-parse str-or-struct))
         (t
          (smartchr-make-struct
           :template template
           :cleanup-fn (lambda ())
           :insert-fn (lambda ()))))))
     ((string-match smartchr-template-cursor-re template)
      (let ((template template))
          (smartchr-make-struct
           :template template
           :cleanup-fn (lambda (marker-start marker-end)
                         (delete-region (marker-position marker-start)
                                        (marker-position marker-end)))
           :insert-fn (lambda (marker-start marker-end)
                        (set-marker marker-start (point))
                        (set-marker marker-end (point))
                        (insert template))
           :region-start rs
           :region-end re
           :region-text region-text)))
     
     ((string-match smartchr-region-text-re template)
      (let ((template template))
          (smartchr-make-struct
           :template template
           :cleanup-fn (lambda (marker-start marker-end)
                         (delete-region (marker-position marker-start)
                                        (marker-position marker-end)))
           :insert-fn (lambda (marker-start marker-end)
                        (message "%s" 'smartchr-region-text-re)
                        (set-marker marker-start (point))
                        (set-marker marker-end (point))
                        (insert template))
           :region-start rs
           :region-end re
           :region-text region-text)))
     (t
      (let ((template template))
        (smartchr-make-struct
         :template template
         :cleanup-fn (lambda (marker-start marker-end)
                       (delete-region (marker-position marker-start)
                                      (marker-position marker-end)))
         :insert-fn (lambda (marker-start marker-end)
                      (set-marker marker-start (point))
                      (set-marker marker-end (point))
                      (insert template))
         :region-start rs
         :region-end re
         :region-text region-text))))))

(defun smartchr-toggle ()
  "Toggle smartchr enable/disable."
  (interactive)
  (setq smartchr-disabled (not smartchr-disabled)))

(defun smartchr-on ()
  "Enable smartchr."
  (interactive)
  (setq smartchr-disabled nil))

(defun smartchr-off ()
  "Disable smartchr."
  (interactive)
  (setq smartchr-disabled t))


(provide 'smartchr)
;;; smartchr.el ends here.
