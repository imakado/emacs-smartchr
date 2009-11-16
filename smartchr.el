;;; smartchr.el --  emacs version of smartchr.vim

;; Copyright (c) 2009 by IMAKADO.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; blog: http://d.hatena.ne.jp/IMAKADO (japanese)
;; Prefix: smartchr

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


;;; TODO:
;; Error with head version of auto-complete.el
;; reported by k1LoW

(eval-when-compile (require 'cl))

(defun smartchr (list-of-string)
  (lexical-let ((los list-of-string)
                (last-word "")
                (count 0))
    (lambda ()
      (interactive)
      (if (eq this-command real-last-command)
          (incf count)
        (setq count 0))
      (when (>= count (length los))
        (setq count 0))
      (let ((word (nth count los)))
        (when (eq this-command real-last-command)
          (delete-backward-char (length last-word)))
        (setq last-word word)
        (insert word)))))


(provide 'smartchr)
