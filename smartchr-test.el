;;; smartchr-test.el --- Tests for smartchr          -*- lexical-binding: t; -*-

;; Copyright (c) 2009 by IMAKADO.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; URL: https://github.com/imakado/emacs-smartchr
;; blog: http://d.hatena.ne.jp/IMAKADO (japanese)
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

;;; Commentary:

;; These tests depends on ert-expectation package.

;;; Code:
(require 'ert-expectations)

(expectations
  (desc "smartchr-parse smartchr-template-cursor-re")
  (expect "{  }"
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re  "`!!'"))
        (let ((struct (smartchr-parse "{ `!!' }")))
          (assert (smartchr-struct-p struct))
          (funcall (smartchr-struct-insert-fn struct))
          (buffer-string)))))

  (expect ""
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re  "`!!'"))
        (let ((struct (smartchr-parse "{ `!!' }")))
          (assert (smartchr-struct-p struct))
          (funcall (smartchr-struct-insert-fn struct))
          (funcall (smartchr-struct-cleanup-fn struct))
          (buffer-string)))))

  (desc "template allow function")
  (expect t
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re  "`!!'")
            (fn-called nil))
        (let ((struct (smartchr-parse (lambda () (setq fn-called t)))))
          (assert (smartchr-struct-p struct))
          (funcall (smartchr-struct-insert-fn struct))
          fn-called))))

  (expect "hi"
    (with-temp-buffer
      (let ((smartchr-struct-cursor-re  "`!!'"))
        (let ((struct (smartchr-parse (lambda () "hi"))))
          (assert (smartchr-struct-p struct))
          (funcall (smartchr-struct-insert-fn struct))
          (buffer-string)))))

  (desc "smartchr-parse pass argument if argument is already struct")
  (expect t
    (smartchr-struct-p
     (smartchr-parse
      (smartchr-make-struct
       :cleanup-fn (lambda ())
       :insert-fn (lambda ())))))

  (desc "smartchr-parse rest args")
  (with-temp-buffer
    (let ((smartchr-struct-cursor-re  "`!!'"))
      (let ((cmd (smartchr "a" "b"))
            (do-nothing (lambda () (interactive))))
        (call-interactively cmd)
        (buffer-string)))))

;;; smartchr-test.el ends here
