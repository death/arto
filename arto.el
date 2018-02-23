;;; arto.el --- Manage aria2 downloads via Emacs

;; Author: death <github.com/death>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: tools, convenience
;; URL: http://github.com/death/arto

;; This file is not part of GNU Emacs.

;; Copyright (c) 2018 death

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

(require 'aria2-rpc)
(require 'tabulated-list)

(defgroup arto nil
  "Manage aria2 downloads."
  :group 'tools
  :group 'convenience)

(defvar arto-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "q" 'quit-window)
    (define-key map "g" 'arto-refresh)
    map))

(define-derived-mode arto-mode tabulated-list-mode "Arto"
  "Major mode for manging aria2 downloads."
  (add-hook 'tabulated-list-revert-hook 'arto-refresh nil t))

;;;###autoload
(defun arto ()
  "Manage aria2 downloads."
  (interactive)
  (switch-to-buffer (arto--noselect)))

(defun arto--noselect ()
  (let ((buffer (get-buffer-create "*Arto*")))
    (with-current-buffer buffer
      (arto-mode)
      (arto-refresh))
    buffer))

(defun arto-refresh ()
  (interactive)
  (setq tabulated-list-format
        `[("Name" 40 t)])
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-entries
        (list (list "id 1"
                    (vector "Name 1"))
              (list "id 2"
                    (vector "Name 2"))
              (list "id 3"
                    (vector "Name 3"))))
  (tabulated-list-init-header)
  (tabulated-list-print t))

(provide 'arto)
