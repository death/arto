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

(defface arto-entry-name
  '((((class color) (background dark))
     :foreground "SteelBlue")
    (((class color) (background light))
     :foreground "SteelBlue4"))
  "Face for arto entry's name."
  :group 'arto)

(defface arto-entry-progress-bar-delimiter
  '((((class color) (background dark))
     :foreground "PaleVioletRed4")
    (((class color) (background light))
     :foreground "PaleVioletRed"))
  "Face for arto entry's progress bar delimiters."
  :group 'arto)

(defface arto-entry-progress-bar-step
  '((((class color) (background dark))
     :foreground "MediumSpringGreen")
    (((class color) (background light))
     :foreground "LimeGreen"))
  "Face for arto entry's progress bar steps."
  :group 'arto)

(defface arto-entry-dn
  '((((class color) (background dark))
     :foreground "LightSeaGreen")
    (((class color) (background light))
     :foreground "LightSeaGreen"))
  "Face for arto entry's download info."
  :group 'arto)

(defvar arto--progress-bar-steps 20
  "The number of 'steps' in the progress bar.")

(defvar arto--progress-bar-step-empty ? )

(defvar arto--progress-bar-step-nonempty ?=)

(defvar arto--progress-bar-step-complete ?*)

(defvar arto-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "a" 'arto-add)
    (define-key map "r" 'arto-remove)
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
  (pop-to-buffer-same-window (arto--noselect)))

(defvar-local arto--refresh-timer nil)

(defun arto--noselect ()
  (let ((buffer (get-buffer-create "*Arto*")))
    (with-current-buffer buffer
      (arto-mode)
      (aria2-rpc-change-global-option '(:seed-time "0"))
      (arto-refresh)
      (unless arto--refresh-timer
        (setq arto--refresh-timer
              (run-with-timer 0 1.0 'arto--refresh-when-visible buffer))
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (timerp arto--refresh-timer)
                      (cancel-timer arto--refresh-timer)
                      (setq arto--refresh-timer nil))))))
    buffer))

(defun arto--refresh-when-visible (buffer)
  (when (and (buffer-live-p buffer)
             (get-buffer-window buffer))
    (arto-refresh buffer)))

(defun arto--active ()
  (let ((raw-active (aria2-rpc-tell-active '(:gid :bittorrent :completedLength :totalLength)))
        (active '()))
    (dotimes (i (length raw-active))
      (let* ((item (aref raw-active i))
             (gid (plist-get item :gid))
             (bt (plist-get item :bittorrent))
             (bt-info (plist-get bt :info))
             (completed (arto--safe-numberize (plist-get item :completedLength)))
             (total (arto--safe-numberize (plist-get item :totalLength))))
        (push (list gid (vector
                         (propertize (or (plist-get bt-info :name) "[unavailable]")
                                     'font-lock-face 'arto-entry-name)
                         (arto--progress-bar (or completed 0) (or total 0))
                         (propertize (concat (if completed (arto--humanize-size completed) "-")
                                             "/"
                                             (if total (arto--humanize-size total) "-"))
                                     'font-lock-face 'arto-entry-dn)))
              active)))
    active))

(defun arto--safe-numberize (x)
  (cond ((null x) x)
        ((stringp x) (string-to-number x))
        ((numberp x) x)
        (t nil)))

(defun arto--progress-bar (completed total)
  (let ((nonempty (ceiling (* arto--progress-bar-steps
                              (if (< completed total)
                                  (/ (float completed) (float total))
                                1))))
        (nonempty-char (if (= completed total)
                           arto--progress-bar-step-complete
                         arto--progress-bar-step-nonempty)))
    (concat (propertize "[" 'font-lock-face 'arto-entry-progress-bar-delimiter)
            (propertize (make-string nonempty nonempty-char)
                        'font-lock-face 'arto-entry-progress-bar-step)
            (propertize (make-string (- arto--progress-bar-steps nonempty)
                                     arto--progress-bar-step-empty)
                        'font-lock-face 'arto-entry-progress-bar-step)
            (propertize "]" 'font-lock-face 'arto-entry-progress-bar-delimiter))))

(defun arto--humanize-size (size)
  (when (stringp size)
    (setq size (string-to-number size)))
  (let* ((units '("TB" "GB" "MB" "KB" "B"))
         (one (expt 1024 (1- (length units)))))
    (while (and units (< size one))
      (pop units)
      (setq one (/ one 1024)))
    (if (zerop one)
        "0 B"
      (concat (if (zerop (mod size one))
                  (number-to-string (/ size one))
                (format "%.2f" (/ (float size) one)))
              " " (car units)))))

(defun arto-refresh (&optional buffer)
  (interactive)
  (condition-case nil
      (let ((active (arto--active)))
        (with-current-buffer (or buffer (get-buffer-create "*Arto*"))
          (setq tabulated-list-format
                `[("Name" 70 t)
                  ("Progress" ,(+ arto--progress-bar-steps 2) t)
                  ("DN" 10 t)])
          (setq tabulated-list-use-header-line t)
          (setq tabulated-list-entries active)
          (tabulated-list-init-header)
          (tabulated-list-print t)))
    (aria2-rpc-error nil)))

(defun arto-add (magnet)
  (interactive "sMagnet URL: ")
  (cond ((equal magnet "")
         (message "No magnet URL supplied; ignoring..."))
        (t
         (aria2-rpc-add-uri magnet))))

(defun arto-remove ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Arto*")
    (let ((gid (tabulated-list-get-id)))
      (when gid
        (aria2-rpc-remove gid)))))

(provide 'arto)
