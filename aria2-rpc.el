;;; aria2-rpc.el --- Communicate with aria2 using JSON RPC.

;; Author: death <github.com/death>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: external
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

(require 'json-rpc)

(defgroup aria2-rpc nil
  "Communicate with aria2 using JSON RPC."
  :group 'external)

(defcustom aria2-rpc-executable "aria2c"
  "The name of the aria2 client executable."
  :type 'string)

(defcustom aria2-rpc-arguments '()
  "A list of additional arguments to pass to the aria2 client
when starting it."
  :type '(list string))

(defcustom aria2-rpc-secret :ask
  "The secret token needed in order to communicate with the aria2
client."
  :type '(choice (const :tag "Ask" :ask)))

(defcustom aria2-rpc-delay 0.5
  "How long to wait before attempting to connect to an aria2
process after starting it."
  :type 'number)

(defcustom aria2-rpc-default-directory "~/Downloads/"
  "Pathname to directory where the aria2 client will work."
  :type 'directory)

(defcustom aria2-rpc-host "localhost"
  "Host for RPC communication."
  :type 'string)

(defcustom aria2-rpc-port 6800
  "Port for RPC communication."
  :type 'integer)

(defvar aria2-rpc--connection nil
  "An RPC connection to aria2.")

(defvar aria2-rpc--process nil
  "The process object of the aria2 client started by aria2-rpc,
if any.")

(defun aria2-rpc--start-process ()
  "Start a new aria2 process."
  (aria2-rpc--kill-process)
  (setq aria2-rpc--process
        (let ((default-directory aria2-rpc-default-directory))
          (apply #'start-process
                 aria2-rpc-executable
                 nil
                 aria2-rpc-executable
                 (list* "--enable-rpc"
                        (concat "--rpc-secret=" (aria2-rpc--secret))
                        aria2-rpc-arguments)))))

(defun aria2-rpc--kill-process ()
  "Delete the current aria2 process, if any."
  (when aria2-rpc--process
    (delete-process aria2-rpc--process)
    (setq aria2-rpc--process nil)))

(defun aria2-rpc--connect ()
  "Open a connection to an aria2 RPC server."
  (aria2-rpc--disconnect)
  (setq aria2-rpc--connection
        (json-rpc-connect aria2-rpc-host aria2-rpc-port)))

(defun aria2-rpc--disconnect ()
  "Close an existing RPC connection to aria2, if any."
  (when aria2-rpc--connection
    (json-rpc-close aria2-rpc--connection)
    (setq aria2-rpc--connection nil)))

(defun aria2-rpc--ensure ()
  "Attempt to ensure that an aria2 RPC server is available."
  (when (or (null aria2-rpc--connection)
            (not (json-rpc-live-p aria2-rpc--connection)))
    (condition-case err
        (aria2-rpc--connect)
      (error
       (aria2-rpc--start-process)
       (sleep-for aria2-rpc-delay)
       (aria2-rpc--connect)))))

(defun aria2-rpc--call (method &optional params)
  "Call an RPC method."
  (aria2-rpc--ensure)
  (json-rpc-2.0 aria2-rpc--connection
                "/jsonrpc"
                method
                (vconcat (vector (concat "token:" (aria2-rpc--secret)))
                         params)))

(defun aria2-rpc--secret ()
  "Return the secret token used to communicate with the aria2
client."
  (if (eq aria2-rpc-secret :ask)
      (setq aria2-rpc-secret
            (read-passwd "Enter aria2 secret token: "))
    aria2-rpc-secret))

(defun aria2-rpc-add-uri (uris &optional options position)
  (aria2-rpc--call "aria2.addUri"
                   (vconcat
                    (vector
                     (if (stringp uris)
                         (vector uris)
                       (vconcat uris))
                     (or options (make-hash-table)))
                    (if position (vector position)))))

(defun aria2-rpc-add-torrent (torrent &optional uris options position)
  (aria2-rpc--call "aria2.addTorrent"
                   (vconcat
                    (vector
                     torrent
                     (vconcat uris)
                     (or options (make-hash-table)))
                    (if position (vector position)))))

(defun aria2-rpc-add-metalink (metalink &optional options position)
  (aria2-rpc--call "aria2.addTorrent"
                   (vconcat
                    (vector
                     metalink
                     (or options (make-hash-table)))
                    (if position (vector position)))))

(defun aria2-rpc-remove (gid &optional force)
  (aria2-rpc--call (if force "aria2.forceRemove" "aria2.remove")
                   (vector gid)))

(defun aria2-rpc-pause (gid &optional force)
  (aria2-rpc--call (if force "aria2.forcePause" "aria2.pause")
                   (vector gid)))

(defun aria2-rpc-pause-all (&optional force)
  (aria2-rpc--call (if force "aria2.forcePauseAll" "aria2.pauseAll")))

(defun aria2-rpc-unpause (gid)
  (aria2-rpc--call "aria2.unpause" (vector gid)))

(defun aria2-rpc-unpause-all ()
  (aria2-rpc--call "aria2.unpauseAll"))

(defun aria2-rpc-tell-status (gid &optional keys)
  (aria2-rpc--call "aria2.tellStatus"
                   (vconcat (vector gid) (vconcat keys))))

(defun aria2-rpc-get-uris (gid)
  (aria2-rpc--call "aria2.getUris" (vector gid)))

(defun aria2-rpc-get-files (gid)
  (aria2-rpc--call "aria2.getFiles" (vector gid)))

(defun aria2-rpc-get-peers (gid)
  (aria2-rpc--call "aria2.getPeers" (vector gid)))

(defun aria2-rpc-get-servers (gid)
  (aria2-rpc--call "aria2.getServers" (vector gid)))

(defun aria2-rpc-active (&optional keys)
  (aria2-rpc--call "aria2.tellActive" (vector (vconcat keys))))

(defun aria2-tell-waiting (offset num &optional keys)
  (aria2-rpc-call "aria2.tellWaiting" (vector offset num (vconcat keys))))

(defun aria2-tell-stopped (offset num &optional keys)
  (aria2-rpc-call "aria2.tellStopped" (vector offset num (vconcat keys))))

(defun aria2-rpc-change-position (gid pos how)
  (aria2-rpc--call "aria2.changePosition" (vector gid pos how)))

(defun aria2-rpc-change-uri (gid file-index del-uris add-uris &optional position)
  (aria2-rpc--call "aria2.changeUri"
                   (vconcat (vector gid
                                    file-index
                                    (vconcat del-uris)
                                    (vconcat add-uris))
                            (if position
                                (vector position)))))

(defun aria2-rpc-get-option (gid)
  (aria2-rpc--call "aria2.getOption" (vector gid)))

(defun aria2-rpc-change-option (gid options)
  (aria2-rpc--call "aria2.changeOption"
                   (vector gid (or options (make-hash-table)))))

(defun aria2-rpc-get-global-option ()
  (aria2-rpc--call "aria2.getGlobalOption"))

(defun aria2-rpc-change-global-option (options)
  (aria2-rpc--call "aria2.changeGlobalOption"
                   (vector (or options (make-hash-table)))))

(defun aria2-rpc-get-global-stat ()
  (aria2-rpc--call "aria2.getGlobalStat"))

(defun aria2-rpc-purge-download-result ()
  (aria2-rpc--call "aria2.purgeDownloadResult"))

(defun aria2-rpc-remove-download-result (gid)
  (aria2-rpc--call "aria2.removeDownloadResult" (vector gid)))

(defun aria2-rpc-get-version ()
  (aria2-rpc--call "aria2.getVersion"))

(defun aria2-rpc-get-session-info ()
  (aria2-rpc--call "aria2.getSessionInfo"))

(defun aria2-rpc-save-session ()
  (aria2-rpc--call "aria2.saveSession"))

(defun aria2-rpc-shutdown (&optional force)
  (aria2-rpc--call (if force "aria2.forceShutdown" "aria2.shutdown")))

;; What about notifications?  json-rpc doesn't seem to support them at
;; the moment.
;;
;; on-download-start
;; on-download-pause
;; on-download-stop
;; on-download-complete
;; on-download-error
;; on-bt-download-complete

(provide 'aria2-rpc)

;;; aria2-rpc.el ends here
