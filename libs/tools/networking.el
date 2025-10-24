;;; -*- lexical-binding: t -*-
;;; Package: --- Summary
;;; Commentary:
;;; Code:

(defcustom networking-wifi-dev "wlo1"
  "The name of the WIFI device."
  :type 'string
  :group 'networking)

(defun networking-sudo-shell-cmd (cmd &optional pass)
  "Run shell command CMD as superuser (asking for password if PASS not supplied)."
  (let ((passwd (or pass (read-passwd "Password:"))))
    (shell-command-to-string (concat "echo " (shell-quote-argument passwd) "| sudo -S " cmd))))

(defun networking-ip-cmd (sudo-pass &rest args)
  "Run the ip command with ARGS, using sudo if SUDO-PASS is not nil."
  (let ((cmd (mapconcat 'shell-quote-argument (cons "ip" args) " ")))
    (if sudo-pass (networking-sudo-shell-cmd cmd sudo-pass) (shell-command-to-string cmd))))

(defun networking-dev-up? (dev)
  "Check the status of networking device DEV (UP/DOWN)."
  (let ((output (networking-ip-cmd nil "link" "show" dev)))
    (string-match "state \\([A-Z]+\\)" output)
    (string= "UP" (match-string 1 output))))

(defun networking-wifi-scan (passwd)
  "Scan the area for wifi networks using sudo PASSWD."
  (interactive (list (read-passwd "Sudo password:")))
  (let ((dev-up? (networking-dev-up? networking-wifi-dev)))
    (if (not dev-up?) (networking-ip-cmd passwd "link" "set" networking-wifi-dev "up"))
    (pop-to-buffer "*wifi-scan*")
    (erase-buffer)
    (insert
     (networking-sudo-shell-cmd (mapconcat 'shell-quote-argument
                                           (list "iwlist" networking-wifi-dev "scan")
                                           " ")
                                passwd))
    (if (not dev-up?) (networking-ip-cmd passwd "linl" "set" networking-wifi-dev "down"))))

(provide 'networking)
;;; networking.el ends here
