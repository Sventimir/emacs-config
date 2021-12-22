;; Functionality for handling packages.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun ensure-package (pkg)
  (progn
    (unless (package-installed-p pkg)
      (package-install pkg))
    (require pkg)))
