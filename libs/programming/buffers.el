;;; Package --- Summary
;;; Commentary:
;;; Code:

(defmacro setup-buffer (name &rest body)
  "If buffer NAME does not exist, create it and execute BODY.
Then display the buffer."
  (declare (indent defun))
  `(progn
     (unless (get-buffer ,name)
       (with-current-buffer (get-buffer-create ,name)
         ,@body))
     (pop-to-buffer ,name)))

(provide 'buffers)
;;; buffers.el ends here
