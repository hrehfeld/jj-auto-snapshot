;;; jj-auto-snapshot.el --- Automatically snapshot the current JJ repository on every save.  -*- lexical-binding: t; -*-

;;; Commentary:
;; see also https://github.com/tim-janik/jj-fzf/blob/trunk/contrib/jj-undirty.el

;;; Code:
(defgroup jj-auto-snapshot nil
  "Automatically snapshot the current JJ repository on every save."
  :group 'tools
  :prefix "jj-auto-snapshot-")

(defcustom jj-auto-snapshot--log-buffer-name "*jj-auto-snapshot-mode-log*"
  "Automatically snapshot the current JJ repository on every save."
  :type 'string
  :group 'jj-auto-snapshot)

(defun jj-auto-snapshot-commit-command ()
  "Commit using jj commit.

See `jj-auto-snapshot-snapshot-command'."
  (let ((args '("jj" "--no-pager" "commit")))
    (cl-assert (buffer-file-name) t "Non-file buffer.")
    (let ((file-relative-name (file-relative-name (buffer-file-name) (jj-auto-snapshot-dominating-file (buffer-file-name)))))
            (append args (list "-m" file-relative-name file-relative-name)))))


(defcustom jj-auto-snapshot-snapshot-command #'jj-auto-snapshot-commit-command
  "Command to execute to snapshot the current repository. Should be a list of strings to hand to `start-process' or a function returning such list."
  :type '(choice (function :tag "Function")
                 (repeat string))
  :group 'jj-auto-snapshot)

(defcustom jj-auto-snapshot-snapshot-hook 'after-save-hook
  "Hook variable to attach snapshot taking function `jj-auto-snapshot--take-snapshot' to."
  :type 'hook
  :group 'jj-auto-snapshot
  )

(defun jj-auto-snapshot-dominating-file (start-at-path)
  (locate-dominating-file start-at-path ".jj"))

(defun jj-auto-snapshot--take-snapshot()
  "Execute `jj status` to snapshot the current repository.
This function checks if the current buffer resides in a JJ repository,
and if so executes `jj status` while logging the command output to
the `jj-auto-snapshot--log-buffer-name' buffer.
"
  (interactive)
  (progn
    (let ((absfile (buffer-file-name))
	        (buffer (get-buffer-create jj-auto-snapshot--log-buffer-name))
 	        (process-connection-type nil))	; use a pipe instead of a pty
      (when (jj-auto-snapshot-dominating-file absfile)	; detect JJ repo
        (let* ((cmd (if (functionp jj-auto-snapshot-snapshot-command)
                        (funcall jj-auto-snapshot-snapshot-command)
                      jj-auto-snapshot-snapshot-command))
               (cmd-str (string-join (mapcar #'shell-quote-argument cmd) " ")))
	        (with-current-buffer buffer
	          (goto-char (point-max))		; append to end of buffer
	          (insert "\n# jj-auto-snapshot--take-snapshot: " absfile "\n" cmd-str "\n")
            (let ((default-directory (file-name-directory absfile)))
              (apply 'start-process
                     cmd-str
                     buffer
                     cmd)))
	        )))))

;; no idea why this is needed, but I get undefined variable errors without it
(defvar-local jj-auto-snapshot-mode nil "Automatically snapshot the current JJ repository on every save.")

;;;###autoload
(define-minor-mode jj-auto-snapshot-mode
  "Automatically snapshot the current JJ repository on every save."
  :lighter " jjsnap"
  :var jj-auto-snapshot-mode
  (if jj-auto-snapshot-mode
      (add-hook jj-auto-snapshot-snapshot-hook 'jj-auto-snapshot--take-snapshot nil 'local)
    (remove-hook jj-auto-snapshot-snapshot-hook 'jj-auto-snapshot--take-snapshot 'local)))

;;;###autoload
(define-minor-mode jj-auto-snapshot-global-mode
  "Automatically snapshot the current JJ repository on every save."
  :lighter " jjsnap"
  :global t
  (if jj-auto-snapshot-global-mode
      (add-hook jj-auto-snapshot-snapshot-hook 'jj-auto-snapshot--take-snapshot)
    (remove-hook jj-auto-snapshot-snapshot-hook 'jj-auto-snapshot--take-snapshot)))

(provide 'jj-auto-snapshot)

;;; jj-auto-snapshot.el ends here
