(defcustom pen-nlsh-histdir ""
  "Directory where history files for nlsh"
  :type 'string
  :group 'pen
  :initialize #'custom-initialize-default)

;; "OS which have a bash-like shell of some kind installed"
(defset list-of-sh-operating-systems '(
                                       ;;  There has been a name change
                                       ;; That's why this is giving bad results
                                       ;; "GNU Guix System"
                                       "GuixSD"
                                       "Alpine Linux"
                                       "RHEL Red Hat Enterprise Linux"
                                       "Amazon Linux 2"
                                       "NixOS"
                                       "macOS"
                                       "Ubuntu 20.04"
                                       "Arch Linux"))

(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(defun pen-nsfa (cmd &optional dir)
  (pen-sn (concat
           (if dir (concat " CWD=" (q dir) " ")
             "")
           " pen-nsfa -E " (q cmd)) nil (or dir (cwd))))

(defun comint-quick (cmd &optional dir)
  (interactive (list (read-string-hist "comint-quick: ")))
  (let* ((slug (slugify cmd))
         (buf (make-comint slug (pen-nsfa cmd dir))))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (turn-on-comint-history (f-join pen-nlsh-histdir slug)))))

(defun nlsh-os (os)
  (interactive (list (fz list-of-sh-operating-systems
                         nil nil "nlsh-os: ")))
  (comint-quick (cmd "nlsh-os" os)))

(provide 'pen-nlsh)