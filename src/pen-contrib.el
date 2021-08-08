;; This is for everything outside of core pen stuff
;; i.e. applications built on pen.el

(require 'pen-handle)
(require 'pen-nlsh)
(require 'pen-nano)
(require 'pen-org-brain)
(require 'pen-org-roam)

(defvar pen-tutor-common-questions
  '("What is <1:q> used for?"
    "What are some good learning materials"))

(defun pen-tutor-mode-assist (&optional query)
  (interactive (let* ((bl (buffer-language t t)))
                 (list
                  (read-string-hist
                   (concat "asktutor (" bl "): ")
                   (pen-thing-at-point)))))
  (let ((bl (pen-detect-language t t nil t)))
    (pf-asktutor bl bl query)))


(provide 'pen-contrib)