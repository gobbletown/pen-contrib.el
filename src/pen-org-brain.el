(require 'org-brain)
(require 'pen-support)

(defun org-brain-current-brain ()
  (f-base org-brain-path))

(defun org-brain-parent-name ()
  (mapconcat 'identity
             (org-brain-remove-irrelevant-names-from-path
              (mapcar
               'org-brain-name-from-list-maybe
               (org-brain-parents org-brain--vis-entry)))
             " "))

(defun org-brain-is-index-name (s)
  (or (string-equal s "index")
      (string-equal s "reference")))

(defun org-brain-pf-topic (&optional short)
  "Topic used for pen functions"
  (let ((cn (org-brain-current-name t)))
    (if (and (org-brain-is-index-name cn)
             (not (sor (org-brain-parent-name))))
        (if (org-brain-is-index-name (org-brain-current-brain))
            "general knowledge"
          (org-brain-current-brain))
      (let ((p (org-brain-parent-name)))
        (if (and (sor p)
                 (not short)
                 (not (org-brain-is-index-name p)))
            (concat cn " (" cn " is a subtopic of " p ")")
          cn)))))

(defun org-brain-show-topic ()
  (interactive)
  (call-interactively 'pen-topic))

(defun pen-org-brain-current-topic (&optional for-external-searching)
  (interactive)
  (let ((cname (org-brain-current-name))
        (pname (org-brain-parent-name))
        (path (mapcar 'org-brain-name-from-list-maybe (append (org-brain-parents org-brain--vis-entry) (list org-brain--vis-entry))))
        (topic))

    (setq topic
          (cond
           ((org-brain-at-child-of-index) cname)
           ((org-brain-at-top-index) "general knowledge")
           (t "general knowledge")))

    (setq path
          (if for-external-searching
              (org-brain-remove-irrelevant-names-from-path path)
            path))

    (let ((topic (chomp (apply 'cmd path))))
      (if (not (sor topic))
          (setq topic "general knowledge"))

      (if (interactive-p)
          (etv topic)
        topic))))

;; TODO Clean this up.
;; I don't want org-template-gen involved.
;; I also don't want to use extensions to org-brain that I have made.
(defun org-brain-describe-topic ()
  (interactive)

  (let* ((p (sor (org-brain-parent-name)))
         (pretext)
         (question (if (and p (not (org-brain-at-child-of-index)))
                       (concat "Could you please explain what " (pen-topic t) " is in the context of " p " and why it is important?")
                     (concat "Could you please explain what " (pen-topic t) " is and why it is important?")))
         (final-question
          (if (sor pretext)
              (concat pretext " " question)
            question)))

    (let ((description (org-brain-asktutor final-question)))
      (if (sor description)
          (progn
            (let ((cb (current-buffer)))
              (my-org-brain-goto-current)
              (let ((block-name (concat (org-brain-current-name) "-description")))
                (if (not (org-babel-find-named-block block-name))
                    (progn
                      (insert
                       (snc (cmd "org-template-gen" "brain-description" block-name) description))
                      (call-interactively 'save-buffer)
                      (call-interactively 'my/revert-kill-buffer-and-window))))
              (with-current-buffer cb
                (revert-buffer))))))))

;; TODO Fix this.
;; Replace ttp with a much simpler thing.
;; I want to incorporate spaCy at some point anyway.
(defun org-brain-asktutor (question)
  (interactive (list (read-string-hist (concat "asktutor about " (pen-topic) ": "))))
  (let ((topic (pen-org-brain-current-topic)))

    (let ((answer
           (snc "ttp" (pf-generic-tutor-for-any-topic topic question))))
      (if (interactive-p)
          (etv answer)
        answer))))


(defun org-brain-suggest-subtopics (&optional update do-not-incorportate-existing suggest-full-list edit-full-list)
  "
update will update the cache
do-not-incorportate-existing will not incorporate existing subtopics into the prompt, so they do not influence suggestions
edit-full-list allows you to edit the list before proceeding
suggest-full-list will ask if you want to add the entire list as subtopics to the current entry
"
  (interactive)

  (let ((inhibit-quit t))
    (unless (with-local-quit
              (progn
                (qa
                 -u (setq update t)
                 -n (setq do-not-incorporate-existing t)
                 -i (setq do-not-incorporate-existing nil)
                 -e (setq edit-full-list t)
                 -f (setq suggest-full-list t)
                 -F (setq suggest-full-list t update t))

                (message "Using pen.el to suggest subtopics...")

                (let ((subtopic-candidates
                       ;; (pf-keyword-extraction (org-brain-current-topic t))
                       (let ((sh-update (or sh-update
                                            update
                                            (eq (prefix-numeric-value current-prefix-arg) 4)))
                             (existing-subtopics-string (if do-not-incorportate-existing
                                                            ""
                                                          (org-brain-existing-subtopics-stringlist t))))
                         (let ((s (pf-subtopic-generation
                                   (pen-topic)
                                   existing-subtopics-string)))
                           (if (not (sor s))
                               (progn
                                 (message "Empty generation 1/3. Trying again.")
                                 (setq s (upd (pf-subtopic-generation
                                               (pen-topic)
                                               existing-subtopics-string)))
                                 (if (not (sor s))
                                     (progn
                                       (message "Empty generation 2/3. Trying again.")
                                       (setq s (upd (pf-subtopic-generation
                                                     (pen-topic)
                                                     existing-subtopics-string)))
                                       (if (not (sor s))
                                           (progn
                                             (message "Empty generation 3/3. Giving up.")
                                             (error "Empty generation 3/3. Giving up."))
                                         s))
                                   s)
                                 s)
                             s)))))

                  (setq subtopic-candidates
                        (-filter-not-empty-string
                         (-uniq-u
                          (str2list
                           (concat
                            (awk1 subtopic-candidates)
                            (org-brain-existing-subtopics-stringlist))))))

                  ;; (tv subtopic-candidates)

                  ;; The prompt does "- " removal on its own now
                  ;; (setq subtopic-candidates
                  ;;       (str2list
                  ;;        (cl-sn
                  ;;         "sed 's/^- //'"
                  ;;         :stdin
                  ;;         (chomp
                  ;;          (snc
                  ;;           (cmd "scrape" "^- [a-zA-Z -]+$")
                  ;;           (concat "- " subtopic-candidates))) :chomp t)))

                  ;; (ns current-prefix-arg)

                  (if (interactive-p)
                      (progn
                        (let ((subtopic-selected
                               (try
                                (cond
                                 (edit-full-list
                                  (nbfs (list2str subtopic-candidates)))
                                 (suggest-full-list
                                  (let ((b (nbfs (list2str subtopic-candidates))))
                                    (with-current-buffer b
                                      (let ((r (if (yn "Add all?")
                                                   subtopic-candidates)))
                                        (kill-buffer b)
                                        r))))
                                 (t
                                  ;; Select one, do not refresh cache
                                  (list (fz subtopic-candidates)))))))

                          (if subtopic-selected
                              (cl-loop for st in subtopic-selected do
                                       (org-brain-add-child-headline org-brain--vis-entry st)))))
                    subtopic-candidates)))
              t)
      (progn
        (message "Cancelled")
        (setq quit-flag nil)))))

;; TODO
;; j:org-brain-asktutor

(provide 'pen-org-brain)