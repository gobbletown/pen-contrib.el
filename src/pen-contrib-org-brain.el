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

(provide 'pen-contrib-org-brain)