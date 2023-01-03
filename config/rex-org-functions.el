;; A function that is mostly just stolen from Doom, but is trimmed down to only
;; do things that I need it to do.
(defun rex/org-dwim-at-point (&optional arg)
  "Do what I mean at point. This will toggle the todo state of a
headline or follow a link."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (pcase type
      ;; toggle todo state
      (`headline
       (cond ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   'todo
                 'done))))
       (org-update-checkbox-count)
       (org-update-parent-todo-statistics))
      ;; follow a link
      (`link (org-open-at-point))
      ;; toggle a checkbox
      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16))))))))
