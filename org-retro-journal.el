;;; org-retro-journal.el --- View past journal entries on this day -*- lexical-binding: t; -*-

(require 'org)

(defun org-retro-journal--find-nearest-day (year-pos month day)
  "In the year starting at YEAR-POS, find the closest DAY in MONTH.
Returns a cons cell (POSITION . DIFF) where DIFF is the days variance."
  (save-excursion
    (goto-char year-pos)
    (let* ((year-str (buffer-substring-no-properties 
                      (+ year-pos 2) (+ year-pos 6)))
           (month-regex (format "^\\*\\* .*%s-%02d" year-str month))
           (limit (save-excursion (org-end-of-subtree t) (point)))
           (found-month (re-search-forward month-regex limit t)))
      
      (when found-month
        (let* ((month-end (save-excursion (org-end-of-subtree t) (point)))
               (day-regex (format "^\\*\\*\\* .*%s-%02d-\\([0-9]\\{2\\}\\)" year-str month))
               (best-pos nil)
               (min-diff 32))
          
          (while (re-search-forward day-regex month-end t)
            (let* ((found-day (string-to-number (match-string 1)))
                   (diff (abs (- day found-day))))
              (when (< diff min-diff)
                (setq min-diff diff
                      best-pos (line-beginning-position)))))
          
          (when best-pos
            (cons best-pos min-diff)))))))

;;;###autoload
(defun org-retro-journal-review ()
  "Show journal entries from this date (or nearest) in previous years."
  (interactive)
  (let* ((today (decode-time (current-time)))
         (this-year (decoded-time-year today))
         (this-month (decoded-time-month today))
         (this-day (decoded-time-day today))
         (source-buffer (current-buffer))
         (results-buffer (get-buffer-create "*Org Retro Review*"))
         (entries-found nil))
    
    (with-current-buffer results-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Journal Review: %02d-%02d\n\n" this-month this-day))))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\([0-9]\\{4\\}\\)" nil t)
        (let* ((year-val (string-to-number (match-string 1)))
               (year-pos (line-beginning-position)))
          (when (< year-val this-year)
            (let ((search-result (org-retro-journal--find-nearest-day year-pos this-month this-day)))
              (when search-result
                (setq entries-found t)
                (let* ((day-pos (car search-result))
                       (diff (cdr search-result))
                       (content (save-excursion
                                  (goto-char day-pos)
                                  (org-copy-subtree)
                                  (with-temp-buffer
                                    (yank)
                                    (buffer-string))))
                       ;; (diff-label (if (= diff 0) 
                       ;;                 "Exact Date" 
                       ;;               (format "%d days off" diff)))
                       )
                  
                  (with-current-buffer results-buffer
                    (let ((inhibit-read-only t))
                      (insert (format "* %d\n" year-val))
                      (insert content)
                      (insert "\n"))))))))))

    (if entries-found
        (progn
          (display-buffer results-buffer)
          (with-current-buffer results-buffer 
            (goto-char (point-min))
            ;; This line sets the visibility to CONTENTS
            (org-content)))
      (message "No past entries found for %02d-%02d." this-month this-day))))
