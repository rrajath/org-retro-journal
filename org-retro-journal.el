;;; org-retro-journal.el --- View past journal entries with navigation -*- lexical-binding: t; -*-

(require 'org)

;;-- Internal Variables --
(defvar-local org-retro-journal--source-buffer nil
  "The buffer where the journal data lives.")

(defvar-local org-retro-journal--current-date nil
  "The date currently being displayed in the review buffer.")

;;-- Keymap & Mode Definition --
(defvar org-retro-journal-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    ;; Changed to C-c prefix to avoid Meow/Evil conflicts
    (define-key map (kbd "C-c n") #'org-retro-journal-next-day)
    (define-key map (kbd "C-c p") #'org-retro-journal-prev-day)
    (define-key map (kbd "C-c j") #'org-retro-journal-goto-date)
    ;; Also allow arrow keys for convenience
    (define-key map (kbd "<right>") #'org-retro-journal-next-day)
    (define-key map (kbd "<left>") #'org-retro-journal-prev-day)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Org Retro Journal mode.")

(define-derived-mode org-retro-journal-mode org-mode "Retro-Journal"
  "Major mode for reviewing past journal entries.
\\{org-retro-journal-mode-map}"
  (read-only-mode 1))

;;-- Navigation Functions --

(defun org-retro-journal-next-day ()
  "Move forward one day."
  (interactive)
  (when org-retro-journal--current-date
    (let ((next (time-add org-retro-journal--current-date (days-to-time 1))))
      (org-retro-journal-review next org-retro-journal--source-buffer))))

(defun org-retro-journal-prev-day ()
  "Move backward one day."
  (interactive)
  (when org-retro-journal--current-date
    (let ((prev (time-subtract org-retro-journal--current-date (days-to-time 1))))
      (org-retro-journal-review prev org-retro-journal--source-buffer))))

(defun org-retro-journal-goto-date ()
  "Pick a specific date to view."
  (interactive)
  (let ((date (org-read-date nil t nil "Jump to date: ")))
    (org-retro-journal-review date org-retro-journal--source-buffer)))

;;-- Logic & Rendering --

(defun org-retro-journal--find-nearest-day (year-pos month day)
  "In the year starting at YEAR-POS, find the closest DAY in MONTH.
Returns the buffer position of the day heading."
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
          
          best-pos)))))

;;;###autoload
(defun org-retro-journal-review (&optional date source-buf)
  "Show journal entries from DATE (defaults to today).
SOURCE-BUF is the buffer containing the journal."
  (interactive)
  (let* ((target-date (or date (current-time)))
         (source (or source-buf (current-buffer)))
         (decoded (decode-time target-date))
         (target-year (decoded-time-year decoded))
         (target-month (decoded-time-month decoded))
         (target-day (decoded-time-day decoded))
         (results-buffer (get-buffer-create "*Org Retro Review*"))
         (entries-found nil))

    ;; Ensure we are reading from the correct buffer
    (unless (buffer-live-p source)
      (error "Original journal buffer is no longer active"))

    (with-current-buffer results-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Activate our custom mode
        (org-retro-journal-mode)
        ;; Store state for navigation
        (setq org-retro-journal--current-date target-date)
        (setq org-retro-journal--source-buffer source)
        
        (insert (format "#+TITLE: Journal Review: %s\n" 
                        (format-time-string "%B %d" target-date)))
        (insert "#+NOTE: Keys: C-c (n)ext, C-c (p)rev, C-c (j)ump, Left/Right Arrows\n\n")))

    ;; Scan the source buffer
    (with-current-buffer source
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\([0-9]\\{4\\}\\)" nil t)
          (let* ((year-val (string-to-number (match-string 1)))
                 (year-pos (line-beginning-position)))
            (when (< year-val target-year)
              (let ((day-pos (org-retro-journal--find-nearest-day year-pos target-month target-day)))
                (when day-pos
                  (setq entries-found t)
                  (let ((content (save-excursion
                                   (goto-char day-pos)
                                   (org-copy-subtree)
                                   (with-temp-buffer
                                     (yank)
                                     (buffer-string)))))
                    
                    (with-current-buffer results-buffer
                      (let ((inhibit-read-only t))
                        ;; Removed the diff-label logic here
                        (insert (format "* %d\n" year-val))
                        (insert content)
                        (insert "\n")))))))))))

    ;; Finalize display
    (if entries-found
        (with-current-buffer results-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (org-content))
          ;; CHANGED: pop-to-buffer forces focus to the new window
          (pop-to-buffer results-buffer))
      
      ;; Empty state handling
      (with-current-buffer results-buffer
        (let ((inhibit-read-only t))
          (insert (format "\nNo entries found for %02d-%02d." target-month target-day)))
        (pop-to-buffer results-buffer)))))

(provide 'org-retro-journal)
