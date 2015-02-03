;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ELisp scripts used to get my results
;;; 
;;; WARNING: only for reference purpose only, too dirty to be re-run directly.


;;; parse meaningless buffers
(with-current-buffer "Botan.diff"
  (goto-char (point-min))
  (ignore-errors
    (loop until (string= "Botan.spec" (car (diff-hunk-file-names)))
          ;; `diff-file-next' can trigger error once reaches eof
          do (diff-file-next))
    (re-search-forward "^+Version:" (cadr (diff-bounds-of-file)))))

;;; parse the tanana html file
(defvar conflict-pkglist nil)
(let ((conflicts-parsed-html
       (with-current-buffer (find-file-noselect "tanana-core-tasks-conflicts-1-29.htm")
         (libxml-parse-html-region (point-min) (point-max))))
      pkglist-table
      pkglist)
  (-tree-map-nodes (lambda (branch)
                     (and
                      (listp branch)
                      (eq (car branch) 'table)
                      (string= (cdr (assq 'id (nth 1 branch))) "packagelist")))
                   (lambda (branch)
                     (setq pkglist-table branch))
                   conflicts-parsed-html)
  ;; (edebug)
  (-tree-map-nodes (lambda (pkg-tr)
                     (and (listp pkg-tr)
                          (eq (car pkg-tr) 'tr)))
                   (lambda (pkg-tr)
                     (push (nth 2 (nth 2 (nth 2 pkg-tr))) pkglist))
                   (nth 3 pkglist-table))
  (setq conflict-pkglist (nreverse pkglist))
  (with-current-buffer (get-buffer-create "*SLE-Conflict-Packages*")
    (loop for pkg in conflict-pkglist
          do (insert pkg "\n"))))

;;; WARNING: the following code generate processes far too quick, which suspends the system...
(and nil
     (loop for pkg in conflict-pkglist
           do (start-process-shell-command
               (format "%s-rdiff-proc" pkg)
               nil
               (format "osc -A https://api.suse.de rdiff openSUSE.org:openSUSE:Factory %s SUSE:SLE-12:GA > diffs/%s.diff"
                       pkg pkg))))

;;; a redefinition to fix a bug see `sle-diff-checker-only-dates-diff?'
(defun diff-ignore-whitespace-hunk ()
  "Re-diff the current hunk, ignoring whitespace differences."
  (interactive)
  (let* ((char-offset (- (point) (diff-beginning-of-hunk t)))
	 (opts (pcase (char-after) (?@ "-bu") (?* "-bc") (_ "-b")))
	 (line-nb (and (or (looking-at "[^0-9]+\\([0-9]+\\)")
			   (error "Can't find line number"))
		       (string-to-number (match-string 1))))
	 (inhibit-read-only t)
	 (hunk (delete-and-extract-region
		(point) (save-excursion (diff-end-of-hunk) (point))))
	 (lead (make-string (if (zerop line-nb) ;handle added new file case
                                line-nb
                              (1- line-nb)) ?\n)) ;Line nums start at 1.
	 (file1 (make-temp-file "diff1"))
	 (file2 (make-temp-file "diff2"))
	 (coding-system-for-read buffer-file-coding-system)
	 old new)
    (unwind-protect
	(save-excursion
	  (setq old (diff-hunk-text hunk nil char-offset))
	  (setq new (diff-hunk-text hunk t char-offset))
	  (write-region (concat lead (car old)) nil file1 nil 'nomessage)
	  (write-region (concat lead (car new)) nil file2 nil 'nomessage)
	  (with-temp-buffer
	    (let ((status
		   (call-process diff-command nil t nil
				 opts file1 file2)))
	      (pcase status
		(0 nil)                 ;Nothing to reformat.
		(1 (goto-char (point-min))
                   ;; Remove the file-header.
                   (when (re-search-forward diff-hunk-header-re nil t)
                     (delete-region (point-min) (match-beginning 0))))
		(_ (goto-char (point-max))
		   (unless (bolp) (insert "\n"))
		   (insert hunk)))
	      (setq hunk (buffer-string))
	      (unless (memq status '(0 1))
		(error "Diff returned: %s" status)))))
      ;; Whatever happens, put back some equivalent text: either the new
      ;; one or the original one in case some error happened.
      (insert hunk)
      (delete-file file1)
      (delete-file file2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filter diffs with meaningless differences
(defun sle-diff-checker-only-dates-diff? (diff-fn &optional
                                                  clean-results-p
                                                  ignored-diffs-result-fn
                                                  meaningful-diffs-result-fn)
  "Check whether DIFF-FN only continas non-functional diffs.

If Clean-Results-P is t, clean results buffers/files.

IGNORED-DIFFS-RESULT-FN is the file recording the list of ignored
diff files, default to a buffer *SLE-Diff-Ignored*.

MEANINGFUL-DIFFS-RESULT-FN is the file recording the list of
meaningful diff files, default to a buffer *SLE-Diff-Meaningful*."
  (let ((sle-diff-ignored-buf (or ignored-diffs-result-fn
                                  (get-buffer-create "*SLE-Diff-Ignored*")))
        (sle-diff-meaningful-buf (or ignored-diffs-result-fn
                                     (get-buffer-create "*SLE-Diff-Meaningful*")))
        ;; whether diff text only contains dates
        meaningful-diff-text
        (only-date-p t))
    (unless (and (buffer-live-p sle-diff-ignored-buf)
                 (buffer-live-p sle-diff-meaningful-buf))
      (error "Result files are not supported yet."))
    (when clean-results-p
      (with-current-buffer sle-diff-ignored-buf (erase-buffer))
      (with-current-buffer sle-diff-meaningful-buf (erase-buffer)))
    ;; main parsing
    (with-temp-buffer
      (insert-file-contents-literally diff-fn)
      (goto-char (point-min))
      ;; (loop until (s-ends-with? ".changes" (car (diff-hunk-file-names)))
      ;;       ;; `diff-file-next' can trigger error once reaches eof
      ;;       do (diff-file-next))
      (while (ignore-errors (progn (diff-file-next) t))
        (while (s-ends-with? ".changes"
                             (progn
                               ;; look ahead without actually changing position
                               (save-excursion
                                 (if (ignore-errors (progn (diff-hunk-next) t))
                                     (car (diff-hunk-file-names))))))
          (diff-hunk-next)
          ;; some diffs contain 0-indexed line numbers (seems to be only for
          ;; newly added files), but `diff-ignore-whitespace-hunk' needs line
          ;; number start from 1, ignore this error
          ;; TODO who is right? I think this is a bug for `diff-ignore-whitespace-hunk'
          (unless (ignore-errors (diff-ignore-whitespace-hunk)))
          (setq meaningful-diff-text (apply #'buffer-substring-no-properties
                                            ;; `diff-ignore-whitespace-hunk' can
                                            ;; lead to the removal of the whole hunk
                                            (or (ignore-errors (diff-bounds-of-hunk))
                                                '(1 1))))
          (with-temp-buffer
            (insert meaningful-diff-text)
            (goto-char (point-min))
            ;; only preserve the changes in SLE-12
            (delete-matching-lines "^[^+].*")
            (setq meaningful-diff-text (buffer-substring-no-properties
                                        (point-min) (point-max)))
            ;; check for dates
            (let ((last-point (goto-char (point-min))))
              (while (and only-date-p
                          (zerop (forward-line)))
                (when (zerop (string-to-number
                              (aref (timezone-parse-date
                                     ;; get current line without leading "+/-" signs
                                     (buffer-substring-no-properties
                                      (1+ last-point) (point)))
                                    0)))
                  (setq only-date-p nil))
                (setq last-point (point)))))
          ;; in-time records for instrospection
          ;; (unless only-date-p
          ;;   (with-current-buffer sle-diff-meaningful-buf
          ;;     (insert meaningful-diff-text)))
          )))
    ;; record results
    (with-current-buffer (if only-date-p
                             sle-diff-ignored-buf
                           sle-diff-meaningful-buf)
      (insert diff-fn "\n"))))

;;; work with all files
(let ((default-directory "~/work/sle_core_tasks/diffs"))
  (loop for file in (file-expand-wildcards "*")
        when (not (s-prefix? "kernel" file))
        do (sle-diff-checker-only-dates-diff? file)))

;; (let ((default-directory "~/work/sle_core_tasks/diffs"))
;;   (sle-diff-checker-only-dates-diff? "seed.diff" t))

