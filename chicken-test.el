;; Synopsis
;; ========
;; A major mode for CHICKEN tests that use the test-egg.
;; It allows you to run your tests from within emacs and allows you to quickly and conveniently navigate the test output
;;

;; Configuration
;; =============
;; Bind the function chicken-test-run-file to a key of your choice. For example:
;; (global-set-key (kbd "C-c c t") 'chicken-test-run-file)
;; (global-set-key (kbd "C-c c T") 'chicken-test-run-file-with-filter)
;; (global-set-key (kbd "C-c c z") 'chicken-test-switch-to-test-buffer)
;;
;; In the test buffer the following keys are bound:
;;
;; n - move to next failure
;; p - move to previous failure
;; f - move to first failure
;; l - move to last failure
;; N - move to next test
;; P - move to previous test
;; q - close buffer
;;
;; Status
;; ======
;; possibly unstable (currently work in progress)
;;
;; Limitations
;; ============
;; * it currently only works with the test egg
;; * it has no proper font-locking of the test buffer (in progress)

(defvar chicken-test-buffer-name "*CHICKEN-Test*")
(defvar chicken-test-mode-hook)
(defvar chicken-test-buffer)
(defvar chicken-test-last-run)

(defvar chicken-test-ok-message
  (progn
    (let ((msg "OK - All tests passed"))
      (put-text-property 0 2 'face '(foreground-color . "dark green") msg)
      msg)))

(defvar chicken-test-fail-message
  (progn
    (let ((msg "FAILED - Some tests failed"))
      (put-text-property 0 6 'face '(foreground-color . "firebrick") msg)
      msg)))

(defvar chicken-test-fail-message-with-reason
  (progn
    (let ((msg "Failed: '%s'"))
      (put-text-property 0 6 'face '(foreground-color . "firebrick") msg)
      msg)))

(defvar chicken-test-not-found-message "No test file found in current project.")

(defun* chicken-test-project-root (&optional (buffer (current-buffer)))
  "Determine the root of the current chicken project"
  (interactive)
  (let ((file (buffer-file-name buffer)))
    (file-name-directory file)))

(defun chicken-test-run-test (command options file buffer)
  (message "Running tests ( %s ) " file)
  (let ((root (chicken-test-project-root)))
    (display-buffer buffer)
    (setq chicken-test-last-run file)
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(set-auto-mode-0 'chicken-test-mode nil)
	(let ((args (append (list command) options)))
	  (let ((directory root)
		(previous-directory default-directory))
	    (and directory (cd directory))
	    (let ((proc (apply 'start-process-shell-command "chicken-test" buffer args)))
	      (set-process-sentinel proc 'chicken-test-runner-sentinel))
	    (and directory (cd previous-directory))))))))

(defun chicken-test-runner-sentinel (process event)
  (save-excursion
    (set-buffer chicken-test-buffer)
    (cond
     ((string= "finished\n" event)
      (message chicken-test-ok-message))
     ((string= "exited abnormally with code 1\n" event)
      (message chicken-test-fail-message))
     (t
      (string-match "\\(.*\\)[^\n]" event)
      (message chicken-test-fail-message-with-reason (match-string 1 event))))))

(defun chicken-test-run-test-file (command file output-buffer)
  (chicken-test-run-test command (list "-s" file) file output-buffer))

(defun chicken-test-full-path (filename)
  (concat (chicken-test-project-root) filename))

(defun chicken-test-ask-for-file ()
  (read-file-name "Path to test-file:"))

(defun chicken-test-find-test-file ()
  (interactive)
  (let ((candidate (chicken-test-full-path "tests/run.scm")))
    (if (file-exists-p candidate)
	(setq chicken-test-last-run candidate)
      (setq chicken-test-last-run (chicken-test-ask-for-file)))
    chicken-test-last-run))

(defun ask-user (prompt)
  (interactive)
  (read-string prompt))

(defun chicken-test-run-file ()
  "Runs the test file"
  (interactive)
  (setq chicken-test-buffer (get-buffer-create chicken-test-buffer-name))
  (let ((test-file (chicken-test-find-test-file)))
    (if test-file
	(chicken-test-run-test-file "csi" test-file chicken-test-buffer)
      (message chicken-test-not-found-message))))

;; TODO: remember last filter
(defun chicken-test-run-file-with-filter ()
  "Runs the test file with the filter that is retrieved from the user"
  (interactive)
  (setq chicken-test-buffer (get-buffer-create chicken-test-buffer-name))
  (let ((test-file (chicken-test-find-test-file))
	(filter    (ask-user "Filter: ")))
    (if test-file
	(chicken-test-run-test-file (concat (concat "TEST_FILTER=" filter) " csi") test-file chicken-test-buffer)
      (message chicken-test-not-found-message))))

(defun chicken-test-switch-to-test-buffer ()
  (interactive)
  (switch-to-buffer chicken-test-buffer-name))

(defface chicken-test-pass-face
  '((t (:foreground "pale green")))
  "Face to highlight passing tests")

(set-face-foreground 'chicken-test-pass-face "green")

(defface chicken-test-fail-face
  '((t (:background "firebrick")))
  "Face to highlight failing tests")

(set-face-foreground 'chicken-test-fail-face "firebrick")

(defun chicken-test-font-locks ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\[ \\(PASS\\)\\]" 1 'chicken-test-pass-face prepend)) t)
  (font-lock-add-keywords
   nil
   '(("\\[\\(ERROR\\)\\]\\|\\[ \\(FAIL\\)\\]" 1 'chicken-test-fail-face prepend)) t))

;; navigation in the test output buffer
(defvar chicken-test-current-failure-line nil)

(defun chicken-test-highlight-current-line ()
  "Highlights the current line using hl-line-mode"
  (interactive)
  (unless global-hl-line-mode
    (global-hl-line-mode 1)
    (global-hl-line-highlight)
    (add-hook 'pre-command-hook 'chicken-test-unhighlight-current-line)))

(defun chicken-test-unhighlight-current-line ()
  "Unhighlights the current line using hl-line-mode"
  (interactive)
  (global-hl-line-mode -1)
  (global-hl-line-unhighlight)
  (remove-hook 'pre-command-hook 'chicken-test-unhgihglight-current-line))

(defun chicken-test-find-failure (search offset)
  "Finds the line of the next failung test using the supplied search strategy which should be either re-search-forward or re-search-backward"
  (save-excursion
    (if (not chicken-test-current-failure-line)
	(progn
	  (setq chicken-test-current-failure-line 1)
	  (beginning-of-buffer))
      (forward-line offset))
    (let ((point (funcall search "\\[ FAIL\\]\\|\\[ERROR\\]" nil t)))
      (if point
	  (let ((line (count-lines 1 point)))
	    (setq chicken-test-current-failure-line line)
	    line)))))

(defun chicken-test-find-next-failure ()
  "Finds the line of the next failing test"
  (chicken-test-find-failure 're-search-forward 1))

(defun chicken-test-find-previous-failure ()
  "Finds the line of the previous failing test"
  (chicken-test-find-failure 're-search-backward -1))

(defun chicken-test-goto-failure (finder)
  "Moves to the next failing test if there is any using the supplied finder"
  (interactive)
  (chicken-test-unhighlight-current-line)
  (let ((failure-line (funcall finder)))
    (if failure-line
	(progn
	  (goto-line failure-line)
	  (chicken-test-highlight-current-line))
      (message "No more failures"))))

(defun chicken-test-goto-next-failure ()
  "Moves to the next failing test"
  (interactive)
  (chicken-test-goto-failure 'chicken-test-find-next-failure))

(defun chicken-test-goto-previous-failure ()
  "Moves to the previous failing test"
  (interactive)
  (chicken-test-goto-failure 'chicken-test-find-previous-failure))

(defun chicken-test-goto-first-failure ()
  "Moves to the first failing test"
  (interactive)
  (setq chicken-test-current-failure-line nil)
  (chicken-test-goto-next-failure))

(defun chicken-test-goto-last-failure ()
  "Moves to the last failing test"
  (interactive)
  (setq chicken-test-current-failure-line nil)
  (chicken-test-goto-previous-failure))

(defun chicken-test-find-test (search offset)
  "Returns the line of the next test using the supplied search strategy"
  (save-excursion
    (if offset
	(forward-line offset))
    (let ((point (funcall search "\\[ FAIL\\]\\|\\[ERROR\\]\\|\\[ PASS\\]" nil t)))
      (if point
	  (count-lines 1 point)))))

(defun chicken-test-goto-test (line-finder)
  (chicken-test-unhighlight-current-line)
  (let ((line (funcall line-finder)))
    (if line
	(progn
	  (goto-line line)
	  (chicken-test-highlight-current-line))
      (message "No more tests"))))

(defun chicken-test-goto-next-test ()
  "Moves to the next test"
  (interactive)
  (chicken-test-goto-test (lambda () (chicken-test-find-test 're-search-forward 1))))

(defun chicken-test-goto-previous-test ()
  "Moves to the previous test"
  (interactive)
  (chicken-test-goto-test (lambda () (chicken-test-find-test 're-search-backward nil))))

(defvar chicken-test-mode-map nil)
(setq chicken-test-mode-map (make-sparse-keymap))
(define-key chicken-test-mode-map (kbd "q") 'quit-window)
(define-key chicken-test-mode-map (kbd "f") 'chicken-test-goto-first-failure)
(define-key chicken-test-mode-map (kbd "l") 'chicken-test-goto-last-failure)
(define-key chicken-test-mode-map (kbd "n") 'chicken-test-goto-next-failure)
(define-key chicken-test-mode-map (kbd "N") 'chicken-test-goto-next-test)
(define-key chicken-test-mode-map (kbd "p") 'chicken-test-goto-previous-failure)
(define-key chicken-test-mode-map (kbd "P") 'chicken-test-goto-previous-test)

(defun chicken-test-mode ()
  (interactive)
  (kill-all-local-variables)
  (chicken-test-font-locks)
  (use-local-map chicken-test-mode-map)
  (make-local-variable 'view-read-only)
  (setq major-mode 'chicken-test-mode)
  (setq mode-name "CHICKEN-Test")
  (run-hooks 'chicken-test-mode-hook))

(provide 'chicken-test)
