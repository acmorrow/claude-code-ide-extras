;;; claude-code-ide-extras-testing.el --- Testing MCP tools for claude-code-ide  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andrew Morrow

;; Author: Andrew Morrow <andrew.c.morrow@gmail.com>
;;         Tim Ransom
;; Keywords: tools, testing, ai, claude, mcp
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides testing MCP (Model Context Protocol) tools
;; for claude-code-ide.el, enabling Claude to:
;;
;; - Discover test functions in files or currently loaded
;; - Run individual tests and inspect results
;; - Run batches of tests matching a pattern with summary reporting
;;
;; Supports both ERT (built-in) and Buttercup (BDD-style) test frameworks.
;; ERT is always available; Buttercup is used when loaded.
;;
;; Part of the claude-code-ide-extras suite.
;;
;; Installation:
;;
;;   (require 'claude-code-ide-extras-testing)
;;   (claude-code-ide-extras-testing-setup)

;;; Code:

(require 'claude-code-ide)
(require 'ert)
(require 'buttercup nil t)

(declare-function ert--print-backtrace "ert" (backtrace))

;; Buttercup declarations
(declare-function buttercup-suite-description "buttercup" (suite))
(declare-function buttercup-suite-children "buttercup" (suite))
(declare-function buttercup-suite-status "buttercup" (suite))
(declare-function buttercup-spec-description "buttercup" (spec))
(declare-function buttercup-spec-status "buttercup" (spec))
(declare-function buttercup-spec-failure-description "buttercup" (spec))
(declare-function buttercup-spec-p "buttercup" (obj))
(declare-function buttercup-suite-p "buttercup" (obj))
(declare-function buttercup-run "buttercup" ())
(defvar buttercup-suites)

(defgroup claude-code-ide-extras-testing nil
  "Testing MCP tools for claude-code-ide."
  :group 'tools
  :group 'claude-code-ide
  :prefix "claude-code-ide-extras-testing-")

(defconst claude-code-ide-extras-testing-version "0.0.4"
  "Version of claude-code-ide-extras-testing.")

;;; MCP Tool Names

(defconst claude-code-ide-extras-testing-list-tests-tool-name
  "claude-code-ide-extras-testing/list_tests"
  "MCP tool name for list_tests.")

(defconst claude-code-ide-extras-testing-run-test-tool-name
  "claude-code-ide-extras-testing/run_test"
  "MCP tool name for run_test.")

(defconst claude-code-ide-extras-testing-run-tests-tool-name
  "claude-code-ide-extras-testing/run_tests"
  "MCP tool name for run_tests.")

(defconst claude-code-ide-extras-testing-run-buttercup-spec-tool-name
  "claude-code-ide-extras-testing/run_buttercup_spec"
  "MCP tool name for run_buttercup_spec.")

;;; Customization

(defcustom claude-code-ide-extras-testing-list-tests-usage-prompt
  "Discover test functions (ERT or Buttercup). Optionally load a file first to register its tests, and filter by regex pattern."
  "Usage guidance for the list_tests MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-testing)

(put 'claude-code-ide-extras-testing-list-tests-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-testing-list-tests-tool-name)

(defcustom claude-code-ide-extras-testing-run-test-usage-prompt
  "Run a single ERT test by name and get pass/fail status with failure details."
  "Usage guidance for the run_test MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-testing)

(put 'claude-code-ide-extras-testing-run-test-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-testing-run-test-tool-name)

(defcustom claude-code-ide-extras-testing-run-tests-usage-prompt
  "Run multiple tests matching a pattern. Supports both ERT and Buttercup frameworks. Returns summary with pass/fail/skip counts and failure details."
  "Usage guidance for the run_tests MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-testing)

(put 'claude-code-ide-extras-testing-run-tests-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-testing-run-tests-tool-name)

(defcustom claude-code-ide-extras-testing-run-buttercup-spec-usage-prompt
  "Run a single Buttercup spec by name (full description path). Returns pass/fail/pending status with failure details. Requires buttercup to be loaded."
  "Usage guidance for the run_buttercup_spec MCP tool."
  :type 'string
  :group 'claude-code-ide-extras-testing)

(put 'claude-code-ide-extras-testing-run-buttercup-spec-usage-prompt
     'claude-code-ide-extras-mcp-tool-name
     claude-code-ide-extras-testing-run-buttercup-spec-tool-name)

;;; Internal helpers - ERT

(defun claude-code-ide-extras-testing--format-test-entry (test-name)
  "Format a single ERT test entry for TEST-NAME.
Returns a string with the test name and its docstring if available."
  (let* ((test (ert-get-test test-name))
         (doc (when test (ert-test-documentation test))))
    (if doc
        (format "  %s - %s" test-name doc)
      (format "  %s" test-name))))

(defun claude-code-ide-extras-testing--format-result (test-name result)
  "Format a single ERT test RESULT for TEST-NAME.
Returns a string with status and failure details if applicable."
  (cond
   ((ert-test-passed-p result)
    (format "  PASSED: %s" test-name))
   ((ert-test-result-expected-p (ert-get-test test-name) result)
    (format "  PASSED (expected): %s" test-name))
   ((ert-test-skipped-p result)
    (format "  SKIPPED: %s" test-name))
   ((ert-test-failed-p result)
    (let ((condition (ert-test-result-with-condition-condition result)))
      (format "  FAILED: %s\n    Condition: %S" test-name condition)))
   (t
    (format "  UNKNOWN: %s - %S" test-name result))))

(defun claude-code-ide-extras-testing--select-tests (pattern)
  "Select ERT tests matching PATTERN.
PATTERN can be a string regex, or nil/\"t\" for all tests.
Returns a list of test name symbols."
  (let ((selector (cond
                   ((or (null pattern) (string= pattern "t")) t)
                   (t pattern))))
    (mapcar #'ert-test-name
            (ert-select-tests selector t))))

;;; Internal helpers - Buttercup

(defun claude-code-ide-extras-testing--buttercup-available-p ()
  "Return non-nil if buttercup is loaded and has suites defined."
  (and (featurep 'buttercup)
       (bound-and-true-p buttercup-suites)
       (not (null buttercup-suites))))

(defun claude-code-ide-extras-testing--buttercup-collect-specs (suite &optional prefix)
  "Collect all specs from SUITE recursively.
PREFIX is the parent suite description for building full spec names.
Returns a list of (full-name . spec) cons cells."
  (let* ((desc (buttercup-suite-description suite))
         (full-prefix (if prefix
                         (concat prefix " " desc)
                       desc))
         (results '()))
    (dolist (child (buttercup-suite-children suite))
      (if (buttercup-spec-p child)
          (push (cons (concat full-prefix " " (buttercup-spec-description child))
                      child)
                results)
        (when (buttercup-suite-p child)
          (setq results (append (claude-code-ide-extras-testing--buttercup-collect-specs
                                 child full-prefix)
                                results)))))
    (nreverse results)))

(defun claude-code-ide-extras-testing--buttercup-list-all-specs ()
  "Collect all buttercup specs from all suites.
Returns a list of (full-name . spec) cons cells."
  (let ((all-specs '()))
    (dolist (suite buttercup-suites)
      (setq all-specs (append all-specs
                              (claude-code-ide-extras-testing--buttercup-collect-specs suite))))
    all-specs))

(defun claude-code-ide-extras-testing--buttercup-format-spec (name-spec-pair)
  "Format a buttercup spec from NAME-SPEC-PAIR for display.
NAME-SPEC-PAIR is a (full-name . spec) cons cell."
  (format "  %s" (car name-spec-pair)))

(defun claude-code-ide-extras-testing--buttercup-format-result (name-spec-pair)
  "Format a buttercup spec result from NAME-SPEC-PAIR.
NAME-SPEC-PAIR is a (full-name . spec) cons cell."
  (let* ((name (car name-spec-pair))
         (spec (cdr name-spec-pair))
         (status (buttercup-spec-status spec)))
    (cond
     ((eq status 'passed)
      (format "  PASSED: %s" name))
     ((eq status 'failed)
      (let ((failure-desc (buttercup-spec-failure-description spec)))
        (format "  FAILED: %s\n    %s" name (or failure-desc "No details"))))
     ((eq status 'pending)
      (format "  PENDING: %s" name))
     (t
      (format "  %s: %s" (upcase (symbol-name status)) name)))))

(defun claude-code-ide-extras-testing--buttercup-find-spec (name)
  "Find a buttercup spec by its full NAME description.
Returns the (name . spec) cons cell or nil."
  (let ((all-specs (claude-code-ide-extras-testing--buttercup-list-all-specs)))
    (seq-find (lambda (pair) (string= (car pair) name)) all-specs)))

;;; Tool implementations

(defun claude-code-ide-extras-testing--list-tests (&optional file-path pattern)
  "List tests, optionally from FILE-PATH, filtered by PATTERN.
If FILE-PATH is provided, load it first to register its tests.
PATTERN is a regex to filter test names; nil or \"t\" means all tests.
Discovers both ERT and Buttercup tests."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (progn
          (when (and file-path (not (string-empty-p file-path)))
            (unless (file-exists-p file-path)
              (error "File not found: %s" file-path))
            (load-file file-path))
          (let ((ert-tests (claude-code-ide-extras-testing--select-tests pattern))
                (buttercup-specs (when (claude-code-ide-extras-testing--buttercup-available-p)
                                   (let ((all-specs (claude-code-ide-extras-testing--buttercup-list-all-specs)))
                                     (if (and pattern (not (string= pattern "t")))
                                         (seq-filter (lambda (pair)
                                                       (string-match-p pattern (car pair)))
                                                     all-specs)
                                       all-specs))))
                (sections '()))
            (when ert-tests
              (push (format "ERT Tests (%d):\n\n%s"
                            (length ert-tests)
                            (mapconcat #'claude-code-ide-extras-testing--format-test-entry
                                       ert-tests "\n"))
                    sections))
            (when buttercup-specs
              (push (format "Buttercup Specs (%d):\n\n%s"
                            (length buttercup-specs)
                            (mapconcat #'claude-code-ide-extras-testing--buttercup-format-spec
                                       buttercup-specs "\n"))
                    sections))
            (if sections
                (mapconcat #'identity (nreverse sections) "\n\n")
              (format "No tests found%s%s."
                      (if (and file-path (not (string-empty-p file-path)))
                          (format " in %s" file-path)
                        "")
                      (if (and pattern (not (string= pattern "t")))
                          (format " matching \"%s\"" pattern)
                        "")))))
      (error (format "Error listing tests: %s" (error-message-string err))))))

(defun claude-code-ide-extras-testing--run-test (test-name)
  "Run a single ERT test by TEST-NAME and return the result.
TEST-NAME is a string naming the test to run."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (let ((test-sym (intern test-name)))
          (unless (ert-test-boundp test-sym)
            (error "Test not found: %s" test-name))
          (let* ((test (ert-get-test test-sym))
                 (result (ert-run-test test)))
            (cond
             ((ert-test-passed-p result)
              (format "PASSED: %s" test-name))
             ((ert-test-skipped-p result)
              (let ((condition (ert-test-result-with-condition-condition result)))
                (format "SKIPPED: %s\n\nReason: %S" test-name condition)))
             ((ert-test-failed-p result)
              (let ((condition (ert-test-result-with-condition-condition result))
                    (backtrace (with-temp-buffer
                                 (ert--print-backtrace
                                  (ert-test-result-with-condition-backtrace result))
                                 (buffer-string))))
                (format "FAILED: %s\n\nCondition: %S\n\nBacktrace:\n%s"
                        test-name condition backtrace)))
             (t
              (format "UNKNOWN RESULT: %s - %S" test-name result)))))
      (error (format "Error running test: %s" (error-message-string err))))))

(defun claude-code-ide-extras-testing--run-tests (&optional pattern file-path)
  "Run tests matching PATTERN and return a summary.
PATTERN is a regex string; nil or \"t\" runs all tests.
If FILE-PATH is provided, load it first to register its tests.
Runs both ERT and Buttercup tests when available."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (progn
          (when (and file-path (not (string-empty-p file-path)))
            (unless (file-exists-p file-path)
              (error "File not found: %s" file-path))
            (load-file file-path))
          (let ((ert-section nil)
                (buttercup-section nil))
            ;; Run ERT tests
            (let ((test-names (claude-code-ide-extras-testing--select-tests pattern)))
              (when test-names
                (let ((passed 0)
                      (failed 0)
                      (skipped 0)
                      (failure-details '()))
                  (dolist (test-name test-names)
                    (let* ((test (ert-get-test test-name))
                           (result (ert-run-test test)))
                      (cond
                       ((ert-test-passed-p result)
                        (setq passed (1+ passed)))
                       ((ert-test-skipped-p result)
                        (setq skipped (1+ skipped)))
                       ((ert-test-failed-p result)
                        (setq failed (1+ failed))
                        (let ((condition (ert-test-result-with-condition-condition result)))
                          (push (format "  FAILED: %s\n    Condition: %S" test-name condition)
                                failure-details)))
                       (t
                        (setq failed (1+ failed))
                        (push (format "  FAILED: %s\n    Result: %S" test-name result)
                              failure-details)))))
                  (setq ert-section
                        (concat (format "ERT Results:\n  Total:   %d\n  Passed:  %d\n  Failed:  %d\n  Skipped: %d"
                                        (length test-names) passed failed skipped)
                                (when failure-details
                                  (concat "\n\n  Failures:\n\n"
                                          (mapconcat #'identity (nreverse failure-details) "\n\n"))))))))
            ;; Run Buttercup tests
            (when (claude-code-ide-extras-testing--buttercup-available-p)
              (let ((specs (claude-code-ide-extras-testing--buttercup-list-all-specs)))
                (when specs
                  (let ((filtered (if (and pattern (not (string= pattern "t")))
                                      (seq-filter (lambda (pair)
                                                    (string-match-p pattern (car pair)))
                                                  specs)
                                    specs)))
                    (when filtered
                      (buttercup-run)
                      (let ((passed 0) (failed 0) (pending 0)
                            (failure-details '()))
                        (dolist (pair filtered)
                          (let ((status (buttercup-spec-status (cdr pair))))
                            (cond
                             ((eq status 'passed) (setq passed (1+ passed)))
                             ((eq status 'failed)
                              (setq failed (1+ failed))
                              (push (format "  FAILED: %s\n    %s"
                                            (car pair)
                                            (or (buttercup-spec-failure-description (cdr pair))
                                                "No details"))
                                    failure-details))
                             ((eq status 'pending) (setq pending (1+ pending))))))
                        (setq buttercup-section
                              (concat (format "Buttercup Results:\n  Total:   %d\n  Passed:  %d\n  Failed:  %d\n  Pending: %d"
                                              (length filtered) passed failed pending)
                                      (when failure-details
                                        (concat "\n\n  Failures:\n\n"
                                                (mapconcat #'identity (nreverse failure-details) "\n\n")))))))))))
            ;; Combine results
            (cond
             ((and ert-section buttercup-section)
              (concat ert-section "\n\n" buttercup-section))
             (ert-section ert-section)
             (buttercup-section buttercup-section)
             (t (format "No tests found%s."
                        (if (and pattern (not (string= pattern "t")))
                            (format " matching \"%s\"" pattern)
                          ""))))))
      (error (format "Error running tests: %s" (error-message-string err))))))

(defun claude-code-ide-extras-testing--run-buttercup-spec (spec-name)
  "Run a single Buttercup spec by SPEC-NAME.
SPEC-NAME is the full description path (e.g. \"A suite contains a spec\")."
  (claude-code-ide-mcp-server-with-session-context nil
    (condition-case err
        (progn
          (unless (featurep 'buttercup)
            (error "Buttercup is not loaded"))
          (unless (claude-code-ide-extras-testing--buttercup-available-p)
            (error "No buttercup suites defined"))
          (let ((found (claude-code-ide-extras-testing--buttercup-find-spec spec-name)))
            (unless found
              (error "Buttercup spec not found: %s" spec-name))
            (let ((spec (cdr found)))
              ;; Run just the suites (buttercup-run runs all suites)
              (buttercup-run)
              (let ((status (buttercup-spec-status spec)))
                (cond
                 ((eq status 'passed)
                  (format "PASSED: %s" spec-name))
                 ((eq status 'failed)
                  (let ((failure-desc (buttercup-spec-failure-description spec)))
                    (format "FAILED: %s\n\nFailure:\n%s"
                            spec-name (or failure-desc "No details"))))
                 ((eq status 'pending)
                  (format "PENDING: %s" spec-name))
                 (t
                  (format "%s: %s" (upcase (symbol-name status)) spec-name)))))))
      (error (format "Error running buttercup spec: %s" (error-message-string err))))))

;;; Tool registration

;;;###autoload
(defun claude-code-ide-extras-testing-setup ()
  "Register all testing MCP tools with claude-code-ide."
  (interactive)

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-testing--list-tests
   :name claude-code-ide-extras-testing-list-tests-tool-name
   :description "List test functions (ERT and Buttercup). Optionally load a file first to register its tests, and filter by regex pattern. Returns test names with docstrings."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to a file to load before listing tests. If given, the file will be loaded to register any tests it defines."
            :optional t)
           (:name "pattern"
            :type string
            :description "Regex pattern to filter test names. Defaults to all tests if omitted."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-testing--run-test
   :name claude-code-ide-extras-testing-run-test-tool-name
   :description "Run a single ERT test by name. Returns pass/fail status, failure condition, and backtrace if the test failed."
   :args '((:name "test_name"
            :type string
            :description "The name of the ERT test to run.")))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-testing--run-tests
   :name claude-code-ide-extras-testing-run-tests-tool-name
   :description "Run multiple tests matching a pattern. Supports both ERT and Buttercup frameworks. Returns a summary with passed/failed/skipped counts and details of any failures."
   :args '((:name "pattern"
            :type string
            :description "Regex pattern to select tests. Defaults to all tests if omitted."
            :optional t)
           (:name "file_path"
            :type string
            :description "Absolute path to a file to load before running tests. If given, the file will be loaded to register any tests it defines."
            :optional t)))

  (claude-code-ide-make-tool
   :function #'claude-code-ide-extras-testing--run-buttercup-spec
   :name claude-code-ide-extras-testing-run-buttercup-spec-tool-name
   :description "Run a single Buttercup spec by its full description path. Returns pass/fail/pending status with failure details. Requires buttercup to be loaded with suites defined."
   :args '((:name "spec_name"
            :type string
            :description "The full description path of the spec (e.g. \"A suite contains a passing spec\").")))

  (message "Claude Code IDE Extras: testing tools registered"))

(provide 'claude-code-ide-extras-testing)
;;; claude-code-ide-extras-testing.el ends here
