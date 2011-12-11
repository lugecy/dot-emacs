(require 'one-key-default)
(require 'one-key-config)
;; (one-key-default-setup-keys)
(setq one-key-items-per-line 2)

;;;; for Org-mode
(ly:eval-after-load 'org
  (org-defkey org-mode-map (kbd "C-c C-x") 'one-key-menu-Org-C-c-C-x-Keymap))
(defvar one-key-menu-Org-C-c-C-x-Keymap-alist nil
  "The `one-key' menu alist for Org C-c C-x Keymap.")
(setq one-key-menu-Org-C-c-C-x-Keymap-alist
      '(
        (("C-a" . "Org Toggle Archive Tag") . org-toggle-archive-tag)
        (("C-b" . "Org Toggle Checkbox") . org-toggle-checkbox)
        (("C-c" . "Org Columns") . org-columns)
        (("C-d" . "Org Clock Display") . org-clock-display)
        (("C-f" . "Org Emphasize") . org-emphasize)
        (("TAB" . "Org Clock In") . org-clock-in)
        (("C-j" . "Org Clock Goto") . org-clock-goto)
        (("C-k" . "Org Mark Entry For Agenda Action") . org-mark-entry-for-agenda-action)
        (("C-l" . "Org Preview Latex Fragment") . org-preview-latex-fragment)
        (("C-n" . "Org Next Link") . org-next-link)
        (("C-o" . "Org Clock Out") . org-clock-out)
        (("C-p" . "Org Previous Link") . org-previous-link)
        (("C-r" . "Org Clock Report") . org-clock-report)
        (("C-s" . "Org Advertized Archive Subtree") . org-advertized-archive-subtree)
        (("C-t" . "Org Toggle Time Stamp Overlays") . org-toggle-time-stamp-overlays)
        (("C-u" . "Org Dblock Update") . org-dblock-update)
        (("C-w" . "Org Cut Special") . org-cut-special)
        (("C-x" . "Org Clock Cancel") . org-clock-cancel)
        (("C-y" . "Org Paste Special") . org-paste-special)
        ;; (("ESC" . "Prefix Command") . Prefix Command)
        (("," . "Org Timer Pause Or Continue") . org-timer-pause-or-continue)
        (("-" . "Org Timer Item") . org-timer-item)
        (("." . "Org Timer") . org-timer)
        (("0" . "Org Timer Start") . org-timer-start)
        (("<" . "Org Agenda Set Restriction Lock") . org-agenda-set-restriction-lock)
        ((">" . "Org Agenda Remove Restriction Lock") . org-agenda-remove-restriction-lock)
        (("A" . "Org Archive To Archive Sibling") . org-archive-to-archive-sibling)
        (("a" . "Org Toggle Archive Tag") . org-toggle-archive-tag)
        (("b" . "Org Tree To Indirect Buffer") . org-tree-to-indirect-buffer)
        (("f" . "Org Footnote Action") . org-footnote-action)
        (("i" . "Org Insert Columns Dblock") . org-insert-columns-dblock)
        (("o" . "Org Toggle Ordered Property") . org-toggle-ordered-property)
        (("p" . "Org Set Property") . org-set-property)

        (("M-w" . "Org Copy Special") . org-copy-special)
        ))
(defun one-key-menu-Org-C-c-C-x-Keymap ()
  "The `one-key' menu for Org C-c C-x Keymap"
  (interactive)
  (one-key-menu "Org C-c C-x Keymap" one-key-menu-Org-C-c-C-x-Keymap-alist))

(defvar one-key-menu-Org-C-c-Keymap-alist nil
  "The `one-key' menu alist for Org C-c Keymap.")
(setq one-key-menu-Org-C-c-Keymap-alist
      '(
        (("C-a" . "Org Attach") . org-attach)
        (("C-b" . "Outline Backward Same Level") . outline-backward-same-level)
        (("C-c" . "Org Ctrl C Ctrl C") . org-ctrl-c-ctrl-c)
        (("C-d" . "Org Deadline") . org-deadline)
        (("C-e" . "Org Export") . org-export)
        (("C-f" . "Outline Forward Same Level") . outline-forward-same-level)
        (("TAB" . "Show Children") . show-children)
        (("C-j" . "Org Goto") . org-goto)
        (("C-k" . "Org Kill Note Or Show Branches") . org-kill-note-or-show-branches)
        (("C-l" . "Org Insert Link") . org-insert-link)
        (("RET" . "Org Ctrl C Ret") . org-ctrl-c-ret)
        (("C-n" . "Outline Next Visible Heading") . outline-next-visible-heading)
        (("C-o" . "Org Open At Point") . org-open-at-point)
        (("C-p" . "Outline Previous Visible Heading") . outline-previous-visible-heading)
        (("C-q" . "Org Set Tags Command") . org-set-tags-command)
        (("C-r" . "Org Reveal") . org-reveal)
        (("C-s" . "Org Schedule") . org-schedule)
        (("C-t" . "Org Todo") . org-todo)
        (("C-u" . "Outline Up Heading") . outline-up-heading)
        (("C-v" . "Org Show Todo Tree") . org-show-todo-tree)
        (("C-w" . "Org Refile") . org-refile)
        (("C-y" . "Org Evaluate Time Range") . org-evaluate-time-range)
        (("C-z" . "Org Add Note") . org-add-note)
        (("C-^" . "Outline Move Subtree Up") . outline-move-subtree-up)
        (("SPC" . "Org Table Blank Field") . org-table-blank-field)
        (("!" . "Org Time Stamp Inactive") . org-time-stamp-inactive)
        (("#" . "Org Update Checkbox Count") . org-update-checkbox-count)
        (("$" . "Org Archive Subtree") . org-archive-subtree)
        (("%" . "Org Mark Ring Push") . org-mark-ring-push)
        (("&" . "Org Mark Ring Goto") . org-mark-ring-goto)
        (("'" . "Org Edit Special") . org-edit-special)
        (("*" . "Org Ctrl C Star") . org-ctrl-c-star)
        (("+" . "Org Table Sum") . org-table-sum)
        (("," . "Org Priority") . org-priority)
        (("-" . "Org Ctrl C Minus") . org-ctrl-c-minus)
        (("." . "Org Time Stamp") . org-time-stamp)
        (("/" . "Org Sparse Tree") . org-sparse-tree)
        ((":" . "Org Toggle Fixed Width Section") . org-toggle-fixed-width-section)
        ((";" . "Org Toggle Comment") . org-toggle-comment)
        (("<" . "Org Date From Calendar") . org-date-from-calendar)
        (("=" . "Org Table Eval Formula") . org-table-eval-formula)
        ((">" . "Org Goto Calendar") . org-goto-calendar)
        (("?" . "Org Table Field Info") . org-table-field-info)
        (("@" . "Outline Mark Subtree") . outline-mark-subtree)
        (("[" . "Org Agenda File To Front") . org-agenda-file-to-front)
        (("\\" . "Org Tags Sparse Tree") . org-tags-sparse-tree)
        (("]" . "Org Remove File") . org-remove-file)
        (("^" . "Org Sort") . org-sort)
        (("`" . "Org Table Edit Field") . org-table-edit-field)
        (("{" . "Org Table Toggle Formula Debugger") . org-table-toggle-formula-debugger)
        (("|" . "Org Table Create Or Convert From Region") . org-table-create-or-convert-from-region)
        (("}" . "Org Table Toggle Coordinate Overlays") . org-table-toggle-coordinate-overlays)
        (("~" . "Org Table Create With Table.El") . org-table-create-with-table.el)
        (("C-<" . "Outline Promote") . outline-promote)
        (("C->" . "Outline Demote") . outline-demote)
        (("C-x" . "Prefix Command") . one-key-menu-Org-C-c-C-x-Keymap)
        ))
(defun one-key-menu-Org-C-c-Keymap ()
  "The `one-key' menu for Org C-c Keymap"
  (interactive)
  (one-key-menu "Org C-c Keymap" one-key-menu-Org-C-c-Keymap-alist))

;;;; for paredit-mode
(setq one-key-menu-paredit-alist
      '(
        (("M-r" . "Raise Sexp") . paredit-raise-sexp)
        ;; Wrap.
        (("h" . "Wrap Left Object") . paredit-backward-slurp-sexp)
        (("l" . "Wrap Right Object") . paredit-forward-slurp-sexp)
        ;; Free.
        (("," . "Free Left Object") . paredit-backward-barf-sexp)
        (("." . "Free Right Object") . paredit-forward-barf-sexp)
        ;; Remove.
        (("<" . "Remove Paren And Left Object") . paredit-splice-sexp-killing-backward)
        ((">" . "Remove Paren And Right Object") . paredit-splice-sexp-killing-forward)
        (("u" . "Remove Paren And Left Object") . paredit-splice-sexp-killing-backward)
        (("d" . "Remove Paren And Right Object") . paredit-splice-sexp-killing-forward)
        ;; Join or Split.
        (("j" . "Join Sexps") . paredit-join-sexps)
        (("k" . "Split Sexps") . paredit-split-sexp)
        (("n" . "Join next list") . paredit-join-with-next-list)
        (("p" . "Join previous list") . paredit-join-with-previous-list)
        (("N" . "Add next list") . paredit-add-to-next-list)
        (("P" . "Add previous list") . paredit-add-to-previous-list)
        ))
(define-key paredit-mode-map (kbd "M-r") 'one-key-menu-paredit)

;;;; replace one-key-menu
(ly:eval-after-load "popup"
  (defadvice one-key-menu (around use-popup activate)
    (let* ((top-height (count-lines (window-start) (point)))
           (buttom-height (- (max 1 (- (window-height)
                                       (if mode-line-format 1 0)
                                       (if header-line-format 1 0)))
                             top-height)))
      (if (and (null current-prefix-arg)
               (> (max top-height buttom-height)
                  (1+ (length (ad-get-arg 1)))))
          (apply 'one-key-menu-popup (ad-get-args 0))
        ad-do-it)))

  (defvar one-key-help-tip nil)
  (defun one-key-help-tip-close ()
    (and (popup-live-p one-key-help-tip)
         (popup-delete one-key-help-tip)))

  (defun one-key-help-tip-open (title alist)
    "Function document"
    (let* ((help-msg (concat title "\n" (one-key-help-tip-format alist)))
           (tip-height (with-temp-buffer
                         (insert help-msg)
                         (count-lines (point-min) (point-max)))))
      (setq one-key-help-tip (popup-tip help-msg :nowait t :height tip-height))))

  (defun one-key-help-tip-toggle (title alist)
    "Function document"
    (if (popup-live-p one-key-help-tip)
        (one-key-help-tip-close)
      (one-key-help-tip-open title alist)))

  (defun one-key-help-tip-format (info-alist)
    "Function document"
    (mapconcat (lambda (item)
                 (destructuring-bind ((key . desc) . cmd) item
                   (format "[%s]: %s" key desc)))
               info-alist "\n"))

  (defun one-key-menu-popup (title
                             info-alist
                             &optional
                             miss-match-exit-p
                             recursion-p
                             protect-function
                             alternate-function
                             execute-last-command-when-miss-match)
    "One key menu feature popup-tip.

`TITLE' is the title of men, any string can use.
`INFO-ALIST' is a special alist
that contain KEY, DESCRIBE and COMMAND.
`MISS-MATCH-EXIT-P' whether hide popup help window
when keystroke can't match in menu.
`RECURSION-P' whether recursion execute self
when keystroke can't match in menu.
`PROTECT-FUNCTION' the protect function
that call in `unwind-protect'.
`ALTERNATE-FUNCTION' the alternate function execute at last.
`EXECUTE-LAST-COMMAND-WHEN-MISS-MATCH' whether execute
last command when it miss match in key alist."
    (let ((self (function
                 (lambda ()
                   (one-key-menu
                    title info-alist miss-match-exit-p
                    recursion-p
                    protect-function
                    alternate-function
                    execute-last-command-when-miss-match))))
          last-key)
      ;; Popup help window when first time call
      ;; and option `one-key-popup-window' is `non-nil'.
      (when (and one-key-menu-call-first-time
                 one-key-popup-window)
        (one-key-help-tip-toggle title info-alist))
      ;; Execute.
      (unwind-protect
          (let* ((event (read-event
                         ;; Just show help message when first call,
                         ;; don't overwritten message from command.
                         (if one-key-menu-call-first-time
                             (progn
                               (one-key-highlight-prompt title)
                               (setq one-key-menu-call-first-time nil))
                           "")))
                 (event (if (eq event meta-prefix-char)
                            (+ (expt 2 27) (read-event)) ;for meta modify char
                          event))
                 (key (if (if (<= emacs-major-version 22)
                              (with-no-warnings
                                (char-valid-p event)) ;for compatibility Emacs 22
                            (characterp event))
                          ;; Transform to string when event is char.
                          (char-to-string event)
                        ;; Otherwise return vector.
                        (make-vector 1 event)))
                 match-key)
            (cond
             ;; Match user keystrokes.
             ((catch 'match
                (loop for ((k . desc) . command) in info-alist do
                      ;; Get match key.
                      (setq match-key k)
                      ;; Call function when match keystroke.
                      (when (one-key-match-keystroke key match-key)
                        ;; Close help window first.
                        (one-key-help-tip-close)
                        ;; Set `one-key-menu-call-first-time' with "t" for recursion execute.
                        (setq one-key-menu-call-first-time t)
                        ;; Execute.
                        (call-interactively command)
                        ;; Set `one-key-menu-call-first-time' with "nil".
                        (setq one-key-menu-call-first-time nil)
                        (throw 'match t)))
                nil)
              ;; Handle last.
              (one-key-handle-last alternate-function self recursion-p))
             ;; Match build-in keystroke.
             ((one-key-match-keystroke key "q")
              ;; quit
              (keyboard-quit))
             ((one-key-match-keystroke key "?")
              ;; toggle help window
              (one-key-help-tip-toggle title info-alist)
              (funcall self))
             ;; Not match any keystrokes.
             (t
              ;; Close help window first.
              (one-key-help-tip-close)
              ;; Quit when keystroke not match
              ;; and argument `miss-match-exit-p' is `non-nil'.
              (when miss-match-exit-p
                ;; Record last key.
                (setq last-key key)
                ;; Abort.
                (keyboard-quit))
              ;; Handle last.
              (one-key-handle-last alternate-function self recursion-p))))
        ;; Restore value of `one-key-call-first-time'.
        (setq one-key-menu-call-first-time t)
        ;; Close help window.
        (one-key-help-tip-close)
        ;; Run protect function
        ;; when `protect-function' is valid function.
        (if (and protect-function
                 (functionp protect-function))
            (call-interactively protect-function))
        ;; Execute last command when miss match
        ;; user key alist.
        (when (and execute-last-command-when-miss-match
                   last-key)
          ;; Execute command corresponding last input key.
          (one-key-execute-binding-command last-key))))))
