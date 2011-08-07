(defvar one-key-menu-C-x-RET-alist nil
  "The `one-key' menu alist for C-x RET.")

(setq one-key-menu-C-x-RET-alist
      '(
        (("C-\\" . "Set Input Method") . set-input-method)
        (("F" . "Set File Name Coding System") . set-file-name-coding-system)
        (("X" . "Set Next Selection Coding System") . set-next-selection-coding-system)
        (("c" . "Universal Coding System Argument") . universal-coding-system-argument)
        (("f" . "Set Buffer File Coding System") . set-buffer-file-coding-system)
        (("k" . "Set Keyboard Coding System") . set-keyboard-coding-system)
        (("l" . "Set Language Environment") . set-language-environment)
        (("p" . "Set Buffer Process Coding System") . set-buffer-process-coding-system)
        (("r" . "Revert Buffer With Coding System") . revert-buffer-with-coding-system)
        (("t" . "Set Terminal Coding System") . set-terminal-coding-system)
        (("x" . "Set Selection Coding System") . set-selection-coding-system)
        ))

(defun one-key-menu-C-x-RET ()
  "The `one-key' menu for C-x RET"
  (interactive)
  (one-key-menu "C-x RET" one-key-menu-C-x-RET-alist))
(global-set-key (kbd "C-x RET") 'one-key-menu-C-x-RET)

(defvar one-key-menu-C-x-ESC-alist nil
  "The `one-key' menu alist for C-x ESC.")

(setq one-key-menu-C-x-ESC-alist
      '(
        (("ESC" . "Repeat Complex Command") . repeat-complex-command)
        ((":" . "Repeat Complex Command") . repeat-complex-command)
        ))

(defun one-key-menu-C-x-ESC ()
  "The `one-key' menu for C-x ESC"
  (interactive)
  (one-key-menu "C-x ESC" one-key-menu-C-x-ESC-alist))
;; (global-set-key (kbd "C-x ESC") 'one-key-menu-C-x-ESC) ; conflict term-raw-escape-map setting

(defvar one-key-menu-C-x-4-alist nil
  "The `one-key' menu alist for C-x 4.")

(setq one-key-menu-C-x-4-alist
      '(
        (("C-f" . "Find File Other Window") . find-file-other-window)
        (("C-o" . "Display Buffer") . display-buffer)
        (("." . "Find Tag Other Window") . find-tag-other-window)
        (("0" . "Kill Buffer And Window") . kill-buffer-and-window)
        (("a" . "Add Change Log Entry Other Window") . add-change-log-entry-other-window)
        (("b" . "Switch To Buffer Other Window") . switch-to-buffer-other-window)
        (("c" . "Clone Indirect Buffer Other Window") . clone-indirect-buffer-other-window)
        (("d" . "Ffap Dired Other Window") . ffap-dired-other-window)
        (("f" . "Ffap Other Window") . ffap-other-window)
        (("m" . "Compose Mail Other Window") . compose-mail-other-window)
        (("r" . "Ffap Read Only Other Window") . ffap-read-only-other-window)
        ))

(defun one-key-menu-C-x-4 ()
  "The `one-key' menu for C-x 4"
  (interactive)
  (one-key-menu "C-x 4" one-key-menu-C-x-4-alist))
(global-set-key (kbd "C-x 4") 'one-key-menu-C-x-4)

(defvar one-key-menu-C-x-5-alist nil
  "The `one-key' menu alist for C-x 5.")

(setq one-key-menu-C-x-5-alist
      '(
        (("C-f" . "Find File Other Frame") . find-file-other-frame)
        (("C-o" . "Display Buffer Other Frame") . display-buffer-other-frame)
        (("." . "Find Tag Other Frame") . find-tag-other-frame)
        (("0" . "Delete Frame") . delete-frame)
        (("1" . "Delete Other Frames") . delete-other-frames)
        (("2" . "Make Frame Command") . make-frame-command)
        (("b" . "Switch To Buffer Other Frame") . switch-to-buffer-other-frame)
        (("d" . "Ffap Dired Other Frame") . ffap-dired-other-frame)
        (("f" . "Ffap Other Frame") . ffap-other-frame)
        (("m" . "Compose Mail Other Frame") . compose-mail-other-frame)
        (("o" . "Other Frame") . other-frame)
        (("r" . "Ffap Read Only Other Frame") . ffap-read-only-other-frame)
        ))

(defun one-key-menu-C-x-5 ()
  "The `one-key' menu for C-x 5"
  (interactive)
  (one-key-menu "C-x 5" one-key-menu-C-x-5-alist))
(global-set-key (kbd "C-x 5") 'one-key-menu-C-x-5)

(defvar one-key-menu-C-x-n-alist nil
  "The `one-key' menu alist for C-x n.")

(setq one-key-menu-C-x-n-alist
      '(
        (("d" . "Narrow To Defun") . narrow-to-defun)
        (("n" . "Narrow To Region") . narrow-to-region)
        (("p" . "Narrow To Page") . narrow-to-page)
        (("s" . "Org Narrow To Subtree") . org-narrow-to-subtree)
        (("w" . "Widen") . widen)
        ))

(defun one-key-menu-C-x-n ()
  "The `one-key' menu for C-x n"
  (interactive)
  (one-key-menu "C-x n" one-key-menu-C-x-n-alist))
(global-set-key (kbd "C-x n") 'one-key-menu-C-x-n)

(defvar one-key-menu-C-x-r-alist nil
  "The `one-key' menu alist for C-x r.")

(setq one-key-menu-C-x-r-alist
      '(
        (("C-@" . "Point To Register") . point-to-register)
        (("SPC" . "Point To Register") . point-to-register)
        (("+" . "Increment Register") . increment-register)
        (("b" . "Bookmark Jump") . bookmark-jump)
        (("c" . "Clear Rectangle") . clear-rectangle)
        (("d" . "Delete Rectangle") . delete-rectangle)
        (("f" . "Frame Configuration To Register") . frame-configuration-to-register)
        (("g" . "Insert Register") . insert-register)
        (("i" . "Insert Register") . insert-register)
        (("j" . "Jump To Register") . jump-to-register)
        (("k" . "Kill Rectangle") . kill-rectangle)
        (("l" . "Bookmark Bmenu List") . bookmark-bmenu-list)
        (("m" . "Bookmark Set") . bookmark-set)
        (("n" . "Number To Register") . number-to-register)
        (("o" . "Open Rectangle") . open-rectangle)
        (("r" . "Copy Rectangle To Register") . copy-rectangle-to-register)
        (("s" . "Copy To Register") . copy-to-register)
        (("t" . "String Rectangle") . string-rectangle)
        (("w" . "Window Configuration To Register") . window-configuration-to-register)
        (("x" . "Copy To Register") . copy-to-register)
        (("y" . "Yank Rectangle") . yank-rectangle)
        (("C-SPC" . "Point To Register") . point-to-register)
        ))

(defun one-key-menu-C-x-r ()
  "The `one-key' menu for C-x r"
  (interactive)
  (one-key-menu "C-x r" one-key-menu-C-x-r-alist))
(global-set-key (kbd "C-x r") 'one-key-menu-C-x-r)

(defvar one-key-menu-C-x-v-alist nil
  "The `one-key' menu alist for C-x v.")

(setq one-key-menu-C-x-v-alist
      '(
        (("+" . "Vc Update") . vc-update)
        (("=" . "Vc Diff") . vc-diff)
        (("D" . "Vc Root Diff") . vc-root-diff)
        (("L" . "Vc Print Root Log") . vc-print-root-log)
        (("a" . "Vc Update Change Log") . vc-update-change-log)
        (("b" . "Vc Switch Backend") . vc-switch-backend)
        (("c" . "Vc Rollback") . vc-rollback)
        (("d" . "Vc Dir") . vc-dir)
        (("g" . "Vc Annotate") . vc-annotate)
        (("h" . "Vc Insert Headers") . vc-insert-headers)
        (("i" . "Vc Register") . vc-register)
        (("l" . "Vc Print Log") . vc-print-log)
        (("m" . "Vc Merge") . vc-merge)
        (("r" . "Vc Retrieve Tag") . vc-retrieve-tag)
        (("s" . "Vc Create Tag") . vc-create-tag)
        (("u" . "Vc Revert") . vc-revert)
        (("v" . "Vc Next Action") . vc-next-action)
        (("~" . "Vc Revision Other Window") . vc-revision-other-window)
        ))

(defun one-key-menu-C-x-v ()
  "The `one-key' menu for C-x v"
  (interactive)
  (one-key-menu "C-x v" one-key-menu-C-x-v-alist))
(global-set-key (kbd "C-x v") 'one-key-menu-C-x-v)

(defvar one-key-menu-M-g-alist nil
  "The `one-key' menu alist for M-g.")

(setq one-key-menu-M-g-alist
      '(
        ;; (("ESC" . "Prefix Command") . Prefix Command)
        (("g" . "Goto Line") . goto-line)
        (("n" . "Next Error") . next-error)
        (("p" . "Previous Error") . previous-error)

        (("M-g" . "Goto Line") . goto-line)
        (("M-n" . "Next Error") . next-error)
        (("M-p" . "Previous Error") . previous-error)
        (("M-a" . "AutoHighlightSymbol") . auto-highlight-symbol-mode)
        ))

(defun one-key-menu-M-g ()
  "The `one-key' menu for M-g"
  (interactive)
  (one-key-menu "M-g" one-key-menu-M-g-alist))
(global-set-key (kbd "M-g") 'one-key-menu-M-g)

(defvar one-key-menu-C-x-a-alist nil
  "The `one-key' menu alist for C-x a.")

(setq one-key-menu-C-x-a-alist
      '(
        (("C-a" . "Add Mode Abbrev") . add-mode-abbrev)
        (("'" . "Expand Abbrev") . expand-abbrev)
        (("+" . "Add Mode Abbrev") . add-mode-abbrev)
        (("-" . "Inverse Add Global Abbrev") . inverse-add-global-abbrev)
        (("e" . "Expand Abbrev") . expand-abbrev)
        (("g" . "Add Global Abbrev") . add-global-abbrev)
        ;; (("i" . "Prefix Command") . Prefix Command)
        (("l" . "Add Mode Abbrev") . add-mode-abbrev)
        (("n" . "Expand Jump To Next Slot") . expand-jump-to-next-slot)
        (("p" . "Expand Jump To Previous Slot") . expand-jump-to-previous-slot)

        ;; (("i g" . "Inverse Add Global Abbrev") . inverse-add-global-abbrev)
        ;; (("i l" . "Inverse Add Mode Abbrev") . inverse-add-mode-abbrev)
        ))

(defun one-key-menu-C-x-a ()
  "The `one-key' menu for C-x a"
  (interactive)
  (one-key-menu "C-x a" one-key-menu-C-x-a-alist))
(global-set-key (kbd "C-x a") 'one-key-menu-C-x-a)

(defvar one-key-menu-C-x-alist nil
  "The `one-key' menu alist for C-x.")

(setq one-key-menu-C-x-alist
      '(
        (("C-@" . "Pop Global Mark") . pop-global-mark)
        (("C-b" . "List Buffers") . list-buffers)
        (("C-c" . "Save Buffers Kill Terminal") . save-buffers-kill-terminal)
        (("C-d" . "Ffap List Directory") . ffap-list-directory)
        (("C-e" . "Eval Last Sexp") . eval-last-sexp)
        (("C-f" . "Anything For Files") . anything-for-files)
        (("TAB" . "Indent Rigidly") . indent-rigidly)
        (("C-j" . "Skk Mode") . skk-mode)
        (("C-k" . "Kmacro Keymap") . kmacro-keymap)
        (("C-l" . "Downcase Region") . downcase-region)
        (("RET" . "One Key Menu C X Ret") . one-key-menu-C-x-RET)
        (("C-n" . "Set Goal Column") . set-goal-column)
        (("C-o" . "Delete Blank Lines") . delete-blank-lines)
        (("C-p" . "Mark Page") . mark-page)
        (("C-q" . "Toggle Read Only") . toggle-read-only)
        (("C-r" . "Ffap Read Only") . ffap-read-only)
        (("C-s" . "Save Buffer") . save-buffer)
        (("C-t" . "Transpose Lines") . transpose-lines)
        (("C-u" . "Upcase Region") . upcase-region)
        (("C-v" . "Ffap Alternate File") . ffap-alternate-file)
        (("C-w" . "Write File") . write-file)
        (("C-x" . "Exchange Point And Mark") . exchange-point-and-mark)
        (("C-y" . "Yas/Oneshot Snippet") . yas/oneshot-snippet)
        (("C-z" . "Suspend Frame") . suspend-frame)
        (("ESC" . "One Key Menu C X Esc") . one-key-menu-C-x-ESC)
        (("#" . "Server Edit") . server-edit)
        (("$" . "Set Selective Display") . set-selective-display)
        (("'" . "Expand Abbrev") . expand-abbrev)
        (("(" . "Kmacro Start Macro") . kmacro-start-macro)
        ((")" . "Kmacro End Macro") . kmacro-end-macro)
        (("*" . "Calc Dispatch") . calc-dispatch)
        (("+" . "Balance Windows") . balance-windows)
        (("-" . "Shrink Window If Larger Than Buffer") . shrink-window-if-larger-than-buffer)
        (("." . "Set Fill Prefix") . set-fill-prefix)
        (("0" . "Delete Window") . delete-window)
        (("1" . "Delete Other Windows") . delete-other-windows)
        (("2" . "Split Window Vertically") . split-window-vertically)
        (("3" . "Split Window Horizontally") . split-window-horizontally)
        (("4" . "One Key Menu C X 4") . one-key-menu-C-x-4)
        (("5" . "One Key Menu C X 5") . one-key-menu-C-x-5)
        (("6" . "2c Command") . 2C-command)
        ;; (("8" . "Prefix Command") . Prefix Command)
        ((";" . "Comment Set Column") . comment-set-column)
        (("<" . "Scroll Left") . scroll-left)
        (("=" . "What Cursor Position") . what-cursor-position)
        ((">" . "Scroll Right") . scroll-right)
        (("?" . "One Key Menu C X") . one-key-menu-C-x)
        (("P" . "Historyf Forward") . historyf-forward)
        (("[" . "Backward Page") . backward-page)
        (("]" . "Forward Page") . forward-page)
        (("^" . "Enlarge Window") . enlarge-window)
        (("`" . "Next Error") . next-error)
        (("a" . "One Key Menu C X A") . one-key-menu-C-x-a)
        (("b" . "Switch To Buffer") . switch-to-buffer)
        (("d" . "Dired At Point") . dired-at-point)
        (("e" . "Kmacro End And Call Macro") . kmacro-end-and-call-macro)
        (("f" . "Set Fill Column") . set-fill-column)
        (("h" . "Mark Whole Buffer") . mark-whole-buffer)
        (("i" . "Insert File") . insert-file)
        (("j" . "Skk Auto Fill Mode") . skk-auto-fill-mode)
        (("k" . "Kill Buffer") . kill-buffer)
        (("l" . "Count Lines Page") . count-lines-page)
        (("m" . "Clmemo") . clmemo)
        (("n" . "One Key Menu C X N") . one-key-menu-C-x-n)
        (("o" . "Other Window") . other-window)
        (("p" . "Historyf Back") . historyf-back)
        (("q" . "Kbd Macro Query") . kbd-macro-query)
        (("r" . "One Key Menu C X R") . one-key-menu-C-x-r)
        (("s" . "Save Some Buffers") . save-some-buffers)
        (("t" . "Skk Tutorial") . skk-tutorial)
        (("u" . "Undo") . undo)
        (("v" . "One Key Menu C X V") . one-key-menu-C-x-v)
        (("z" . "Repeat") . repeat)
        (("{" . "Shrink Window Horizontally") . shrink-window-horizontally)
        (("}" . "Enlarge Window Horizontally") . enlarge-window-horizontally)
        (("DEL" . "Backward Kill Sentence") . backward-kill-sentence)
        (("C-SPC" . "Pop Global Mark") . pop-global-mark)
        (("C-+" . "Text Scale Adjust") . text-scale-adjust)
        (("C--" . "Text Scale Adjust") . text-scale-adjust)
        (("C-/" . "Session Jump To Last Change") . session-jump-to-last-change)
        (("C-0" . "Text Scale Adjust") . text-scale-adjust)
        (("C-=" . "Text Scale Adjust") . text-scale-adjust)
        (("<C-left>" . "Previous Buffer") . previous-buffer)
        (("<C-right>" . "Next Buffer") . next-buffer)
        (("<left>" . "Previous Buffer") . previous-buffer)
        (("<right>" . "Next Buffer") . next-buffer)
        (("<undo>" . "Session Jump To Last Change") . session-jump-to-last-change)

        ;; (("C-k C-a" . "Kmacro Add Counter") . kmacro-add-counter)
        ;; (("C-k C-c" . "Kmacro Set Counter") . kmacro-set-counter)
        ;; (("C-k C-d" . "Kmacro Delete Ring Head") . kmacro-delete-ring-head)
        ;; (("C-k C-e" . "Kmacro Edit Macro Repeat") . kmacro-edit-macro-repeat)
        ;; (("C-k C-f" . "Kmacro Set Format") . kmacro-set-format)
        ;; (("C-k TAB" . "Kmacro Insert Counter") . kmacro-insert-counter)
        ;; (("C-k C-k" . "Kmacro End Or Call Macro Repeat") . kmacro-end-or-call-macro-repeat)
        ;; (("C-k C-l" . "Kmacro Call Ring 2nd Repeat") . kmacro-call-ring-2nd-repeat)
        ;; (("C-k RET" . "Kmacro Edit Macro") . kmacro-edit-macro)
        ;; (("C-k C-n" . "Kmacro Cycle Ring Next") . kmacro-cycle-ring-next)
        ;; (("C-k C-p" . "Kmacro Cycle Ring Previous") . kmacro-cycle-ring-previous)
        ;; (("C-k C-s" . "Kmacro Start Macro") . kmacro-start-macro)
        ;; (("C-k C-t" . "Kmacro Swap Ring") . kmacro-swap-ring)
        ;; (("C-k C-v" . "Kmacro View Macro Repeat") . kmacro-view-macro-repeat)
        ;; (("C-k SPC" . "Kmacro Step Edit Macro") . kmacro-step-edit-macro)
        ;; (("C-k b" . "Kmacro Bind To Key") . kmacro-bind-to-key)
        ;; (("C-k e" . "Edit Kbd Macro") . edit-kbd-macro)
        ;; (("C-k l" . "Kmacro Edit Lossage") . kmacro-edit-lossage)
        ;; (("C-k n" . "Kmacro Name Last Macro") . kmacro-name-last-macro)
        ;; (("C-k q" . "Kbd Macro Query") . kbd-macro-query)
        ;; (("C-k r" . "Apply Macro To Region Lines") . apply-macro-to-region-lines)
        ;; (("C-k s" . "Kmacro Start Macro") . kmacro-start-macro)

        ;; (("6 2" . "2c Two Columns") . 2C-two-columns)
        ;; (("6 b" . "2c Associate Buffer") . 2C-associate-buffer)
        ;; (("6 s" . "2c Split") . 2C-split)
        ;; (("6 <f2>" . "2c Two Columns") . 2C-two-columns)

        ;; (("8 RET" . "Ucs Insert") . ucs-insert)
        ))

(defun one-key-menu-C-x ()
  "The `one-key' menu for C-x"
  (interactive)
  (one-key-menu "C-x" one-key-menu-C-x-alist))
(global-set-key (kbd "C-x ?") 'one-key-menu-C-x)
