;;; snap.el --- save/load snapshot of application to/from text

;; Copyright (c) 2003, 2004, 2005, 2006, 2007, 2008
;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;; $Id: snap.el,v 1.41 2008/04/24 15:38:35 hira Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License is available by anonymouse ftp from
;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;;; Commentary:

;; Usage:
;;
;; (1) M-x snap-record on application, e.g. Wanderlust.
;; (2) Yank (C-y) on any buffer, e.g. *scratch* or ~/memo.txt.
;; (3) M-x snap-play on yanked text ==> snapshot (1) is restored.

;; Supported applications:
;;
;; - BBDB
;; - BibTeX
;; - Bookmark
;; - Dired
;; - Emacs-wiki
;; - Gnus
;; - Help
;; - howm-search ( C-c , g )
;; - Info
;; - Man
;; - Mew
;; - Navi2ch (Article buffer)
;; - occur (experimental, using fake cgi-extension)
;; - PCVS
;; - Shell
;; - Thumbs
;; - w3m
;; - Wanderlust (Summary buffer)
;; - snap:///  (only message it's version)
;;
;; For unsupported buffers,
;; file name and current position are recorded.

;; Caution for byte-compilation:
;;
;; Byte-compiling this file is not recommended.
;; Some fucntions will be dropped silently if required features are not
;; available at compile time.
;; For example, snap-*:w3m are ignored if the feature w3m is not available.
;; You may want to recompile this file after you set up these features.

;; Internal:
;;
;; Format of snapshot string is "snap://MAJOR-MODE/SPELL".
;; Format and meaning of SPELL depend on MAJOR-MODE.
;; For example,
;; snap://wl-summary-mode/+ME/hira/<20031101192305.AFA8C43EDC@hoge.fuga.piyo>
;; is a snapshot string of wl-summary-mode for the spell
;; +ME/hira/<20031101192305.AFA8C43EDC@hoge.fuga.piyo>,
;; which indicates
;; message-id <20031101192305.AFA8C43EDC@hoge.fuga.piyo>
;; in the folder +ME/hira.
;;
;; Please define snap-record:MAJOR-MODE and snap-play:MAJOR-MODE
;; if you want to support your favorite application.
;; (snap-record:MAJOR-MODE) returns SPELL string for current snapshot.
;; (snap-play:MAJOR-MODE SPELL) restores snapshot from SPELL string.

;; Abbreviation (experimental):
;;
;; You can add abbreviation rules of snap strings
;; to the variable `snap-abbrev'. See its docstring for details.

;; Fake cgi-extension (experimental):
;;
;; When `snap-record-cgi' is not empty, you can use a
;; fake cgi like "snap://MAJOR-MODE/SPELL??g=110&s=str&q=word&x=",
;; which calls snap-play::g, snap-play::s, snap-play::q and
;; snap-play::x.
;;
;; At this experimental stage, format of url is not
;; strict like RFC and not *escaped*. (and I have no idea for doing it
;; :-) An example of the problem is
;; "snap://occur-mode/dired-mode/~/??q=drwx??g=2", but it still works
;; because of longest-match tricks.  See `snap-cgi-decode'
;;
;; Supported cgi-functions:
;; g=110  goto-line
;; s=str  search string
;; q=word occur word
;; x=     dired-x
;; 
;; For some cases, mode-specific commands may be desired.
;; See `snap-play-cgi' and `snap-play:help-mode:' for example.

;; Repair (experimental):
;;
;; When you fail snap-play, you can try M-x snap-repair
;; to repair snapshot text.
;; This can happen, e.g. when you move mails to other folders.
;;
;; You have to write your own 'my-snap-search-mail' function
;; which receives message-id and returns its file name.
;; My version requires namazu and howm.
;; - namazu: full text search engine <http://www.namazu.org/index.html.en>
;; - howm: note-taking tool <http://howm.sourceforge.jp/>
;; (defvar my-namazu-mail-dir (expand-file-name "~/PATH/NMZ/Mail"))
;; (defun my-snap-search-mail (message-id)
;;   (let* ((query (format "+message-id:%s" message-id))
;;          (args `("-l" "-n" "1" ,query ,my-namazu-mail-dir)))
;;     (car (howm-view-call-process "namazu" args))))

;; Replace environment variables in file name.
;;
;; If you like "snap:///${HOME}/hoge" and "snap:///${FOODIR}/bar"
;; instead of "snap:///~/hoge" and "snap:///usr/local/foo/bar", try this.
;; I'm not sure whether there is considerable demand for this feature.
;;
;; (defvar snap-abbreviate-environment-variables '("FOODIR" "HOME"))
;; (defadvice snap-abbreviate-file-name (around env-var (raw-path) activate)
;;   ad-do-it
;;   (let ((path (expand-file-name raw-path))
;;         (rules (mapcar (lambda (var)
;;                          (let ((val (getenv var)))
;;                            (and val
;;                                 (cons (concat "^" (regexp-quote val))
;;                                       (format "${%s}" var)))))
;;                        snap-abbreviate-environment-variables)))
;;     (mapc (lambda (r)
;;             (when (and r (string-match (car r) path))
;;               (setq ad-return-value
;;                     (replace-regexp-in-string (car r) (cdr r) path))))
;;           (reverse rules))))

;; With bookmark and ffap (experimental):
;;
;; ;; Put this code into your .emacs to enable bookmark+snap feature.
;; (eval-after-load "bookmark"
;;   (ad-enable-advice 'bookmark-buffer-file-name 'around 'with-snap)
;;   (ad-enable-advice 'bookmark-jump-noselect 'around 'with-snap))
;; 
;; ;; Put this code into your .emacs to enable ffap+snap feature.
;; (setq ffap-url-regexp snap-ffap-url-regexp)
;; (setq ffap-url-fetcher snap-ffap-url-fetcher)

;; ChangeLog:
;; 
;; [2008-04-24] Mew is supported. (thx > dareka)
;; [2007-05-16] use snap-define-op instead of require in defun.
;; [2007-05-16] PCVS and Thumbs are supported. (thx > Ma)
;; [2007-05-16] experimental features with bookmark and ffap (thx > Ma)
;; [2007-02-24] byte-compilation is now OK.
;;              (thx > Taiki SUGAWARA <buzz.taiki at gmail.com>)
;; [2006-06-15] snap-record:dired-mode also supports environment variables
;;              (thx > taku)
;; [2006-06-07] snap-play:dired-mode also supports environment variables
;;              (thx > taku)
;; [2006-05-28] snap-try-decode for atode.el
;; [2006-04-23] replace environment variables in snap-record: (thx > taku)
;; [2006-04-11] environment variables in file path are expanded (thx > taku)
;; [2006-03-25] fix: Obsolete constant name in bibtex. (thx > 20)
;; [2006-03-21] add document on byte-compilation. (thx > 20)
;; [2006-01-31] fix: Drive letter problem in windows. (thx > Touhi)
;; [2005-09-28] cgi for Man-mode. (thx > Ma)
;; [2005-09-27] mode-specific cgi command. (thx > Ma)
;; [2005-07-03] Gnus is supported. (thx > Wi)
;;              Variable snap-mode-functions for extension.
;; [2005-05-24] snap-record-string never cause error again.
;;              This is necessary for my another tool, atode.el.
;;              http://howm.sourceforge.jp/a/atode.el
;; [2005-05-19] BBDB, BibTeX, Shell ,occur, howm-search are supported.
;;              fix: `snap-play' and extend fake cgi and `snap-expand-alist'.
;;              And set `snap-record-default-format'. (thx > Ma)
;; [2005-03-03] snap-record-string doesn't cause error any more.
;; [2004-11-16] fix: second -> cadr (thx > Toorisugari)
;; [2004-09-11] Emacs-wiki, Navi2ch, w3m, Dired are supported. (thx > Ma)
;; [2004-04-21] fix: Error when action-lock is not available (thx > Nanashi)
;; [2004-04-18] Goto occurrence when it is unique match.
;; [2004-04-10] Help, Bookmark, Man, Info are supported. (thx > Ma)
;; [2004-02-25] action-lock
;; [2004-02-23] fix: Error on CVS latest Wanderlust (thx > hirose31)
;; [2004-01-16] Jump to specified position
;; [2003-11-09] fix: All modes said 'not supported'.
;; [2003-11-08] First upload
;; [2003-11-05] First version

;; Bug/Restriction
;; - thing-at-point fails to recognize "snap:///file#1: snap:///"

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'thingatpt)

(defvar snap-version "$Id: snap.el,v 1.41 2008/04/24 15:38:35 hira Exp $")
(defvar snap-prt "snap://")
(defvar snap-format (concat snap-prt "%s/%s"))
(defvar snap-regexp (concat (regexp-quote snap-prt) "\\([^/\r\n]*\\)/\\(.*\\)"))
(defvar snap-mode-pos 1)
(defvar snap-spell-pos 2)
(defvar snap-root-dir (expand-file-name "/")) ;; "c:/" etc. for windows
(defvar snap-record-string-no-error t
  "For private use by other packages.
It indicates that old bug on `snap-record-string' is already fixed.")
(defvar snap-spell-format "%s??%s"
  "Note: You can change this default to \"%s?%s\" like a cgi.  But you
will face to ploblem; how to deal with
\"snap://w3m-mode/http://www.google.com?q=1?q=2\".")
(defvar snap-cgi-format "%s=%s")
(defvar snap-spell-regexp "\\(.*\\)[?][?]\\([a-z][=].*\\)"
  "Note: Longest match of first part is important for the case:
\"snap://occur-mode/dired-mode/~/??q=drwx??g=2\"")
(defvar snap-nocgi-pos 1)
(defvar snap-cgi-pos 2)
(defvar snap-cgi-separator "&")
(defvar snap-record-cgi nil
  "List of recorded cgi types in `snap-record'")
;;; for test use:
;;; (setq snap-record-cgi '("g" "s" "q"))

(defvar snap-abbrev nil
  "List of rules on abbreviation for snap string.
Each rule is a list of three strings: ABBREV, MODE, and SPELL-HEAD.
snap://ABBREV/xxx is expanded as snap://MODE/SPELL-HEADxxx.

Example:
 ;; snap://l/file ==> snap://dired-mode/usr/local/meadow/1.15/lisp/file
 ;; snap://s/dir  ==> snap://shell-mode/~/#dir
 (setq snap-abbrev
       '((\"l\" \"dired-mode\" \"usr/local/meadow/1.15/lisp/\")
         (\"s\" \"shell-mode\" \"~/\#\")))
")

(defvar snap-mode-functions nil
  "List of functions which give the mode string of current buffer
instead of the variable `major-mode'.
Each function must return nil if it cannot determine the mode, so that
decision is passed to the next function.

This variable is prepared for applications which does not use
their own major-mode, e.g. message buffers in Wanderlust.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main

(defun snap-record ()
  "Convert snapshot of application to string, and put it to kill-ring."
  (interactive)
  (let ((snap (snap-record-string)))
    (when (null snap)
      (error "This buffer is not supported."))
    (kill-new snap)
    (message "%s" snap)))

(defun snap-play ()
  "Restore snapshot of application from string at point."
  (interactive)
  (let ((snap (thing-at-point 'snap)))
    ;; avoid (snap-play-string nil)
    (and snap (snap-play-string snap))))

(defun snap-repair ()
  (interactive)
  (let ((snap (thing-at-point 'snap))
        (beg (match-beginning 0))
        (end (match-end 0)))
    (let ((repaired (snap-repair-string snap)))
      (goto-char beg)
      (delete-region beg end)
      (insert repaired)
      (message "Repaired."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; util

(defun snap-record-string ()
  (let ((long (snap-record-string-exact)))
    (and long (snap-shrink-string long))))
(defun snap-play-string (snap)
  (snap-play-string-exact (snap-expand-string snap)))

(defun snap-shrink-string (snap)
  "String SNAP is shrinked according to rules in `snap-abbrev'.
When several rules are applicable, the shortest result is returned."
  (let ((candidates (mapcar (lambda (rule)
                              (snap-shrink-string-by-rule snap rule))
                            snap-abbrev)))
    (if candidates
        (car (sort candidates (lambda (x y) (< (length x) (length y)))))
      snap)))

(defun snap-shrink-string-by-rule (snap rule)
  (apply (lambda (abbrev mode spell-head)
           (apply (lambda (o-mode o-spell)
                    (let ((reg (concat "^" (regexp-quote spell-head))))
                      (if (and (string= mode o-mode)
                               (string-match reg o-spell))
                          (snap-encode abbrev (substring o-spell (match-end 0)))
                        snap)))
                  (snap-decode snap)))
         rule))

(defun snap-expand-string (snap)
  (apply (lambda (a-mode a-spell)
           (let ((rule (assoc a-mode snap-abbrev)))
             (if rule
                 (apply (lambda (abbrev mode spell-head)
                          (snap-encode mode (concat spell-head a-spell)))
                        rule)
               snap)))
         (snap-decode snap)))

(defun snap-record-string-exact ()
  "Convert snapshot of application to string.
Nil is returned for unsupported buffer."
  (let* ((mode (snap-get-mode snap-mode-functions))
         (recorder (or (snap-op 'record mode t)
                       (progn (setq mode "") (snap-op 'record mode))))
         (spell (funcall recorder)))
    (and spell
         (snap-encode mode spell
                      (delq nil (mapcar #'snap-record-cgi snap-record-cgi))))))

(defun snap-get-mode (functions)
  (if (null functions)
      major-mode
    (or (funcall (car functions))
        (snap-get-mode (cdr functions)))))

(defun snap-play-string-exact (snap)
  "Restore snapshot of application from string. "
  (let* ((x (snap-decode snap snap-record-cgi))
         (mode (car x))
         (spell (cadr x))
         (cgi (cddr x))
         (player (snap-op 'play mode)))
    (funcall player spell)
    (mapcar (lambda (z)
              (apply (lambda (op val) (snap-play-cgi op val mode))
                     z))
            cgi)))

(defun snap-play-cgi (op val &optional mode)
  "Find fake cgi command for operation OP and call it with the argument VAL.
If MODE is given, snap-play:MODE:OP or snap-play:MODE: are used preferably
rather than general snap-play::OP.
They are called as
  (snap-play:MODE:OP VAL)
  (snap-play:MODE: OP VAL)
  (snap-play::OP VAL)
respectively."
  (let ((player-mo (and mode (snap-op 'play (concat mode ":" op) t)))
        (player-m (and mode
                       (let ((f (snap-op 'play (concat mode ":"))))
                         (and f
                              ;; elisp is not scheme. sigh...
                              `(lambda (val) (funcall (function ,f)
                                                      (quote ,op)
                                                      val))))))
        (player-o (snap-op 'play (concat ":" op))))
    (funcall (or player-mo player-m player-o) val)))

(defun snap-play-cgi-on (buffer op val)
  (snap-do-on buffer (lambda () (snap-play-cgi op val))))

(defun snap-do-on (buffer proc)
  (save-selected-window
    (select-window (get-buffer-window buffer t))
    (funcall proc)))

(defun snap-record-cgi (op)
  (let ((s (funcall (snap-op 'record (concat ":" op)))))
    (if s
        (snap-cgi-encode op s)
      nil)))

(defun snap-spell-decode (spell)
  ;; suppose: spell has no-property
  ;; Example:
  ;; (snap-spell-decode "body#tag1?g=1&q=2??g=op1&q=?q=&x=#tag2&x")
  ;; => ("body#tag1?g=1&q=2" ("g" "op1") ("q" "?q") ("x" "#tag2&x"))
  (if (string-match snap-spell-regexp spell)
      (cons (match-string snap-nocgi-pos spell)
            (snap-cgi-decode (match-string snap-cgi-pos spell)))
    (list spell)))

(defun snap-cgi-decode (cgi)
  ;; (snap-cgi-decode "a=1&b=c&d&e=&f")
  ;; => '(("a" "1") ("b" "c&d") ("e" "&f"))
  (let* ((f-regexp (snap-cgi-encode "\\([a-z]\\)" "\\(.*\\)"))
         (s-regexp (concat "^\\(.*\\)" snap-cgi-separator f-regexp))
         ;; using longest-match of the first part.
         (rest cgi)
         (olist '()))
    (while (string-match s-regexp rest)
      (setq olist (cons (list (match-string 2 rest) (match-string 3 rest)) olist))
      (setq rest (match-string 1 rest)))
    (if (string-match f-regexp rest)
        (setq olist (cons (list (match-string 1 rest) (match-string 2 rest)) olist))
      (message "unknown error"))
    olist))

(defun snap-repair-string (snap)
  (let* ((x (snap-decode snap))
         (mode (car x))
         (spell (cadr x)))
    (let ((repairer (snap-op 'repair mode)))
      (snap-encode mode (funcall repairer spell)))))

(defun snap-encode (mode spell &optional cgi-list)
  (when cgi-list
    (setq spell
          (format snap-spell-format
                  spell
                  (mapconcat #'identity cgi-list
                             snap-cgi-separator))))
  (format snap-format mode spell))

(defun snap-spell-encode (spell cgi)
  (format snap-spell-format spell cgi))

(defun snap-cgi-encode (op str)
  (format snap-cgi-format op str))

(defun snap-decode (snap &optional cgi-p)
  (or (snap-try-decode snap cgi-p)
      (error "Wrong snapshot format: %s" snap)))

(defun snap-try-decode (snap &optional cgi-p)
  (and (string-match snap-regexp snap)
       (let ((mode (match-string-no-properties snap-mode-pos snap))
             (spell (match-string-no-properties snap-spell-pos snap)))
         (if cgi-p
             (cons mode (snap-spell-decode spell))
           (list mode spell)))))

(defun snap-op (op mode &optional no-err)
  (let ((f (intern-soft (format "snap-%s:%s" op mode))))
    (cond ((functionp f) f)
          (no-err nil)
          (t (error "%s is not supported." mode)))))

;;; for thing-at-point
(defun forward-snap (arg)
  (interactive "p")
  (if (natnump arg)
      (re-search-forward snap-regexp nil 'move arg)
    (progn
      (skip-chars-forward "^ \t\r\n")
      (while (< arg 0)
        (if (re-search-backward snap-regexp nil 'move)
            (skip-chars-backward "^ \t\r\n"))
        (setq arg (1+ arg))))))

;;; You need your own 'my-snap-search-mail'
;;; which receives message-id and returns its file name.
(eval-when-compile
  (defalias 'my-snap-search-mail 'ignore))

(defun snap-search-mail (message-id)
  (message "Searching...")
  (or (my-snap-search-mail message-id)
      (error "Not found: %s" message-id)))

(defun snap-line-number ()
  (let ((raw (count-lines (point-min) (point))))
    ;; see (describe-function 'count-lines)
    (if (bolp)
        (+ raw 1)
      raw)))

;;; check
(let ((snap-abbrev '(("l" "dired-mode" "usr/meadow/1.15/lisp/")
                     ("s" "shell-mode" "~/#")))
      (qa '(("snap://l/file" "snap://dired-mode/usr/meadow/1.15/lisp/file")
            ("snap://s/dir" "snap://shell-mode/~/#dir"))))
  (mapcar (lambda (z)
            (apply (lambda (short long)
                     (if (and (string= short (snap-shrink-string long))
                              (string= (snap-expand-string short) long))
                         t
                       (error "incorrect snap-abbrev: %s %s" short long)))
                   z))
          qa))

(defun snap-find-file (path)
  (find-file (expand-file-name (substitute-in-file-name path)
                               snap-root-dir)))

(put 'snap-with-features 'lisp-indent-function 1)
(put 'snap-define-op 'lisp-indent-function 2)
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(snap-with-features\\)\\>" 1 font-lock-keyword-face)
   ("(\\(snap-define-op\\)\\>[
 \t]+\\(\\sw+\\)"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))))

(defmacro snap-with-features (snap-features &rest body)
  "Check existence of all SNAP-FEATURES and evaluate BODY if ok.
This macro also attaches requirements of SNAP-FEATURES
inside `eval-when-compile'.

In this environment, a macro `snap-define-op' is available.
This macro is similar to `defun', but requires SNAP-FEATURES."
  (unless (memq nil
                (mapcar (lambda (feature)
                          (locate-library (symbol-name feature)))
                        snap-features))
;;       `(eval-when-compile
;;          (message "Ignore some features which require %s." ',snap-features))
    `(progn
       (eval-when-compile
         ,@(mapcar (lambda (feature)
                     `(require ',feature))
                   snap-features))
       (macrolet
           ((snap-define-op
                (name arg &rest body)
                (append (list 'defun name arg)
                        (mapcar (lambda (feature)
                                  (list 'require (list 'quote feature)))
                                ',snap-features)
                        body)))
         ,@body))))

;; dummy definition for completion
(defalias 'snap-define-op 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; applications

;;; <Application>
;;; <Sample>

;;; (Default)
;;; snap:///~/elisp/snap.el#177:(defun snap-record: ()

(defvar snap-record-default-format "%s#%s:%s")
                                        ;see also `snap-record:occur-mode'
(defun snap-record: ()
  (let ((raw-path (buffer-file-name)))
    (if (null raw-path)
        nil
      (let* ((line (snap-line-number))
             (text (save-excursion
                     (beginning-of-line)
                     (looking-at "^[ \t]*\\(.*\\)")
                     (match-string-no-properties 1)))
             (path (snap-abbreviate-file-name raw-path)))
        (format snap-record-default-format path line text)))))

(defun snap-abbreviate-file-name (raw-path)
  (let ((relative-path  ;; not snap:////etc but snap:///etc
         (file-relative-name raw-path snap-root-dir))
        (abbrev-path  ;; not snap:///home/foo but snap:///~foo
         (abbreviate-file-name raw-path)))
    ;; use shorter one
    (if (< (length relative-path) (length abbrev-path))
        relative-path
      abbrev-path)))

(defun snap-play: (spell)
  (cond
   ((or (null spell) (string= spell ""))
    (message "snap-version %s" snap-version))
   ((string-match "\\([^#\r\n]+\\)\\(#\\([0-9]+\\):\\(.*\\)\\)?" spell)
    (let ((path (match-string-no-properties 1 spell))
          (positionp (match-string-no-properties 2 spell))
          (line (match-string-no-properties 3 spell))
          (text (match-string-no-properties 4 spell)))
      (snap-find-file path)
      (when positionp
        (snap-play-search: (concat "^[ \t]*" (regexp-quote text) "$")
                           (string-to-number line)))))
   (t
    (error "not supported: %s" spell))))

(defun snap-play-search: (regexp line-number)
  (goto-line line-number)
  (cond ((looking-at regexp) t)
        ((snap-occur-p regexp) (snap-occur regexp line-number))
        (t (message "No match."))))

(defun snap-occur-p (regexp)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp nil t)))

(defun snap-occur (regexp line-number)
  (occur regexp 0)
  (switch-to-buffer "*Occur*") ;; why needed??
  (let ((hits (snap-looking-at-number)))
    (forward-line)
    (if (= hits 1)
        (snap-occur-goto-occurence)
      (snap-occur-goto-line line-number))))

(defun snap-occur-goto-occurence ()
  (message "Line number is obsolete.")
  (occur-mode-goto-occurrence)
  ;; I prefer bol.
  (beginning-of-line))

(defun snap-occur-goto-line (line-number)
  (while (let* ((n (snap-looking-at-number))
                (stop (and n (>= n line-number))))
           (and (not stop)
                (= (forward-line) 0)))
    ;; nothing to do
    nil)
  (if (not (snap-looking-at-number))
      (forward-line -1)))

(defun snap-looking-at-number ()
  (and (looking-at "[ \t]*\\([0-9]+\\)")
       (string-to-number (match-string-no-properties 1))))

;;; Wanderlust
;;; snap://wl-summary-mode/+ME/hira/<20031101192305.AFA8C43EDC@hoge.fuga.piyo>

(snap-with-features (wl)
  (snap-define-op snap-record:wl-summary-mode ()
    (let ((n (wl-summary-message-number)))
      (and (numberp n)
           (let* ((folder wl-summary-buffer-elmo-folder)
                  (fld-name (elmo-folder-name-internal folder))
                  (id (elmo-message-field folder n 'message-id)))
             (snap-encode:wl-summary-mode fld-name id)))))

  (snap-define-op snap-play:wl-summary-mode (spell)
    (let ((prefix-arg 4))
      (wl prefix-arg)) ;; skip folder checking
    (let* ((state (snap-decode:wl-summary-mode spell))
           (fld-name (car state))
           (id (cadr state))
           (summary-buf (wl-summary-get-buffer-create fld-name)))
      (wl-summary-goto-folder-subr fld-name
                                   (wl-summary-get-sync-range
                                    (wl-folder-get-elmo-folder fld-name))
                                   nil nil t)
      (wl-summary-jump-to-msg-by-message-id id)
      (wl-summary-redisplay)))

  (snap-define-op snap-repair:wl-summary-mode (spell)
    (let* ((state (snap-decode:wl-summary-mode spell))
           (id (cadr state))
           (found-file (snap-search-mail id))
           (folder (snap:wl-file-folder found-file)))
      (when (null folder)
        (error "No folder for %s" found-file))
      (snap-encode:wl-summary-mode folder id)))

  (defun snap-encode:wl-summary-mode (folder-name message-id)
    (concat folder-name "/" message-id))

  (defun snap-decode:wl-summary-mode (spell)
    (and (string-match "\\(.*\\)/\\([^/]*\\)" spell)
         (let ((fld-name (match-string-no-properties 1 spell))
               (id (match-string-no-properties 2 spell)))
           (list fld-name id))))

  (defun snap:wl-file-folder (file)
    (setq file (file-truename file))
    (let ((buf (current-buffer)))
      (wl 4)
      (goto-char (point-min))
      (wl-folder-open-all)
      (prog1
          (catch 'found
            (while (not (eobp))
              (let* ((name (wl-folder-get-entity-from-buffer))
                     (folder (wl-folder-search-entity-by-name
                              name
                              wl-folder-entity 'folder))
                     (ef (and folder (wl-folder-get-elmo-folder folder)))
                     (dir (and ef
                               (eq (elmo-folder-type-internal ef) 'localdir)
                               (elmo-localdir-folder-directory-internal ef))))
                (when (and dir
                           (string-match (format "^%s"
                                                 (regexp-quote
                                                  (file-truename dir)))
                                         file))
                  (throw 'found name))
                (forward-line)))
            nil)
        (switch-to-buffer buf))))
  )

;;; Help
;;; snap://help-mode/f/find-file

(snap-with-features (help)
  (snap-define-op snap-record:help-mode ()
    (let ((function (car help-xref-stack-item))
          (variable (car (cdr help-xref-stack-item))))
      (cond
       ((equal function 'describe-function) (format "f/%s" variable))
       ((equal function 'describe-variable) (format "v/%s" variable))
       (help-xref-stack-item help-xref-stack-item)
       (t ""))))

  (snap-define-op snap-play:help-mode (spell)
    (if (string-match "\\([^/\n \t]+\\)/\\(.+\\)" spell)
        (let ((function (match-string 1 spell))
              (variable (match-string 2 spell)))
          (cond
           ((or (string-match "^f.*" function)
                (string-match "descrive-function" function))
            (describe-function (intern variable)))
           ((or (string-match "^v.*" function)
                (string-match "descrive-variable" function))
            (describe-variable (intern variable)))
           (t
            (message "Not support this method %s" spell))))
      (message "I can't all %s" spell)))

  ;; for fake cgi
  (defun snap-play:help-mode: (op val)
    (snap-play-cgi-on "*Help*" op val))
  )

;;; Bookmark
;;; snap://bookmark-bmenu-mode/kuzu

(snap-with-features (bookmark)
  (snap-define-op snap-record:bookmark-bmenu-mode ()
    (bookmark-bmenu-bookmark))

  (snap-define-op snap-play:bookmark-bmenu-mode (spell)
    (if (equal spell "")
        (progn
          (bookmark-bmenu-list)
          (switch-to-buffer "*Bookmark List*"))
      (bookmark-jump spell)))
  )

;;; Man
;;; snap://Man-mode/printf/3

(snap-with-features (man)
  (defvar snap-man-spacer "/")

  (defvar *snap-man-mode-cgi* nil "for internal use")
  (defvar *snap-man-mode-buffer* nil "for internal use")

  (snap-define-op snap-record:Man-mode ()
    (let ((buf (buffer-name)))
      (cond
       ((string-match "^[*]Man[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[*]" buf)
        (concat (match-string 2 buf) snap-man-spacer (match-string 1 buf)))
       ((string-match "^[*]Man[ \t]+\\([^ \t]+\\)[*]" buf)
        (concat (match-string 1 buf)))
       (t
        (error "not support buffer-name of man-mode: %s" buf)))))

  (snap-define-op snap-play:Man-mode (spell)
    (let* ((strs (split-string spell (regexp-quote snap-man-spacer)))
           (str-com (car strs))
           (str-sec (mapconcat 'concat (cdr strs) snap-man-spacer))
           (topic (if (equal str-sec "")
                      (concat str-com)
                    (concat  str-com "(" str-sec ")"))))
      ;; `snap-play:Man-mode:' needs this information.
      (setq *snap-man-mode-buffer* (snap-man-mode-buffer topic))
      (man topic)))

  ;; For fake cgi, we need to adjourn operations
  ;; because man process is run asynchronously.

  (defun snap-play:Man-mode: (op val)
    (if *snap-man-mode-buffer*
        ;; When corresponding man-buffer already exists, `man' just notifies it.
        ;; Man-mode-hook is not run in this case.
        ;; So, we will do our job immediately.
        (snap-play-cgi-on *snap-man-mode-buffer* op val)
      (snap-man-mode-adjourn-cgi op val)))

  (defadvice Man-bgproc-sentinel (around snap-cgi (process msg) activate)
    ad-do-it
    (snap-do-on (snap-man-mode-process-buffer process)
                #'snap-man-mode-play-cgi))

  (defun snap-man-mode-adjourn-cgi (op val)
    (add-to-list '*snap-man-mode-cgi* (cons op val)))

  (defun snap-man-mode-play-cgi (&optional buf)
    (unwind-protect
        (mapcar (lambda (pair)
                  (snap-play-cgi (car pair) (cdr pair)))
                *snap-man-mode-cgi*)
      (setq *snap-man-mode-cgi* nil)))

  ;; copied from man.el

  (defun snap-man-mode-buffer (topic)
    ;; from `Man-getpage-in-background'
    (let* ((man-args topic)
           (bufname (concat "*Man " man-args "*"))
           (buffer  (get-buffer bufname)))
      buffer))

  (defun snap-man-mode-process-buffer (process)
    ;; from `Man-bgproc-sentinel'
    (if (stringp process)
        (get-buffer process)
      (process-buffer process)))

  ;; ;; I used `Man-mode-hook' instead of defadvice at first.
  ;; ;; But, "q" operation failed because Man-mode-hook is run
  ;; ;; inside save-excursion in `Man-bgproc-sentinel'.
  ;; (add-hook 'Man-mode-hook #'snap-man-mode-play-cgi)
  )

;;; Info
;;; snap://Info-mode/cvs#Tracking sources

(snap-with-features (info)
  (defvar snap-info-spacer "#")

  (snap-define-op snap-record:Info-mode ()
    (let ((str-file (if Info-current-file
                        (file-name-nondirectory Info-current-file)
                      ""))
          (str-node (or Info-current-node "")))
      (concat str-file snap-info-spacer str-node)))

  (snap-define-op snap-play:Info-mode (spell)
    (let* ((strs (split-string spell (regexp-quote snap-info-spacer)))
           (str-file (or (car strs) "dir"))
           (str-node (mapconcat 'concat (cdr strs) snap-info-spacer)))
      (Info-goto-node (concat "(" str-file ")" str-node))))
  )

;;; Emacs-wiki
;;; snap://emacs-wiki-mode/WelcomePage#title

(snap-with-features (emacs-wiki)
  (snap-define-op snap-record:emacs-wiki-mode ()
    (let ((raw-path (buffer-file-name)))
      (if (null raw-path)
          nil
        (format "%s" (file-name-nondirectory raw-path)))))

  (snap-define-op snap-play:emacs-wiki-mode (spell)
    (emacs-wiki-visit-link spell))
  )

;;; Navi2ch
;;; snap://navi2ch-article-mode/pc5.2ch.net/test/read.cgi/tech/1068351911/100-200
;;; snap://navi2ch-article-mode/http://pc5.2ch.net/test/read.cgi/tech/1068351911/150

(snap-with-features (navi2ch)
  (defvar snap-navi2ch-set-offline t)

  (snap-define-op snap-record:navi2ch-article-mode ()
    (save-match-data
      (let* ((n (navi2ch-article-get-current-number))
             (s (navi2ch-article-to-url navi2ch-article-current-board
                                        navi2ch-article-current-article
                                        n n t)))
        (when (string-match "^http://" s)
          (setq s (substring s (match-end 0))))
        s)))

  (snap-define-op snap-play:navi2ch-article-mode (spell)
    (when snap-navi2ch-set-offline
      (setq navi2ch-offline t))
    (navi2ch-goto-url (if (string-match "^http://" spell)
                          spell
                        (concat "http://" spell))))
  )

;;; w3m
;;; snap://w3m-mode/http://www

(snap-with-features (w3m)
  (snap-define-op snap-record:w3m-mode ()
    w3m-current-url)

  (snap-define-op snap-play:w3m-mode (spell)
    (w3m spell))
  )

;;; Dired
;;; snap://dired-mode/~/

(snap-with-features (dired)
  (snap-define-op snap-record:dired-mode ()
    (snap-abbreviate-file-name dired-directory))

  (snap-define-op snap-play:dired-mode (spell)
    (snap-find-file spell))
  )

;;; BBDB
;;; snap://bbdb-mode/name

(snap-with-features (bbdb)
  (snap-define-op snap-play:bbdb-mode (spell)
    (bbdb spell nil))

  (snap-define-op snap-record:bbdb-mode ()
    (let ((bbdb-record (bbdb-current-record)))
      (car (bbdb-record-net bbdb-record))))

  (defun snap-play:bbdb-mode: (op val)
    ;; disable fake cgi
    )
  )

;;; Bibtex
;;; snap://bibtex-mode/file#bibtex-key

(snap-with-features (bibtex)
  (defvar snap-bibtex-spacer "#")
  (snap-define-op snap-play:bibtex-mode (spell)
    (if (string-match "^\\(.*\\)#\\(.*\\)$" spell)
        (let ((k (match-string 2 spell)))
          (find-file (match-string 1 spell))
          (and k
               (not (snap-bibtex-search k))
               (message "No such bibtex-key \"%s\"" k)))
      (find-file spell)))
  (defun snap-bibtex-search (k)
    (let ((regexp (concat "^@.*" k)))
      (goto-char (point-max))
      (while (and (re-search-backward regexp nil t)
                  (not (string= k (snap-bibtex-key)))))
      (string= k (snap-bibtex-key))))
  (defun snap-bibtex-key ()
    (save-excursion                     ;c.f. `bibtex-clean-entry'
      (let ((case-fold-search t)
            (eob (bibtex-end-of-entry))
            (head (cond ((boundp 'bibtex-entry-head) ; new
                         bibtex-entry-head)
                        ((boundp 'bibtex-reference-head) ; old
                         bibtex-reference-head)
                        (t
                         (error "Neither bibtex-entry-head nor bibtex-reference-head is defined.")))))
        (bibtex-beginning-of-entry)
        (if (re-search-forward head eob t)
            (buffer-substring-no-properties
             (match-beginning bibtex-key-in-head)
             (match-end bibtex-key-in-head))))))
  (snap-define-op snap-record:bibtex-mode ()
    (let ((f (buffer-file-name))
          (k (snap-bibtex-key)))
      (if k
          (concat f snap-bibtex-spacer k)
        f)))
  )

;;; Shell
;;; snap://shell-mode/~/#pwd

;;; ToDo directory with # is not allowed!

(snap-with-features (shell)
  (defvar snap-shell-spacer "#")
  (defvar snap-shell-buffer-name "*shell*snap*")

  (snap-define-op snap-record:shell-mode ()
    "record now directory and a command now inputed"
    (let ((pm (process-mark (get-buffer-process (current-buffer))))
          (p (point)))
      ;; c.f. comint-kill-input
      (concat default-directory
              (if (> p (marker-position pm))
                  (concat snap-shell-spacer (buffer-substring-no-properties pm p))))))
  (snap-define-op snap-play:shell-mode (spell)
    "1. start shell-mode for snap 2.  insert a command (without
execution)"
    (string-match "\\([^#\r\n]+\\)#?\\(.*\\)" spell)
    (let ((default-directory (match-string-no-properties 1 spell))
          (c (or (match-string-no-properties 2 spell) ""))
          nn no)
      (if (not (comint-check-proc "*shell*"))
          (shell)
        ;;duplicate shell
        (set-buffer "*shell*")
        (setq no (rename-buffer "*shell*" t))
        (shell)
        (setq nn (rename-buffer snap-shell-buffer-name t))
        (set-buffer no)
        (rename-buffer "*shell*" t)
        (set-buffer nn)
        )
      (insert c)))
  )

;;; Occur
;;; snap://occur-mode/dired-mode/~/??q=drwx??g=2
;;; by using "snap://MAJOR-MODE/SPELL??q=word"

(snap-with-features ()
  (defvar snap-occur-cgi-string "q")
  (snap-define-op snap-record:occur-mode ()
    (let* ((b occur-buffer)
           (s (car occur-command-arguments))
           (snap-record-cgi nil)
           (snap-record-default-format "%s")
           (x (snap-decode (save-excursion (set-buffer b) (snap-record-string))))
           (mode (car x))
           (spell (cadr x))
           (snap (snap-encode mode (snap-spell-encode spell (snap-cgi-encode snap-occur-cgi-string s)))))
      (if (string-match (concat "^" snap-prt) snap)
          (substring snap (match-end 0))
        snap)))

  (snap-define-op snap-play:occur-mode (spell)
    (save-window-excursion
      (snap-play-string (concat snap-prt spell)))
    (if (get-buffer "*Occur*")
        (switch-to-buffer "*Occur*")
      (message "maybe failed to match")))
  )

;;; Howm
;;; snap://howm-view-summary-mode/word
;;; snap://howm-view-contents-mode/word
                                        ; checked on howm-test-050518

(snap-with-features (howm)
  (snap-define-op snap-record:howm-view-summary-mode ()
    (howm-view-name))
  (snap-define-op snap-record:howm-view-contents-mode ()
    (howm-view-name))
  (snap-define-op snap-play:howm-view-summary-mode (spell)
    ;; completion-p is always nil in my case.
    (message "howm searching %s ..." spell)
    ;; message is needed because howm-search needs long time.
    (howm-search spell nil))
  (snap-define-op snap-play:howm-view-contents-mode (spell)
    (message "howm searching %s ..." spell)
    (howm-search spell nil))
  )

;;; Gnus
;;; snap://gnus-summary-mode/group/article-number:<20031101.ACDC@hoge.fuga.piyo>

(snap-with-features (gnus gnus-sum)
  (snap-define-op snap-record:gnus-summary-mode ()
    (snap-encode:gnus-summary-mode
     gnus-newsgroup-name
     (gnus-summary-article-number)
     (mail-header-message-id (gnus-summary-article-header))))

  (snap-define-op snap-play:gnus-summary-mode (spell)
    (unless (and (fboundp 'gnus-alive-p) (gnus-alive-p)) (gnus))
    (require 'gnus-score)
    (let* ((state (snap-decode:gnus-summary-mode spell))
           (group (car state))
           (article (cadr state))
           (id (car (cddr state)))
           backend
           ;; cf. gnus-group-quick-select-group
           ;; gnus-visual
           gnus-score-find-score-files-function
           gnus-home-score-file
           gnus-apply-kill-hook
           gnus-summary-expunge-below)
      (setq backend
            (if (string-match "\\([^+]+\\).*:.+" group)
                (match-string 1 group)
              (symbol-name (car gnus-select-method))))
      ;; disable getting new message
      (eval `(let ((,(intern (concat backend "-get-new-mail")) nil))
               (gnus-group-read-group 0 t group)))
      (unless (and
               (gnus-summary-goto-article article nil t)
               (string= id (mail-header-message-id (gnus-summary-article-header))))
        (gnus-summary-goto-article id nil t))))

  (defun snap-encode:gnus-summary-mode (group article id)
    (format "%s/%s:%s" group article id))

  (defun snap-decode:gnus-summary-mode (spell)
    (when (string-match "\\(.*\\)/\\([0-9]+\\):\\([^/]*\\)" spell)
      (list (match-string-no-properties 1 spell)
            (match-string-no-properties 2 spell)
            (match-string-no-properties 3 spell))))
  )

;;; PCVS
;;; snap://cvs-mode/~/hoge/

(snap-with-features (pcvs)
  (snap-define-op snap-play:cvs-mode (spell)
    (cvs-examine spell t))
  (snap-define-op snap-record:cvs-mode ()
    (abbreviate-file-name default-directory))
  )

;;; Thumb
;;; snap://thumbs-mode/~/hoge/
;;; snap://thumbs-view-image-mode/~/tmp.jpg

(snap-with-features (thumbs)
  (snap-define-op snap-record:thumbs-mode ()
    ;; only for `thumbs-show-all-from-dir' not `thumbs-dired-show-marked'.
    (abbreviate-file-name thumbs-current-dir))
  (snap-define-op snap-play:thumbs-mode (spell)
    (thumbs-show-all-from-dir spell nil t))
  (snap-define-op snap-record:thumbs-view-image-mode ()
    (abbreviate-file-name thumbs-current-image-filename))
  (snap-define-op snap-play:thumbs-view-image-mode (spell)
    (if (file-exists-p spell)
        (thumbs-find-image spell)
      (message "No such file:%s" spell)))
  )

;;; Mew

(snap-with-features (mew)
  (snap-define-op snap-record:mew-summary-mode ()
    (let ((folder (mew-summary-folder-name))
          (msgno (mew-summary-message-number))
          (msgid (mew-summary-my-id)))
      (snap-encode:mew-summary-mode folder msgno msgid)))

  (snap-define-op snap-play:mew-summary-mode (spell)
    (let* ((state (snap-decode:mew-summary-mode spell))
           (fld-name (car state))
           (msgno (string-to-number (cadr state)))
           (msgid (nth 2 state))
           (mode (car (snap-decode (thing-at-point 'snap)))))
      (unless mew-init-p (mew))
      (mew-summary-visit-folder fld-name)
      (when (mew-summary-search-msg msgno)
        (when (equal mode "mew-virtual-mode")
          (mew-summary-make-thread))
        (if mew-summary-goto-line-then-display
            (mew-summary-display)))))

  ;; smewを使って、Message-Idから目的のメールの位置を返す。
  ;; ローカルフォルダしかテストしてない。
  (snap-define-op snap-repair:mew-summary-mode (spell)
    (let* ((state (snap-decode:mew-summary-mode spell))
           (fld-name (car state))
           (msgno (cadr state))
           (msgid (nth 2 state)))
      (with-temp-buffer
        (call-process mew-prog-smew nil t nil "-p"
                      (mew-expand-file "+" mew-id-db-file)
                      msgid "")
        (goto-char (point-min))
        (when (looking-at (format "\\(.*\\)/\\([0-9]+\\)\\(%s\\)?"
                                  (regexp-quote mew-suffix)))
          (setq fld-name (concat mew-proto (mew-match-string 1)))
          (setq msgno (mew-match-string 2))))
      (snap-encode:mew-summary-mode fld-name msgno msgid)))

  (defun snap-encode:mew-summary-mode (folder-name msgno message-id)
    (concat folder-name "/" msgno ":" message-id))

  (defun snap-decode:mew-summary-mode (spell)
    (and (string-match "\\(.*\\)/\\([^/]*\\):\\([^/]*\\)" spell)
         (let ((fld-name (match-string-no-properties 1 spell))
               (msgno (match-string-no-properties 2 spell))
               (msgid (match-string-no-properties 3 spell)))
           (list fld-name msgno msgid))))

  (defalias 'snap-play:mew-virtual-mode 'snap-play:mew-summary-mode)
  (defalias 'snap-record:mew-virtual-mode 'snap-record:mew-summary-mode)
  (defalias 'snap-repair:mew-virtual-mode 'snap-repair:mew-summary-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cgi extension
;;;
;;; Examples:
;;;|goto-line       |snap:///file??g=110
;;;|search&move str |snap:///file??s=str
;;;|occur str       |snap:///file??q=str
;;;|dired-x (file)  |snap:///??x=file
;;;|dired-x (buffer)|snap:///??x=
;;;|open & dired-x  |snap:///file??x=
;;;|open &dired-x   |snap:///dir??x=file
;;;|find & dired-x  |snap:///dir??s=str&x=
;;;|move & dired-x  |snap:///dir??g=10&x=
;;;
;;; ToDo: find and compilation

(defun snap-play-dired-x (file)
  ""
  (let ((dir (or (file-name-directory file) default-directory))
        (filename (file-name-nondirectory file))
        (font-lock-global-modes nil))
    (save-excursion
      (find-file dir)
      (goto-char (point-min))
      (search-forward-regexp (concat "[ ]" (regexp-quote filename) "$") nil)
      (call-interactively 'dired-do-shell-command)
      (bury-buffer))))

(defun snap-play::x (spell)
  "snap-record cgi extension for execute"
  (if (or (null spell) (string= "" spell))
      (cond
       (buffer-file-name
        (snap-play-dired-x buffer-file-name))
       ((eq major-mode 'dired-mode)
        (call-interactively 'dired-do-shell-command))
       (t
        (message "error")))
    (cond
     ((or (file-exists-p spell) (eq major-mode 'dired-mode))
      (snap-play-dired-x spell))
     (buffer-file-name
      (snap-play-dired-x buffer-file-name))
     (t
      (message "error")))))
(defun snap-record::g ()
  "snap-record cgi extension for goto-line"
  (number-to-string (snap-line-number)))
(defun snap-play::g (spell)
  "snap-record cgi extension for goto-line"
  (goto-line (string-to-number spell)))
(defun snap-record:: ()
  "snap-record cgi extension for default tag"
  (number-to-string (snap-line-number)))
(defun snap-play:: (spell)
  "snap-record cgi extension for default tag"
  (goto-line (string-to-number spell)))
(defun snap-record::s ()
  "snap-record cgi extension for search return the string of
kill-ring. (not work. help) "
  (cond
   ;;  ((eq last-command 'kill-ring-save)
   ;;    (remove-text-properties (current-kill 0))
   ;;    )
   (t
    (save-excursion
      (beginning-of-line)
      (looking-at "^[ \t]*\\(.*\\)")
      (match-string-no-properties 1)))))
(defun snap-play::s (spell)
  "snap-play cgi extension for search around point"
  (or (search-forward spell nil t)
      (progn (goto-char (point-max))
             (search-backward spell nil t))
      (message "Failed search")))
(defun snap-record::q ()
  "snap-record cgi extension for search

return 1. the string of kill-ring.  (not yet)

2. the word at cursor."
  (cond
   ;;   ((eq last-command 'kill-ring-save)
   ;;    (remove-text-properties (current-kill 0))
   ;;    )
   ((provide 'thingatpt)
    (or (thing-at-point 'word) (thing-at-point 'symbol)))
   (t
    nil)))

(defun snap-play::q (spell)
  "snap-play cgi extension for occur"
  (occur spell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with other tools

;;; with action-lock.el
;;; (in howm: <http://howm.sourceforge.jp/>)

(snap-with-features (action-lock)
  (defun snap-action-lock (regexp arg-pos &optional hilit-pos)
    (action-lock-general #'(lambda (f u)
                             (call-interactively 'snap-play))
                         regexp arg-pos hilit-pos t))
  )

(eval-after-load "action-lock"
  '(let ((snap-action-lock-rules (list (snap-action-lock snap-regexp 0))))
     (setq action-lock-default-rules
           (append snap-action-lock-rules action-lock-default-rules))))

;;; with bookmark

(snap-with-features (bookmark)
  (defadvice bookmark-buffer-file-name 
    (around with-snap first () disable)
    "Extend it's function for snap protocol"
    ad-do-it
    (unless ad-return-value
      (setq ad-return-value (snap-record-string))))
  (defadvice bookmark-jump-noselect (around with-snap first (str) disable)
    "Extend it's function for snap protocol with the help of
`snap-bookmark-jump-noselect'.

Suppose `bookmark-jump-noselect' has (str) as inputs and
returns (BUFFER . POINT)
"
    (bookmark-maybe-load-default-file)
    (let* ((str (ad-get-arg 0))
           (url (bookmark-get-filename str)))
      (cond 
       ((string-match snap-regexp url)
        (setq ad-return-value (snap-bookmark-jump-noselect str)))
       (t ad-do-it))))
  (defun snap-bookmark-jump-noselect (str)
    (let* ((url (bookmark-get-filename str))
           (snap-p (string-match snap-regexp url))
;;            (file (if snap-p url (expand-file-name url)))
           (forward-str (bookmark-get-front-context-string str))
           (behind-str (bookmark-get-rear-context-string str))
;;            (place (bookmark-get-position str))
;;            (info-node (bookmark-get-info-node str))
;;            (orig-file file)
           )
      (if snap-p
          (save-excursion
            (save-window-excursion
              (snap-play-string url)
              (when (and forward-str
                         (search-forward forward-str (point-max) t))
                (goto-char (match-beginning 0)))
              (when (and behind-str
                         (search-backward behind-str (point-min) t))
                (goto-char (match-end 0)))
              (setq bookmark-current-bookmark str)
              (cons (current-buffer) (point))))
        (ding))))
  )

;;; with ffap

(snap-with-features (ffap)
  (defvar snap-ffap-url-regexp
    (concat
     "\\`\\("
     "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
     "\\|"
     "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\|snap\\)://" ; needs host
     "\\)."                             ; require one more character
     ))
  (defvar snap-ffap-url-fetcher 'snap-ffap-browse-url)
  (defun snap-ffap-browse-url (url &rest args)
    "Deal with a snap protocol in addition to the function `browse-url'"
    (if (string-match snap-regexp url)
        (snap-play-string url)
      (browse-url url args)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; provide

(provide 'snap)

;;; snap.el ends here.
