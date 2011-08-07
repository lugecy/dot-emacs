;; auto-vc-checkin.el --- automatic checking-in minor mode used vc
;;
;;	$Id: auto-vc-checkin.el,v 1.72 2003/10/24 05:22:16 gnrr Exp $
;;
;; auto-vc-checkin.el comes with ABSOLUTELY NO WARRANTY.
;; This software is distributed under the GNU General Public License.

;;; Commentary:

;; What is this:
;;   This minor mode provides automatic checking-in used vc-mode.
;;   Checking-in may be executed once every doing save-buffer. Though,
;;   you can adjust that checking-in once how many times to save-buffer.
;;
;; Features:
;;   * adjustable frequency to checking-in
;;   * supports RCS, CVS, SVN(Subversion)
;;   * working indicator in mode line
;;
;; Install:
;;   Put this file into your "load-path" directory, and byte compile
;;   it if desired. And put the following expressions into your
;;   dot.emacs.
;;
;;     (require 'auto-vc-checkin)
;;
;;     ;; frequency to checking-in
;;     ;; in this case, checking-in once at saving-buffer 3 times.
;;     (setq auto-vc-checkin-full-count 3)
;;
;;     ;; what kind of backends should be enabled
;;     (setq auto-vc-checkin-backend '(RCS CVS SVN))
;;
;;     ;; you don't like default comment? ok, write as you like.
;;     (setq auto-vc-checkin-comment
;;       "-- automatic checking in --")
;;
;;     ;; if you get something wrong about vc, you might figure it out
;;     ;; as below. (this setting means "don't touch mode line")
;;     (setq auto-vc-checkin-disp-mode-line nil)
;;
;; Usage:
;;   Auto-vc-checkin starts to work just following enabled vc. You can
;;   see it by sign following revision number in mode line.
;;
;;    e.g.  RCS:1.2.8+
;;                   ^
;;   "+" indicates auto-vc-checkin is ON. Alternatively, "-" indicates
;;   it's OFF. You can turn it ON/OFF by typing "C-x v -" in the
;;   meantime (using vc default key bindings).
;;
;;   Checking-in will be executed automatically, when it's time to
;;   save.
;;
;; Environment:
;;   tested under combination below.
;;     vc.el		1.360
;;     vc-hooks.el	1.160

;;; Todo:

;;   1. unvisible indicator after manual checking in/out

;;; Code:

;;
;; requirements
(require 'vc)		;; needs for vc-checkin and vc-checkout
(require 'vc-hooks)	;; needs depends on vc.el version

;;
;; user variable
;;
(defvar auto-vc-checkin-full-count 10
  "*Number by checking-in for every count of times to save.")

(defvar auto-vc-checkin-backend ()
  "*List of backends which should be enabled auto-vc-checkin.")

(defvar auto-vc-checkin-comment
  "::: AUTOMATIC :::"
  "*Comment string used for log message automatically.")

(defvar auto-vc-checkin-disp-mode-line t
  "*Non-nil means displaying \"+\" or \"-\" follows revision number in mode line.
Otherwise, nil means NOT-displaying it (leave mode line as vc's default).\n\n
\"+\" indicates auto-vc-checkin is turned on (working).
\"-\" indicates auto-vc-checkin is turned off (NOT-working).")

;;
;; internal variable
;;
(defvar auto-vc-checkin-save-count 1)
(make-variable-buffer-local 'auto-vc-checkin-save-count)

(defvar auto-vc-checkin nil
  "Mode variable for auto-vc-checkin minor mode.")
(make-variable-buffer-local 'auto-vc-checkin)

;;
;; functions
;;

;; checkin routine
(defun auto-vc-checkin-do-checking-in (backend)
  (let ((comment auto-vc-checkin-comment))
    (cond
     ;; RCS: checkin then checkout
     ((eq backend 'RCS)
      (vc-checkin (buffer-file-name (current-buffer)) nil comment)
      (vc-checkout (buffer-file-name (current-buffer)) t)
      (message "checked in automatically.")
      t)
     ;; CVS or SVN (Subversion): checkin only
     ((or (eq backend 'CVS) (eq backend 'SVN))
      (vc-checkin (buffer-file-name (current-buffer)) nil comment)
      (message "checked in automatically.")
      t)
     ;; other: do nothing
     (t nil)
     )))

;; display working-state follows revision number in mode line.
(defun auto-vc-checkin-disp-mode-line (sw)
  (when auto-vc-checkin-disp-mode-line
    (let (vs)
      (setq vs (substring vc-mode 0 (string-match "[+-]$" vc-mode)))
      (setq vc-mode (concat vs (if sw "+" "-"))))))

;; entrance
(defun auto-vc-checkin (&optional arg)
  "Turn on/off auto-vc-checkin by each buffers."
  (interactive "P")
  (setq auto-vc-checkin
	(if (null arg)
	    (not auto-vc-checkin)
	  (> (prefix-numeric-value arg) 0)))
  (if auto-vc-checkin
      ;; enable
      (progn
	(ad-enable-advice 'save-buffer 'after 'auto-vc-checkin-adv)
	(ad-activate 'save-buffer))
    ;; disable
    (ad-disable-advice 'save-buffer 'after 'auto-vc-checkin-adv)
    (ad-activate 'save-buffer))
  ;; indication
  (auto-vc-checkin-disp-mode-line auto-vc-checkin)
  (force-mode-line-update)
  (unless auto-vc-checkin-disp-mode-line
    (message "auto-vc-checkin: %s" (if auto-vc-checkin "on" "off"))
    nil))

;;
;; hooks and advices
;;

;; just behind switching vc
(defadvice vc-find-file-hook (after auto-vc-checkin-adv-1 activate)
  (when vc-mode
    (auto-vc-checkin 1)
    (define-key vc-prefix-map "-" 'auto-vc-checkin)))

;; en/disable by function "auto-vc-checkin"
(defadvice save-buffer (after auto-vc-checkin-adv disable)
  (when vc-mode
    (let* ((filename (buffer-file-name (current-buffer)))
	   (backend (vc-backend filename)))
      (when (memq backend auto-vc-checkin-backend)
	(if (or (> auto-vc-checkin-full-count auto-vc-checkin-save-count)
		(vc-workfile-unchanged-p filename))
	    ;; increment save-count
	    (setq auto-vc-checkin-save-count (1+ auto-vc-checkin-save-count))
	  ;; do checking-in
	  (let (ulist buffer-undo-list)		;; save undo-info
	    (auto-vc-checkin-do-checking-in backend)
	    (setq buffer-undo-list ulist))	;; revert undo-info
	  ;; indicator update
	  (auto-vc-checkin-disp-mode-line auto-vc-checkin)
	  (force-mode-line-update)
	  (setq auto-vc-checkin-save-count 1)
	  )))
    ))

;;;
;;; end
;;;

(provide 'auto-vc-checkin)

;;; auto-vc-checkin.el ends here
