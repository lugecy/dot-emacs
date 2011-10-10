;;;; one-line scroll
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t) ; scroll-up/downでカーソル位置を保持する

;;;; なぜか半スクロールになるのを回避
(defun sane-newline (arg)
  "Put newline\(s\) by ARG with scrolling sanely if needed."
  (interactive "p")
  (if (and (boundp 'skk-henkan-mode) skk-henkan-mode) ; skk変換確定のとき変にインデントされてしまう対策
      (newline arg)
    (let ((newpt (save-excursion (newline arg) (indent-according-to-mode) (point))))
      (while (null (pos-visible-in-window-p newpt)) (scroll-up 1))
      (goto-char newpt)
      (setq this-command 'newline)
      ())))
