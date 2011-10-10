(require 'sequential-command)
(define-sequential-command seq-home
  lugecy-beginning-of-line back-to-indentation beginning-of-buffer seq-return)
(define-sequential-command seq-end
  end-of-line back-to-comment-beginning end-of-buffer seq-return)

(defun back-to-comment-beginning ()
  "Move point to the beginning comment position in current line."
  (interactive)
  (when comment-start-skip
    (end-of-line)
    (comment-search-backward (line-beginning-position) t)))

;;;; seq-move-window-line
(defun lugecy-move-to-window-top ()
  (interactive)
  (move-to-window-line 0))
(defun lugecy-move-to-window-bottom ()
  (interactive)
  (move-to-window-line -1))
(define-sequential-command seq-move-to-window-line
  move-to-window-line lugecy-move-to-window-top lugecy-move-to-window-bottom seq-return)
