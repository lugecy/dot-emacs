;; (defadvice twittering-render-timeline (around unread-bm-mark activate)
;;   (let* ((buf (ad-get-arg 0))
;;          (unread-mark (make-marker))
;;          unread-bm)
;;     (with-current-buffer buf
;;       (save-excursion
;;         (goto-char (point-min))
;;         (setq unread-bm (cadr (bm-lists 'forward)))
;;         (set-marker unread-mark (overlay-start unread-bm) (current-buffer))))
;;     ad-do-it
;;     (with-current-buffer buf
;;       (bm-bookmark-remove unread-bm)
;;       (save-excursion
;;         (goto-char unread-mark)
;;         (bm-bookmark-add)
;;         (setq ly:twit-unread-id (twittering-get-id-at))))
;;     (set-marker unread-mark nil)))
;; (ad-disable-advice 'twittering-render-timeline 'around 'unread-bm-mark)
;; (ad-activate 'twittering-render-timeline)

(defvar ly:twit-unread-id nil)
(defvar ly:twit-unread-border-overlay nil)
(defface ly:twit-unread-face
  '((t (:background "SeaGreen")))
  "For twittering-mode unread border face.")

(defadvice twittering-render-timeline (after unread-bm-mark activate)
  "for unread management"
  (unless ly:twit-unread-id
    (ly:twit-load-status))
  (when ly:twit-unread-id
    (let ((buf (ad-get-arg 0)))
      (with-current-buffer buf
        (unless (and (overlayp ly:twit-unread-border-overlay)
                     (overlay-buffer ly:twit-unread-border-overlay))
          (let ((pos (point-min)))
            (while (and pos
                        (not (twittering-status-id= (twittering-get-id-at pos)
                                                    ly:twit-unread-id)))
              (setq pos (twittering-get-next-status-head pos)))
            (when pos
              (ly:twit-unread-mark pos 'norec))))))))

(defun ly:twit-make-unread-border-overlay ()
  (let ((ov (make-overlay (point-min) (1+ (point-min)))))
    (overlay-put ov 'face 'ly:twit-unread-face)
    (overlay-put ov 'evaporate t)
    ov))

(defun ly:twit-unread-mark (&optional pos norecord)
  (interactive)
  (unless ly:twit-unread-border-overlay
    (set (make-local-variable 'ly:twit-unread-border-overlay)
         (ly:twit-make-unread-border-overlay)))
  (let ((pos (or pos (twittering-get-current-status-head))))
    (save-excursion
      (goto-char pos)
      (move-overlay ly:twit-unread-border-overlay (point-at-bol) (1+ (point-at-eol)))))
  (when (and (not norecord)
             (or (string= ":home" (twittering-current-timeline-spec-string))
                 (y-or-n-p "This timeline is no :home, save unread-id?")))
    (setq ly:twit-unread-id (twittering-get-id-at))
    (ly:twit-save-status)))
(define-key twittering-mode-map (kbd "@") 'ly:twit-unread-mark)

(defun ly:twit-unread-mark-norecord ()
  (interactive)
  (ly:twit-unread-mark nil 'norec))
(define-key twittering-mode-map (kbd "/") 'ly:twit-unread-mark-norecord)

(defun ly:twit-goto-unread-border ()
  (interactive)
  (if (and (overlayp ly:twit-unread-border-overlay)
           (overlay-buffer ly:twit-unread-border-overlay))
      (goto-char (overlay-start ly:twit-unread-border-overlay))
    (message "Unread Border not fetched yet.")))
(define-key twittering-mode-map (kbd ":") 'ly:twit-goto-unread-border)

(defvar ly:twit-save-status-filename (expand-file-name "~/.emacs.d/twit-last-unread"))
(defun ly:twit-save-status ()
  (interactive)
  (when ly:twit-unread-id
    (with-temp-buffer
      (let ((alist `((unread-id . ,ly:twit-unread-id))))
        (insert (format "%S" alist))
        (write-region (point-min) (point-max) ly:twit-save-status-filename nil 0)
        (message "Save twit unread-id.")))))

(defun ly:twit-load-status ()
  (interactive)
  (when (file-exists-p ly:twit-save-status-filename)
    (with-temp-buffer
      (insert-file-contents ly:twit-save-status-filename)
      (goto-char (point-min))
      (let ((alist (read (current-buffer))))
        (setq ly:twit-unread-id (assoc-default 'unread-id alist))))))
