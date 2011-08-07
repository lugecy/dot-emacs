(defun ly:twit-content-view-scroll ()
  (interactive)
  (let* ((win (get-buffer-window (current-buffer)))
         (win-start (window-start win))
         (next-pos (twittering-get-next-status-head win-start)))
    (when (and (not (= win-start (point-min)))
               (get-text-property (1- win-start) 'id))
      (set-window-start win next-pos))))

(defun ly:twit-content-view-next ()
  (interactive)
  (let* ((next-pos (twittering-get-next-status-head (point)))
         (h 0))
    (when (and next-pos
               (not (pos-visible-in-window-p next-pos)))
      (save-excursion
        (while (and (eq (vertical-motion 1) 1)
                    (not (>= (point) (1- next-pos))))
          (incf h)))
      (when (> h 0)
        (recenter (- h))))))

(defadvice twittering-goto-next-status (after content-view activate)
  (ly:twit-content-view-next))

(defadvice twittering-scroll-down (after content-view activate)
  (ly:twit-content-view-scroll))

(defadvice twittering-scroll-up (around content-view activate)
  (let* ((we-pos (window-end))
         (we-st-pos (twittering-get-previous-status-head we-pos)))
    ad-do-it
    (and we-st-pos (set-window-start (selected-window) we-st-pos))))
