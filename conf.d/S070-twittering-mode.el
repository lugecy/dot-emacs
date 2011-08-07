;;;; for twittering-mode
(defun ly:twit-init ()
  (add-to-list 'load-path (expand-file-name "~/src/twittering-mode/"))
  (require 'twittering-mode)
  (safe-loading "twittering-manage-unread")
  (safe-loading "twittering-scroll-ext")
  (define-key twittering-mode-map (kbd "F") 'twittering-switch-to-previous-timeline)
  (define-key twittering-mode-map (kbd "b") 'twittering-scroll-down)
  (define-key twittering-mode-map (kbd "y") 'twittering-favorite)
  (define-key twittering-mode-map (kbd ";") 'bm-toggle)
  (ly:eval-after-load 'jaunte
    (define-key twittering-mode-map (kbd "e") 'jaunte-twittering))
  (setq twittering-auth-method 'xauth
        twittering-username "lugecy")
  (setq twittering-fill-column 80)
  (setq twittering-icon-mode t)
  (setq twittering-use-native-retweet t)
  (setq twittering-convert-program "e:/cygwin/bin/convert.exe")
  (setq twittering-use-convert t)
  (setq twittering-timer-interval 60)
  (setq twittering-display-remaining t)
  (twittering-enable-unread-status-notifier)
  (setq twittering-convert-fix-size 32)
  (add-hook 'twittering-mode-hook 'ly:twit-setup-modeline))

(defun ly:twit-init-and-start ()
  (interactive)
  (ly:web-clientize)
  (ly:twit-init)
  (twit))

;;;; for mode-line
(defun ly:twit-current-status-linum ()
  (loop with bpos = (twittering-get-current-status-head (point))
        for pos = bpos then (twittering-get-previous-status-head pos)
        while pos
        count pos))

(defun ly:twit-status-linum ()
  (format "(sl:%d)" (ly:twit-current-status-linum)))

(defun ly:twit-setup-modeline ()
  (add-to-list 'mode-line-buffer-identification '(:eval (ly:twit-status-linum)) t))
