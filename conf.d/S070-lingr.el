;;;; for lingr.el configure
(defun ly:lingr-init ()
  (interactive)
  (add-to-list 'load-path (expand-file-name "~/src/lingr-el"))
  (require 'lingr)
  (setq lingr-username "lugecy")
  ;; (setq lingr-debug t)
  (setq lingr-say-window-height-per 30)
  (setq lingr-icon-mode t)
  (setq lingr-http-use-wget t)
  (ly:eval-after-load 'viper
    (add-to-list 'viper-emacs-state-mode-list 'lingr-room-mode))
  (defvar growl-program-path "E:/Program/Growl for Windows/growlnotify.exe")
  ;; (add-hook 'lingr-message-hook 'lingr-growl-on-message)
  (defvar lingr-hook-last-join-username nil)

  (defadvice turn-on-undo-tree-mode (around disable-on-lingr-mode activate)
    (unless (eq major-mode 'lingr-room-mode)
      ad-do-it))
  (defadvice lingr-say-command (after enable-undo-tree activate)
    (when (and (not (minibufferp))
               (featurep 'undo-tree))
      (undo-tree-mode 1)
      (auto-complete-mode 1))))

(defun lingr-growl-on-message (message)
  (when (and (file-executable-p growl-program-path)
             (member (lingr-message-room message) '("emacs" "editor")))
    (start-process "Growl from lingr.el" nil growl-program-path
                   (format "/t:Lingr-%s" (lingr-message-room message)) ; "/i:E:\Document\Pictures\lingr_logo_100x42.png"
                   (format "%s: update message from %s"
                           (lingr-message-room message)
                           (lingr-message-nick message)))))

(defun lingr-growl-on-join (presence)
  (when (and (file-executable-p growl-program-path)
             (not (string= lingr-hook-last-join-username (lingr-presence-username presence)))
             (member "emacs" (lingr-get-rooms-by-username (lingr-presence-username presence))))
    (start-process "Growl from lingr.el" nil growl-program-path
                   (format "/t:Lingr-%s" (lingr-presence-room presence))
                   (format "%s: %s join."
                           (lingr-presence-room presence)
                           (lingr-presence-username presence))))
  (setq lingr-hook-last-join-username (lingr-presence-username presence)))
;; (add-hook 'lingr-join-hook 'lingr-growl-on-join)
