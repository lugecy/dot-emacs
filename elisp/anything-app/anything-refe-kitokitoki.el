(defvar anything-refe-index-file nil)
(defvar anything-refe-command "refe")
(defvar anything-c-source-refe
  `((name . "refe index")
    (candidates-file . anything-refe-index-file)
    (action ("Show" . anything-refe-action))))
(defvar anything-c-source-refe-raw
  '((name . "refe command")
    (dummy)
    (action ("Show" . anything-refe-action))))

(defun anything-refe-action (word)
  (let ((buf-name (concat "*refe:" word "*")))
    (with-current-buffer (get-buffer-create buf-name)
      (let (;(default-process-coding-system '(euc-jp . utf-8))
            (coding-system-for-read 'euc-jp))
        (call-process anything-refe-command nil t t word))
      (goto-char (point-min))
      (view-buffer-other-window buf-name t
                                   (lambda (dummy)
                                     (kill-buffer-and-window))))))

(defun anything-refe ()
  (interactive)
  (anything-other-buffer
   (list anything-c-source-refe anything-c-source-refe-raw)
   "anything-refe"))

;; view-buffer-other-window の switch-to-buffer-other-window を switch-to-buffer にしたもの. letf でもよい.
(defun my-view-buffer-other-window (buffer &optional not-return exit-action)
  (let* ((win				; This window will be selected by
          (get-lru-window))		; switch-to-buffer-other-window below.
         (return-to
          (and (not not-return)
               (cons (selected-window)
                     (if (eq win (selected-window))
                         t			; Has to make new window.
                       (list
                        (window-buffer win)	; Other windows old buffer.
                        (window-start win)
                        (window-point win)))))))
    (switch-to-buffer buffer) ;変更
    (view-mode-enter (and return-to (cons (selected-window) return-to))
                     exit-action)))
