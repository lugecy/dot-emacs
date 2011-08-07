(require 'historyf)
(setq historyf-limit 10)
(add-to-list 'historyf-minor-modes 'clmemo-mode)
(global-set-key (kbd "C-x p") 'historyf-back)
(global-set-key (kbd "C-x P") 'historyf-forward)

;;;; for escreen
(ly:eval-after-load 'escreen
  (defun escreen-map-save-historyf ()
    (list historyf-mark historyf-history historyf-forward-temp))
  (defun escreen-map-restore-historyf (data)
    (setq historyf-mark (nth 0 data)
          historyf-history (nth 1 data)
          historyf-forward-temp (nth 2 data)))
  (add-to-list 'escreen-map-data-format '(escreen-map-save-historyf . escreen-map-restore-historyf) t)
  (defadvice escreen-create-screen (after historyf activate)
    (setq historyf-mark nil
          historyf-history nil
          historyf-forward-temp nil)))

;;;; for anything
(defvar anything-c-source-historyf
  '((name . "Historyf")
    (candidates . (lambda ()
                    (loop with lst = (cons (or historyf-forward-temp
                                               (with-current-buffer anything-current-buffer
                                                 (historyf-make-history)))
                                           historyf-history)
                          for x in lst
                          for i = (- (or (position historyf-mark lst) 0)) then (1+ i)
                          when x
                          collect (cons (format "%3d: %s" i (or (get-file-buffer (cdr x))
                                                                (cdr x)))
                                        i))))
    (action . (lambda (num)
                (let ((func (if (> num 0) 'historyf-back 'historyf-forward)))
                  (dotimes (i (abs num))
                    (funcall func)))))))
(defun anything-historyf ()
  (interactive)
  (anything :sources 'anything-c-source-historyf
            :buffer "*anything historyf*"
            :preselect (format "^%3d:" 0)))
(global-set-key (kbd "C-x C-:") 'anything-historyf)
