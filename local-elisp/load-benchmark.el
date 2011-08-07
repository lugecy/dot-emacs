;
;;;; 読み込み速度測定用
;
;Petit emacs lisp tips on .emacs.el
;http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html
(defconst my-time-zero (current-time))
(defvar my-time-list nil)

(defun my-time-lag-calc (lag label)
  (if (assoc label my-time-list)
      (setcdr (assoc label my-time-list)
              (- lag (cdr (assoc label my-time-list))))
    (setq my-time-list (cons (cons label lag) my-time-list))))

(defun my-time-lag (label)
  (let* ((now (current-time))
         (high (- (car now) (car my-time-zero)))
         (low (- (car (cdr now)) (car (cdr my-time-zero))))
         (msec (/ (- (car (cdr (cdr now)))
                     (car (cdr (cdr my-time-zero))))
                  1000))
         (lag (+ (* 1000 (+ low (* high (expt 2 16)))) msec)))
    (my-time-lag-calc lag label)))

(defun my-time-lag-print ()
  (message (prin1-to-string
            (sort my-time-list
                  (lambda  (x y)  (> (cdr x) (cdr y)))))))
(add-hook 'after-init-hook (lambda () (my-time-lag-print)) t)
;(add-hook 'window-setup-hook (lambda () (my-time-lag-print)) t)
(provide 'load-benchmark)
