;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ツールバーを消す
(tool-bar-mode -1)
;;;; スクロールバーを左に表示
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'left))

;;;; Visual Bell を使う
(setq visible-bell t)

;;;; region の色指定(viper-visual-modeと区別がつくように)
(set-face-background 'region "RoyalBlue3")

;;;; 行数・桁を表示
;;(line-number-mode t)
(column-number-mode t)

;;;; 行番号を表示
;; (when (require 'linum nil t)
;;   (setq linum-delay t)
;;   (setq linum-eager nil))

;;;; 対応する括弧を強調表示
(show-paren-mode t)
(setq show-paren-style 'mixed)

;;;; 行末の空白を強調表示
;; (setq-default show-trailing-whitespace t)
(defun ly:turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
                    'ruby-mode-hook 'c-mode-common-hook 'python-mode-hook
                    'sh-mode-hook 'makefile-mode-hook
                    'change-log-mode-hook 'howm-mode-hook 'diff-mode-hook))
  (add-hook hook 'ly:turn-on-show-trailing-whitespace))

;;;; for whitespace-mode
(eval-after-load "whitespace"
  '(progn
     (set-face-background 'whitespace-tab "snow4")
     (setq whitespace-style (delq 'tab-mark whitespace-style))))

;;;; completionバッファの色付け
;;;; http://homepage1.nifty.com/blankspace/emacs/tips.html
(defadvice display-completion-list (after display-completion-list-highlight activate)
  (let* ((str-list (mapcar (lambda(x) (cond ((stringp x) x)
                                            ((symbolp x) (symbol-name x))
                                            ((listp x) (concat (car x)
                                                               (cadr x)))))
                           (ad-get-arg 0)))
         (str (car str-list)))
    (mapc (lambda (x)
            (while (or (> (length str) (length x))
                       (not (string= (substring str 0 (length str))
                                     (substring   x 0 (length str)))))
              (setq str (substring str 0 -1))))
          str-list)
    (save-current-buffer
      (set-buffer "*Completions*")
      (save-excursion
        (re-search-forward "Possible completions are:" (point-max) t)
        (while (re-search-forward (concat "[ \n]\\<" str) (point-max) t)
          (let ((o1 (make-overlay (match-beginning 0) (match-end 0)))
                (o2 (make-overlay (match-end 0)       (1+ (match-end 0)))))
            (overlay-put o1 'face '(:foreground "HotPink3"))
            (overlay-put o2 'face '(:foreground "white" :background "DeepSkyBlue4"))))))))

;;;; バッファの行数をモードラインに表示する
(loop for s on mode-line-position
      if (eq (caar s) 'size-indication-mode)
      do (setcdr s (cons '(:eval (format "/%d" (count-lines (point-min) (point-max)))) (cdr s)))
      and return nil)

