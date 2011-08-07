(autoload 'sdic-describe-word
  "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point
  "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)
;; 文字色
(setq sdic-face-color "pink")
;; ;; 英和検索で使用する辞書
;; (setq sdic-eiwa-dictionary-list
;;       `((sdicf-client
;;          ,(expand-file-name "../../etc/sdic/gene.sdic" (invocation-directory)))
;;         (sdicf-client
;;          ,(expand-file-name "../../etc/sdic/eedict.sdic" (invocation-directory)))))
;; ;; 和英検索で使用する辞書
;; (setq sdic-waei-dictionary-list
;;       `((sdicf-client
;;          ,(expand-file-name "../../etc/sdic/jedict.sdic" (invocation-directory)))))
