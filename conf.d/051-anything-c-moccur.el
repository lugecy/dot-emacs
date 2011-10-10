(require 'anything-c-moccur)
(define-key isearch-mode-map "\M-i" 'anything-c-moccur-from-isearch) ;; isearch-modeから移行する
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern nil) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

(defadvice anything-c-moccur-occur-by-moccur-base (around override-anything-limit activate)
  (let ((anything-candidate-number-limit 2000))
    ad-do-it))

(defadvice anything-c-moccur-from-isearch (before from-isearch activate)
  (isearch-exit))

(defun anything-c-moccur-toggle-auto-look ()
  (interactive)
  (setq anything-c-moccur-enable-auto-look-flag
        (if anything-c-moccur-enable-auto-look-flag nil t))
  (minibuffer-message "Toggle auto-look-flag %s"
                      (if anything-c-moccur-enable-auto-look-flag "on" "off")))
(define-key anything-c-moccur-anything-map (kbd "C-c C-f") 'anything-c-moccur-toggle-auto-look)

;;;; for temporary initial pattern
(defadvice anything-c-moccur-occur-by-moccur (around prefix-init-patten activate)
  "Minus argument enable initial-pattern."
  (if (< (prefix-numeric-value current-prefix-arg) 0)
      (let ((anything-c-moccur-enable-initial-pattern t))
        (ad-set-args 0 nil)
        ad-do-it)
    ad-do-it))
