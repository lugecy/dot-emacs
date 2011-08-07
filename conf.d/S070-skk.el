;;;; skk.el
;; [2009-01-22] snapshot20080907 と入れ替える
;; [2009-03-19] snapshot-20090309
(when (require 'skk-autoloads nil t)
  (global-set-key "\C-x\C-j" 'skk-mode)
  (global-set-key "\C-xj" 'skk-auto-fill-mode)
  (setq skk-share-private-jisyo t)
  (setq skk-large-jisyo nil)
  (setq skk-use-azik t)
  (setq skk-egg-like-newline t)
  (setq-default skk-kutouten-type 'en)
  (setq skk-show-annotation t)          ; 注釈を表示
  (setq skk-dcomp-activate t)           ; 動的補完モード有効(単体)
  ;;(setq skk-dcomp-show-multiple t) ; snapshot20080907 動的補完複数表示有効
  (setq skk-dcomp-multiple-activate t)
  ;;(setq skk-dcomp-show-multiple-rows 7) ; default:7
  (setq skk-isearch-start-mode 'latin)
  (setq skk-auto-okuri-process t)
  (setq skk-henkan-strict-okuri-precedence t)
  ;;(setq skk-jisyo-save-count 10)
  ;; (setq skk-show-inline t)
  (setq skk-show-inline 'vertical)
  (setq skk-undo-kakutei-word-only t)   ; 確定した結果のみを表面化するようにする
  (eval-after-load "viper"
    '(progn
       (setq skk-use-viper t)))
  ;; なぜこれをviperで外しているのだろう・・・
  ;; (char-to-string <char>)という指定ではEmacs23(おそらくlanguage-environmentがUTF-8)ではダメみたい
  ;; (char-to-string skk-start-henkan-with-completion-char) => "_" とか
  (defun ly:skk-init-j-mode-map ()
    (define-key skk-j-mode-map
      ;;(char-to-string skk-backward-and-set-henkan-point-char) ;; Emacs23ではダメ
      (kbd "M-Q") 'skk-backward-and-set-henkan-point)
    (define-key skk-j-mode-map
      ;;(char-to-string skk-start-henkan-with-completion-char) ;; Emacs23ではダメ
      (kbd "M-SPC") 'skk-comp-start-henkan)
    (define-key skk-j-mode-map
      (kbd "C-SPC") 'skk-comp-start-henkan) ;; 無変換 = Ctrlに移行したので
    (define-key skk-j-mode-map
      (kbd "M-U") 'skk-undo-kakutei)
    (define-key skk-j-mode-map
      (kbd "+") 'skk-set-henkan-point-subr))
  (add-hook 'skk-load-hook 'ly:skk-init-j-mode-map)
  ;; skk +.azik の場合にsticky-key = ";" だと
  ;; "取って"を入力する際に送り仮名指定の"っ"の際に問題あり
  ;; なので，(re-search-forward "^;;;; for-sticky-with-azik")にそれをカバーする関数を定義
  (setq skk-sticky-key (kbd ";"))       ;元は<convert>
  ;; 不都合なruleを微調整
  (setq skk-rom-kana-rule-list
        '(("-" nil "-")
          ("z." nil "…");なぜskk-azik.elで上書きされているんだ
          ("kE" nil ("ケ" . "け"))
          ("kA" nil ("カ" . "か"))
          ("k1" nil ("ヵ" . "ヵ"))
          ("k3" nil ("ヶ" . "ヶ"))
          ("tU" nil ("ツ" . "つ"))
          ("tt" nil ("ッ" . "っ"))      ;tt=たちを潰して送り仮名の"っ"を打ちやすく？
          ))
  (defun ly:skk-custom-azik-kana-rule ()
    (let ((q (assoc "q" skk-rom-kana-rule-list))
          (orig '("q" nil skk-toggle-kana)))
      (setq skk-rom-kana-rule-list (delq q skk-rom-kana-rule-list))
      (add-to-list 'skk-rom-kana-rule-list orig)))
  (add-hook 'skk-azik-load-hook 'ly:skk-custom-azik-kana-rule)
  ;;一定時間アイドル状態が続くとバッファを自動保存
  ;;skkimeとの共存を考えてやっぱり無効化
  ;; (defvar skk-auto-save-jisyo-interval 600)
  ;; (defun skk-auto-save-jisyo ()
  ;;   (skk-save-jisyo))
  ;; (run-with-idle-timer skk-auto-save-jisyo-interval skk-auto-save-jisyo-interval 'skk-auto-save-jisyo)

  ;;サーバー補完用の設定
  (defun ly:skk-init-comp-server ()
    (add-to-list 'skk-completion-prog-list '(skk-comp-by-server-completion) t))
  (add-hook 'skk-comp-load-hook 'ly:skk-init-comp-server)
  ;; ruby-electric-mode時のspace問題
  (defadvice ruby-electric-space (around skk-ruby-mode (arg) activate)
    (if (and (boundp 'skk-henkan-mode) skk-henkan-mode)
        (skk-insert)
      ad-do-it))
  ;; 候補の絞りこみ
  (defun ly:skk-init-hint ()
    (require 'skk-hint)
    (setq skk-hint-start-char (string-to-char "|")))
  (add-hook 'skk-load-hook 'ly:skk-init-hint)
  ;; 候補の学習
  (defun ly:skk-init-study ()
    (require 'skk-study))
  (add-hook 'skk-load-hook 'ly:skk-init-study)

  ;; 一旦skkを実行すると minibuffer で C-j でexit-minibufferできなくなるので
  ;; それを修正
  (defadvice skk-kakutei (around on-minibuffer activate)
    "no execute when skk-mode on minibuffer."
    (if (and (window-minibuffer-p) (not skk-mode))
        (call-interactively 'exit-minibuffer)
      ad-do-it))
  ;; 変換キーをskk-j-modeへの入口に(auto-completeとの連携)
  (defun ly:skk-kakutei-maybe ()
    (interactive)
    (when (and (boundp 'skk-mode) skk-mode)
      (setq this-command 'skk-kakutei)
      (call-interactively 'skk-kakutei)))
  (global-set-key (kbd "<convert>") 'ly:skk-kakutei-maybe)
  (defun ly:skk-latin-mode-maybe ()
    (interactive)
    (when (and (boundp 'skk-mode) skk-mode)
      (setq this-command 'skk-latin-mode)
      (call-interactively 'skk-latin-mode)))
  (global-set-key (kbd "<non-convert>") 'ly:skk-latin-mode-maybe)
  ;; カーソルがeobにあってバッファ末尾に改行がない場合に入力位置がずれるのを防ぐ
  (defvar skk-last-newline-inserted nil)
  (make-variable-buffer-local 'skk-last-newline-inserted)
  (defvar skk-dcomp-multiple-show-progress nil)
  (defadvice skk-dcomp-multiple-show (around empty-newlines activate)
    (when (and (not skk-last-newline-inserted)
               (eobp)
               (not (eq (char-before (point-max)) ?\n))
               (not buffer-read-only))
      (save-excursion (insert "\n"))
      (setq skk-last-newline-inserted t))
    (let ((skk-dcomp-multiple-show-progress t))
      ad-do-it))
  (defadvice skk-dcomp-multiple-hide (after empty-newlines activate)
    (when (and (not skk-dcomp-multiple-show-progress)
               skk-last-newline-inserted
               (not buffer-read-only))
      (save-excursion
        (goto-char (point-max))
        (and (looking-back "\n")
             (backward-delete-char 1)))
      (setq skk-last-newline-inserted nil)))
  ;; annotetation付きの候補に対してauto-okuri-processで検索した場合に
  ;; 送りがなが消えてしまうバグを修正する
  (defadvice skk-okuri-search-subr-original (after okuri-process-annotation-bugfix activate)
    (setq ad-return-value
          (mapcar (lambda (cand)
                    (let ((match (string-match ";" cand)))
                      (if match
                          (concat (substring cand 0 match) (substring cand -1) (substring cand match -1))
                        cand)))
                  ad-return-value)))
  ;; skk-henkan で片仮名への変換も行う
  (defun ly:skk-init-search-prog-list ()
    (setq skk-search-prog-list (skk-nunion skk-search-prog-list '((skk-search-katakana)))))
  (add-hook 'skk-load-hook 'ly:skk-init-search-prog-list)
  )

;;;; インクリメンタルサーチで日本語入力するための設定([2009-03-10]migemoインストールにつき不要化)
;; (add-hook 'isearch-mode-hook
;;          (lambda ()
;;              (when (and (boundp 'skk-mode)
;;                         skk-mode
;;                         skk-isearch-mode-enable)
;;                (skk-isearch-mode-setup))))
;; (add-hook 'isearch-mode-end-hook
;;          (lambda ()
;;              (when (and (featurep 'skk-isearch)
;;                         skk-isearch-mode-enable)
;;                (skk-isearch-mode-cleanup))))

;;;; for-sticky-with-azik
(ly:eval-after-load 'skk-sticky
  (defun skk-sticky-orig-output ()
    (let ((data skk-sticky-key-orig-output))
      (when data
        (when (functionp data)
          (setq data (funcall data)))
        (when (stringp (if (consp data)
                           (car data)
                         data))
          (if (consp data)
              (if skk-katakana
                  (car data)
                (cdr data))
            data)))))
  (defun skk-sticky-same-key-p (char)
    (when (stringp skk-sticky-key)
      (eq char (string-to-char skk-sticky-key))))
  ;; re-define
  (defun skk-sticky-set-okuri-mark ()
    "送り開始点を `*' を挿入することで標識し、送りあり変換の待ち状態に入る。"
    (when (eq skk-henkan-mode 'on)
      (if (and skk-sticky-okuri-flag
               (skk-sticky-looking-back-okuri-mark))
          (progn
            (delete-backward-char 1)
            (setq skk-sticky-okuri-flag nil)
            (skk-sticky-orig-output))
        (when (and skk-dcomp-activate
                   (skk-dcomp-marked-p))
          (skk-dcomp-before-kakutei))
        (insert-and-inherit "*")
        (setq skk-sticky-okuri-flag t)
        nil)))
  (defadvice skk-insert (before skk-sticky-ad activate)
    "`*' の直後であれば入力を大文字に変換する。"
    (when (and skk-sticky-okuri-flag
               (skk-sticky-looking-back-okuri-mark)
               (not (skk-sticky-same-key-p (skk-last-command-char)))
               (string= "" skk-prefix))
      (let ((pair (rassq (skk-last-command-char) skk-downcase-alist)))
        (skk-set-last-command-char (if pair
                                       (car pair)
                                     (upcase (skk-last-command-char))))))))
