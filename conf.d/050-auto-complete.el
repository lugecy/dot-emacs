(require 'auto-complete)
;; (global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-height 7)
(setq ac-dwim t)
(setq ac-delay 0.3)
(setq ac-auto-show-menu 0.5)
(setq ac-disable-faces nil)
(setq ac-use-comphist t)
(setq ac-modes (append ac-modes '(change-log-mode log-edit-mode remember-mode html-mode rhtml-mode)))
(dolist (command '(paredit-backward-delete viper-del-backward-char-in-insert
                                           ac-trigger-key-command))
  (add-to-list 'ac-trigger-commands command t))
(setq ac-compatible-packages-regexp "^\\(ac\\|auto-complete\\)-")
(ly:eval-after-load "skk-autoloads"
  (setq ac-compatible-packages-regexp (concat ac-compatible-packages-regexp "\\|^ly:skk-")))
(setq ac-enable-prefix-visual t)      ;my-addtional option
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map (kbd "C-j") 'ac-complete)
(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)
(define-key ac-completing-map (kbd "C-g") 'ac-stop)
(define-key ac-completing-map (kbd "M-o") 'ac-expand-restart)
(define-key ac-completing-map (kbd "M-i") 'ac-mid-expand)
(define-key ac-completing-map (kbd "C-c C-u") 'ac-symbols-cache-reset)
(define-key ac-completing-map (kbd "C-c C-.") 'ac-switch-source)
(define-key ac-completing-map (kbd "C-c C-/") 'ac-switch-source-reverse)
(define-key ac-mode-map (kbd "M-\\") 'auto-complete-select-source)
(define-key ac-mode-map (kbd "C-c C-f") 'auto-complete-filename-maybe)
(define-key ac-mode-map (kbd "C-M-o") 'auto-complete-swank-maybe)
(define-key popup-menu-keymap (kbd "C-j") 'popup-select)

(custom-set-variables ;; set-defaultでもOK
 '(ac-sources '(ac-source-yasnippet
                ac-source-abbrev
                ac-source-scatter-match-word-in-buffer
                ac-source-fuzzy-words-in-buffer
                ac-source-words-in-visible-buffer)))

(defun ly:ac-elisp-setup ()
  (setq ac-sources (append '(ac-source-functions ly:ac-source-elisp-local-variable) ac-sources))
  (let ((add-sources '(ac-source-symbols ac-source-features)))
    (setq ac-sources
          (append (butlast ac-sources) add-sources (last ac-sources)))))
(add-hook 'emacs-lisp-mode-hook 'ly:ac-elisp-setup)

;;;; idle-save ac-comphist
(setq ly:idle-save-ac-comphist (run-with-idle-timer 190 t 'ac-comphist-save))

;;;; configured source customize
(ly:eval-after-load 'yasnippet
  (require 'auto-complete-yasnippet)
  (add-to-list 'ac-source-yasnippet '(meta . ly:yas/template-key2name) t))
;;; 候補のfaceをどのsource由来か分かるように変更
(defface ac-source-symbols-candidate-face
  '((t (:background "lightgreen" :foreground "black")))
  "ac-candidate-face for `ac-source-symbols'")
(defface ac-source-symbols-selection-face
  '((t (:background "green3" :foreground "black")))
  "ac-selection-face for `ac-source-symbols'")
(setq ac-source-symbols
      (append ac-source-symbols '((candidate-face
                                   . ac-source-symbols-candidate-face)
                                  (selection-face
                                   . ac-source-symbols-selection-face))))

;;;; ポイント位置の近くにある単語が上位にくるようにするソース
(defvar ac-candidate-word-regexp "\\(?:\\s_\\|\\sw\\)+")
(defun ac-candidate-search-backward (point regexp cache)
  (save-excursion
    (goto-char point)
    (let (found cand)
      (while (and (setq found (re-search-backward regexp nil t))
                  (member (setq cand (match-string-no-properties 0)) cache)))
      (when found
        (cons cand found)))))

(defun ac-candidate-search-forward (point regexp cache)
  (save-excursion
    (goto-char point)
    (let (found cand)
      (while (and (setq found (re-search-forward regexp nil t))
                  (member (setq cand (match-string-no-properties 0)) cache)))
      (when found
        (cons cand found)))))

(defun ac-candidate-words-in-buffer+ (regexp &optional limit)
  "search word list in current buffer. list top is near word now point"
  (let ((ac-point (or ac-point (point)))
        (ac-prefix (or ac-prefix "")))
    (when (> (length ac-prefix) 0)
      (let (bcons fcons bpos fpos cand candidates)
        (setq bcons (ac-candidate-search-backward ac-point regexp candidates)
              fcons (ac-candidate-search-forward (+ ac-point (length ac-prefix)) regexp candidates))
        (while (and (or bcons fcons)
                    (or (not (integerp limit))
                        (<= (length candidates) limit)))
          (cond ((and bcons fcons
                      (string= (car bcons) (car fcons))) ;; same candidate
                 (push (car bcons) candidates)
                 (setq fcons (ac-candidate-search-forward (cdr fcons) regexp candidates)
                       bcons (ac-candidate-search-backward (cdr bcons) regexp candidates)))
                ((or (null fcons)       ; empty forward
                     (and bcons         ; backward near than forward
                          (< (- ac-point (cdr bcons)) (- (cdr fcons) ac-point)))) 
                 (push (car bcons) candidates)
                 (setq bcons (ac-candidate-search-backward (cdr bcons) regexp candidates)))
                ((or (null bcons)       ; empty backward
                     (and fcons         ; forward near then backward
                          (>= (- ac-point (cdr bcons)) (- (cdr fcons) ac-point))))
                 (push (car fcons) candidates)
                 (setq fcons (ac-candidate-search-forward (cdr fcons) regexp candidates)))))
        (nreverse candidates)))))

(defvar ac-source-words-in-buffer+
  '((candidates . (ac-candidate-words-in-buffer+ (concat "\\<" (regexp-quote ac-prefix) ac-candidate-word-regexp "\\>") t)))
  "search word list in current buffer. list top is near word now point")

(defvar ac-source-fuzzy-words-in-buffer
  '((candidates . (when ac-fuzzy-enable
                    (delete ac-prefix (split-string (buffer-substring-no-properties (point-min) (point-max))
                                                    "\\(?:^\\|\\_>\\).*?\\(?:\\_<\\|$\\)"))))
    (cache)
    (requires . 3)
    (symbol . "w"))
  "return word list in current buffer.")

(defvar ac-source-scatter-match-word-in-buffer
  '((candidates . (ac-candidate-words-in-buffer+ (concat "\\<"
                                                         (mapconcat 'identity (split-string ac-prefix "" t) "\\(?:\\s_\\|\\sw\\)*")
                                                         "\\(?:\\s_\\|\\sw\\)*")))
    (match . (lambda (prefix list)
               (if (and (= (length list) 1)
                        (= (length prefix) (length (car list))))
                   (append list (list prefix))
                 list)))
    (requires . 2)
    (symbol . "*")))

;;;; 単語の次の区切り文字(-とか_，CamelCaseとか？)までの候補を返す関数
(defvar ac-word-delimter-regexp "\\>")
(defun ac-mid-expand ()
  (interactive)
  (let ((string (ac-selected-candidate)))
    (when (< (length ac-prefix) (length string))
      (let ((mid-pos (string-match ac-word-delimter-regexp string (length ac-prefix))))
        (if mid-pos
            (progn
              (ac-expand-string (substring-no-properties string 0 (if (equal mid-pos (length ac-prefix))
                                                                      (1+ mid-pos) ; まずはdelimter直前まで補完
                                                                    mid-pos))) ; 次にdelimterまで補完
              (move-overlay ac-prefix-overlay ac-point (1+ (point)))))))))

;;;; select-source-complete (use popup.el)
(defvar ly:ac-source-file-comp '(ac-source-filename-plus ac-source-files-in-current-dir-plus))
(defvar ac-select-source-map-alist `(("filename" . ,ly:ac-source-file-comp)
                                     ("swank" . (ac-source-el-swank))))
(defun auto-complete-select-source ()
  (interactive)
  (let* ((source-alist (mapcar #'car ac-select-source-map-alist))
         (select-source (popup-menu* source-alist))
         (ac-sources (assoc-default select-source ac-select-source-map-alist)))
    (auto-complete)))

(defadvice auto-complete (around auto-complete-enable-src-select activate)
  "Auto-complete enable source select."
  (setq ad-return-value
        (if (< (prefix-numeric-value current-prefix-arg) 0)
            (progn
              (setq current-prefix-arg nil)
              (auto-complete-select-source))
          ad-do-it)))

;;; specific-source-command
(defun ac-walk-past-key-command ()
  (let* ((auto-complete-mode nil)
         (keys (this-command-keys-vector))
         (command (if keys (key-binding keys))))
    (when (commandp command)
      (setq this-command command)
      (call-interactively command))))

(defmacro ac-define-select-source-command (name)
  `(defun ,(intern (format "auto-complete-%s-maybe" name)) ()
     ,(format "Execute auto-compete-%s or key-command walk past auto-complte-mode."
              name)
     (interactive)
     (if (or ac-completing
             (eq real-last-command 'ac-trigger-key-command)
             (and (symbolp last-command)
                  (string-match "auto-complete" (symbol-name last-command))))
         (let ((ac-sources (assoc-default ,name ac-select-source-map-alist)))
           (auto-complete))
       (ac-walk-past-key-command))))
(ac-define-source filename-plus
  '((init . (setq ac-filename-cache nil))
    (candidates . ac-filename-candidate)
    (prefix . valid-file)
    (requires . 0)
    (action . ac-source-filename-restart)
    (limit . nil)))
(ac-define-source files-in-current-dir-plus
  '((candidates . (directory-files default-directory))
    (action . ac-source-filename-restart)
    (cache)))
(defun ac-source-filename-restart ()
  (let ((ac-sources ly:ac-source-file-comp))
    (ac-start)))
(ac-define-select-source-command "filename")
(ac-define-select-source-command "swank")

;;;; for ac-source-yasnippet meta func
(defun ly:yas/template-key2name (key)
  "Return yas/template-name from key.
Only first name when exist some name."
  (let ((tables (yas/get-snippet-tables)))
    (car (mapcan (lambda (table)
                   (mapcar #'car
                           (yas/fetch table key))) ; return (name . yas/template)
                 tables))))

;;;; 一度補完した後続けて補完モードに入る
(defun ac-expand-restart ()
  (interactive)
  (ac-complete)
  (ac-start))

;;;; For Skk dcomp, paredit and select-source
(ly:eval-after-load 'skk
  (defadvice ac-trigger-command-p (after ac-trigger-command-p-for-viper activate)
    "Return non-nil if `this-command' is a trigger command for viper-mode."
    (setq ad-return-value
          (if (or skk-henkan-mode skk-j-mode)
              nil
            ad-return-value))))

;;;; point位置の前にマルチバイト文字があってもascii文字の補完を可能にする関数
(defalias 'ac-prefix-default 'ac-prefix-for-ja)
(defun ac-prefix-for-ja ()
  (save-match-data
    (let ((prefix-regexp "\\(?:\\sw\\|\\s_\\)+")
          (category-regexp (concat (lugecy-char-category-to-regexp (or (char-before) 0)) "+"))
          prefix-limit)
      (and
       (looking-back prefix-regexp (line-beginning-position) t)
       (setq prefix-limit (match-beginning 0))
       (looking-back category-regexp prefix-limit t)
       (max (match-beginning 0) prefix-limit)))))

(defun lugecy-char-category-to-regexp (char)
  (let ((c (char-category-set char)))
    (cond
     ((aref c ?j)                       ; Japanese
      (cond
       ((aref c ?K) "\\cK")             ; katakana
       ((aref c ?A) "\\cA")             ; 2byte alphanumeric
       ((aref c ?H) "\\cH")             ; hiragana
       ((aref c ?C) "\\cC")             ; kanji
       (t "\\cj")))
     ((aref c ?k) "\\ck")               ; hankaku-kana
     ((aref c ?a) "\\ca")               ; ASCII
     (t "\\(?:\\sw\\|\\s_\\)"))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "test ac-prefix-for-ja")
      (expect 1
        (with-temp-buffer
          (insert "foobar")
          (ac-prefix-for-ja)))
      (expect 4
        (with-temp-buffer
          (insert "これはfoobar")
          (ac-prefix-for-ja)))
      (expect 2
        (with-temp-buffer
          (insert "(foobar")
          (ac-prefix-for-ja)))
      (expect 2
        (with-temp-buffer
          (insert "，foobar")
          (ac-prefix-for-ja)))
      (expect 7
        (with-temp-buffer
          (insert "foobar便利")
          (ac-prefix-for-ja)))
      (expect 2
        (with-temp-buffer
          (insert "，便利")
          (ac-prefix-for-ja))))))

;;;; ASCII -> multibyte-char な候補から multibyte-char部分を取り除く
(defadvice ac-candidates (after cut-multibyte-char activate)
  "advice-document"
  (setq ad-return-value (ac-cut-multibyte-char ad-return-value)))

(defun ac-cut-multibyte-char (candidates)
  "ascii->multibyte-charと続く文字からmultibyte部分を取りのぞく．
マルチバイト文字から始まっている場合は取り除かない"
  (save-match-data
    (delete-dups
     (mapcar (lambda (cand)
               (let ((regexp "\\(?:\\cK\\|\\cH\\|\\cC\\|\\cA\\)"))
                 (if (string-match (concat "\\`" regexp "+\\'") cand)
                     cand
                   (replace-regexp-in-string regexp "" cand))))
             candidates))))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "test cut-multibyte-char")
      (expect '("loop")
        (ac-cut-multibyte-char '("loopマルチ")))
      (expect '("loop")
        (ac-cut-multibyte-char '("loop漢字")))
      (expect '("loop")
        (ac-cut-multibyte-char '("loopひらがな")))
      (expect '("loop")
        (ac-cut-multibyte-char '("loopひらがな" "loopカナ")))
      (expect '("かなだけ")
        (ac-cut-multibyte-char '("かなだけ")))
      )))

;;;; ac-completeで表示されたメニューのi番目にアクセスする
(defun ac-select-candidate (num)
  "Select row item."
  (interactive)
  (let ((scroll-top (popup-scroll-top ac-menu)))
    (setf (popup-cursor ac-menu) (+ scroll-top num))
    (popup-draw ac-menu)))

(dotimes (i 10)
  (progn
    (when (> i 0)
      (let ((func-name (intern (concat "ac-selection-line-" (number-to-string i))))
            (row (1- i)))
        (eval `(progn
                 (defun ,func-name ()
                   (interactive)
                   (if (< ,row (popup-height ac-menu))
                       (ac-select-candidate ,row)
                     (message "Out of menu")))
                 (define-key ac-completing-map (kbd ,(concat "C-" (number-to-string i))) (quote ,func-name))))))))

;;;; view meta-info
(defadvice popup-draw (after echo-meta-info activate)
  "Echo meta-info of candidate."
  (when (and ac-candidates ac-menu)
    (ac-echo-meta-info)))

(defun ac-echo-meta-info ()
  "Function document"
  (let* ((cand (ac-selected-candidate))
         (src-name (and cand (popup-item-property cand 'source)))
         (meta (and src-name (assoc-default 'meta (symbol-value src-name)))))
    (let ((message-log-max nil))
      (message "[%s/%s]: %s"
               (1+ (popup-cursor ac-menu))
               (length ac-candidates)
               (if (and meta (functionp meta))
                   (funcall meta cand)
                 "")))))

;;;; for ac-select-candidate display row number
(defadvice popup-set-line-item (after for-ac-select-candidate-number activate)
  "For ac-select-candidate display row number."
  (when ac-completing
    (let* ((menu (ad-get-arg 0))
           (pos (ad-get-arg 1))
           (ov (popup-line-overlay menu pos)))
      (overlay-put ov 'after-string (format "%s%s"
                                            (overlay-get ov 'after-string)
                                            (1+ (- pos (popup-offset menu))))))))

;;;; ac-source-symbols のcacheをクリアする
(defun ac-symbols-cache-reset ()
  "Reset ac-symbols-cache."
  (interactive)
  (setq ac-symbols-cache nil)
  (message "ac-symbols-cache reset now."))

;;;; omni状態からsourceを切り替える
(defun ac-switch-source (&optional prev-flg)
  (interactive "P")
  (let ((pre-ignoring-prefix ac-ignoring-prefix-def))
    (if prev-flg
        (setq ac-ignoring-prefix-def (cdr ac-ignoring-prefix-def))
      (add-to-list 'ac-ignoring-prefix-def ac-current-prefix-def))
    (ac-inline-update)
    (ac-remove-quick-help)
    (ac-menu-delete)
    (ac-start :force-init t)
    (unless ac-current-prefix-def
      (let ((message-log-max nil))
        (message "None prefix-def, rollback prefix."))
      (setq ac-ignoring-prefix-def pre-ignoring-prefix)
      (ac-start :force-init t))))
(defun ac-switch-source-reverse ()
  (interactive)
  (ac-switch-source 'pref))

;;;; for local-variable(let local) candidate from company-elisp.el
(defvar ly:ac-elisp-parse-limit 30)
(defvar ly:ac-elisp-parse-depth 100)
(defvar ly:ac-elisp-binding-regexp
  (concat "([ \t\n]*\\_<" (regexp-opt '("let" "defun" "defmacro" "defsubst"
                                        "lambda" "lexical-let" "flet" "labels"))
          "\\*?")
  "Regular expression matching sexps containing variable bindings.")
(defvar ly:ac-elisp-binding-regexp-1
  (concat "([ \t\n]*\\_<" (regexp-opt '("dolist" "dotimes")))
  "Regular expression matching sexps containing one variable binding.")

(defun ly:ac-elisp-parse-local (prefix)
  (let ((regexp (concat "[ \t\n]*\\(\\_<" (regexp-quote prefix)
                        "\\(?:\\sw\\|\\s_\\)*\\_>\\)"))
        (pos (point))
        (vars nil))
    (ignore-errors
      (save-excursion
        (dotimes (i ly:ac-elisp-parse-depth)
          (up-list -1)
          (save-excursion
            (cond
             ((looking-at ly:ac-elisp-binding-regexp)
              (down-list 2)
              (ignore-errors
                (dotimes (i ly:ac-elisp-parse-limit)
                  (save-excursion
                    (when (looking-at "[ \t\n]*(")
                      (down-list 1))
                    (and (looking-at regexp)
                         ;; Don't add incomplete text as candidate.
                         (not (eq (match-end 0) pos))
                         (add-to-list 'vars (match-string-no-properties 1))))
                  (forward-sexp))))
             ((looking-at ly:ac-elisp-binding-regexp-1)
              (down-list 2)
              (and (looking-at regexp)
                   ;; Don't add incomplete text as candidate.
                   (not (eq (match-end 0) pos))
                   (add-to-list 'vars (match-string-no-properties 1)))))))))
    vars))

(defvar ly:ac-source-elisp-local-variable
  '((candidates . (ly:ac-elisp-parse-local ac-prefix))
    (symbol . "@")))

;;;; ac-source-words-in-visible-buffer
(defun ly:ac-invisible-buffer-p (buf)
  (or (string-match "\\` " (buffer-name buf))
      (string-match (regexp-opt (list "-preprocessed*" "clmemo.howm")) (buffer-name buf)))) ; for c-eldoc cache
(defun ac-update-word-index-for-visible ()
  (dolist (buffer (buffer-list))
    (unless (or (eq buffer (current-buffer))
                (ly:ac-invisible-buffer-p buffer))
      (with-current-buffer buffer
        (ac-update-word-index-1)))))
(defalias 'ac-prefix-for-otherbuf 'ac-prefix-default)
(ac-define-source words-in-visible-buffer
  '((init . ac-update-word-index-for-visible)
    ;; (prefix . ac-prefix-for-otherbuf)
    (candidates . (loop for w in (ac-word-candidates
                                  (lambda (buffer)
                                    (not (ly:ac-invisible-buffer-p buffer))))
                        unless (string-match "\\.\\'" w) collect w))
    (candidate-face . (:foreground "BlueViolet" :background "lightgray"))
    (selection-face . (:foreground "BlueViolet" :background "SkyBlue"))))

;;;; fot el-swank-fuzzy
(safe-loading 'el-swank-fuzzy)
(ac-define-source el-swank
  '((candidates . ac-el-swank-candidates)
    (symbol . "S")
    (match . (lambda (prefix list) list))
    (requires . 3)))
(defun ac-el-swank-candidates ()
  (mapcar 'car
          (car (el-swank-fuzzy-completions ac-prefix 1000
                                           (lambda (x)
                                             (or (fboundp x) (boundp x) (symbol-plist x)))))))
;; fuzzyやscatterでの候補が一つだけしか存在しない場合でもmenu表示するように
(defadvice ac-update-candidates (after for-fuzzy-source activate)
  (when (and ac-candidates ac-completing ac-prefix
             (not (ac-inline-live-p))
             (not (eq ac-common-part t))
             ac-menu ac-show-menu)
    (popup-draw ac-menu)))

;;;; numberだけの入力の時は補完候補を探さないようにする
(defvar ac-source-number-ignore
  '((prefix . "\\<[0-9]+\\=")
    (candidates . (list ac-prefix))
    (match . (lambda (prefix list) list))
    (symbol . "n")))
(defadvice ac-prefix (before number-ignore activate)
  (when (and ac-sources (boundp 'clmemo-mode) clmemo-mode)
    (setq ac-sources
          (append '(ac-source-number-ignore) (delq 'ac-source-number-ignore ac-sources)))))

;;;; for ruby-mode
(defadvice ac-trigger-command-p (after ac-trigger-command-p-for-ruby activate)
  (setq ad-return-value
        (if (and (eq major-mode 'ruby-mode)
                 (string= (thing-at-point 'symbol) "end"))
            nil
          ad-return-value)))
