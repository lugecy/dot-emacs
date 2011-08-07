;; (require 'clmemo)
(autoload 'mclgrep "mclgrep" "clgrep for multiple files." t)
(global-set-key "\C-xm" 'clmemo-launch) ; default: compose-mail
(defun clmemo-launch (arg)
  "Open ChangeLog memo file `clmemo-file-name' and ask title.

With prefix argument ARG, just open ChangeLog memo file.
 If already visited the ChangeLog memo file,
 ask title and insert it in the date at point.
With prefix argument more than once, call `clmemo-grep-function'.

See also `add-change-log-entry' and `clmemo-get-title'."
  (interactive "P")
  (require 'clmemo)
  (cond
   ((equal arg '(64)) (clmemo-grep nil))       ;C-u C-u C-u
   ((equal arg '(16))                          ;C-u C-u
    (let ((clmemo-grep-function #'mclgrep))
      (clmemo-grep nil)))
   ((equal arg '(4))  (clmemo-one-prefix-arg)) ;C-u
   (t (clmemo-remember))))

(ly:eval-after-load "clmemo"
  (define-key clmemo-mode-map (kbd "TAB") nil)) ;; clmemoでのTAB動作(補完)を可能に
(setq clmemo-time-string-with-weekday t) ;; from [2009-03-30]
(setq clmemo-title-list
      '("emacs"
        "GogoTea"
        ("book-history" . "[book-history]")
        ("bookmark" . "[bookmark]")
        "assam"
        ("ebm" . "emacs [bookmark]")
        "charon"
        "rosalind"
        "life-log"
        "life-buy"
        ("todo-today" . (format-time-string "[%Y-%m-%d]-7"))
        ))
;; (setq clmemo-subtitle-char "[")
(setq clmemo-subtitle-punctuation-char '(" [" . "]"))
;; titleにlisp式の評価結果を挿入できるようにする
(defun clmemo-title-format-eval (title)
  (cond ((consp title)
         (eval title))
        (t title)))
(setq clmemo-title-format-function 'clmemo-title-format-eval)

;;;; エントリの末尾に日時間を挿入する
(add-hook 'clmemo-new-title-hook 'clmemo-insert-item-footer)
(defun clmemo-insert-item-footer ()
  (save-excursion
    (newline)
    (insert (clmemo-item-footer))))

(defun clmemo-item-footer ()
  (format-time-string "[%Y-%m-%d %R]"))

(defun clmemo-reindent (buf)
  (save-excursion
    (let ((beg (point))
          (end (progn (re-search-forward "^\t\\*[ ]+" nil t) ; 次のitemを検索
                      (point))))
      (indent-region beg end))))
(setq clmemo-buffer-function-list
      '(clmemo-insert-region clmemo-reindent))

;; 最近の記事からヘッダを拾い出す
;; http://www.bookshelf.jp/soft/meadow_38.html#SEC554
(defun clmemo-recent-header-picks ()
  (when (file-exists-p clmemo-file-name)
    (with-temp-buffer
      ;; 行頭から5000文字を拾い出す
      (insert-file-contents clmemo-file-name
                            nil 0 5000)
      (let ((entry nil) (subentry nil))
        (goto-char (point-min))
        (while (re-search-forward
                "^\t\\*[ ]+\\([^:\n]+\\):" nil t)
          (setq entry
                (buffer-substring-no-properties
                 (match-beginning 1)
                 (match-end 1)))
          (when (string-match "(" entry)
            (setq subentry
                  (substring
                   entry
                   (+ (string-match "(" entry) 1)
                   (- (length entry) 1)))
            (setq entry
                  (substring entry
                             0
                             (string-match "(" entry)))
            (if (or (member subentry clmemo-title-list)
                    (not entry))
                ()
              (setq clmemo-title-list
                    (cons subentry clmemo-title-list))
              (setq subentry nil)))
          (if (or (member entry clmemo-title-list)
                  (not entry))
              ()
            (setq clmemo-title-list
                  (cons entry clmemo-title-list))
            (setq entry nil)))))))
(defadvice clmemo-completing-read
  (after set-entry activate)
  (let ((entry ad-return-value))
    (if (or (member entry clmemo-title-list)
            (not entry))
        ()
      (if (string-match "+" entry)
          (setq entry
                (substring
                 entry
                 0
                 (string-match "+" entry))))
      (setq clmemo-title-list
            (cons entry clmemo-title-list)))))

;;;; Mercurialのコミットをclmemoにコピーする
(defun hg-commit-to-clmemo (target-place revision)
  "Mercurialのコミットをclmemoにコピーする"
  (interactive "DDirectory: \nsrevision: ")
  (let (repository-root)
    ;; Mercurialレポジトリなのかの確認
    (let ((default-directory target-place))
      (setq repository-root (clmemo-hg-root)))
    (unless repository-root
      (error "%s" "This Directory is no Mercurial Repository"))
    ;; clmemoへのエントリ追加(対話的にタイトルを聞いてくるのは
    ;; 省きたい？
    (clmemo-remember "[HG-commit]"
                     (with-temp-buffer
                       ;; コミットログを拾ってくる
                       (let* ((template (concat "{desc}\n"
                                                "repository: " repository-root "\n"
                                                "revision:   {rev}:{node|short}\n"
                                                "date:       {date|date}")))
                         (let ((begin (point))
                               (coding-system-for-read 'utf-8-auto)
                               (rev (if (equal "" revision) "tip" revision)))
                           (call-process "hg" nil (current-buffer) nil
                                         "-R" repository-root
                                         "log" "-r" rev
                                         "--template" template)
                           (buffer-string)))))))

;; from ahg-root
(defun clmemo-hg-root ()
  "Returns the root of the tree handled by Mercurial, or nil if
the current dir is not under hg."
  (with-temp-buffer
    (when (= (ahg-call-process "root") 0)
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

;;;; クリップボードからclmemoへブックマークを追加する
(defun clmemo-to-bookmark-in-clipboard ()
  "クリップボードからclmemoへブックマークを追加する"
  (interactive)
  (clmemo-tumble-from-clipboard "[bookmark]"))

(defun clmemo-to-quote-in-clipboard ()
  "クリップボードからclmemoへ引用文を追加する"
  (interactive)
  (clmemo-tumble-from-clipboard "[quote]" t))

(defun clmemo-tumble-from-clipboard (&optional title fill-title)
  (interactive)
  (clmemo-remember (or title "") (current-kill 0))
  (clmemo-remember-finalize nil fill-title))

;;;; clgrep.el
(autoload 'clgrep "clgrep" "ChangeLog grep." t)
(autoload 'clgrep-item "clgrep" "ChangeLog grep." t)
(autoload 'clgrep-item-header "clgrep" "ChangeLog grep for item header" t)
(autoload 'clgrep-item-tag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-item-notag "clgrep" "ChangeLog grep for item except for tag" t)
(autoload 'clgrep-item-nourl "clgrep" "ChangeLog grep item except for url" t)
(autoload 'clgrep-entry "clgrep" "ChangeLog grep for entry" t)
(autoload 'clgrep-entry-header "clgrep" "ChangeLog grep for entry header" t)
(autoload 'clgrep-entry-no-entry-header "clgrep" "ChangeLog grep for entry except entry header" t)
(autoload 'clgrep-entry-tag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-entry-notag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-entry-nourl "clgrep" "ChangeLog grep entry except for url" t)
(ly:eval-after-load "clmemo"
    (define-key clmemo-mode-map (kbd "C-c g") 'clgrep))

(eval-after-load "viper"
  '(add-to-list 'viper-emacs-state-mode-list 'clgrep-mode))
(eval-after-load "clgrep"
  #'(eval-after-load "migemo"
      #'(defadvice clgrep (around clgrep-migemo (query &optional rev) activate)
          (setq query (ly:get-migemo-pattern query))
          ad-do-it)))
(ly:eval-after-load 'mclgrep
  (ly:eval-after-load 'migemo
    (defadvice mclgrep (around clgrep-migemo (query &optional rev) activate)
      (setq query (ly:get-migemo-pattern query))
      ad-do-it)))
(defun ly:get-migemo-pattern (query)
  "queryが正規表現らしきものならばそのまま，それ以外ならばmigemo-patternを通す."
  (if (string= query (regexp-quote query))
      (migemo-get-pattern query)
    query))
;; (eval-after-load "howm"
;;   '(add-hook 'howm-mode-hook 'clmemo-mode))

;;;; host depend setting
;; (setq user-full-name "lugecy")
;; (setq user-mail-address "lugecy@GogoTea")
;;(setq clmemo-file-name "e:/sysprofile/Desktop/howm/memo/clmemo.howm")

;;;; howm todo timestamp
(defun howm-todo-initial-format ()
  "For first howm-timestamp insert."
  (format-time-string "[%Y-%m-%d]-7"))

(defun clmemo-insert-howm-todo ()
  "現在のclmemo-itemの先頭にhowm-todo-timestampを挿入する"
  (interactive)
  (let ((regexp "^\t\\* "))
   (save-excursion
     (re-search-backward regexp nil t)
     (goto-char (match-end 0))
     (insert (concat (howm-todo-initial-format) " ")))))

;;;; clmemo-remember
(defvar clmemo-remember-header "* ")
;; (defalias 'clmemo-new-title-today 'clmemo-remember)
;; (defadvice clmemo-new-title-today (around clmemo-with-remember activate)
;;   (clmemo-remember))
(defun clmemo-remember (&optional title content)
  "clmemo-remember."
  (interactive)
  (require 'remember)
  (require 'clmemo)
  (unless title
    (setq title (clmemo-get-title)))
  (unless content
    (setq content (and mark-active
                       transient-mark-mode
                       (buffer-substring (region-beginning) (region-end)))))
  (let ((remember-annotation-functions nil)
        (remember-mode-hook nil)
        (header (concat clmemo-remember-header (unless (string= "" title) (concat (replace-regexp-in-string "^ +" "" title) ": "))))
        (remember-initial-contents content)
        )
    (remember header)
    (goto-char (+ (point-min) (length header)))
    (clmemo-remember-mode 1)))

(defun clmemo-remember-finalize (&optional nosave fill-title)
  "clmemo-remember-finalize."
  (interactive "P")
  (let ((remember-handler-functions '(clmemo-remember-add-item)))
    (remember-finalize)
    (let ((reg (get-register ?C)))
      (pop-to-buffer (marker-buffer reg))
      (goto-char (marker-position reg))
      (when fill-title
        (fill-region (point) (line-end-position)))
      (setq clmemo-winconf (car (get-register remember-register)))
      (unless nosave
        (save-buffer)))))

(defun clmemo-remember-add-item ()
  "clmemo-remember-add-item."
  (let* ((content-end (if (equal ?\n (char-before (point-max)))
                          (1- (point-max))
                        (point-max)))
         (remember-text (buffer-substring (1+ (length clmemo-remember-header)) content-end)))
    (save-excursion
      (clmemo-add-new-item clmemo-file-name remember-text))))

(defun clmemo-add-new-item (file-name text)
  "指定されたファイルに新しいitemを追加する．追加したitemの先頭位置をregister ?Cに保存される．"
  (let ((add-log-always-start-new-record nil)
        (add-log-time-format (if clmemo-time-string-with-weekday
                                 'add-log-iso8601-time-string-with-weekday
                               'add-log-iso8601-time-string))
        (current-prefix-arg '()) ;add-change-log-entry内でprefix-argが伝播してrawmodeでfile-openしてしまうのを防止する
        )
    (save-excursion
      (add-change-log-entry nil file-name)
      (beginning-of-line)
      (when (looking-at "^\t\\* .*:? ?")
        (replace-match "\t* "))
      (end-of-line)
      (run-hooks 'clmemo-new-title-hook)
      (set-register ?C (point-marker))
      (let ((beg (point))
            (end (progn (insert text) (point))))
        (clmemo-indent-region beg end)))))

(defvar clmemo-remember-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'clmemo-remember-finalize)
    (define-key map (kbd "C-x C-s") 'clmemo-remember-finalize)
    (define-key map (kbd "C-c C-k") 'remember-destroy)
    map))
(defvar clmemo-remember-mode-hook nil)
(define-minor-mode clmemo-remember-mode
  "remember for clmemo."
  nil " Clrem" clmemo-remember-mode-map
  (run-hooks clmemo-remember-mode-hook))

;;;; clmemoでoutline-minor-modeを使えるようにする
(defun clmemo-outline-minor-configure ()
  (let (
        (head "^20[0-9][0-9]-[01][0-9]-[0-3][0-9]") ;1エントリ開始
        (term "^\t\\* ")                            ;2大見出し
        (subterm "^\t\\*\\* ")                      ;3中見出し
        (subsubterm "^\t\\*\\*\\* ")                ;4小見出し
        (b "\\(")
        (m "\\)\\|\\(")
        (e "\\)")
        )
    (setq outline-regexp (concat b head m term m subterm m subsubterm e ))

    (setq outline-level
          (function
           (lambda ()
             (save-excursion
               (looking-at outline-regexp)
               (cond                    ;レベルを返す
                                        ; 2006-09-06 (Wed)  ITSUMI Ken-ichi  <itsumi@gmail.com>
                ((match-string 1) 1)
                ((match-string 2) 2)    ; \t* Today:
                ((match-string 3) 3)    ; \t** emacs 環境整備
                ((match-string 4) 4) ; \t*** outline-minor-mode 4 change-log-mode
                (t 5)                ;
                ))))))
  (outline-minor-mode t))
(add-hook 'clmemo-mode-hook 'clmemo-outline-minor-configure)
(add-hook 'clgrep-mode-hook 'clmemo-outline-minor-configure)

;;;; 複数タグの色付け
(defun font-lock-clmemo-tags-p (limit)
  (let (find-flg)
    (while (and (not find-flg)
                (re-search-forward "\\[\\([^]\n]+\\)\\][:[]" limit t))
      (and (save-match-data
             (save-excursion
               (beginning-of-line)
               (looking-at "\t\\* ")))
           (setq find-flg t)))
    find-flg))
(font-lock-add-keywords 'change-log-mode
                        '((font-lock-clmemo-tags-p 1 'change-log-conditionals t))
                        t)

;;;; 一行目に色付け
(font-lock-add-keywords 'change-log-mode
                        '(("^\t\\* [^:\n]*: \\([^\n]+\\)"
                           (1 'change-log-file)))
                        t)

;;;; emerge -uDN -pv | eformat.rb の結果(summary)を整形する
(defun ly:eformat-align (start end)
  (interactive "r")
  (let ((indent-tabs-mode nil))
    (align-regexp start end " " 0 1 t)))

;;;; TomblooからのPost用関数
(defun clmemo-from-tombloo (type title url &optional tag descript)
  (interactive)
  (require 'clmemo)
  (flet ((dec-str (str)
                  (decode-coding-string (string-make-unibyte (url-unhex-string str)) 'utf-8)))
    (let ((title (dec-str title))
          (url (url-unhex-string url))
          (tags (and tag (not (string= tag ""))
                     (split-string (url-unhex-string tag) ",")))
          (descript (and descript (not (string= descript ""))
                         (dec-str descript))))
      (cond
       ((string= type "link")
        (ly:clmemo-add-bookmark-suck title url tags descript))
       ((string= type "quote")
        (clmemo-remember "[quote]"
                         (format "%s\n-\n%s\n%s"
                                 (cond ((eq (window-system) 'w32)
                                        (w32-get-clipboard-data))
                                       ((eq (window-system) 'x)
                                        (x-get-clipboard)))
                                 title url))
        (clmemo-remember-finalize nil 'fill))
       ((string= type "regular"))
       (t
        (error "tumbloo post type error."))))))

(defun ly:clmemo-add-bookmark-suck (title url &optional tags desc)
  (save-excursion
    (save-window-excursion
      (find-file clmemo-file-name)
      (or (derived-mode-p 'change-log-mode)
          (change-log-mode))
      (ly:clmemo-insert-bookmark-item)
      (unless (looking-at " >>>")
        (insert " >>>")
        (save-buffer))
      (let ((bookmark-item
             (mapconcat #'identity
                        (delq nil
                              (list (format "[bookmark]: %s" title)
                                    url desc
                                    (and tags
                                         (mapconcat (lambda (tag) (format "[%s]" tag))
                                                    tags nil))))
                        "\n")))
        (clmemo-add-new-item clmemo-bookmark-suck-file-name bookmark-item)
        (with-current-buffer (marker-buffer (get-register ?C))
          (save-buffer))))))

;;;; C-c RET で snap-play する
(defadvice clmemo-jump (around with-snap-play activate)
  (cond ((or (clmemo-inline-date-p) (clmemo-tag-p))
         ad-do-it)
        ((and (require 'snap nil t)
              (thing-at-point 'snap))
         (snap-play))
        ((string= (thing-at-point 'line)
                  (format "\t* %s >>>\n" ly:clmemo-bookmark-title))
         (let ((entry-head (save-excursion (clmemo-backward-entry) (thing-at-point 'line))))
           (find-file clmemo-bookmark-suck-file-name)
           (goto-char (point-min))
           (search-forward entry-head)))
        (t
         ad-do-it)))

;;;; bookmark-itemはentryの末尾に集める
(defvar ly:clmemo-bookmark-title "bkmk-anchor:")

(defun ly:clmemo-add-bookmark-obj (title url &optional tags desc)
  (save-excursion
    (save-window-excursion
      (find-file clmemo-file-name)
      (or (derived-mode-p 'change-log-mode)
          (change-log-mode))
      (ly:clmemo-insert-bookmark-item)
      (insert "\n\n")
      (let ((beg (point))
            (has-obj (looking-at "^\t.")))
        (insert "* " (mapconcat (lambda (tag) (format "[%s]" tag))
                                (append (list "bookmark") tags)
                                "")
                ": " title "\n"
                url "\n"
                (if desc (concat desc "\n") "")
                (clmemo-item-footer))
        (clmemo-indent-region beg (point)))
      (save-buffer))))

(defun ly:clmemo-insert-bookmark-item ()
  (if (clmemo-exist-today-entry-p)
      (progn
        (goto-char (point-min))
        (let ((yesterday-entry-pos (save-excursion (clmemo-forward-entry) (point))))
          (unless (re-search-forward (concat clmemo-heading-regexp ly:clmemo-bookmark-title) yesterday-entry-pos t)
            (clmemo-forward-entry)
            (insert "\t* \n\n")
            (clmemo-previous-item)
            (end-of-line))))
    (let ((add-log-always-start-new-record nil)
          (add-log-time-format (if clmemo-time-string-with-weekday
                                   'add-log-iso8601-time-string-with-weekday
                                 'add-log-iso8601-time-string)))
      (add-change-log-entry nil clmemo-file-name)
      ;; 余計なタグ(関数名など)を消す
      (beginning-of-line)
      (when (looking-at "^\t\\* .+: ")
        (replace-match "\t* "))
      (end-of-line)))
  (unless (looking-back (regexp-quote ly:clmemo-bookmark-title))
    (insert ly:clmemo-bookmark-title)))

(defun clmemo-exist-today-entry-p ()
  (save-excursion
    (goto-char (point-min))
    (clmemo-today-entry-p)))

(defun clmemo-today-entry-p ()
  (let ((add-log-time-format (if clmemo-time-string-with-weekday
                                 'add-log-iso8601-time-string-with-weekday
                               'add-log-iso8601-time-string)))
    (looking-at (funcall add-log-time-format))))

;;;; clipboardからbookmark-objを追加する
(defun ly:clmemo-add-bookmark-obj-from-clipboard ()
  (interactive)
  (apply 'ly:clmemo-add-bookmark-obj
         (split-string
          (cond ((eq (window-system) 'w32)
                 (w32-get-clipboard-data))
                ((eq (window-system) 'x)
                 (x-get-clipboard)))
          "\n")))

(defun ly:clmemo-add-bookmark-suck-from-clipboard ()
  (interactive)
  (apply 'ly:clmemo-add-bookmark-suck
         (split-string
          (cond ((eq (window-system) 'w32)
                 (w32-get-clipboard-data))
                ((eq (window-system) 'x)
                 (x-get-clipboard)))
          "\n")))
