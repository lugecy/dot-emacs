(defun company-yasnippet (command) nil)
(defun company-word-in-buffer (command) nil)
(defun company-dabbrev-code+ (command) nil)
(defun company-filenames (command) nil)
;; (setq-default company-backends '(company-yasnippet company-word-in-buffer company-elisp company-dabbrev-code company-dabbrev company-files company-etags company-keywords))
(setq-default company-backends '(company-yasnippet company-dabbrev-code+ company-elisp company-dabbrev company-filenames company-etags company-keywords))
(require 'company)
;; (global-company-mode t)
(setq company-minimum-prefix-length 2   ; default: 3
      company-show-numbers t
      company-idle-delay 0.2) ; default: 0.7
(setq company-use-overriding-terminal-local-map nil)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "M-e") 'company-abort)
(define-key company-active-map (kbd "C-e") 'company-other-backend)
(define-key company-active-map (kbd "RET") nil) ;; local-mapを使うとRETがvimpulseに取られる
(define-key company-active-map (kbd "C-o") 'company-complete-selection)
(define-key company-active-map (kbd "M-<f1>") 'company-show-location)
(define-key company-active-map (kbd "M-i") 'company-complete-mid)
(define-key company-search-map (kbd "C-o") 'company-complete-selection)
(define-key company-search-map (kbd "M-C-o") 'company-search-kill-others)
(define-key company-mode-map (kbd "M-\\") 'company-manual-begin)

(eval-after-load "viper"
  '(defadvice company-idle-begin (around company-with-viper activate)
     (if (eq viper-current-state 'vi-state)
         (and company-candidates (company-cancel))
       ad-do-it)))
(eval-after-load "skk"
  '(defadvice company-idle-begin (around company-with-skk activate)
     (if (or skk-henkan-mode)
         (and company-candidates (company-cancel))
       ad-do-it)))

;;;; mid-expandと同等の機能
;;;; unit test => snap://clmemo-path/2009/08/18/20090818-062054.el#48:(expectations
;;;; (@> "unit test")
(defvar company-word-delimter "-_/:")
(defun company-complete-mid ()
  (interactive)
  (let ((cand (nth company-selection company-candidates)))
    (when (< (length company-prefix) (length cand))
      (let* ((c (aref cand (length company-prefix)))
             (regexp (if (string-match (char-to-string c) company-word-delimter)
                         (concat "[" company-word-delimter "]")
                       (company-char-category-regexp c)))
             (expand-regexp (if (string-equal regexp "\\ca")
                                "[A-Za-z0-9]"
                              regexp)))
        (if (string-match (concat "\\(" expand-regexp "+\\)")
                          cand
                          (length company-prefix))
            (insert (match-string 0 cand))
          (company-complete-selection))))))

;;;; company-yasnippet.el
(defun company-yasnippet-find-template (prefix tables)
  (apply #'append
         (mapcar (lambda (table)
                   (let ((templates (yas/snippet-table-to-templates-alist table)))
                     (remove-if-not (lambda (templ-cons)
                                      (string-match prefix (car templ-cons)))
                                    templates)))
                 tables)))

(defun company-yasnippet-candidates (prefix)
  (let ((tables (yas/get-snippet-tables major-mode)))
    (mapcar #'car (company-yasnippet-find-template (concat "^" prefix) tables))))

(defun company-yasnippet-key-name (key)
  (let ((tables (yas/get-snippet-tables major-mode)))
    (caar (apply #'append
                 (mapcar (lambda (table)
                           (yas/fetch table key))
                         tables)))))

(defun company-yasnippet (command &optional arg &rest ignored)
  "A `company-mode' competition back-end for yasnnipet."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-yasnippet))
    ('prefix (and (yas/get-snippet-tables major-mode)
                  (company-grab-symbol)))
    ('candidates (company-yasnippet-candidates arg))
    ('meta (company-yasnippet-key-name arg))
    ('candidate-face 'ac-yasnippet-from-name-candidate-face)))

(defun company-yasnippet-execute-expand (arg)
  (when (and (eq company-backend 'company-yasnippet)
             (eq this-command 'company-complete-selection))
    (yas/expand)))
(add-hook 'company-completion-finished-hook 'company-yasnippet-execute-expand)

;;;; buffer内でpoint近くの単語が前にくるようにするソース
(defvar company-candidate-word-regexp "\\(\\s_\\|\\sw\\)*")
(defun company-word-in-buffer-candidates (prefix)
  "search word list in current buffer. list top is near word now point"
  (if (> (length prefix) 0)
      (let ((i 0)
            (now-pos (point))
            candidate
            candidates
            (regexp (concat "\\b" (regexp-quote prefix) company-candidate-word-regexp "\\b")))
        (save-excursion
          ;; Search backward
          (goto-char (- now-pos (length prefix)))
          (while (re-search-backward regexp nil t)
            (setq candidate (match-string-no-properties 0))
            (unless (member-if (lambda (x) (string= (cdr x) candidate)) candidates)
              (push (cons (point) candidate) candidates)
              (setq i (1+ i))))
          ;; Search backward
          (goto-char now-pos)
          (while (re-search-forward regexp nil t)
            (setq candidate (match-string-no-properties 0))
            (unless (member-if (lambda (x) (string= (cdr x) candidate)) candidates)
              (push (cons (point) candidate) candidates)
              (setq i (1+ i))))
          (mapcar 'cdr
                  (sort candidates
                        (lambda (a b)
                          (< (abs (- (car a) now-pos))
                             (abs (- (car b) now-pos))))))
          ))))

(defun company-word-in-buffer (command &optional arg &rest ignored)
  "A `company-mode' completion back-end word-in-buffer+."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-word-in-buffer))
    ('prefix (company-grab-word-ja)
             ;; (company-grab-symbol)
             )
    ('sorted t)
    ('candidates (company-word-in-buffer-candidates arg))
    ('candidate-face 'ac-candidate-face)))

;;;; word-in-buffer と dabbrev-code を合わせたbackend
;;;; 通常のadapterだとソートされてnear-pointなソースが上位に来ない
(require 'company-dabbrev-code) ;; company-init-backendは実行されないのでrequire必須
(defun company-dabbrev-code+ (command &optional arg &rest ignored)
  "A `company-mode' completion backend adapt word-in-buffer and dabbrev-code."
  (interactive (list 'interactive))
  (let ((backends '(company-word-in-buffer company-dabbrev-code)))
    (case command
      ('interactive (company-begin-backend 'company-dabbrev-code+))
      ('candidates
       (delete arg
               (delete-dups
                (apply 'append
                       (mapcar (lambda (backend)
                                 (let ((cands (funcall backend 'candidates arg ignored)))
                                   (if (funcall backend 'sorted)
                                       cands
                                     (sort cands 'string<))))
                               backends)))))
      ('sorted t)
      ('duplicates nil)
      (otherwise
       (let (value)
         (dolist (backend backends)
           (when (setq value (funcall backend command arg ignored))
             (return value))))))))

;;;; ac-menu風表示をおこなうfrontend
(require 'company-pseudo-menu)
(setq company-frontends '(company-menu-unless-just-one-frontend
                          company-preview-if-just-one-frontend
                          company-echo-metadata-frontend))

;;;; multibyteとasciiとを区別するprefix-function
(defun company-grab-word-ja ()
  "Function document"
  (when (looking-at "\\>")
    (let* ((prefix-char-category (company-char-before-category))
           (prefix-char-regexp (if (equal prefix-char-category "\\ca")
                                   "\\([A-za-z0-9]\\)\\|\\s_"
                                 prefix-char-category))
          (pos (point)))
      ;; 参考・・というか丸パクリ(dabbrev--goto-start-of-abbrev)
      (save-excursion
        (save-match-data
          (when (> (point) (minibuffer-prompt-end)) ;; minibufferでないことを確かめる
            (forward-char -1)
            (while (and (looking-at prefix-char-regexp)
                        (> (point) (minibuffer-prompt-end))
                        (not (= (point) (field-beginning (point) nil
                                                         (1- (point))))))
              (forward-char -1))
            (or (looking-at prefix-char-regexp)
                (forward-char 1))
            (buffer-substring-no-properties (point) pos)))))))

(defun company-char-before-category (&optional pos)
  "Return regexp correspond character category before point."
  (company-char-category-regexp (char-before pos)))

(defun company-char-category-regexp (char)
  (let ((c (char-category-set char)))
    (cond
     ((aref c ?a) "\\ca")    ; ASCII
     ;; ((aref c ?a) "[A-Za-z0-9-_:/]")    ; ASCII
     ((aref c ?j)                       ; Japanese
      (cond
       ((aref c ?K) "\\cK")             ; katakana
       ((aref c ?A) "\\cA")             ; 2byte alphanumeric
       ((aref c ?H) "\\cH")             ; hiragana
       ((aref c ?C) "\\cC")             ; kanji
       (t "\\cj")))
     ((aref c ?k) "\\ck")               ; hankaku-kana
     ((aref c ?r) "\\cr")               ; Japanese roman ?
     (t "\\sw\\|\\s_"))))

;;;; (@* "unit test")
(defmacro generate-test-buffer-and-execute (&rest body)
  `(let ((buff (get-buffer-create " *company-test")))
     (with-current-buffer buff
       (erase-buffer)
       ,@body)))
(expectations
  (expect "foo"
    (generate-test-buffer-and-execute
     (insert "foo")
     (company-grab-word)))
  (expect nil
    (generate-test-buffer-and-execute
     (insert "foo")
     (save-excursion (insert "bar"))
     (company-grab-word)))
  (desc "マルチバイト文字含みな場合")
  (expect "foo"
    (generate-test-buffer-and-execute
     (insert "ひらがなfoo")
     (company-grab-word-ja)))
  (expect nil
    (generate-test-buffer-and-execute
     (insert "ひらがなfoo")
     (save-excursion (insert "bar"))
     (company-grab-word-ja)))
  (expect 4
    (generate-test-buffer-and-execute
     (insert "foo")
     (company-grab-word-ja)
     (point)))
  (expect "ひらがな"
    (generate-test-buffer-and-execute
     (insert "ひらがな")
     (company-grab-word-ja)))
  (expect "ひらがな"
    (generate-test-buffer-and-execute
     (insert "fooひらがな")
     (company-grab-word-ja)))
  (expect "ひらがな"
    (generate-test-buffer-and-execute
     (insert "カタカナ，ひらがな")
     (company-grab-word-ja)))
  (desc "括弧含みな場合")
  (expect "foo"
    (generate-test-buffer-and-execute
     (insert "(foo")
     (company-grab-word-ja)))
  (desc "asciiでword構成文字ではなく，symbol構成文字の場合")
  (expect "foo-bar"
    (generate-test-buffer-and-execute
     (insert "foo-bar")
     (company-grab-word-ja)))
  (desc "company-complete-mid")
  (expect 'called
    (let ((company-selection 0)
          (company-candidates '("foobar-baz"))
          (company-prefix "fo"))
      (mocklet ((company-manual-begin => t)
                (company-complete-selection => nil)
                ((insert "obar") => 'called))
        (company-complete-mid)
        )))
  (expect 'called
    (let ((company-prefix "fo")
          (company-candidates '("foooから"))
          (company-selection 0))
      (mocklet ((company-manual-begin => t)
                (company-complete-selection => nil)
                ((insert "oo") => 'called))
        (company-complete-mid)
        )))
  (expect t
    (let ((company-prefix "まだ")
          (company-candidates '("まだまだ途中"))
          (company-selection 0))
      (mocklet ((company-manual-begin => t)
                (company-complete-selection => nil)
                ((insert "まだ") => t))
        (company-complete-mid)
        )))
  (expect t
    (let ((company-prefix "途中")
          (company-candidates '("途中からloop"))
          (company-selection 0))
      (mocklet ((company-manual-begin => t)
                (company-complete-selection => nil)
                ((insert "から") => t))
        (company-complete-mid)
        )))
  (expect 'called
    (let ((company-prefix "foo")
          (company-candidates '("foo-bar"))
          (company-selection 0))
      (mocklet ((company-manual-begin => t)
                (company-complete-selection => nil)
                ((insert "-") => 'called))
        (company-complete-mid)
        )))
  )

;;;; company 側で根本的な解決が必要か・・・
(when (boundp 'anything-before-initialize-hook)
  (add-hook 'anything-before-initialize-hook 'company-abort))

;;;; company-filenames (ac-source-files-in-current-dir + ac-source-filename)
(defface company-filenames-face
  '((t (:background "BlueViolet" :foreground "White")))
  "Face for filename candidates.")

(defun company-filename-candidate (prefix)
  "from ac-filename-candidate"
  (let ((dir (file-name-directory prefix)))
    (if dir
        (ignore-errors
          (delq nil
                (mapcar (lambda (file)
                          (if (not (member file '("./" "../")))
                              (concat dir file)))
                        (file-name-all-completions
                         (file-name-nondirectory prefix) dir))))
      (all-completions prefix (directory-files default-directory)))))

(defun company-filenames (command &optional arg &rest ignored)
  "A `company-mode' completion back-end word-in-buffer+."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-filenames))
    ('prefix (thing-at-point 'filename))
    ('candidates (company-filename-candidate arg))
    ('candidate-face 'company-filenames-face)
    ))
