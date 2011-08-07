
(defface hit-a-hint-face
  '((t (:foreground "black" :background "yellow")))
  "for hit-a-hint face")

(defvar hah-char-list '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
  "Hit-a-hint label char list.")

(defvar hah-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    map)
  "for hah minibuffer map.")

(defmacro hah-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  (declare (indent 2)
           (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro hah-awhen (test-form &rest body)
  (declare (indent 1)
           (debug (form body)))
  `(hah-aif ,test-form
        (progn ,@body)))

(defun hah-make-label-list (require-length chars)
  "make label list."
  (let ((list-1 (mapcar #'list chars)))
    (loop for list = list-1 then (hah-make-label-list-aux list list-1)
          while (< (length list) require-length)
          finally return (mapcar (lambda (clist)
                                   (mapconcat #'char-to-string clist ""))
                                 list))))

(defun hah-make-label-list-aux (list-1 list-2)
  (loop for x in list-1
        append (loop for y in list-2
                     collect (append x y))))

(defun hah-make-label-overlays (pos-list)
  "make label overlays in current-buffer."
  (loop for label in (hah-make-label-list (length pos-list) hah-char-list)
        for pos in pos-list
        for ov = (let ((ov (make-overlay pos pos)))
                   (prog1 ov
                     (overlay-put ov 'before-string (propertize (format "[%s]" label)
                                                                'face 'hit-a-hint-face))))
        collect (cons label ov)))

(defun hit-a-hint (pos-list)
  "Return hit label position."
  (let* ((ov-alist (hah-make-label-overlays pos-list))
         (singlep (<= (length ov-alist) (length hah-char-list))))
    (unwind-protect
        (let ((label (let ((enable-recursive-minibuffers t))
                       (if singlep
                           (char-to-string (read-char "input hit: "))
                         (read-from-minibuffer "input multi hit: " nil hah-minibuffer-map)))))
          (hah-aif (assoc-default label ov-alist)
              (overlay-start it)
            (prog1 nil
              (minibuffer-message "Opps!!!"))))
      (dolist (pair ov-alist)
        (delete-overlay (cdr pair))))))

;;;; interactive functions
(defun hah-collect-pos (func &optional reverse)
  (let ((wbeg (window-start))
        (wend (window-end))
        pos-list)
    (save-excursion
      (save-restriction
        (narrow-to-region wbeg wend)
        (goto-char (if reverse wend wbeg))
        (setq pos-list (loop for pos = (funcall func)
                             while pos
                             collect pos))
        (if reverse
            (nreverse pos-list)
          pos-list)))))

(defvar hit-a-hint-pos-source
  '(beginning-of-defun end-of-defun beginning-of-line end-of-line backward-up-list up-list))

(defun hit-a-hint-sexp ()
  "Function documantion."
  (interactive)
  (let ((pos-list (sort (delete-dups (loop for pos-func in hit-a-hint-pos-source
                                           for pos = (condition-case err
                                                         (save-excursion (funcall pos-func) (point))
                                                       (error nil))
                                           if pos collect pos))
                        #'<)))
    (goto-char (hit-a-hint pos-list))))

(defun hah-search-word-pos (word)
  (hah-collect-pos (lambda ()
                     (re-search-backward word nil t))
                   t))

(defun hit-a-hint-word (word)
  "Function documantion."
  (interactive "sHaH word: \n")
  (highlight-regexp word 'hi-green)     ; XXX: バッファ全体が対象になってしまう
  (unwind-protect
      (hah-aif (hit-a-hint (hah-search-word-pos word))
          (goto-char it))
    (unhighlight-regexp word)))

(defun isearch-hah ()
  "Function documantion."
  (interactive)
  (isearch-exit)
  (hit-a-hint-word (if isearch-regexp
                       isearch-string
                     (regexp-quote isearch-string))))

(defun hah-on-info-mode ()
  (interactive)
  (hah-aif (hit-a-hint (hah-collect-pos
                (lambda ()
                  (let ((pat "\\*note[ \n\t]+\\([^:]+\\):\\|^\\* .*:\\|[hf]t?tps?://"))
                    (when (re-search-forward pat nil t)
                      (goto-char (or (match-beginning 1) (match-beginning 0)))
                      (when (looking-at "^\\* ")
                        (forward-char 2))
                      (point))))))
      (goto-char it)))

(defun hit-a-hint-goto-symbol ()
  "Goto symbol head position."
  (interactive)
  (hah-aif (hit-a-hint
            (hah-collect-pos
             (lambda ()
               (re-search-backward "\\_<" nil t))
             t))
      (goto-char it)))

(global-set-key (kbd "C-c C-l") (make-sparse-keymap))
(global-set-key (kbd "C-c C-l C-l") 'hit-a-hint-sexp)
(global-set-key (kbd "C-c C-l C-s") 'hit-a-hint-word)
(define-key isearch-mode-map (kbd "C-M-l") 'isearch-hah)
(global-set-key (kbd "C-c C-l C-f") 'hit-a-hint-goto-symbol)
(ly:eval-after-load "info"
  (define-key Info-mode-map (kbd "C-c C-l C-l") 'hah-on-info-mode))

;;;; for minibuffer functions
(defun ly:nullstr-p (obj)
  (and (stringp obj) (eq (length obj) 0)))

(defun hah-on-minibuffer-get-target (cont)
  "Get target from minibuffer-contents or selected-window."
  (with-selected-window (minibuffer-selected-window)
    (save-excursion
      (let ((w (if (ly:nullstr-p cont)
                   (thing-at-point 'word)
                 cont)))
        (when (and (not (ly:nullstr-p w))
                   (hit-a-hint-word w))
          (thing-at-point 'symbol))))))

(defun hah-on-minibuffer (&optional window)
  (interactive)
  (when (minibufferp)
    (let* ((cont (minibuffer-contents))
           (target (hah-on-minibuffer-get-target cont)))
      (if target
          (progn
            (delete-minibuffer-contents)
            (insert target))
        (minibuffer-message "hah error")))))

(define-key anything-map (kbd "M-e") 'hah-on-minibuffer)
(define-key anything-c-moccur-anything-map (kbd "M-e") 'hah-on-minibuffer)
(define-key minibuffer-local-map (kbd "M-e") 'hah-on-minibuffer)

;;;; for w3m
(ly:eval-after-load "w3m"
  (defun w3m-hah-anchor-list ()
    (let ((wbeg (window-start))
          (wend (window-end)))
      (save-excursion
        (goto-char wbeg)
        (loop while (and (w3m-goto-next-anchor)
                         (< (point) wend))
              collect (point)))))

  (defun w3m-hah ()
    (interactive)
    (goto-char (hit-a-hint (w3m-hah-anchor-list))))

  (define-key w3m-mode-map (kbd "C-c TAB") 'w3m-hah))

;;;; for twittering-mode
(ly:eval-after-load 'twittering-mode
  (defun twit-hah-uri-list (window)
    (let ((start (window-start window))
          (end (window-end window))
          pos-list pos)
      (setq pos (next-single-property-change start 'uri nil end))
      (while (and pos (< pos end))
        (hah-awhen (get-text-property pos 'uri)
          (push pos pos-list))
        (setq pos (next-single-property-change pos 'uri nil end)))
      (nreverse pos-list)))
  (defun twit-hah ()
    (interactive)
    (save-excursion
      (hah-awhen (hit-a-hint (twit-hah-uri-list (selected-window)))
        (goto-char it)
        (twittering-enter))))
  (defun twit-hah-righthand ()
    (interactive)
    (let ((hah-char-list (list ?h ?j ?k ?l)))
      (twit-hah)))
  (define-key twittering-mode-map (kbd "e") 'twit-hah)
  (define-key twittering-mode-map (kbd "m") 'twit-hah-righthand))

;;;; unit-test
(expectations
  (expect '("aa" "ab" "ac" "ba" "bb" "bc" "ca" "cb" "cc")
    (let ((list (list ?a ?b ?c)))
      (hah-make-label-list 9 list)))
  (expect '("aaa" "aab" "aac" "aba" "abb" "abc" "aca" "acb" "acc" "baa" "bab" "bac" "bba" "bbb" "bbc" "bca" "bcb" "bcc" "caa" "cab" "cac" "cba" "cbb" "cbc" "cca" "ccb" "ccc")
    (let ((list (list ?a ?b ?c)))
      (hah-make-label-list 10 list)))
  (expect '(1 5 9)
    (let ((str "foo bar baz"))
      (stub window-start => 1)
      (stub window-end => (1+ (length str)))
      (with-temp-buffer
        (insert str)
        (hah-collect-pos (lambda ()
                           (when (re-search-forward "\\_<" nil t)
                             (prog1 (match-beginning 0)
                               (forward-char))))))))
  (expect '(1 5 9)
    (let ((str "foo bar baz"))
      (stub window-start => 1)
      (stub window-end => (1+ (length str)))
      (with-temp-buffer
        (insert str)
        (hah-collect-pos (lambda ()
                           (re-search-backward "\\_<" nil t))
                         t))))
  (expect 1
    (stub read-char => ?a)
    (with-temp-buffer
      (insert "(foo bar baz)")
      (goto-char 5)
      (hit-a-hint-sexp)
      (point)))
  (expect 14
    (stub read-char => ?s)
    (with-temp-buffer
      (insert "(foo bar baz)")
      (goto-char 5)
      (hit-a-hint-sexp)
      (point)))
  (expect 6
    (let ((str "(foo bar baz)"))
      (stub read-char => ?a)
      (stub window-start => 1)
      (stub window-end => (1+ (length str)))
      (with-temp-buffer
        (insert str)
        (hit-a-hint-word "ba")
        (point))))
  (expect 1
    (let ((str "x1x2x3x4x5x6x7x8x9x10x11x12"))
      (stub read-string => "aa")
      (stub window-start => 1)
      (stub window-end => (1+ (length str)))
      (with-temp-buffer
        (insert str)
        (hit-a-hint-word "x")
        (point))))
  (expect 19
    (let ((str "x1x2x3x4x5x6x7x8x9x10x11x12"))
      (stub read-string => "sa")
      (stub window-start => 1)
      (stub window-end => (1+ (length str)))
      (with-temp-buffer
        (insert str)
        (hit-a-hint-word "x")
        (point))))
  (expect 4
    (let ((str "x1 x2 x3 x4 x5"))
      (stub read-char => ?s)
      (stub window-start => 1)
      (stub window-end => (1+ (length str)))
      (with-temp-buffer
        (insert str)
        (hit-a-hint-goto-symbol)
        (point))))
  )

