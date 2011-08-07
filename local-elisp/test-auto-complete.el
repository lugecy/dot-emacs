;;;; test for auto-complete.el
(defmacro generate-test-buffer-and-prefix-test (&rest body)
  `(let ((buff (get-buffer-create " *test-buffer*")))
     (with-current-buffer buff
       (erase-buffer)
       (buffer-substring-no-properties (progn ,@body) (point)))))

(defmacro ly:ac-wildcard-test-1 (pre-string prefix &rest body)
  `(with-temp-buffer
     (insert ,pre-string)
     (let ((ac-point 1)
           (ac-prefix ,prefix)
           (ac-sources '(((candidates . '("foobar" "foobaz" "foobarbaz"))))))
       ,@body)))

(defmacro ly:ac-wildcard-test-2 (prefix wildcard &rest body)
  (declare (indent 2))
  `(with-temp-buffer
    (let* ((auto-complete-mode t)
           (ac-sources '(((candidates . '("foobar" "foobaz" "foobarbaz"))))))
      (insert ,prefix)
      (ac-start t)
      (insert ,wildcard)
      (ac-start t)
      (prog1 ,@body (ac-abort)))))

(expectations
  (desc "prefix function test")
  (desc "basic case")
  (expect "foo"
    (generate-test-buffer-and-prefix-test
     (insert "foo")
     (ac-prefix-default)))
  (desc "マルチバイト文字含みな場合")
  (expect "foo"
    (generate-test-buffer-and-prefix-test
     (insert "ひらがなfoo")
     (ac-prefix-default)))
  (expect "foo"
    (generate-test-buffer-and-prefix-test
     (insert "ひらがなfoo")
     (save-excursion (insert "bar"))
     (ac-prefix-default)))
  (expect "ひらがな"
    (generate-test-buffer-and-prefix-test
     (insert "ひらがな")
     (ac-prefix-default)))
  (expect "ひらがな"
    (generate-test-buffer-and-prefix-test
     (insert "fooひらがな")
     (ac-prefix-default)))
  (expect "ひらがな"
    (generate-test-buffer-and-prefix-test
     (insert "カタカナ，ひらがな")
     (ac-prefix-default)))
  (desc "括弧含みな場合")
  (expect "foo"
    (generate-test-buffer-and-prefix-test
     (insert "(foo")
     (ac-prefix-default)))
  (desc "asciiでword構成文字ではなく，symbol構成文字の場合")
  (expect "foo-bar"
    (generate-test-buffer-and-prefix-test
     (insert "foo-bar")
     (ac-prefix-default)))
  (expect "bar"
    (generate-test-buffer-and-prefix-test
     (insert "foo[bar")
     (ac-prefix-default))))

;;;; popup-isearchが実装されたので廃止
;; (desc "neocompcache風のwildcard指定を考慮したprefix関数")
;; (expect t
;;   (ly:ac-wildcard-test-1 "foo*bar*baz" "foo"
;;                          (ac-prefix-wildcard-p)))
;; (expect '("bar" "baz")
;;   (ac-prefix-subword-list "foo*bar*baz" "foo"))
;; (expect nil
;;   (ly:ac-wildcard-test-1 "foobar" nil
;;                          (ac-prefix-wildcard-p)))
;; (expect t
;;   (ly:ac-wildcard-test-1 "foo*" "foo"
;;                          (ac-prefix-wildcard-p)))
;; (expect nil
;;   (ac-prefix-subword-list "foo*" "foo"))
;; (desc "ac-prefix-submatchを考慮した候補探索関数")
;; (expect '("foobarbaz")
;;   (let ((cands '("foobar" "foobaz" "foobarbaz"))
;;         (submatchlist '("bar" "baz")))
;;     (ac-submatch-filter "foo" cands submatchlist)))
;; (desc "ac-start with wildcard")
;; (expect '("foo" ("bar"))
;;   (ly:ac-wildcard-test-2 "foo" "*bar"
;;     (list ac-prefix ac-prefix-submatch)))
;; (expect '("foo" ("bar" "baz"))
;;   (ly:ac-wildcard-test-2 "foo" "*bar*baz"
;;     (list ac-prefix ac-prefix-submatch)))
;; (expect '("foobarbaz")
;;   (ly:ac-wildcard-test-2 "foo" "*bar*baz"
;;     (mapcar (lambda (s) (substring-no-properties s 0)) (ac-candidates))))
;; (expect "foo"
;;   (ly:ac-wildcard-test-2 "* foo" ""
;;     ac-prefix))
;; (expect nil
;;   (ly:ac-wildcard-test-2 "* foo" ""
;;     (ac-prefix-wildcard-p)))
;; (expect nil
;;   (ly:ac-wildcard-test-2 "foo" "*bar"
;;     (progn
;;       (ac-abort)
;;       (ac-start t)
;;       (delete-backward-char 1)
;;       (ac-start t)
;;       (ac-prefix-wildcard-p))))
  
