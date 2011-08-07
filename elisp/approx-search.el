;;; -*- coding: euc-jp -*-
;;; approx-search.el --- approximate pattern matching library, 曖昧検索ライブラリ

;; Copyright (C) 2004 Susumu Ota

;; Author: Susumu ota <ccbcc@black.livedoor.com>
;; Keywords: approximate pattern matching, search, isearch

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:
;;
;; 概要:
;;
;;   曖昧検索, approximate pattern matching を可能にする Emacs Lisp ラ
;;   イブラリ.
;;
;;   他人の書いたプログラムや, 遠い昔に自分で書いたプログラムをメンテ
;;   している時, 難しい単語やスペルミスした単語がうまく検索できなくて
;;   困ったことありませんか? そういう時に曖昧検索できると便利だと思っ
;;   てこのライブラリを書きました.
;;
;;   以前 Levenshtein distance(*1) というアルゴリズムを使って曖昧検索
;;   を実装したのですが, あまりにも遅すぎるため, いまいち使えませんで
;;   した. 今回は正規表現ベースのアルゴリズムを使って高速に曖昧検索で
;;   きるようにしました.
;;
;;   基本的なアイデアは, 入力された文字列から曖昧検索のための正規表現
;;   を生成するというもので, Migemo(*2) を参考にしました. Migemo の場
;;   合はローマ字入力から日本語の正規表現を生成しますが, このライブラ
;;   リでは入力から曖昧検索の正規表現を生成します. 具体的にはこんな感
;;   じです.
;;
;;     (approx-generate-regexp "abcd")
;;     => "\\(bcd\\|acd\\|abd\\|abc\\|a.cd\\|ab.d\\|ab.cd\\)"
;;
;;   この生成された正規表現を `re-search-forward' に渡して曖昧検索を実
;;   現しています. 入力された文字数を N 個とすると, だいたい 3N 個程度
;;   のパターンを生成して, その OR パターンで正規表現検索します.
;;
;;   曖昧度(ambiguousness)を指定できるようにしました. 曖昧度を大きくす
;;   ると再帰的にパターンを生成して, より曖昧な検索が可能となります.
;;   デフォルトの曖昧度は 1 です. M-x approx-set-ambiguousness で曖昧
;;   度を設定できます.
;;
;;   インクリメンタルサーチ(isearch)にも対応しています. Meadow でも動
;;   作するように修正しました(*3).
;;
;;   Migemo を使った曖昧検索はできません. Migemo に対応することもでき
;;   ますが, 作者がこのライブラリを使う場面は主にプログラムを編集する
;;   時(つまり Migemo を off にしている時)なので, 今のところ Migemo で
;;   利用できるようにする必要はあまり感じていません.
;;
;;
;;   (*1) Levenshtein distance (edit distance, 編集距離) については以
;;        下を参照.
;;        http://www.merriampark.com/ld.htm
;;
;;   (*2) http://migemo.namazu.org/
;;
;;   (*3) http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=approx-search
;;
;;
;; 設定方法:
;;
;;   かなりテキトーなものですが, Makefile を付属しました. Makefile の
;;   先頭部分を適当に編集した後,
;;
;;     % make
;;     % make install
;;
;;   でインストールできます.
;;
;;   ~/.emacs に
;;
;;     (add-to-list 'load-path "~/elisp")
;;     (require 'approx-search)
;;     (if (boundp 'isearch-search-fun-function)
;;         (require 'approx-isearch)
;;       (require 'approx-old-isearch))
;;     (approx-isearch-set-enable)
;;
;;   と書きます.
;;
;;   Mmigemo と併用する場合, Migemo の設定の後に以下のように書くと,
;;   M-x migemo-toggle-isearch-enable で Migemo を無効にした場合のみ
;;   approx-isearch が有効にできます(逆に Migemo を有効にしたら
;;   approx-isearch が無効になります).
;;
;;     (add-to-list 'load-path "~/elisp")
;;     (require 'approx-search)
;;     (if (boundp 'isearch-search-fun-function)
;;         (require 'approx-isearch)
;;       (require 'approx-old-isearch))
;;
;;     (if migemo-isearch-enable-p
;;         (approx-isearch-set-disable)
;;       (approx-isearch-set-enable))
;;
;;     (defadvice migemo-toggle-isearch-enable (before approx-ad-migemo-toggle-isearch-enable activate)
;;       "migemo を使う時は approx-search を使わない."
;;       (if migemo-isearch-enable-p
;;           (approx-isearch-set-enable) ; NOT disable!!! before advice なので
;;         (approx-isearch-set-disable)))
;;


;; 使い方:
;;
;;   [1] approx-search-{forward,backward}
;;
;;     M-x approx-search-forward
;;       曖昧検索を使って STRING を point から前方検索して見つかった位置を返す.
;;
;;     M-x approx-search-backward
;;       曖昧検索を使って STRING を point から後方検索して見つかった位置を返す.
;;
;;       例:
;;         (approx-search-forward "approximately")
;;         (approx-search-backward "approximately")
;;           "aproximately", "appproximately", "apploximately" にもマッチする.
;;
;;     M-x approx-set-ambiguousness
;;       曖昧度を設定する. この値を大きくすると許容できる曖昧度が増す.
;;       デフォルトは 1.
;;
;;
;;   [2] isearch
;;
;;     M-x approx-isearch-enable-p
;;       曖昧検索を使った isearch が有効か否かを返す.
;;
;;     M-x approx-isearch-set-enable
;;       曖昧検索を使った isearch を有効にする.
;;
;;     M-x approx-isearch-set-disable
;;       曖昧検索を使った isearch を無効にする.
;;
;;     M-x approx-isearch-toggle-enable
;;       曖昧検索を使った isearch の有効/無効を切り換える.
;;
;;     変数 approx-isearch-auto-p
;;       通常の search で見つからなかった場合のみ曖昧検索を行う.
;;       デフォルトは nil.
;;
;;

;;; Code:

(eval-when-compile
  (require 'cl))


;;;
;;; parameter
;;;
(defconst approx-cvs-id "$Id: approx-search.el,v 1.2 2004/02/14 17:15:23 ota Exp $")

(defvar approx-ambiguousness 1
  "Ambiguousness of approximate pattern matching.
曖昧度.")

(defvar approx-wild-card ?.
  "Wild card character.
正規表現の「任意の1文字にマッチ」. 文字と文字列のどっちでも指定できる.")

(defvar approx-back-slash ?\\
  "Back slash character.
バックスラッシュ文字. ユーザの入力文字列をエスケープするために使用. 変更不可.")

(defvar approx-re-control-char-list '(?| ?\\ ?. ?\" ?\[ ?\] ??)
  "Control char list in RE.
ユーザの入力文字列をバックスラッシュでエスケープしなければならない文字のリスト. これだけで十分かよくわからない. 要テスト.
see `approx-add-back-slash'." )

(defvar approx-re-pipe-mark "\\|"
  "Pipe mark in RE.
正規表現のパイプ文字. 正規表現の OR, \\(...\\|...\\|...\\) を生成するために使用.")

(defvar approx-re-left-paren "\\("
  "Left paren in RE.
正規表現の左括弧. 正規表現の OR, \\(...\\|...\\|...\\) を生成するために使用.")

(defvar approx-re-right-paren "\\)"
  "Right paren in RE.
正規表現の右括弧. 正規表現の OR, \\(...\\|...\\|...\\) を生成するために使用.")


;;;
;;; utility
;;;
(defsubst approx-copy-list (list)
  "(approx-copy-list '(1 2 3 4)) => (1 2 3 4)."
  (mapcar #'identity list))

(defsubst approx-concat-list (string-or-char-list)
  "(approx-concat-list '(\"a\" ?b \"c\" ?d)) => \"abcd\"."
  ;; (approx-concat-list '("a" ?b "c" ?d))
  (apply #'concat (mapcar #'(lambda (item)
			      (if (stringp item) item (string item)))
			  string-or-char-list)))

(defsubst approx-concat (&rest string-or-char-list)
  "(approx-concat \"a\" ?b \"c\" ?d) => \"abcd\"."
  ;; (approx-concat "a" ?b "c" ?d)
  (approx-concat-list string-or-char-list))

(defsubst approx-join (sequence separator)
  "(approx-join '(\"a\" \"b\" \"c\" \"d\") \"|\") => \"a|b|c|d\"."
  ;; (approx-join '("a" "b" "c" "d") "|")
  (mapconcat #'identity sequence separator))

(defsubst approx-chop-first-and-last (list)
  "(approx-chop-first-and-last '(1 2 3 4)) => (2 3)."
  (when (caddr list)
    (nreverse (cdr (reverse (cdr list))))))

(defsubst approx-remove-if-not (approx-remove-if-not-function list)
  "(approx-remove-if-not #'oddp '(1 2 3 4)) => (1 3).
see `remove-if-not'."
  ;; inline?
  (let ((result nil))
    (dolist (item list)
      (when (funcall approx-remove-if-not-function item)
	(push item result)))
    (nreverse result)))

(defsubst approx-unique-member (list)
  "(approx-unique-member '((a b c) (1 2 3) (a b c) (4 5 6)))
=> ((a b c) (1 2 3) (4 5 6))."
  (let ((result nil))
    (dolist (item list)
      (unless (member item result)
	(push item result)))
    (nreverse result)))

(defsubst approx-inset-nth (n list object)
  "(approx-inset-nth 1 '(a b c d) 'x) => (a x b c d)."
  (let ((i 0) (result nil))
    (dolist (item list)
      (when (= i n)
	(push object result))
      (push item result)
      (incf i))
    (when (= i n)
      (push object result))
    (nreverse result)))


;;;
;;; generate pattern
;;;
(defun approx-add-back-slash (char-list &optional re-control-char-list back-slash)
  "(approx-add-back-slash '(?a ?. ?c ?d)) => (?a \"\\\\.\" ?c ?d)."
  (let ((re-control-char-list (or re-control-char-list
				  approx-re-control-char-list))
	(back-slash (or back-slash approx-back-slash)))
    (mapcar #'(lambda (char)
		(if (memq char re-control-char-list) (string back-slash char)
		  char))
	    char-list)))

(defun approx-generate-replace-pattern (src-list replace-object)
  "(approx-generate-replace-pattern '(?a ?b ?c ?d) ?.)
=> ((?. ?b ?c ?d) (?a ?. ?c ?d) (?a ?b ?. ?d) (?a ?b ?c ?.))."
  (let ((index -1))
    (mapcar #'(lambda (dummy)
		(let ((dst (approx-copy-list src-list)))
		  (setf (nth (incf index) dst) replace-object)
		  dst))
	    src-list)))

(defun approx-generate-insert-pattern (src-list insert-object)
  "(approx-generate-insert-pattern '(?a ?b ?c ?d) ?.)
=> ((?. ?a ?b ?c ?d) (?a ?. ?b ?c ?d) (?a ?b ?. ?c ?d) (?a ?b ?c ?. ?d))."
  (let ((index -1))
    (mapcar #'(lambda (dummy)
		(approx-inset-nth (incf index) src-list insert-object))
	    src-list)))

(defun approx-generate-replace-nil-pattern (src-list)
  "(approx-generate-replace-nil-pattern '(?a ?b ?c ?d))
=> ((?b ?c ?d) (?a ?c ?d) (?a ?b ?d) (?a ?b ?c))."
  (mapcar #'(lambda (pattern) (approx-remove-if-not #'identity pattern))
	  (approx-generate-replace-pattern src-list nil)))

(defun approx-generate-replace-wild-card-pattern (src-list &optional wild-card)
  "(approx-generate-replace-wild-card-pattern '(?a ?b ?c ?d) ?.)
=> ((?a ?. ?c ?d) (?a ?b ?. ?d))."
  (let ((wild-card (or wild-card approx-wild-card)))
    (approx-chop-first-and-last
     (approx-generate-replace-pattern src-list wild-card))))

(defun approx-generate-insert-wild-card-pattern (src-list &optional wild-card)
  "(approx-generate-insert-wild-card-pattern '(?a ?b ?c ?d) ?.)
=> ((?a ?b ?. ?c ?d))."
  (let ((wild-card (or wild-card approx-wild-card)))
    (approx-chop-first-and-last
     (cdr (approx-generate-insert-pattern src-list wild-card)))))

(defun approx-generate-pattern-1 (src-list &optional wild-card)
  "(approx-generate-pattern-1 '(?a ?b ?c ?d) ?.)
=> ((?b ?c ?d) (?a ?c ?d) (?a ?b ?d) (?a ?b ?c) (?a ?. ?c ?d) (?a ?b ?. ?d) (?a ?b ?. ?c ?d))."
  (nconc (approx-generate-replace-nil-pattern src-list)
	 (approx-generate-replace-wild-card-pattern src-list wild-card)
	 (approx-generate-insert-wild-card-pattern src-list wild-card)))

(defun approx-generate-pattern (src-string &optional ambiguousness wild-card)
  "(approx-generate-pattern \"abcd\" 1 ?.)
=> (\"bcd\" \"acd\" \"abd\" \"abc\" \"a.cd\" \"ab.d\" \"ab.cd\")."
  ;; (approx-generate-pattern "abcd" 1 ?.)
  (let ((ambiguousness (or ambiguousness approx-ambiguousness)))
    (let* ((src-list (approx-add-back-slash (string-to-list src-string)))
	   (result (list src-list)))
      (dotimes (i ambiguousness)
	(setq result
	      (approx-unique-member
	       (apply #'nconc
		      (mapcar #'(lambda (src-list)
				  (approx-generate-pattern-1 src-list
							     wild-card))
			      result)))))
      (mapcar #'approx-concat-list result))))

(defun approx-generate-regexp (src-string &optional ambiguousness re-pipe-mark re-left-paren re-right-paren wild-card)
  "(approx-generate-regexp \"abcd\")
=> \"\\\\(bcd\\\\|acd\\\\|abd\\\\|abc\\\\|a.cd\\\\|ab.d\\\\|ab.cd\\\\)\"."
  ;; (approx-generate-regexp "abcd")
  (let ((re-pipe-mark (or re-pipe-mark approx-re-pipe-mark))
	(re-left-paren (or re-left-paren approx-re-left-paren))
	(re-right-paren (or re-right-paren approx-re-right-paren)))
    (concat re-left-paren
	    (approx-join (approx-generate-pattern src-string
						  ambiguousness
						  wild-card)
			 re-pipe-mark)
	    re-right-paren)))


;;;
;;; search-{forward,backward}
;;;
(defun approx-search-forward (string &optional bound noerror count)
  "Approx search forward from point for STRING.
曖昧検索を使って STRING を point から前方検索して見つかった位置を返す.
see `search-forward', `re-search-forward'."
  (interactive "sSearch: \nP\nP")
  (re-search-forward (approx-generate-regexp string) bound noerror count))

(defun approx-search-backward (string &optional bound noerror count)
  "Approx search backward from point for STRING.
曖昧検索を使って STRING を point から後方検索して見つかった位置を返す.
see `search-backward', `re-search-backward'."
  (interactive "sSearch: \nP\nP")
  (re-search-backward (approx-generate-regexp string) bound noerror count))

(defun approx-auto-search-forward (string &optional bound noerror count)
  "Approx search forward from point for STRING.
曖昧検索を使って STRING を point から前方検索して見つかった位置を返す. まず `search-forward' で検索して見つからなかった場合のみ曖昧検索 `approx-search-forward' を行う.
see `search-forward', `re-search-forward'."
  (interactive "sSearch: \nP\nP")
  (let ((result (condition-case err
		    (search-forward string bound noerror count)
		  (search-failed
		   (approx-search-forward string bound noerror count)))))
    (if result result
      (approx-search-forward string bound noerror count))))

(defun approx-auto-search-backward (string &optional bound noerror count)
  "Approx search backward from point for STRING.
曖昧検索を使って STRING を point から後方検索して見つかった位置を返す. まず `search-backward' で検索して見つからなかった場合のみ曖昧検索 `approx-search-backward' を行う.
see `search-backward', `re-search-backward'."
  (interactive "sSearch: \nP\nP")
  (let ((result (condition-case err
		    (search-backward string bound noerror count)
		  (search-failed
		   (approx-search-backward string bound noerror count)))))
    (if result result
      (approx-search-backward string bound noerror count))))

(defun approx-set-ambiguousness (ambiguousness)
  "Set ambiguousness value.
曖昧度を設定する. この値を大きくすると許容できる曖昧度が増す."
  (interactive "nAmbiguousness: ")
  (let ((old-value approx-ambiguousness))
    (setq approx-ambiguousness ambiguousness)
    old-value))


(provide 'approx-search)

;;; approx-search.el ends here
