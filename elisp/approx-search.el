;;; -*- coding: euc-jp -*-
;;; approx-search.el --- approximate pattern matching library, ۣ�渡���饤�֥��

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
;; ����:
;;
;;   ۣ�渡��, approximate pattern matching ���ǽ�ˤ��� Emacs Lisp ��
;;   ���֥��.
;;
;;   ¾�ͤν񤤤��ץ�����, ���Τ˼�ʬ�ǽ񤤤��ץ��������
;;   ���Ƥ����, �񤷤�ñ��䥹�ڥ�ߥ�����ñ�줬���ޤ������Ǥ��ʤ���
;;   ���ä����Ȥ���ޤ���? ������������ۣ�渡���Ǥ�����������Ȼפ�
;;   �Ƥ��Υ饤�֥���񤭤ޤ���.
;;
;;   ���� Levenshtein distance(*1) �Ȥ������르�ꥺ���Ȥä�ۣ�渡��
;;   ����������ΤǤ���, ���ޤ�ˤ��٤����뤿��, ���ޤ����Ȥ��ޤ����
;;   ����. ���������ɽ���١����Υ��르�ꥺ���Ȥäƹ�®��ۣ�渡����
;;   ����褦�ˤ��ޤ���.
;;
;;   ����Ū�ʥ����ǥ���, ���Ϥ��줿ʸ���󤫤�ۣ�渡���Τ��������ɽ��
;;   ����������Ȥ�����Τ�, Migemo(*2) �򻲹ͤˤ��ޤ���. Migemo �ξ�
;;   ��ϥ��޻����Ϥ������ܸ������ɽ�����������ޤ���, ���Υ饤�֥�
;;   ��Ǥ����Ϥ���ۣ�渡��������ɽ�����������ޤ�. ����Ū�ˤϤ���ʴ�
;;   ���Ǥ�.
;;
;;     (approx-generate-regexp "abcd")
;;     => "\\(bcd\\|acd\\|abd\\|abc\\|a.cd\\|ab.d\\|ab.cd\\)"
;;
;;   �����������줿����ɽ���� `re-search-forward' ���Ϥ���ۣ�渡�����
;;   �����Ƥ��ޤ�. ���Ϥ��줿ʸ������ N �ĤȤ����, �������� 3N ������
;;   �Υѥ��������������, ���� OR �ѥ����������ɽ���������ޤ�.
;;
;;   ۣ����(ambiguousness)�����Ǥ���褦�ˤ��ޤ���. ۣ���٤��礭����
;;   ��ȺƵ�Ū�˥ѥ��������������, ���ۣ��ʸ�������ǽ�Ȥʤ�ޤ�.
;;   �ǥե���Ȥ�ۣ���٤� 1 �Ǥ�. M-x approx-set-ambiguousness ��ۣ��
;;   �٤�����Ǥ��ޤ�.
;;
;;   ���󥯥��󥿥륵����(isearch)�ˤ��б����Ƥ��ޤ�. Meadow �Ǥ�ư
;;   ���褦�˽������ޤ���(*3).
;;
;;   Migemo ��Ȥä�ۣ�渡���ϤǤ��ޤ���. Migemo ���б����뤳�Ȥ�Ǥ�
;;   �ޤ���, ��Ԥ����Υ饤�֥���Ȥ����̤ϼ�˥ץ������Խ�����
;;   ��(�Ĥޤ� Migemo �� off �ˤ��Ƥ����)�ʤΤ�, ���ΤȤ��� Migemo ��
;;   ���ѤǤ���褦�ˤ���ɬ�פϤ��ޤ괶���Ƥ��ޤ���.
;;
;;
;;   (*1) Levenshtein distance (edit distance, �Խ���Υ) �ˤĤ��Ƥϰ�
;;        ���򻲾�.
;;        http://www.merriampark.com/ld.htm
;;
;;   (*2) http://migemo.namazu.org/
;;
;;   (*3) http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=approx-search
;;
;;
;; ������ˡ:
;;
;;   ���ʤ�ƥ��ȡ��ʤ�ΤǤ���, Makefile ����°���ޤ���. Makefile ��
;;   ��Ƭ��ʬ��Ŭ�����Խ�������,
;;
;;     % make
;;     % make install
;;
;;   �ǥ��󥹥ȡ���Ǥ��ޤ�.
;;
;;   ~/.emacs ��
;;
;;     (add-to-list 'load-path "~/elisp")
;;     (require 'approx-search)
;;     (if (boundp 'isearch-search-fun-function)
;;         (require 'approx-isearch)
;;       (require 'approx-old-isearch))
;;     (approx-isearch-set-enable)
;;
;;   �Ƚ񤭤ޤ�.
;;
;;   Mmigemo ��ʻ�Ѥ�����, Migemo ������θ�˰ʲ��Τ褦�˽񤯤�,
;;   M-x migemo-toggle-isearch-enable �� Migemo ��̵���ˤ������Τ�
;;   approx-isearch ��ͭ���ˤǤ��ޤ�(�դ� Migemo ��ͭ���ˤ�����
;;   approx-isearch ��̵���ˤʤ�ޤ�).
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
;;       "migemo ��Ȥ����� approx-search ��Ȥ�ʤ�."
;;       (if migemo-isearch-enable-p
;;           (approx-isearch-set-enable) ; NOT disable!!! before advice �ʤΤ�
;;         (approx-isearch-set-disable)))
;;


;; �Ȥ���:
;;
;;   [1] approx-search-{forward,backward}
;;
;;     M-x approx-search-forward
;;       ۣ�渡����Ȥä� STRING �� point ���������������Ƹ��Ĥ��ä����֤��֤�.
;;
;;     M-x approx-search-backward
;;       ۣ�渡����Ȥä� STRING �� point ��������������Ƹ��Ĥ��ä����֤��֤�.
;;
;;       ��:
;;         (approx-search-forward "approximately")
;;         (approx-search-backward "approximately")
;;           "aproximately", "appproximately", "apploximately" �ˤ�ޥå�����.
;;
;;     M-x approx-set-ambiguousness
;;       ۣ���٤����ꤹ��. �����ͤ��礭������ȵ��ƤǤ���ۣ���٤�����.
;;       �ǥե���Ȥ� 1.
;;
;;
;;   [2] isearch
;;
;;     M-x approx-isearch-enable-p
;;       ۣ�渡����Ȥä� isearch ��ͭ�����ݤ����֤�.
;;
;;     M-x approx-isearch-set-enable
;;       ۣ�渡����Ȥä� isearch ��ͭ���ˤ���.
;;
;;     M-x approx-isearch-set-disable
;;       ۣ�渡����Ȥä� isearch ��̵���ˤ���.
;;
;;     M-x approx-isearch-toggle-enable
;;       ۣ�渡����Ȥä� isearch ��ͭ��/̵�����ڤ괹����.
;;
;;     �ѿ� approx-isearch-auto-p
;;       �̾�� search �Ǹ��Ĥ���ʤ��ä����Τ�ۣ�渡����Ԥ�.
;;       �ǥե���Ȥ� nil.
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
ۣ����.")

(defvar approx-wild-card ?.
  "Wild card character.
����ɽ���Ρ�Ǥ�դ�1ʸ���˥ޥå���. ʸ����ʸ����Τɤä��Ǥ����Ǥ���.")

(defvar approx-back-slash ?\\
  "Back slash character.
�Хå�����å���ʸ��. �桼��������ʸ����򥨥������פ��뤿��˻���. �ѹ��Բ�.")

(defvar approx-re-control-char-list '(?| ?\\ ?. ?\" ?\[ ?\] ??)
  "Control char list in RE.
�桼��������ʸ�����Хå�����å���ǥ��������פ��ʤ���Фʤ�ʤ�ʸ���Υꥹ��. ��������ǽ�ʬ���褯�狼��ʤ�. �ץƥ���.
see `approx-add-back-slash'." )

(defvar approx-re-pipe-mark "\\|"
  "Pipe mark in RE.
����ɽ���Υѥ���ʸ��. ����ɽ���� OR, \\(...\\|...\\|...\\) ���������뤿��˻���.")

(defvar approx-re-left-paren "\\("
  "Left paren in RE.
����ɽ���κ����. ����ɽ���� OR, \\(...\\|...\\|...\\) ���������뤿��˻���.")

(defvar approx-re-right-paren "\\)"
  "Right paren in RE.
����ɽ���α����. ����ɽ���� OR, \\(...\\|...\\|...\\) ���������뤿��˻���.")


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
ۣ�渡����Ȥä� STRING �� point ���������������Ƹ��Ĥ��ä����֤��֤�.
see `search-forward', `re-search-forward'."
  (interactive "sSearch: \nP\nP")
  (re-search-forward (approx-generate-regexp string) bound noerror count))

(defun approx-search-backward (string &optional bound noerror count)
  "Approx search backward from point for STRING.
ۣ�渡����Ȥä� STRING �� point ��������������Ƹ��Ĥ��ä����֤��֤�.
see `search-backward', `re-search-backward'."
  (interactive "sSearch: \nP\nP")
  (re-search-backward (approx-generate-regexp string) bound noerror count))

(defun approx-auto-search-forward (string &optional bound noerror count)
  "Approx search forward from point for STRING.
ۣ�渡����Ȥä� STRING �� point ���������������Ƹ��Ĥ��ä����֤��֤�. �ޤ� `search-forward' �Ǹ������Ƹ��Ĥ���ʤ��ä����Τ�ۣ�渡�� `approx-search-forward' ��Ԥ�.
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
ۣ�渡����Ȥä� STRING �� point ��������������Ƹ��Ĥ��ä����֤��֤�. �ޤ� `search-backward' �Ǹ������Ƹ��Ĥ���ʤ��ä����Τ�ۣ�渡�� `approx-search-backward' ��Ԥ�.
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
ۣ���٤����ꤹ��. �����ͤ��礭������ȵ��ƤǤ���ۣ���٤�����."
  (interactive "nAmbiguousness: ")
  (let ((old-value approx-ambiguousness))
    (setq approx-ambiguousness ambiguousness)
    old-value))


(provide 'approx-search)

;;; approx-search.el ends here
