;; org-mode + remember-mode でEmacs内で瞬時にメモをする→コードリーディングに生かす・メモ検索する - (rubikitch loves (Emacs Ruby CUI))
;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
;;;; org-mode と remember-mode の連携の為の設定
(require 'org-install)
(setq org-startup-truncated nil
      org-return-follows-link t
      org-tags-column -90
      org-agenda-tags-column -90)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (org-remember-insinuate)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;;; (@file :file-name "../init-gogotea.el" :to "org-remember" :display "host-local-setting place")
;; (setq org-directory (concat clmemo-directory "memo/"))
;; (setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Task-suck")
        ("Bug"  ?b "** %?   :bug:\n   %i\n   %a\n   %t" nil "Task-suck")
        ("Idea" ?i "** %?   :idea:\n   %i\n   %a\n   %t" "idea_suck.org" "Idea-suck")
        ("Memo" ?m "** %?   :memo:\n   %i\n   %a\n   %t" nil "Task-suck")
        ("Entry" ?e "** %?\n   %i\n   %a\n   %t" nil "Task-suck")
        )
      org-tag-alist
      '(("WantBook" . ?w)))
(setq org-agenda-custom-commands
      '(("w" "Agenda and TODO tasks"
         ((agenda ""
                  ((org-agenda-ndays 1)))
          (todo nil
           ((org-agenda-todo-ignore-scheduled t)))))))

;;;; コードリーディング用の設定
(defvar org-code-reading-software-name nil)
;; ~/memo/code-reading.org に記録する
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: "
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))

(defun org-code-reading-get-prefix (lang)
  (concat "[" lang "]"
          "[" (org-code-reading-read-software-name) "]"))
(defun org-remember-code-reading ()
  (interactive)
  (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (org-remember-templates
          `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
             ,org-code-reading-file "Memo"))))
    (org-remember)))

(eval-after-load "auto-complete"
  #'(progn
      (add-to-list 'ac-modes 'org-mode)))
(ly:eval-after-load 'org
  (eval-after-load "sequential-command"
    #'(progn
        (define-sequential-command seq-org-home
          org-beginning-of-line back-to-indentation beginning-of-buffer seq-return)
        (define-key org-mode-map (kbd "C-a") 'seq-org-home))))

;;;; archive する前にclmemoに記録するためのコマンド
(eval-after-load "conf-clmemo"
  #'(defun org-archive-to-clmemo ()
      (interactive)
      ;; (org-cut-special)
      (org-copy-special)
      (clmemo-remember nil
                       (replace-regexp-in-string "\\(^ +<[-0-9]+ .+>\\|^ +\\|^\\*+ \\)" "" (current-kill 0)))))

;;;; TODO タグがOPENEDになったと同時にClockをスタートさせる
(defun org-opened-clock-in ()
  (when (string= state "OPENED")
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook 'org-opened-clock-in)

;;;; outline-mode 強化
(add-hook 'outline-mode-hook (lambda ()
                               (require 'outline-magic)
                               (define-key outline-mode-map [(f9)] 'outline-cycle)))
(add-hook 'outline-minor-mode-hook (lambda ()
                                     (require 'outline-magic)
                                     (define-key outline-minor-mode-map [(f9)] 'outline-cycle)))

;;;; title 部分でyankした時にtag部分を考慮してyankするようにする
(defadvice org-yank (around tag-move-yank activate)
  (let (tag-str title-pos)
    (when (and (looking-back "\\* +")   ;maybe title position
               (not (eolp)))
      (setq title-pos (point))
      (setq tag-str (buffer-substring-no-properties (point) (line-end-position)))
      (delete-region (point) (line-end-position)))
    ad-do-it
    (when (and title-pos tag-str)
      (save-excursion
        (goto-char title-pos)
        (end-of-line)
        (insert tag-str)))))
