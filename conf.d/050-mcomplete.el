;;;; iswitchbのM-x版
(require 'mcomplete)
(load "mcomplete-history")
(turn-on-mcomplete-mode)

;;;; 履歴の先頭一致から補完する
(load-library "tails-history")

;;;; history から重複したのを消す
(defun minibuffer-delete-duplicate ()
  (let (list)
    (dolist (elt (symbol-value minibuffer-history-variable))
      (unless (member elt list)
        (push elt list)))
    (set minibuffer-history-variable (nreverse list))))
(add-hook 'minibuffer-setup-hook 'minibuffer-delete-duplicate)

