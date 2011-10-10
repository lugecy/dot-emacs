(require 'thing-cmds)

;;;; for thing-cmds.el feature popup
(ly:eval-after-load "popup"
  (defvar thing-types-map
    '((?w . word)
      (?s . symbol)
      (?e . sexp)
      (?l . list)
      (?L . line)
      (?t . sentence)
      (?p . paragraph)
      (?P . page)
      (?f . defun)
      (?n . number)
      (?F . form)))

  (defun mark-thing-popup ()
    "mark-thing feature popup."
    (interactive)
    (mark-thing (popup-menu* (loop for (c . type) in thing-types-map
                                   collect (popup-make-item (format "(%c):%s" c type)
                                                            :value type))
                             :fallback 'mark-thing-popup-fallback)))

  (defun mark-thing-popup-fallback (event default)
    "mark-thing-popup-fallback "
    (let* ((c (cond ((characterp event) event)
                    ((stringp event) (string-to-char event))))
           (thing (assoc-default c thing-types-map)))
      (if thing (return thing))))
  )

;;;; thing-opt.el
(require 'thing-opt)
;;;; table
;; (?w . word)
;; (?e . sexp)
;; (?s . symbol)
;; (?t . sentence)
;; (?p . paragraph)
;; (?f . defun)
;; (?F . filename)
;; (?l . list)
;; (?L . up-list)
;; (?S . string)
;; (?U . url)
;; (?P . page)

(defun wrap-unix-werase-or-kill (arg)
  (interactive "P")
  (if (equal arg '(4))
      (kill-region-dwim)
    (call-interactively 'unix-werase-or-kill))) ; conf-misc.el
(eval-after-load "viper"
  '(define-key viper-insert-global-user-map (kbd "C-w") 'wrap-unix-werase-or-kill))
