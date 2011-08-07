(require 'auto-complete)
(require 'yasnippet)

(defun ac-yasnippet-candidate-1 (table)
  (let ((parent (if (fboundp 'yas/snippet-table-parent)
                    (yas/snippet-table-parent table)))
        (templates (yas/snippet-table-to-templates-alist table))
        candidates)
    (setq candidates (all-completions ac-prefix
                                      (mapcar (lambda (templ-cons)
                                                (popup-item-propertize (car templ-cons)
                                                                         'info (yas/template-name (cdr templ-cons))))
                                              templates)))
    (if parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent))))
    candidates))

(defun ac-yasnippet-candidate ()
  (if (fboundp 'yas/get-snippet-tables)
      ;; >0.6.0
      (apply 'append (mapcar 'ac-yasnippet-candidate-1 (yas/get-snippet-tables major-mode)))
    (let ((table
           (if (fboundp 'yas/snippet-table)
               ;; <0.6.0
               (yas/snippet-table major-mode)
             ;; 0.6.0
             (yas/current-snippet-table))))
      (if table
          (ac-yasnippet-candidate-1 table)))))

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")

(provide 'auto-complete-yasnippet)
