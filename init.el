;; -*- coding: utf-8-unix -*-
;; (setq debug-on-error t)
(when (eq (window-system) 'w32)
  (display-warning 'Trick "With dwm-win32 fix window trick")
  (delete-other-windows))
;;;; configure load-path
(defun add-load-path-subdirs (path)
  (let ((dir (expand-file-name path)))
    (unless (member dir load-path)
      (add-to-list 'load-path dir)
      (let ((default-directory dir))
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))
        (byte-recompile-directory dir)))))
(add-load-path-subdirs "~/.emacs.d/elisp")
(add-load-path-subdirs "~/.emacs.d/local-elisp")

(defun reload-path-subdirs (path)
  (setq load-path (delete path load-path)
        normal-top-level-add-subdirs-inode-list (delete path normal-top-level-add-subdirs-inode-list))
  (add-load-path-subdirs path))

;;;; safe-load functions
(defun require-dwim (pkg)
  (let ((successp (if (symbolp pkg)
                      (require pkg nil t)
                    (load pkg t))))
    (unless successp
      (warn "loading failed, Maybe cannnot open [%s]" pkg))
    successp))

(defun safe-loading (pkg)
  (if debug-on-error
      (require-dwim pkg)
    (condition-case err
        (require-dwim pkg)
      (error (warn "loading error, [%s]:\n%s"
                   (locate-library (format "%s" pkg)) (error-message-string err))
             nil))))

(defmacro with-loading (pkg &rest body)
  (declare (indent 1))
  (if debug-on-error
      `(when (require-dwim ,pkg)
         ,@body)
    `(when (safe-loading ,pkg)
       (condition-case err
           (progn ,@body)
         (error (warn "with configure error [%s]:\n%s" ,pkg (error-message-string err)) nil)))))

;;;; eval-after-load macro
(defmacro ly:eval-after-load (file &rest body)
  (declare (indent 1))
  `(eval-after-load ,file
     '(progn ,@body)))

;;;; Load directory numbered configure files
(defvar ly:confd-directory "~/.emacs.d/conf.d/")
(defun ly:conf-directory-load (directory)
  "Load directory numbered configure files."
  (require 'warnings)
  (let ((default-directory directory))
    (dolist (f (file-expand-wildcards "*.el"))
      (if (and (not (file-directory-p f)) (string-match "^[0-9]" f))
          (safe-loading (concat directory f))))))

(defun ly:custom-conf-load (filename)
  (let (successp)
    (dolist (f (directory-files ly:confd-directory))
      (when (and (not (file-directory-p f))
                 (string-match (concat "^[A-Z]?[0-9]\\{3\\}-" filename "\.el") f))
        (setq successp (safe-loading (concat ly:confd-directory f)))))
    successp))

(require 'cl)
;;;; load elisp configure
(ly:conf-directory-load ly:confd-directory)

;;;; elisp以下のファイルからあんまり使用してないやつを
;;;; M-x update-file-autoloads したもの
;; (require 'my-loaddefs)

;;;; global enable mode setting
;; (when (featurep 'linum) (global-linum-mode t))
(when (featurep 'auto-complete) (global-auto-complete-mode))
(when (featurep 'undo-tree) (global-undo-tree-mode t))

;;;; For emacsclient / gnuclient
(server-start)

;;;; customize-save-variable
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((clmemo-mode . t))))
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
