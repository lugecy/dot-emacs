;;;; windowを一つにする
(global-set-key (kbd "<f8>") 'delete-other-windows)
(global-set-key (kbd "<C-f8>") 'kill-buffer-and-window)

;; (require 'windmove)
(define-prefix-command 'C-q-keymap)
(define-key C-q-keymap (kbd "C-q") 'quoted-insert)
(define-key C-q-keymap (kbd "C-j") 'windmove-down)
(define-key C-q-keymap (kbd "C-k") 'windmove-up)
(define-key C-q-keymap (kbd "C-h") 'windmove-left)
(define-key C-q-keymap (kbd "DEL") 'windmove-left)
(define-key C-q-keymap (kbd "C-l") 'windmove-right)
(define-key C-q-keymap (kbd "TAB") 'other-window-or-split)
(define-key C-q-keymap (kbd "C-w") 'window-resizer)
(define-key C-q-keymap (kbd "C-u") 'window-toggle-division)
(define-key C-q-keymap (kbd "C-o") 'delete-other-windows)
(define-key C-q-keymap (kbd "C-M-o") 'kill-buffer-and-window)
(define-key C-q-keymap (kbd "0") 'delete-window)
(define-key C-q-keymap (kbd "C-0") 'delete-window)
(define-key C-q-keymap (kbd "C-v") 'popwin:display-last-buffer)
(define-key C-q-keymap (kbd "C-M-v") 'popwin:close-popup-window)
(define-key C-q-keymap (kbd "C-@") 'popwin:stick-popup-window)
(global-set-key (kbd "C-q") 'C-q-keymap)
;; windowが無い方向への移動しようとした場合に良きに図らう
(setq windmove-wrap-around t)

;;;; そろそろEmacsのウィンドウについて一言いっとくか - (rubikitch loves (Emacs Ruby CUI))
;;;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (> (window-width) 160)
        (split-window-horizontally)
      (split-window-vertically)))
  (other-window 1))
(unless (locate-library "elscreen")
  (global-set-key (kbd "C-t") 'other-window-or-split))

;;;; ウィンドウ 2 分割時に、縦分割<->横分割
; ネタ元
; http://www.bookshelf.jp/soft/meadow_30.html#SEC401
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let ((other-buf (window-buffer (next-window)))
        before-height split-width-threshold)
    (setq before-height (window-height))
    (delete-other-windows)

    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))

    (switch-to-buffer-other-window other-buf)
    (other-window -1)))

;;;; 分割したウィンドウの大きさをインタラクティヴに変更する - リタマス
;;;; http://d.hatena.ne.jp/mooz/20100119/p1
;;;; +
;;;; Re: 分割したウィンドウの大きさをインタラクティヴに変更する - とりあえず暇だったし何となく始めたブログ
;;;; http://d.hatena.ne.jp/khiker/20100119/window_resize
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action (read-key-sequence-vector (format "size[%dx%d]"
                                                       (window-width) (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

;;;; escreen configure
(require 'escreen)
(setq escreen-prefix-char (kbd "C-t")
      escreen-install-number-mode-format nil
      escreen-mode-line-format '(escreen-number-mode ("[S" escreen-current-screen-string "] ")))
(define-key escreen-map (kbd "C-n") 'escreen-goto-next-screen)
(define-key escreen-map (kbd "C-p") 'escreen-goto-prev-screen)
(define-key escreen-map (kbd "C-c") 'escreen-create-screen)
(define-key escreen-map (kbd "k") 'escreen-kill-screen)
(define-key escreen-map (kbd "C-d") 'escreen-clone)
(escreen-install)
(setq-default mode-line-format (cons escreen-mode-line-format (default-value 'mode-line-format)))
;; integrate C-q-keymap
(set-keymap-parent C-q-keymap escreen-map)

;; extend escreen
(defun escreen-clone ()
  (interactive)
  (let ((new-screen-number (escreen-first-unused-screen-number)))
    (or new-screen-number
        (error "escreen: No more screens (see \"escreen-max-screens\")"))
    ;; Save window configuration before switching to a new one.
    (escreen-save-current-screen-configuration)
    (and (> new-screen-number escreen-highest-screen-number-used)
         (setq escreen-highest-screen-number-used new-screen-number))
    (setq escreen-last-screen-number escreen-current-screen-number)
    (setq escreen-current-screen-number new-screen-number)
    (setq escreen-current-screen-string (int-to-string new-screen-number))
    ;; Save new window configuration so that it's in the alist.
    (escreen-save-current-screen-configuration))
  (run-hooks 'escreen-goto-screen-hook))

;; for menu select
(defvar anything-c-source-escreen
  '((name . "escreen")
    (init . (lambda () (escreen-save-current-screen-configuration)))
    (candidates . (lambda ()
                    (loop for screen-data in (sort (mapcar 'identity escreen-configuration-alist)
                                                   (lambda (a b)
                                                     (< (escreen-configuration-screen-number a)
                                                        (escreen-configuration-screen-number b))))
                          for screen-number = (escreen-configuration-screen-number screen-data)
                          for data-map = (escreen-configuration-data-map screen-data)
                          collect (concat (format "%d: " screen-number)
                                          (mapconcat (lambda (d)
                                                       (escreen-configuration-data-map-critical-buffer-name
                                                        (escreen-configuration-data-map-critical d)))
                                                     data-map " | ")))))
    (action . (("Goto Screen" . (lambda (candidate)
                                    (escreen-goto-screen (- (aref candidate 0) (aref "0" 0)))))
               ("Create Screen" . (lambda (candidate)
                                    (escreen-create-screen)))))))

(defun anything-escreen ()
  (interactive)
  (anything :sources 'anything-c-source-escreen
            :buffer "*anything escreen*"
            :preselect (format "^%d:" escreen-current-screen-number)))
(define-key escreen-map (kbd "C-t") 'anything-escreen)
(define-key escreen-map (kbd "C-:") 'anything-escreen)
(defvar anything-c-source-escreen-for-open
  '((name . "Escreen for open")
    (init . (lambda () (escreen-save-current-screen-configuration)))
    (candidates . (lambda ()
                    (loop for screen-data in (sort (mapcar 'identity escreen-configuration-alist)
                                                   (lambda (a b)
                                                     (< (escreen-configuration-screen-number a)
                                                        (escreen-configuration-screen-number b))))
                          for screen-number = (escreen-configuration-screen-number screen-data)
                          for data-map = (escreen-configuration-data-map screen-data)
                          append (delq nil
                                       (mapcar (lambda (d)
                                                 (let ((buf (escreen-configuration-data-map-critical-buffer-name
                                                             (escreen-configuration-data-map-critical d))))
                                                   (unless (and (equal screen-number (escreen-get-current-screen-number))
                                                                (equal buf (buffer-name anything-current-buffer)))
                                                     (cons (format "%d: %s" screen-number buf)
                                                           (cons screen-number buf)))))
                                               data-map)) into buf-list
                          finally return (delete-dups buf-list))))
    (action . (("Goto Screen" . (lambda (candidate)
                                  (let ((scr-num (car candidate))
                                        (buf-name (cdr candidate)))
                                    (escreen-goto-screen scr-num)
                                    (select-window (get-buffer-window buf-name)))))))))

;; for escreen tab emulation
;; tail -f /dev/dim
;; http://blog.tapoueh.org/news.dim.html#%20Escreen%20integration
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((message-log-max nil))
    (message "escreen: active screens: %s"
             (mapconcat (lambda (s)
                          (apply 'propertize (number-to-string s)
                                 (if (= escreen-current-screen-number s)
                                     '(face font-lock-warning-face))))
                        (escreen-get-active-screen-numbers) " "))))
(define-key escreen-map (kbd "C-a") 'escreen-get-active-screen-numbers-with-emphasis)
(dolist (func '(escreen-goto-next-screen
                escreen-goto-prev-screen
                escreen-create-screen
                escreen-kill-screen
                escreen-clone))
  (eval `(defadvice ,func (after escreen-tab-emu activate)
           (escreen-get-active-screen-numbers-with-emphasis))))

;;;; popwin configure
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config (list "*expectations result*" :noselect t))
(add-to-list 'popwin:special-display-config (list "*Backtrace*"))
