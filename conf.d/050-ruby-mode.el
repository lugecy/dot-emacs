(ly:eval-after-load "viper"
  (add-to-list 'viper-vi-state-mode-list 'ruby-mode)
  (add-to-list 'viper-emacs-state-mode-list 'inferior-ruby-mode))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(autoload 'ruby-electric-mode "ruby-electric" "Mode for ruby electric mode" t)
(add-hook 'ruby-mode-hook 'ruby-misc-setup)
(defun ruby-misc-setup ()
  (inf-ruby-keys)
  (ruby-electric-mode 1))
(setq ruby-electric-expand-delimiters-list nil);; 閉じ括弧の自動挿入をストップ
;; (add-hook 'ruby-mode-hook 'ruby-mode-private-setting)
;; (defun ruby-mode-private-setting ()
;;   (when (boundp 'ac-complete-mode-map)
;;     (define-key ac-complete-mode-map (kbd "SPC") 'ruby-electric-space))
;;   (define-key ruby-mode-map (kbd "C-m") 'ruby-reindent-then-newline-and-indent)
;;   (define-key ruby-mode-map (kbd "C-j") nil)
;;   )

;;;; ffapでrequireからファイルへ飛べるようにする．
;; FFAP and Ruby in Emacs | Chmouel's Blog
;; http://blog.chmouel.com/2008/05/04/ffap-and-ruby-in-emacs/
;; and
;; lib/ruby-which.rb at master from Pistos/ruby-which - GitHub
;; https://github.com/Pistos/ruby-which/blob/master/lib/ruby-which.rb
;; を参考にsinatraなどをrequireしてしまうと固まってしまうバグを回避
(defun ruby-module-path (module)
  (shell-command-to-string
   (concat
    "ruby -e "
    "'ret=%{()};"
    "begin require ARGV[0]; rescue LoadError; require %{rubygems}; Gem.try_activate ARGV[0];end;"
    "$LOAD_PATH.each{|p| "
    "x=p+%{/}+ARGV[0].gsub(%{.rb}, %{})+%{.rb};"
    "ret=File.expand_path(x) if(File.exist?(x))"
    "};"
    "print ret' "
    module)))
(ly:eval-after-load 'ffap
  (add-to-list 'ffap-alist '(ruby-mode . ruby-module-path)))
