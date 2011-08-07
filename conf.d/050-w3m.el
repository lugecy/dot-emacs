(when (require 'w3m-load nil t)
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (global-set-key (kbd "C-c w") 'w3m-browse-url)
  (setq w3m-imagick-convert-program "e:/cygwin/bin/convert.exe"))
