(ly:eval-after-load 'viper
  (define-key viper-vi-global-user-map "\C-e" 'seq-end))
(ly:eval-after-load 'vimpulse
  (add-to-list 'vimpulse-movement-cmds 'seq-end)
  (add-to-list 'vimpulse-movement-cmds 'seq-home))
