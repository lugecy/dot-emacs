(require 'bm)
(ly:eval-after-load 'vimpulse
  (dolist (cmd '(bm-previous bm-next))
    (add-to-list 'vimpulse-movement-cmds cmd)))
