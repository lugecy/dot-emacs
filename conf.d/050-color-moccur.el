(require 'color-moccur)
(require 'moccur-edit)
(setq moccur-split-word t)
(when (and (executable-find "cmigemo")
           (not (eq system-type 'windows-nt)))
  (setq moccur-use-migemo t))

;;;; ここで指定したディレクトリ直下のファイルは，listingはされるがsearchはされない
(add-to-list 'dmoccur-exclusion-mask "\\.git/.+" t)
(add-to-list 'dmoccur-exclusion-mask "\\.hg/.+" t)
(add-to-list 'dmoccur-exclusion-mask "/!.+" t) ; auto-save-list, undohist, etc...

;;;; moccur-grep-find時に gotoとquitを同時に行う
(defadvice moccur-grep-goto (after goto-and-quit activate)
  "When with prefix, goto and quit."
  (when current-prefix-arg
    (moccur-remove-overlays-on-all-buffers)
    (delete-other-windows)))
