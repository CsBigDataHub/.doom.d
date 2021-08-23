;;; private/bm/config.el -*- lexical-binding: t; -*-


(use-package bm

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)


  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  ;;(setq bm-repository-file "~/.emacs.d/var/bm-repository");; handled by no-littering package

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


  ;; :bind (("H-b n" . bm-next)
  ;;        ("H-b p" . bm-previous)
  ;;        ("H-b b" . bm-toggle))

  :pretty-hydra
  ((:title (pretty-hydra-title "bookmarks" 'faicon "bookmark" :height 1.1 :v-adjust -0.1)
    :foreign-keys warn :quit-key "q")
   ("toggle"
    (("b"   bm-toggle "bm toggle")
     ("a"   bm-bookmark-annotate "bm annotate")
     ("A"   bm-bookmark-show-annotation "bm show annotation"))
    "navigation"
    (("n"   bm-common-next "bm next")
     ("N"   bm-lifo-next "bm next in lifo order")
     ("l"   bm-show "bm list bookmark")
     ("L"   bm-show-all "bm list all booksmarks")
     ("p"   bm-common-previous "bm previous")
     ("P"   bm-lifo-previous "bm previous in lifo order"))
    "remove"
    (("x"   bm-remove-all-current-buffer "bm remove all bookmarks in current buffer")
     ("X"   bm-remove-all-all-buffers "bm remove all bookmarks"))
    "burly"
    (("f" burly-bookmark-frames "bookmark frame config")
     ("w" burly-bookmark-windows "bookmark window config")
     ("o" burly-open-bookmark "open burly bookmarks")
     ("s" bookmark-set "set bookmark")
     ("m" list-bookmarks )
     ("S" bookmark-save)
     ("j" bookmark-jump)
     ("d" bookmark-delete))))
  :bind (("C-c h b" . bm-hydra/body))
  )
