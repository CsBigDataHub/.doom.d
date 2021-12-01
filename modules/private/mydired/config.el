;;; private/mydired/config.el -*- lexical-binding: t; -*-

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)
;;; Dired copy or move to split window suggestions
;;https://emacs.stackexchange.com/questions/5603/how-to-quickly-copy-move-file-in-emacs-dired
(setq dired-dwim-target t)

(when IS-MAC
  ;; Suppress the warning: `ls does not support --dired'.
  (setq dired-use-ls-dired nil)

  (when (executable-find "gls")
    ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program "gls")))

(when (or (and IS-MAC (executable-find "gls"))
          (and (not IS-MAC) (executable-find "ls")))
  ;; Using `insert-directory-program'
  (setq ls-lisp-use-insert-directory-program t)
  )

(after! wdired
  :config
  (setq wdired-create-parent-directories t)
  ;; Make permission bits editable
  (setq wdired-allow-to-change-permissions t)
  )

(use-package! dired-du
  :after dired
  :config
  ;; human readable size format
  (setq dired-du-size-format t))

(use-package! dired-hacks-utils
  :hook (dired-mode . dired-utils-format-information-line-mode))
