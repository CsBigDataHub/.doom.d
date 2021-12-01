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


(use-package bookmark
  :config
  (with-no-warnings
    ;; Display icons for bookmarks
    (defun my-bookmark-bmenu--revert ()
      "Re-populate `tabulated-list-entries'."
      (let (entries)
        (dolist (full-record (bookmark-maybe-sort-alist))
          (let* ((name       (bookmark-name-from-full-record full-record))
                 (annotation (bookmark-get-annotation full-record))
                 (location   (bookmark-location full-record))
                 (file       (file-name-nondirectory location))
                 (type       (let ((fmt "%-8.8s"))
                               (cond ((null location)
                                      (propertize (format fmt "NOFILE") 'face 'warning))
                                     ((file-remote-p location)
                                      (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                     ((not (file-exists-p location))
                                      (propertize (format fmt "NOTFOUND") 'face 'error))
                                     ((file-directory-p location)
                                      (propertize (format fmt "DIRED") 'face 'warning))
                                     (t (propertize (format fmt "FILE") 'face 'success)))))
                 (icon       (if (icons-displayable-p)
                                 (cond
                                  ((file-remote-p location)
                                   (all-the-icons-octicon "radio-tower" :height 0.8 :v-adjust 0.0))
                                  ((file-directory-p location)
                                   (all-the-icons-icon-for-dir location :height 0.9 :v-adjust 0.01))
                                  ((not (string-empty-p file))
                                   (all-the-icons-icon-for-file file :height 0.9 :v-adjust 0.0)))
                               "")))
            (push (list
                   full-record
                   `[,(if (and annotation (not (string-equal annotation "")))
                          "*" "")
                     ,icon
                     ,(if (display-mouse-p)
                          (propertize name
                                      'font-lock-face 'bookmark-menu-bookmark
                                      'mouse-face 'highlight
                                      'follow-link t
                                      'help-echo "mouse-2: go to this bookmark in other window")
                        name)
                     ,type
                     ,@(if bookmark-bmenu-toggle-filenames
                           (list (propertize location 'face 'completions-annotations)))])
                  entries)))
        (tabulated-list-init-header)
        (setq tabulated-list-entries entries))
      (tabulated-list-print t))
    (advice-add #'bookmark-bmenu--revert :override #'my-bookmark-bmenu--revert)

    (defun my-bookmark-bmenu-list ()
      "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
      (interactive)
      (bookmark-maybe-load-default-file)
      (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
        (if (called-interactively-p 'interactive)
            (pop-to-buffer buf)
          (set-buffer buf)))
      (bookmark-bmenu-mode)
      (bookmark-bmenu--revert))
    (advice-add #'bookmark-bmenu-list :override #'my-bookmark-bmenu-list)

    (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (setq tabulated-list-format
            `[("" 1)                               ;; Space to add "*" for bookmark with annotation
              ("" ,(if (icons-displayable-p) 2 0)) ;; Icons
              ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
              ("Type" 9)
              ,@(if bookmark-bmenu-toggle-filenames
                    '(("File" 0 bookmark-bmenu--file-predicate)))])
      (setq tabulated-list-padding bookmark-bmenu-marks-width)
      (setq tabulated-list-sort-key '("Bookmark" . nil))
      (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
      (setq revert-buffer-function #'bookmark-bmenu--revert)
      (tabulated-list-init-header))))
