;;; private/edits/config.el -*- lexical-binding: t; -*-

;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Rectangle
(use-package rect
  :after org
  :bind (:map text-mode-map
         ("<C-return>" . rect-hydra/body)
         :map prog-mode-map
         ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'material "border_all" :height 1.2 :v-adjust -0.225)
    :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Pass a URL to a WWW browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
(use-package! avy
  ;; ;;my-personal-config
  :bind (("M-s s" . avy-goto-char)
         ("M-s W" . avy-goto-word-0)
         ("M-s a" . avy-goto-char-2)
         ("M-s l" . avy-goto-line)
         (:map evil-normal-state-map
          ("g s" . avy-goto-char-2)))
  ;;;;my-personal-config end
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; my-personal
;; Replace zap-to-char functionaity with the more powerful zop-to-char
(use-package! zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-s-ω" . zop-to-char)
         ("M-Z" . zop-to-char)
         ))

;; my-personal

;; Quickly follow links
(use-package! ace-link
  :defines (org-mode-map
            gnus-summary-mode-map
            gnus-article-mode-map
            ert-results-mode-map
            paradox-menu-mode-map
            elfeed-show-mode-map)
  :bind ("M-o" . ace-link-addr)
  :hook (after-init . ace-link-setup-default)
  :config
  (with-eval-after-load 'org
    (bind-key "M-o" #'ace-link-org org-mode-map))

  (with-eval-after-load 'gnus
    (bind-keys
     :map gnus-summary-mode-map
     ("M-o" . ace-link-gnus)
     :map gnus-article-mode-map
     ("M-o" . ace-link-gnus)))

  (with-eval-after-load 'ert
    (bind-key "o" #'ace-link-help ert-results-mode-map))

  (bind-keys
   :map package-menu-mode-map
   ("o" . ace-link-help)
   :map process-menu-mode-map
   ("o" . ace-link-help))
  (with-eval-after-load 'paradox
    (bind-key "o" #'ace-link-help paradox-menu-mode-map))

  (with-eval-after-load 'elfeed
    (bind-key "o" #'ace-link elfeed-show-mode-map)))

;; Jump to Chinese characters
;; (use-package ace-pinyin
;;   :diminish
;;   :hook (after-init . ace-pinyin-global-mode))

;; Minor mode to aggressively keep your code always indented
(use-package! aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode lisp-interaction-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package! anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Redefine M-< and M-> for some modes
(use-package! beginend
    :diminish beginend-global-mode
    :hook (after-init . beginend-global-mode)
    :config (mapc (lambda (pair)
                    (diminish (cdr pair)))
                  beginend-modes))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  ;; To get combined result of A and B in Ediff ;; my-personal
  ;; https://stackoverflow.com/a/29757750

  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
  ;; my-personal
  )

;; Automatic parenthesis pairing
(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; disable <> auto pairing in electric-pair-mode for org-mode

(add-hook
 'org-mode-hook
 (lambda ()
   (setq-local electric-pair-inhibit-predicate
               `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))


(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local electric-pair-inhibit-predicate
               `(lambda (c)
                  (if (char-equal c ?\') t (,electric-pair-inhibit-predicate c))))))

;; disable {} auto pairing in electric-pair-mode for web-mode
;; (add-hook
;;  'web-mode-hook
;;  (lambda ()
;;    (setq-local electric-pair-inhibit-predicate
;;                `(lambda (c)
;;                   (if (char-equal c ?{) t (,electric-pair-inhibit-predicate c))))))


;; (setq electric-pair-inhibit-predicate
;;       `(lambda (c)
;;          (if (char-equal c ?\") t (,electric-pair-inhibit-predicate c))))

;; Edit multiple regions in the same way simultaneously
(use-package! iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package! expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursors
(use-package! multiple-cursors
  :demand t ;;;; mc/num-cursors is not autoloaded
  :preface
  ;; preface is my-personal config
  ;; insert specific serial number
  (defvar my/mc/insert-numbers-hist nil)
  (defvar my/mc/insert-numbers-inc 1)
  (defvar my/mc/insert-numbers-pad "%01d")
  (defun my/mc/insert-numbers (start inc pad)
    "Insert increasing numbers for each cursor specifically."
    (interactive
     (list (read-number "Start from: " 0)
           (read-number "Increment by: " 1)
           (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
    (setq mc--insert-numbers-number start)
    (setq my/mc/insert-numbers-inc inc)
    (setq my/mc/insert-numbers-pad pad)
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor
      'my/mc--insert-number-and-increase
      cursor)))

  (defun my/mc--insert-number-and-increase ()
    (interactive)
    (insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
    (setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))

  (defhydra my/hydra-multiple-cursors (:hint nil :foreign-keys run)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("q" nil))
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C-c h m"       . my/hydra-multiple-cursors/body)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; Smartly select region, rectangle, multi cursors
;;(use-package smart-region
;;  :hook (after-init . smart-region-on))

;; On-the-fly spell checker
(use-package! flyspell
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  (use-package! flyspell-correct
    :after flyspell
    :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
  ;; Correcting words with flyspell via Ivy
  (use-package! flyspell-correct-ivy
    :after ivy
    :bind (:map flyspell-mode-map
           ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
    :init (setq flyspell-correct-interface #'flyspell-correct-ivy)))

;; ;; Hungry deletion
;; (use-package hungry-delete
;;   :diminish
;;   :hook (after-init . global-hungry-delete-mode)
;;   :init (setq hungry-delete-chars-to-skip " \t\f\v"
;;               hungry-delete-except-modes
;;               '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; ;; Framework for mode-specific buffer indexes
;; (use-package imenu
;;   :ensure nil
;;   :bind (("C-." . imenu)))

;; ;; my-personal
;; (use-package imenu-anywhere
;;   :bind (("M-I" . ivy-imenu-anywhere)
;;          ("C-c i i" . ivy-imenu-anywhere)))


;; (use-package imenu-list
;;   :config
;;   (setq-default imenu-list-position "left"
;;                 imenu-list-size 0.65))

;; ;; my-personal

;; ;; Move to the beginning/end of line or code
;; (use-package mwim
;;   :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
;;          ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; ;; Windows-scroll commands
;; (use-package pager
;;   :bind (([remap scroll-up-command] . pager-page-down)
;;          ([remap scroll-down-command] . pager-page-up)
;;          ([next]   . pager-page-down)
;;          ([prior]  . pager-page-up)
;;          ([M-up]   . pager-row-up)
;;          ([M-kp-8] . pager-row-up)
;;          ([M-down] . pager-row-down)
;;          ([M-kp-2] . pager-row-down)))

;; ;; Treat undo history as a tree
;; (use-package undo-tree
;;   :diminish
;;   :hook (after-init . global-undo-tree-mode)
;;   :config
;;   (add-to-list 'undo-tree-incompatible-major-modes #'magit-modes)
;;   :init
;;   (setq undo-tree-visualizer-timestamps t
;;         undo-tree-enable-undo-in-region nil
;;         undo-tree-auto-save-history nil)

;;   ;; HACK: keep the diff window
;;   (with-no-warnings
;;     (make-variable-buffer-local 'undo-tree-visualizer-diff)
;;     (setq-default undo-tree-visualizer-diff t)))

;; ;; Goto last change
;; (use-package goto-chg
;;   :bind ("C-," . goto-last-change))

;; ;; Preview when `goto-char'
;; (use-package goto-char-preview
;;   :bind ([remap goto-char] . goto-char-preview))

;; ;; Preview when `goto-line'
;; (use-package goto-line-preview
;;   :bind ([remap goto-line] . goto-line-preview))

;; ;; Handling capitalized subwords in a nomenclature
;; (use-package subword
;;   :ensure nil
;;   :diminish
;;   :hook ((prog-mode . subword-mode)
;;          (minibuffer-setup . subword-mode)))

;; ;; Hideshow
;; (use-package hideshow
;;   :ensure nil
;;   :diminish hs-minor-mode
;;   :bind (:map hs-minor-mode-map
;;          ("C-`" . hs-toggle-hiding)))

;; ;; Flexible text folding
;; ;; https://github.com/gregsexton/origami.el/wiki/Origami---Evil-Operator-Example - for evil configuration.
;; (use-package origami
;;   :pretty-hydra
;;   ((:title (pretty-hydra-title "Origami" 'octicon "fold" :height 1.1 :v-adjust -0.05)
;;     :foreign-keys run :color amaranth :quit-key "q")
;;    ("Node"
;;     ((";" origami-recursively-toggle-node "toggle recursively")
;;      ("a" origami-toggle-all-nodes "toggle all")
;;      ("t" origami-toggle-node "toggle current")
;;      ("o" origami-open-node "open current")
;;      ("c" origami-close-node "close current")
;;      ("s" origami-show-only-node "only show current"))
;;     "Actions"
;;     (("u" origami-undo "undo")
;;      ("d" origami-redo "redo")
;;      ("r" origami-reset "reset")
;;      ("n" origami-next-fold "next fold")
;;      ("p" origami-previous-fold "previous fold"))))
;;   :bind (:map origami-mode-map
;;          ("C-~" . origami-hydra/body))
;;   :hook (prog-mode . origami-mode)
;;   :init (setq origami-show-fold-header t)
;;   :config (face-spec-reset-face 'origami-fold-header-face))

;; ;; Narrow/Widen
;; (use-package fancy-narrow
;;   :diminish
;;   :hook (after-init . fancy-narrow-mode))

;; (provide 'init-edit)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; init-edit.el ends here
