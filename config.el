;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(setq fancy-splash-image "~/.doom.d/banner/emacs-head-color.png")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'Regular))
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

(map! :leader
      :desc "fzf" "f z" #'counsel-fzf)

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; -------------custom-functions-start------------------------------------------
(defcustom centaur-icon (or (display-graphic-p) (daemonp))
  "Display icons or not."
  :group 'centaur
  :type 'boolean)

(setq centaur-icon t)

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and centaur-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

;; -------------custom-functions-end------------------------------------------
;; -------------basic-init---------------------------------------------------------
(with-no-warnings
  ;; Key Modifiers
  (cond
   (IS-MAC
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo))))

  ;; Optimization
  (unless IS-MAC
    (setq command-line-ns-option-alist nil))
  (unless IS-LINUX
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)
  )

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(after! recentf
  :config
  (setq recentf-max-saved-items 300
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  )

(after! savehist
  :config
 (setq enable-recursive-minibuffers t    ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300)
  )

(setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package! time
  ;; :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(when EMACS28+
  (use-package! so-long
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 120
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Fullscreen
;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen."
  (and IS-MAC
       EMACS28+
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))

(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))

;; -------------basic-init---END---------------------------------------------------
;; ------------------calendar------------------------------------------------------
(use-package! calendar
  :custom
  ((calendar-holidays holiday-general-holidays)
   (calendar-mark-holidays-flag t)
   (holiday-general-holidays ;; US public holidays
    '((holiday-fixed 1 1 "New Year's Day")
      (holiday-float 1 1 3 "Martin Luther King Day")
      (holiday-float 2 1 3 "President's Day")
      (holiday-float 5 1 -1 "Memorial Day")
      (holiday-fixed 7 4 "Independence Day")
      (holiday-float 9 1 1 "Labor Day")
      (holiday-float 10 1 2 "Columbus Day")
      (holiday-fixed 11 11 "Veteran's Day")
      (holiday-fixed 12 25 "Christmas")
      (holiday-float 11 4 4 "Thanksgiving")))
   (calendar-time-zone -360)
   (calendar-latitude 45.018270)
   (calendar-longitude -93.473890)
   (calendar-standard-time-zone-name "CST")
   (calendar-daylight-time-zone-name "CDT")

   )
  :config
  (defadvice calendar-generate-month
      (after highlight-weekend-days (month year indent) activate)
    "Highlight weekend days"
    (dotimes (i 31)
      (let ((date (list month (1+ i) year)))
        (if (or (= (calendar-day-of-week date) 0)
                (= (calendar-day-of-week date) 6))
            (calendar-mark-visible-date date 'font-lock-doc-string-face)))))

  ;; https://stackoverflow.com/questions/23566000/how-to-count-days-excluding-weekends-and-holidays-in-emacs-calendar
  (eval-after-load "calendar"
    `(progn
       (require 'holidays)
       (defun my-calendar-count-days(d1 d2)
         (let* ((days (- (calendar-absolute-from-gregorian d1)
                         (calendar-absolute-from-gregorian d2)))
                (days (1+ (if (> days 0) days (- days)))))
           days))

       (defun my-calendar-count-holidays-on-weekdays-in-range (start end)
         (let ((holidays (holiday-in-range start end))
               (counter 0))
           (dolist (element holidays)
             (let ((day (calendar-day-of-week (car element))))
               (if (and (> day 0)
                        (< day 6))
                   (incf counter))))
           counter))

       (defun my-calendar-count-weekend-days(date1 date2)
         (let* ((tmp-date (if (< date1 date2) date1 date2))
                (end-date (if (> date1 date2) date1 date2))
                (weekend-days 0))
           (while (<= tmp-date end-date)
             (let ((day-of-week (calendar-day-of-week
                                 (calendar-gregorian-from-absolute tmp-date))))
               (if (or (= day-of-week 0)
                       (= day-of-week 6))
                   (incf weekend-days ))
               (incf tmp-date)))
           weekend-days))

       (defun calendar-count-days-region2 ()
         "Count the number of days (inclusive) between point and the mark
  excluding weekends and holidays."
         (interactive)
         (let* ((d1 (calendar-cursor-to-date t))
                (d2 (car calendar-mark-ring))
                (date1 (calendar-absolute-from-gregorian d1))
                (date2 (calendar-absolute-from-gregorian d2))
                (start-date (if (<  date1 date2) date1 date2))
                (end-date (if (> date1 date2) date1 date2))
                (days (- (my-calendar-count-days d1 d2)
                         (+ (my-calendar-count-weekend-days start-date end-date)
                            (my-calendar-count-holidays-on-weekdays-in-range
                             start-date end-date)))))
           (message "Region has %d workday%s (inclusive)"
                    days (if (> days 1) "s" ""))))
       (define-key calendar-mode-map (kbd "M-s-=") 'calendar-count-days-region2)
       )))

;; ------------------calendar-END------------------------------------------------------
