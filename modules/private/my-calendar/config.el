;;; private/my-calendar/config.el -*- lexical-binding: t; -*-

(use-package calendar
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
(evil-set-initial-state 'calendar-mode 'emacs)
