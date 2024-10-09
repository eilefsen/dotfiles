;; vars
(defvar user-full-name "Emma Eilefsen Glenna"
  "Default name if not loaded from file")
(defvar user-mail-address "emma@eilefsen.net"
  "Default email if not loaded from file")

;; font

(if (eq system-type 'darwin) ; set higher font size on macos, due to high dpi
    (set-frame-font "Cascadia Code 16" nil t)
  (set-frame-font "Cascadia Code 12" nil t))

;; indentation / tabs

(setq-default tab-width 4)


;; Show matching parentheses
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)

(setq apropos-do-all t)


;; macos specific stuff
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq mac-right-option-modifier nil
      mac-option-key-is-meta nil)


;;; Pet Peeves

(fset 'yes-or-no-p 'y-or-n-p) ;; replace yes or no prompts with y or n
(setq scroll-conservatively most-positive-fixnum)

;;; nice to haves

;; Enable electric-pair mode for automatic pairing of brackets and quotes
(electric-pair-mode 1)

;; Enable clipboard integration with system
(setq select-enable-clipboard t)


(provide 'config)
