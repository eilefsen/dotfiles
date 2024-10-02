;; vars
(defvar user-full-name "Emma Eilefsen Glenna"
  "Default name if not loaded from file")
(defvar user-mail-address "emma@eilefsen.net"
  "Default email if not loaded from file")

;; font

(if (eq system-type 'darwin) ; set higher font size on macos, due to high dpi
    (set-frame-font "Cascadia Code 16" nil t)
  (set-frame-font "Cascadia Code 12" nil t))

;; theme

(use-package ef-themes
  :straight t
  :init
   (load-theme 'ef-light t t)
   (load-theme 'ef-dark t t))

(use-package auto-dark
  :defer t
   :straight t
   :init
   (auto-dark-mode)
   :custom
   (auto-dark-themes '((ef-dark) (ef-light)))
   (auto-dark-polling-interval-seconds 600)
   :config
   (setq auto-dark-allow-osascript t))

;; indentation / tabs

(setq-default tab-width 4)


;;; gui stuff

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)

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

;;; nice to haves

;; Enable electric-pair mode for automatic pairing of brackets and quotes
(electric-pair-mode 1)

;; Enable clipboard integration with system
(setq select-enable-clipboard t)

(provide 'config)
