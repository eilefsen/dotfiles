;; font

(if (eq system-type 'darwin) ; set higher font size on macos, due to high dpi
    (set-frame-font "Cascadia Code 16" nil t)
  (set-frame-font "Cascadia Code 12" nil t))

;; theme

(use-package auto-dark
   :straight t
   :init
   (load-theme 'wombat t t)
   (load-theme 'leuven t t)
   (auto-dark-mode)
   :custom
   (auto-dark-themes '((wombat) (leuven)))
   (auto-dark-polling-interval-seconds 600)
   :config
   (setq auto-dark-allow-osascript t))


;; gui stuff
(add-hook 'window-setup-hook #'toggle-frame-maximized)
(tool-bar-mode -1)


;; macos specific stuff
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq mac-right-option-modifier nil
      mac-option-key-is-meta nil)


;;; Pet Peeves

(fset 'yes-or-no-p 'y-or-n-p) ;; replace yes or no prompts with y or n


(provide 'config)
