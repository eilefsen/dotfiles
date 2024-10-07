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

(defface emma/lsp-face-semh-modifier-declaration '((t :slant italic)) "Face for LSP semantic token modifier Declaration")
(defface emma/lsp-face-semh-modifier-readonly '((t)) "Face for LSP semantic token modifier Declaration")

(use-package ef-themes
  :ensure t
  :config
  (setq ef-symbiosis-palette-overrides
		'((yellow "#FFCA6A")
		  (yellow-warmer "#FFA050")
		  (yellow-cooler "#FFE8BF")
		  (green-cooler "#94E596")
		  (red "#FF6461")
		  (bg-main "#23272d")
		  ))
  (defun emma/ef-themes-custom-faces ()
	"Emma' customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
	(ef-themes-with-colors
	  (custom-set-faces
	   `(font-lock-constant-face ((,c :foreground ,magenta-cooler)))
	   `(lsp-face-semhl-macro ((,c :foreground ,yellow-warmer)))
	   `(lsp-face-semhl-constant ((,c :foreground ,yellow-warmer)))
	   `(lsp-face-semhl-interface ((,c :foreground nil)))
	   `(font-lock-type-face ((,c :foreground ,yellow :inherit 'bold)))
	   `(font-lock-variable-name-face ((,c :foreground ,yellow-cooler)))
	   `(font-lock-property-name-face ((,c :foreground ,red)))
	   `(lsp-face-semhl-property ((,c :foreground ,red)))
	   `(lsp-face-semhl-member ((,c :foreground ,red-faint)))
	   `(font-lock-preprocessor-face ((,c :foreground ,magenta-cooler)))
	   `(font-lock-builtin-face ((,c :foreground ,magenta-cooler)))
	   `(font-lock-keyword-face ((,c :foreground ,magenta-warmer)))
	   `(font-lock-string-face ((,c :foreground ,green-cooler)))
	   `(font-lock-function-name-face ((,c :foreground ,blue)))
	   ;; `(emma/lsp-face-semh-modifier-readonly ((,c :foreground ,yellow-warmer)))
	   )))
  (add-hook 'ef-themes-post-load-hook #'emma/ef-themes-custom-faces))



(defun emma/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (ef-themes-select 'ef-summer))
    ('dark (ef-themes-select 'ef-symbiosis))))

(if (eq system-type 'darwin) ; set higher font size on macos, due to high dpi
	(add-hook 'ns-system-appearance-change-functions #'emma/apply-theme)
  (ef-themes-select 'ef-symbiosis)
  )

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
(setq scroll-conservatively most-positive-fixnum)

;;; nice to haves

;; Enable electric-pair mode for automatic pairing of brackets and quotes
(electric-pair-mode 1)

;; Enable clipboard integration with system
(setq select-enable-clipboard t)

(provide 'config)
