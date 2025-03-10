;; install MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

;; theme

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-with-colors
	(setq ef-symbiosis-palette-overrides
		  `((yellow "#FFCA6A")
			(yellow-warmer "#FFA050")
			(yellow-cooler "#FFE8BF")
			(green-cooler "#94E596")
			(red "#FF6461")
			(bg-main "#23272d")
			(bg-inactive "#272C33")
			(bg-mode-line "#163659")
			(bg-alt "#0C1E33")
			(bg-dim "#1E2226")
			(bg-active "#3B424C")
			(bg-region "#4F5866")
			)))
  (defface treesit-preproc-identifier-face nil "Face for preprocessor identifiers"
	:group  'emma/faces)
  (defface treesit-preproc-definition-face
	'((t . (:inherit treesit-preproc-identifier-face :slant italic))) "Face for preprocessor definitions"
	:group  'emma-faces)
  (defface treesit-function-declaration-face
	'((t . (:inherit font-lock-function-name-face :slant italic))) "Face for function declarations"
	:group  'emma-faces)

  (defun emma/ef-themes-custom-faces ()
	"Emma' customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
	(ef-themes-with-colors
	  (custom-set-faces
	   `(font-lock-constant-face ((,c :foreground ,magenta-cooler)))
	   `(treesit-preproc-identifier-face ((,c :foreground ,yellow-warmer)))
	   `(font-lock-type-face ((,c :foreground ,yellow :inherit 'bold)))
	   `(font-lock-variable-name-face ((,c :foreground ,yellow-cooler)))
	   `(font-lock-property-name-face ((,c :foreground ,red)))
	   `(font-lock-preprocessor-face ((,c :foreground ,blue-warmer)))
	   `(font-lock-builtin-face ((,c :foreground ,magenta-cooler)))
	   `(font-lock-keyword-face ((,c :foreground ,magenta-warmer)))
	   `(font-lock-string-face ((,c :foreground ,green-cooler)))
	   `(font-lock-function-name-face ((,c :foreground ,cyan)))
	   )))
  (add-hook 'ef-themes-post-load-hook #'emma/ef-themes-custom-faces)

  (defun emma/apply-theme (appearance)
	"Load theme, taking current system APPEARANCE into consideration."
	(mapc #'disable-theme custom-enabled-themes)
	(pcase appearance
	  ('light (ef-themes-select 'ef-symbiosis)) ;; use dark theme for both, but can be changed easily
	  ('dark (ef-themes-select 'ef-symbiosis))))

  (if (eq system-type 'darwin) ; set higher font size on macos, due to high dpi
	  (add-hook 'ns-system-appearance-change-functions #'emma/apply-theme)
	(mapc #'disable-theme custom-enabled-themes)
	(ef-themes-select 'ef-symbiosis)))


;; set backup file directory, so backups are not dumped in current dir
(let ((backup-files-directory
       (file-name-concat emma-cache-directory "backup/")))
  (make-directory backup-files-directory :parents)

  (setq backup-directory-alist
		`(("." . ,backup-files-directory))))
;; same as above, but with autosave
(let ((save-files-directory
       (file-name-concat emma-cache-directory "autosave/")))
  (make-directory save-files-directory :parents)
  (setq auto-save-file-name-transforms
		`(("\\(?:[^/]*/\\)*\\(.*\\)" ,save-files-directory t))))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default nil               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      create-lockfiles nil              ; disable annoying lockfiles (starting with ".#"
      )

(add-to-list 'desktop-locals-to-save 'buffer-undo-list)

(setq enable-recursive-minibuffers t)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; elide GNU propaganda
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)
(setq inhibit-startup-screen t)

;; dont wrap lines, scroll with trackpad
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
(setq-default truncate-lines t)

(setq echo-keystrokes 0.01)

(defun emma/open-buffer-with (name mode txt)
  "create a new buffer with name, of mode, insert txt"
  (pop-to-buffer (get-buffer-create name))
  (with-current-buffer name
	(read-only-mode -1)
	(erase-buffer)
	(insert txt)
	(funcall mode)))

(use-package config :load-path "modules")

(use-package emma-evil :load-path "modules")
(use-package emma-ui :load-path "modules/emma-ui")
(use-package emma-utils :load-path "modules/emma-utils")
(use-package emma-lang :load-path "modules/emma-lang")
(use-package emma-format :load-path "modules")
(use-package emma-org :load-path "modules")
(use-package emma-fun :load-path "modules")

(use-package recentf
  :defer 1
  :config
  (recentf-mode 1))
(use-package autorevert
  :defer 1
  :config
  (global-auto-revert-mode t))

(use-package project
  :custom
  (project-vc-extra-root-markers '("package.json" "tsconfig.json" ".project.el")))
(setq fit-window-to-buffer-horizontally t)

;; turn off annoying bell
(defun my-bell-function ()
  (unless (memq this-command
				'(isearch-abort abort-recursive-edit exit-minibuffer
								keyboard-quit mwheel-scroll down up next-line previous-line
								backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

(global-display-line-numbers-mode 1)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((((class color) (min-colors 256)) :foreground "#af85ff")))
 '(font-lock-constant-face ((((class color) (min-colors 256)) :foreground "#af85ff")))
 '(font-lock-function-name-face ((((class color) (min-colors 256)) :foreground "#4fbaef")))
 '(font-lock-keyword-face ((((class color) (min-colors 256)) :foreground "#e580ea")))
 '(font-lock-preprocessor-face ((((class color) (min-colors 256)) :foreground "#6a9fff")))
 '(font-lock-property-name-face ((((class color) (min-colors 256)) :foreground "#FF6461")))
 '(font-lock-string-face ((((class color) (min-colors 256)) :foreground "#94E596")))
 '(font-lock-type-face ((((class color) (min-colors 256)) :foreground "#FFCA6A" :inherit 'bold)))
 '(font-lock-variable-name-face ((((class color) (min-colors 256)) :foreground "#FFE8BF")))
 '(lsp-face-semhl-constant ((((class color) (min-colors 256)) :foreground "#FFA050")))
 '(lsp-face-semhl-macro ((((class color) (min-colors 256)) :foreground "#FFA050")))
 '(lsp-face-semhl-member ((((class color) (min-colors 256)) :foreground "#d56f72")))
 '(lsp-face-semhl-property ((((class color) (min-colors 256)) :foreground "#FF6461")))
 '(treesit-preproc-identifier-face ((((class color) (min-colors 256)) :foreground "#FFA050")) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia buffer-name-relative citre cmake-mode consult-dir
			  consult-eglot-embark devdocs diff-hl ef-themes evil
			  evil-collection evil-commentary evil-org
			  evil-textobj-tree-sitter exec-path-from-shell fireplace
			  flymake-eslint helpful kurecolor marginalia
			  markdown-mode mood-line orderless org-journal popper
			  rainbow-delimiters undo-fu undo-fu-session vertico
			  visual-fill-column vterm vue-ts-mode))
 '(package-vc-selected-packages
   '((vue-ts-mode :url "https://github.com/8uff3r/vue-ts-mode")))
 '(safe-local-variable-values '((vue-ts-mode-indent-offset . 2))))
