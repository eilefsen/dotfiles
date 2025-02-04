(use-package eglot
  :hook ((go-ts-mode . eglot-ensure)
		 (go-mode . eglot-ensure)
		 (c-mode . eglot-ensure)
		 (c-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :custom
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  :config
  (defun emma/disable-eldoc ()
	(when (eglot-managed-p)
	  (eldoc-mode -1)))
  (add-hook 'eglot-managed-mode-hook #'emma/disable-eldoc)
  (global-eldoc-mode -1)
  (define-key eglot-mode-map [remap eldoc-doc-buffer] #'eldoc)
  (defun emma/disable-inlay-hints ()
	(when (eglot-managed-p)
	  (eglot-inlay-hints-mode -1)))
  (add-hook 'eglot-managed-mode-hook #'emma/disable-inlay-hints)
  (evil-define-key 'normal 'eglot-mode-map (kbd "<leader>u h") '("Toggle inlay hints" . eglot-inlay-hints-mode))

  (evil-define-key 'normal 'eglot-mode-map (kbd "<leader>c a") #'eglot-code-actions)
  (evil-define-key 'normal 'eglot-mode-map (kbd "<leader>c r") #'eglot-rename))

(use-package xref
  :config
  (keymap-set code-prefix-map "g" 'xref-find-apropos))

(use-package consult-eglot
  :vc (:url "https://github.com/mohkale/consult-eglot" :rev "9b490eb384d34ffc60c16cf16fe725ce5c72303a" )
  :config
  (keymap-set code-prefix-map "s" #'consult-eglot-symbols)
  )

(use-package consult-eglot-embark
  :ensure t
  :after consult-eglot
  :config
  (consult-eglot-embark-mode))

(use-package eldoc
  :config
  (defun hide-dos-eol ()
	"Hide ^M in files containing mixed UNIX and DOS line endings."
	(interactive)
	(setq buffer-display-table (make-display-table))
	(aset buffer-display-table ?\^M []))
  (defun show-dos-eol ()
	"Show ^M in files containing mixed UNIX and DOS line endings."
	(interactive)
	(setq buffer-display-table (make-display-table))
	(aset buffer-display-table ?\^M ?\^M))
  (defun hide-dos-eol-in-eldoc ()
	(when (string-match-p "\\*eldoc.\*" (buffer-name))
	  (hide-dos-eol)))
  (add-hook 'special-mode-hook 'hide-dos-eol-in-eldoc)

  (global-eldoc-mode -1)
  (setq eldoc-display-functions '(eldoc-display-in-buffer))

  ;; (defun eldoc-display-in-echo-area 'ignore)
  (setq eldoc-idle-delay 0)
  )

;; provides syntax highlighting in eldoc output
(use-package markdown-mode
  :ensure t)

(provide 'emma-lang-eglot)
