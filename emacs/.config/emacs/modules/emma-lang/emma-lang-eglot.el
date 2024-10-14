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

  (evil-define-key 'normal 'eglot-mode-map (kbd "<leader>c a") #'eglot-code-actions)
  (evil-define-key 'normal 'eglot-mode-map (kbd "<leader>c r") #'eglot-rename)
  )

(use-package xref
  :config
  (keymap-set code-prefix-map "f" 'xref-find-apropos))

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
