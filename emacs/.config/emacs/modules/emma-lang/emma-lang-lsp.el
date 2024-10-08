(use-package lsp-mode
  :ensure t
  :defer t
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  (lsp-disabled-clients '(vue-semantic-server))
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-eldoc-enable-hover t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-eldoc-render-all t)
  (lsp-enable-imenu t)

  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; disable
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-lens-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  :config
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (define-key global-map (kbd "C-c l") `("lsp" . ,lsp-mode-map))
  (define-key global-map (kbd "C-c l") `("lsp" . ,lsp-command-map))

  :commands (lsp lsp-deferred))

(use-package xref)

(use-package eldoc
  :defer 2
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  :config

  (defun eldoc-display-in-echo-area 'ignore)
  (setq eldoc-idle-delay 0))
  

;; provides syntax highlighting in eldoc output
(use-package markdown-mode
  :defer 2
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t))


(provide 'emma-lang-lsp)
