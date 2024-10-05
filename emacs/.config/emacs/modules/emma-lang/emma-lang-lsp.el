(use-package lsp-mode
  :ensure t
  :config
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (define-key global-map (kbd "C-c l") `("lsp" . ,lsp-mode-map))
  (define-key global-map (kbd "C-c l") `("lsp" . ,lsp-command-map))
  (setq lsp-keymap-prefix "C-c l"
		lsp-disabled-clients '(vue-semantic-server)
		lsp-headerline-breadcrumb-enable nil
		lsp-diagnostics-provider :flymake
		lsp-eldoc-enable-hover t
		lsp-semantic-tokens-enable t)

  :commands (lsp lsp-deferred))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (define-key lsp-mode-map (kbd "<mouse-3>") nil))

(use-package xref)

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  :config

  (defun eldoc-display-in-echo-area 'ignore)
  (setq eldoc-idle-delay 0))
  

;; provides syntax highlighting in eldoc output
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t))


(provide 'emma-lang-lsp)
