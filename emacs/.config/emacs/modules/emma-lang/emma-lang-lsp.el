(use-package lsp-mode
  :ensure t
  :config
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package xref)

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  :config

  (defun eldoc-display-in-echo-area 'ignore)
  (setq eldoc-idle-delay 0))
  

;; provides syntax highlighting in eldoc output
(use-package markdown-mode
  :ensure t)

(provide 'emma-lang-lsp)
