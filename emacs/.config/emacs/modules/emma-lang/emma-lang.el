(use-package emma-lang-treesit
  :load-path "modules/emma-lang")
(use-package emma-lang-eglot
  :load-path "modules/emma-lang")

(use-package paren
  :ensure t
  :config (show-paren-mode)
  :custom (show-paren-style 'expression))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; langs
(use-package emma-lang-elisp
  :load-path "modules/emma-lang/langs")
(use-package emma-lang-vue
  :load-path "modules/emma-lang/langs")

(provide 'emma-lang)
