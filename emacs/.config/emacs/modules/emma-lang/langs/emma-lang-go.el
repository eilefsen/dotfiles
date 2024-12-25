(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  )

(provide 'emma-lang-go)
