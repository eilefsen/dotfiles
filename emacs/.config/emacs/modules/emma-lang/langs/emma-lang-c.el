(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode))
  :config
  (add-hook 'c-ts-mode-hook #'lsp))

(use-package lsp-clangd
  :config
  (setq
   lsp-clients-clangd-args
   '("-j=4"
     "--header-insertion=never"
     "--all-scopes-completion"
     "--background-index"
     "--clang-tidy"
     "--cross-file-rename"
     "--suggest-missing-includes"
     "--query-driver=/opt/homebrew/bin/avr-gcc"  ;; support avr
     )
   ))

(provide 'emma-lang-c)
