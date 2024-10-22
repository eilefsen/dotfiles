(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
				 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  )

(provide 'emma-lang-rust)
