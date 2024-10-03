(use-package eglot
  :hook ((go-ts-mode . eglot-ensure)
		 (go-mode . eglot-ensure)
		 (c-mode . eglot-ensure)
		 (c-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  ;; :bind (:map eglot-mode-map
  ;;             ("C-c a r" . #'eglot-rename)
  ;;             ("C-<down-mouse-1>" . #'xref-find-definitions)
  ;;             ("C-S-<down-mouse-1>" . #'xref-find-references)
  ;;             ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  )


(provide 'emma-lang-eglot)
