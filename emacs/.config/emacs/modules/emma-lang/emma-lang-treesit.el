(use-package treesit
  :defer t
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
		'((bash "https://github.com/tree-sitter/tree-sitter-bash")
		  (cmake "https://github.com/uyha/tree-sitter-cmake")
		  (css "https://github.com/tree-sitter/tree-sitter-css")
		  (vue "https://github.com/ikatyang/tree-sitter-vue")
		  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
		  (go "https://github.com/tree-sitter/tree-sitter-go")
		  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
		  (rust "https://github.com/tree-sitter/tree-sitter-rust")
		  (c "https://github.com/tree-sitter/tree-sitter-c")
		  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		  (html "https://github.com/tree-sitter/tree-sitter-html")
		  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		  (json "https://github.com/tree-sitter/tree-sitter-json")
		  (make "https://github.com/alemuller/tree-sitter-make")
		  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
		  (python "https://github.com/tree-sitter/tree-sitter-python")
		  (toml "https://github.com/tree-sitter/tree-sitter-toml")
		  (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(use-package evil-textobj-tree-sitter
  :ensure t
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  )
(use-package html-ts-mode
  :mode "\\.html\\'"
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode)))

(provide 'emma-lang-treesit)
