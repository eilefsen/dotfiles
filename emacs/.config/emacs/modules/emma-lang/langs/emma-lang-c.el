(defun emma/apply-c-font-lock-rules ()
  (treesit-add-font-lock-rules (treesit-font-lock-rules
								:language 'c
								:feature 'preprocessor
								:override t
								`((preproc_ifdef name: (identifier) @treesit-preproc-definition-face)
								  (preproc_def name: (identifier) @treesit-preproc-definition-face)
								  ((identifier) @treesit-preproc-identifier-face
								   (:match "\\`[A-Z_][0-9A-Z_]*\\'" @treesit-preproc-identifier-face)))
								) :before nil)
  )

(use-package c-ts-mode
  :defer t
  :mode (("\\.c\\'" . c-ts-mode))
  :config
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook #'emma/apply-c-font-lock-rules)
  )

(use-package c++-ts-mode
  :defer t
  :mode (("\\.cpp\\'" . c++-ts-mode))
  :config
  (add-hook 'c++-ts-mode-hook #'eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   '(c-ts-mode
				 . ("clangd"
					"-j=4"
					"--query-driver=/opt/homebrew/bin/avr-gcc" ;; allowlist for which compilers clangd will query (based on compile_commands.txt)
					"--log=error"
					"--background-index"
					"--clang-tidy"
					"--completion-style=detailed"
					"--pch-storage=memory"
					"--header-insertion=never"
					"--header-insertion-decorators=0"))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   '(c++-ts-mode
				 . ("clangd"
					"-j=4"
					"--log=error"
					"--background-index"
					"--clang-tidy"
					"--completion-style=detailed"
					"--pch-storage=memory"
					"--header-insertion=never"
					"--header-insertion-decorators=0"))))

(provide 'emma-lang-c)
