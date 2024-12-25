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
  :custom
  (c-ts-mode-indent-offset 4)
  :config
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook #'emma/apply-c-font-lock-rules))

(use-package apheleia
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
		'("clang-format" "-assume-filename" "-style=file"
		  (or (apheleia-formatters-local-buffer-file-name)
			  (apheleia-formatters-mode-extension) ".c")))
  )

(use-package c++-ts-mode
  :defer t
  :mode (("\\.cpp\\'" . c++-ts-mode))
  :config
  (add-hook 'c++-ts-mode-hook #'eglot-ensure))

(defvar emma/clangd-query-driver '("/opt/homebrew/bin/*gcc" "~/.espressif/tools/*elf/**/*gcc" "/opt/homebrew/bin/avr-gcc*")
  "List of strings with paths pointing to additional query drivers for clangd")
(defvar emma/clangd-binary-path "clangd" 
  "Path to clangd")

(defun emma/clangd--get-query-driver ()
  (if emma/clangd-query-driver
	  (concat "--query-driver="
			  (progn (require 'subr-x)
					 (string-join emma/clangd-query-driver ",")))
	""))

(defun emma/clangd-replace-binary ()
  (interactive)
  (when-let (l (alist-get 'c-ts-mode eglot-server-programs))
	(setf (nth 0 l) emma/clangd-binary-path)))

(defun emma/clangd-config ()
  (add-to-list 'eglot-server-programs
			   `(c-ts-mode
				 . (,emma/clangd-binary-path
					"-j=4"
					,(emma/clangd--get-query-driver) ;; allowlist for which compilers clangd will query (based on compile_commands.txt)
					"--log=error"
					"--background-index"
					"--clang-tidy"
					"--fallback-style=llvm"
					"--completion-style=detailed"
					"--pch-storage=memory"
					"--header-insertion=never"
					"--header-insertion-decorators=0"
					"--function-arg-placeholders=0"
					)))
  )

(with-eval-after-load 'eglot (emma/clangd-config))

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
