;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp")

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq doom-theme 'doom-one)
(custom-theme-set-faces! 'doom-one
  `(font-lock-function-name-face :foreground ,(doom-color 'blue))
  `(font-lock-preprocessor-face :slant italic :foreground ,(doom-color 'magenta))
  `(font-lock-keyword-face :foreground ,(doom-color 'magenta))
  `(font-lock-variable-name-face :foreground ,(doom-color 'red))
  )

(setq doom-font-increment 2)
(setq doom-font (font-spec :family "Cascadia Code" :size 16))

(setq display-line-numbers-type t)

(setq user-full-name "Emma Eilefsen Glenna"
      user-mail-address "emma@eilefsen.net")

(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0)

(setq org-directory "~/org/")
(setq org-return-follows-link  t)


;;lsp
(use-package vue-ts-mode
  :load-path "lisp"
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
  (add-hook 'vue-ts-mode-hook #'eglot-ensure)
  )
(after! eglot
  (put 'vue-ts-mode 'eglot-language-id "vue")
  (defun vue-eglot-init-options ()
    (let ((tsdk-path "/usr/local/lib/node_modules/typescript/lib/"))
      `(:typescript (:tsdk ,tsdk-path
                     :languageFeatures (:completion
                                        (:defaultTagNameCase "both"
                                         :defaultAttrNameCase "kebabCase"
                                         :getDocumentNameCasesRequest nil
                                         :getDocumentSelectionRequest nil)
                                        :diagnostics
                                        (:getDocumentVersionRequest nil))
                     :documentFeatures (:documentFormatting
                                        (:defaultPrintWidth 100
                                         :getDocumentPrintWidthRequest nil)
                                        :documentSymbol t
                                        :documentColor t)))))
  (add-to-list 'eglot-server-programs
               `(vue-ts-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
  )


;; treesit
(use-package! treesit
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")
          (arduino "https://github.com/tree-sitter-grammars/tree-sitter-arduino")
          (swift "https://github.com/tree-sitter/tree-sitter-swift")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)
          (vue "https://github.com/ikatyang/tree-sitter-vue")
          (svelte "https://github.com/tree-sitter-grammars/tree-sitter-svelte")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (zsh "https://github.com/tree-sitter-grammars/tree-sitter-zsh")
          (regex "https://github.com/tree-sitter/tree-sitter-regex")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
          (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
          (linkerscript "https://github.com/tree-sitter-grammars/tree-sitter-linkerscript")
          )))

(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode))
  :config
  (add-hook 'c-ts-mode-hook #'eglot-ensure))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :config
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  )
(use-package tsx-ts-mode
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
  )

;; eslint
(use-package flymake-eslint
  :custom
  (flymake-eslint-executable-name "eslint_d")
  :config
  (add-hook! '(vue-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook) #'flymake-eslint-enable)
  )
(after! apheleia
  (add-to-list 'apheleia-formatters '(eslint_d . ("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath)))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . eslint_d))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . eslint_d))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . eslint_d))
  )
