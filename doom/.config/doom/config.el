;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp")
(add-load-path! "lsp")

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(defface emma-lsp-face-semh-modifier-readonly '((t)) "Face for LSP semantic token modifier Declaration")
(defface emma-lsp-face-semh-modifier-declaration '((t :slant italic)) "Face for LSP semantic token modifier Declaration")


(setq doom-theme 'doom-one)
(custom-theme-set-faces! 'doom-one
  `(lsp-face-semhl-macro :foreground ,(doom-color 'orange))
  `(lsp-face-semhl-function :inherit font-lock-function-name-face)
  `(font-lock-function-name-face :foreground ,(doom-color 'blue))
  `(font-lock-preprocessor-face :slant italic :foreground ,(doom-color 'magenta))
  `(font-lock-keyword-face :foreground ,(doom-color 'magenta))
  `(font-lock-variable-name-face :foreground ,(doom-color 'red))
  `(emma-lsp-face-semh-modifier-readonly :foreground ,(doom-color 'orange))
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

(use-package! treesit
  :custom
  (treesit-font-lock-level 3)
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
          ))
  )



(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode))
  :config
  (add-hook! '(c-ts-mode-hook) #'lsp!)
  )

(after! lsp-clangd
  (setq
   lsp-clients-clangd-args
   '("-j=4"
     "--header-insertion=never"
     "--all-scopes-completion"
     "--background-index"
     "--clang-tidy"
     "--cross-file-rename"
     "--suggest-missing-includes"
     "--query-driver=/opt/homebrew/bin/avr-gcc"
     )
   ))

(after! (:or lsp-clangd lsp-javascript lsp-volar)
  (setq lsp-semantic-tokens-enable t)
  (setq-default lsp-semantic-token-modifier-faces
                (cons '("declaration" . emma-lsp-face-semh-modifier-declaration)
                      (assoc-delete-all "declaration" lsp-semantic-token-modifier-faces)))
  (setq-default lsp-semantic-token-modifier-faces
                (cons '("readonly" . emma-lsp-face-semh-modifier-readonly)
                      (assoc-delete-all "readonly" lsp-semantic-token-modifier-faces)))
  )

(after! lsp-mode
  (setq lsp-log-io t))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook) #'lsp!)
  )
(use-package tsx-ts-mode
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(tsx-ts-mode-hook) #'lsp!)
  )

(use-package vue-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
  (add-hook! 'vue-ts-mode-hook #'lsp!)
  )

(after! lsp-javascript
  (set-lsp-priority! 'ts-ls 10)) ;; higher priority than vue language server

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

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode
             vue-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))
