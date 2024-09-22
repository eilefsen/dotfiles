;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp")

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(require-theme 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-hl-line '(underline accented)
      modus-themes-subtle-line-numbers t
      modus-themes-syntax '(alt-syntax)
      )
(setq doom-theme 'modus-operandi)

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
          ))
  )


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

(require 'vue-ts-mode)
(add-hook! 'vue-ts-mode-hook #'lsp!)

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
