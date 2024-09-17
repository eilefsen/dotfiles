;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-load-path! "lisp")

(setq doom-theme 'doom-one)

(setq display-line-numbers-type t)

(setq user-full-name "Emma Eilefsen Glenna"
      user-mail-address "emma@eilefsen.net")

(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0)

(setq org-directory "~/org/")
(setq org-return-follows-link  t)


(after! treesit
  (setq treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)
          (vue "https://github.com/ikatyang/tree-sitter-vue")
          (css "https://github.com/tree-sitter/tree-sitter-css"))))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!)
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) 'auto-revert-mode)
  )

(require 'vue-ts-mode)
(add-hook! 'vue-ts-mode-hook #'lsp!)

(after! lsp-javascript
  (set-lsp-priority! 'ts-ls 10)) ;; higher priority than vue language server

(use-package flymake-eslint
  :custom
  (flymake-eslint-executable-name "eslint_d")
  :config
  (add-hook 'vue-ts-mode-hook
            (lambda () (flymake-eslint-enable))
            )
  )



(use-package apheleia
  :defer t
  :config
  (add-to-list 'apheleia-formatters '(eslint_d . (npx "eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" file)))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . eslint_d))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . eslint_d))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . eslint_d))
  (apheleia-global-mode +1)
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
