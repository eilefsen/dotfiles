;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! typescript-mode :disable t)

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! flymake-eslint)
(package! apheleia)
