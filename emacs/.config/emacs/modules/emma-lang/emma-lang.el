(use-package emma-lang-treesit
  :load-path "modules/emma-lang")
(use-package emma-lang-eglot
  :load-path "modules/emma-lang")
;; (use-package emma-lang-lsp
;;   :load-path "modules/emma-lang")

(use-package paren
  :defer t
  :config (show-paren-mode)
  :custom (show-paren-style 'parenthesis))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package flymake
  :config
  (defun emma/flymake--get-diagnostic-at-point-text ()
	"Get the flymake diagnostic text for the thing at point."
	(flymake--diag-text (get-char-property (point) 'flymake-diagnostic)))
  (defun emma/flymake-diagnostic-at-point ()
	(interactive)
	(emma/open-buffer-with "*diagnostic*" 'special-mode (emma/flymake--get-diagnostic-at-point-text)))
  (define-key language-prefix-map (kbd "f") #'emma/flymake-diagnostic-at-point))

;; langs
(use-package emma-lang-elisp
  :load-path "modules/emma-lang/langs")
(use-package emma-lang-typescript
  :load-path "modules/emma-lang/langs")
(use-package emma-lang-c
  :load-path "modules/emma-lang/langs")
(use-package emma-lang-rust
  :load-path "modules/emma-lang/langs")

(provide 'emma-lang)
