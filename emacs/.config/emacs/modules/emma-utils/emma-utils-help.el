(use-package helpful
  :defer t
  ;; :commands (helpful-callable
  ;; helpful-variable
  ;; helpful-key
  ;; helpful-command
  ;; helpful-at-point)
  :ensure t
  :init ;; run before loading, so that keybinds can load the package
  (define-key help-map (kbd "f") #'helpful-callable)
  (define-key help-map (kbd "v") #'helpful-variable)
  (define-key help-map (kbd "k") #'helpful-key)
  (define-key help-map (kbd "x") #'helpful-command)
  (define-key help-map (kbd "h") #'helpful-at-point)

  (define-key global-map (kbd "C-h .") #'eldoc)
  )


(use-package devdocs
  :ensure t
  :bind (:map help-map
			  ("d" . devdocs-lookup-at-point)
			  ("D" . devdocs-lookup))
  :custom
  (devdocs-data-dir "~/.local/share/emacs/devdocs")
  :config
  (defun devdocs-lookup-at-point ()
	(interactive)
	(devdocs-lookup nil (thing-at-point 'symbol)))

  )

(provide 'emma-utils-help)
