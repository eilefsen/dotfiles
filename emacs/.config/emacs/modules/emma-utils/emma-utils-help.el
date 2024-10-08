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

  (define-key global-map (kbd "C-h .") #'eldoc-doc-buffer)
  )


(provide 'emma-utils-help)
