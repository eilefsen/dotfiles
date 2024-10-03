(use-package helpful
  :after evil
  :straight t
  :config
  (define-key help-map (kbd "f") #'helpful-callable)

  (define-key help-map (kbd "v") #'helpful-variable)
  (define-key help-map (kbd "k") #'helpful-key)
  (define-key help-map (kbd "x") #'helpful-command)
  (define-key help-map (kbd "h") #'helpful-at-point)
  (define-key global-map (kbd "C-h .") #'eldoc-doc-buffer)
  )


(provide 'emma-utils-help)
