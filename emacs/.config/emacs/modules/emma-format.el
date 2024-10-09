(use-package apheleia
  :ensure t
  :after evil
  :config
  (define-key code-prefix-map (kbd "f") '("Format buffer" . apheleia-format-buffer))
  (define-key ui-prefix-map (kbd "f") '("Toggle auto-format (buffer)" . apheleia-mode))
  (define-key ui-prefix-map (kbd "F") '("Toggle auto-format (global)" . apheleia-global-mode))
  (apheleia-global-mode +1))

(provide 'emma-format)
