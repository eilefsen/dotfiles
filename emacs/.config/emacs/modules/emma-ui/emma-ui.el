(use-package emma-ui-dashboard
  :load-path "modules/emma-ui")
(use-package emma-ui-buffers
  :load-path "modules/emma-ui")

(use-package mood-line
  :straight t
  :config
  (mood-line-mode))

(provide 'emma-ui)
