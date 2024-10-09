(use-package emma-ui-dashboard
  :load-path "modules/emma-ui")
(use-package emma-ui-buffers
  :load-path "modules/emma-ui")
(use-package emma-ui-term
  :load-path "modules/emma-ui")

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  :config
  (setq-default frame-title-format "%b (%f)")
  )

(use-package buffer-name-relative
  :ensure t
  :config
  (buffer-name-relative-mode))

(provide 'emma-ui)
