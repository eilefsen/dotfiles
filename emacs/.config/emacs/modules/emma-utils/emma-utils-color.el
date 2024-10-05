(use-package kurecolor
  :ensure t
  :config

  (setq kurecolor-prefix-map (make-sparse-keymap))
  (define-key global-map (kbd "C-c k") `("kurecolor" . ,kurecolor-prefix-map))
  (define-key kurecolor-prefix-map (kbd "h") '("Set hue" . kurecolor-set-hue))
  (define-key kurecolor-prefix-map (kbd "s") '("Set saturation" . kurecolor-set-saturation))
  (define-key kurecolor-prefix-map (kbd "l") '("Set lightness" . kurecolor-set-brightness))
  )

(provide 'emma-utils-color)
