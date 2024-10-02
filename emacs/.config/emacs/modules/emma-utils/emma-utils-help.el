(use-package helpful
  :after evil
  :straight t
  :config
  (setq evil-lookup-func 'helpful-at-point)
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  )

(use-package eldoc
  :after evil
  :preface
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :init
  (global-eldoc-mode 1)
  :config 
  (setq-default evil-lookup-func #'eldoc)
  )


(provide 'emma-utils-help)
