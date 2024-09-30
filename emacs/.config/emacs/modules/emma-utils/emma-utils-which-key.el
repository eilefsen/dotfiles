(use-package which-key
  :straight t
  :config 
  (setq which-key-idle-delay 0.3
	which-key-idle-secondary-delay 0)
  (setq which-key-separator " → " 
	which-key-unicode-correction 3)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(provide 'emma-utils-which-key)
