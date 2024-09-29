(use-package dashboard
  :straight t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startup-banner 'logo)
  (setq initial-buffer-choice
	(lambda () (get-buffer-create dashboard-buffer-name)))
  (forward-line 8))


(provide 'emma-ui-dash)
