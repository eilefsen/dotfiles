 (use-package embark
	:ensure t
	:defer 1
    :bind
	(:map embark-buffer-map
		  ("_" . embark-popper-toggle))
    :config
    (defun embark-popper-toggle (buf)
      "Toggle popup status."
      (popper-toggle-type buf))
	:init
	(setq prefix-help-command #'embark-prefix-help-command)
	(global-set-key (kbd "C-h C-h" ) prefix-help-command) ;; didn't really need help-for-help anyway
	)

(use-package embark-consult
  :ensure t)

(use-package evil
  :config
  (define-key evil-window-map (kbd "C-h" ) prefix-help-command)
  )

(provide 'emma-utils-embark)
