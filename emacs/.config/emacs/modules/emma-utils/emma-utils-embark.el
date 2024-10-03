 (use-package embark
	:straight t
    :bind
	(:map embark-buffer-map
		  ("_" . embark-popper-toggle))
    :config
    (defun embark-popper-toggle (buf)
      "Toggle popup status."
      (popper-toggle-type buf))
	(setq prefix-help-command #'embark-prefix-help-command)
	(global-set-key (kbd "C-h C-h" ) prefix-help-command)) ;; didn't really need help-for-help anyway

 (use-package embark-consult
	:straight t)

(provide 'emma-utils-embark)
