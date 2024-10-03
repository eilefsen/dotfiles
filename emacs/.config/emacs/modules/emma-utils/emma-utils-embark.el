 (use-package embark
	:straight t
    :bind (:map embark-buffer-map
           ("_" . embark-popper-toggle))
    :config
    (defun embark-popper-toggle (buf)
      "Toggle popup status."
      (popper-toggle-type buf))
	(setq prefix-help-command #'embark-prefix-help-command))


(provide 'emma-utils-embark)
