(use-package helpful
  :after evil
  :straight t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  )

(use-package xref
  :bind (("s-r" . #'xref-find-references)
         ("s-[" . #'xref-go-back)
         ("C-<down-mouse-2>" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))
:

(use-package eldoc
  :after evil
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  :config

  (defun eldoc-display-in-echo-area 'ignore)
  (setq eldoc-idle-delay 0)
  (evil-define-key 'motion 'global (kbd "K") '("Lookup" . eldoc))
  )

(use-package markdown-mode
  :straight t)

(provide 'emma-utils-help)
