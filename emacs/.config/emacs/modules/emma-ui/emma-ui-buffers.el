(use-package popper
  :straight t
  :after evil
  :bind ((""   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc for"
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  (define-key toggle-prefix-map (kbd "p") '("Toggle popup" . popper-toggle)))


(provide 'emma-ui-buffers)
