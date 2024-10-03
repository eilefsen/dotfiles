(use-package popper
  :straight t
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  (define-key global-map (kbd "C-c p") '("Toggle popup" . popper-toggle))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              helpful-mode
                              prodigy-mode
                              "magit:.\*"
                              "\\*deadgrep.\*"
                              "\\*eldoc.\*"
                              "\\*xref\\*"
                              "\\*direnv\\*"
                              "\\*Warnings\\*"
							  "\\*Bookmark List\\*"
							  "^\\*eshell.*\\*$" eshell-mode 
							  "^\\*shell.*\\*$"  shell-mode  
							  "^\\*term.*\\*$"   term-mode   
							  "^\\*vterm.*\\*$"  vterm-mode
							  haskell-compilation-mode
                              compilation-mode
                              bqn-inferior-mode)))

(provide 'emma-ui-buffers)
