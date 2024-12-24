(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-show-staged-changes t)
  :config 
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)
  (global-diff-hl-mode 1))

(provide 'emma-utils-git)
