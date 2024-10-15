(use-package magit
  :ensure t
  :defer t
  :bind ((:map git-prefix-map (("g" . #'magit-status)
							   ("B" . #'magit-blame-addition)
							   ("c" . #'magit-checkout)
							   ("bc" . #'magit-branch-checkout)
							   ("L" . #'magit-log)
							   ("ll" . #'magit-log-current)
							   ("la" . #'magit-log-all))))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-show-staged-changes nil)
  :config 
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)
  (global-diff-hl-mode 1))

(provide 'emma-utils-git)
