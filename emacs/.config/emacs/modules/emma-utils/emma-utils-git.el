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

(provide 'emma-utils-git)
