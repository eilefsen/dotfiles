(use-package magit
  :straight t
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/src" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; (use-package code-review
;;   :after magit
;;   :bind (:map forge-topic-mode-map ("C-c r" . #'code-review-forge-pr-at-point))
;;   :bind (:map code-review-mode-map (("C-c n" . #'code-review-comment-jump-next)
;;                                     ("C-c p" . #'code-review-comment-jump-previous))))

(provide 'emma-utils-git)
