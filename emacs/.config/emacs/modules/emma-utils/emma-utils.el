(use-package emma-utils-dired
  :load-path "modules/emma-utils")
(use-package emma-utils-which-key
  :load-path "modules/emma-utils")
(use-package emma-utils-completion
  :load-path "modules/emma-utils")
(use-package emma-utils-help
  :load-path "modules/emma-utils")
(use-package emma-utils-git
  :load-path "modules/emma-utils")

(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))



(provide 'emma-utils)
