(use-package citre
  :ensure t
  :defer t
  :init
  (require 'citre-config)
  :config
  (defvar emma/citre-map 
	(make-sparse-keymap))
  (keymap-set citre-mode-map "C-x c" '("citre" . emma/citre-map))
  (keymap-set emma/citre-map "j" 'citre-jump)
  (keymap-set emma/citre-map "J" 'citre-jump-back)
  (keymap-set emma/citre-map "p" 'citre-ace-peek)
  (keymap-set emma/citre-map "u" 'citre-update-this-tags-file)
  (setq
   citre-completion-backends '(eglot tags)
   citre-default-create-tags-file-location 'global-cache
   citre-edit-ctags-options-manually nil
   citre-auto-enable-citre-mode-modes '(prog-mode)))

(provide 'emma-utils-citre)
