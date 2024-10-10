(use-package eglot
  :hook ((go-ts-mode . eglot-ensure)
		 (go-mode . eglot-ensure)
		 (c-mode . eglot-ensure)
		 (c-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  ;; :bind (:map eglot-mode-map
  ;;             ("C-c a r" . #'eglot-rename)
  ;;             ("C-<down-mouse-1>" . #'xref-find-definitions)
  ;;             ("C-S-<down-mouse-1>" . #'xref-find-references)
  ;;             ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-autoshutdown t)
  :config
  (defun emma/disable-eldoc ()
	(when (eglot-managed-p)
	  (eldoc-mode -1)))
  (add-hook 'eglot-managed-mode-hook #'emma/disable-eldoc)
  (global-eldoc-mode -1)
  (define-key eglot-mode-map [remap eldoc-doc-buffer] #'eldoc)
  )

(use-package xref)

(use-package eldoc
  :config
  (defun hide-dos-eol ()
	"Hide ^M in files containing mixed UNIX and DOS line endings."
	(interactive)
	(setq buffer-display-table (make-display-table))
	(aset buffer-display-table ?\^M []))
  (defun show-dos-eol ()
	"Show ^M in files containing mixed UNIX and DOS line endings."
	(interactive)
	(setq buffer-display-table (make-display-table))
	(aset buffer-display-table ?\^M ?\^M))
  (defun hide-dos-eol-in-eldoc ()
	(when (string-match-p "\\*eldoc.\*" (buffer-name))
	  (hide-dos-eol)))
  (add-hook 'special-mode-hook 'hide-dos-eol-in-eldoc)

  (global-eldoc-mode -1)
  (setq eldoc-display-functions '(eldoc-display-in-buffer))

  ;; (defun eldoc-display-in-echo-area 'ignore)
  (setq eldoc-idle-delay 0)
  )

(use-package peek
  :vc (peek :url "https://git.sr.ht/~meow_king/peek" :rev :newest)
  :custom
  (peek-overlay-window-size 11)  ; lines
  (peek-overlay-border-character ?\N{BOX DRAWINGS LIGHT HORIZONTAL})
  (peek-overlay-position 'above)
  (peek-overlay-distance 4)
  (peek-definition-surrounding-above-lines 1)
  (peek-live-update t)
  (peek-enable-eldoc-message-integration nil)
  (peek-enable-eldoc-display-integration nil)

  :config
  (global-peek-mode 1)

  ;; Keybindings 
  ;; default keybindings in peek-mode-keymap
  (define-key peek-mode-keymap (kbd "M-n") 'peek-next-line)
  (define-key peek-mode-keymap (kbd "M-p") 'peek-prev-line)
  
  ;; or you can use `keymap-global-set', which is introduced in emacs 29

  (setq peek-prefix-map (make-sparse-keymap))
  (define-key code-prefix-map (kbd "p") `("peek" . ,peek-prefix-map))
  (define-key peek-prefix-map (kbd "p") #'peek-overlay-dwim)
  (define-key peek-prefix-map (kbd "d") #'peek-xref-definition)
  
  ;; you may also want to set scroll margin (see its docs)
  (setq-default scroll-margin 5))

;; provides syntax highlighting in eldoc output
(use-package markdown-mode
  :ensure t)

(provide 'emma-lang-eglot)
