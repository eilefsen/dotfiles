(use-package vterm
  :ensure t
  :defer t
  :init
  :bind (:map terminal-prefix-map
			  ("t" . #'emma/toggle-terminal))
  :commands (emma/toggle-terminal vterm)
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (define-key vterm-mode-map (kbd "<escape>") '("Exit vterm" . emma/close-terminal))
  (defun turn-off-chrome ()
	(hl-line-mode -1)
	(display-line-numbers-mode -1))
  (defun emma/open-terminal ()
	(interactive)
	(if (get-buffer "*vterm*")
		(pop-to-buffer "*vterm*")
	  (vterm)))
  (defun emma/close-terminal ()
	(interactive)
	(if (or (eq popper-popup-status 'popup)
			(eq popper-popup-status 'user-popup))
		(popper-toggle)
	  (delete-window)))
  (defun emma/toggle-terminal ()
	(interactive)
	(if (string= (buffer-name) "*vterm*")
		(emma/close-terminal)
	  (emma/open-terminal)))

  :hook
  (vterm-mode . turn-off-chrome))



(provide 'emma-ui-term)
