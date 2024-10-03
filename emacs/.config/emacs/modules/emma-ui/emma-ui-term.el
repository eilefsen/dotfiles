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

(define-key terminal-prefix-map (kbd "t") '("Toggle terminal" . emma/toggle-terminal))

(use-package vterm
  :straight t
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (define-key vterm-mode-map (kbd "<escape>") '("Exit vterm" . emma/close-terminal))
  (define-key vterm-mode-map (kbd "C-c") `("terminal" . ,terminal-prefix-map))
  (defun turn-off-chrome ()
	(hl-line-mode -1)
	(display-line-numbers-mode -1))
  :hook
  (vterm-mode . turn-off-chrome))



(provide 'emma-ui-term)
