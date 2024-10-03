(use-package vterm
  :straight t
  :config
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (define-key vterm-mode-map (kbd "<escape>") '("Exit vterm" . popper-toggle))
  (define-key vterm-mode-map (kbd "C-t") `("terminal" . ,terminal-prefix-map))
  (defun turn-off-chrome ()
	(hl-line-mode -1)
	(display-line-numbers-mode -1))
  :hook
  (vterm-mode . turn-off-chrome))

(defun emma/open-terminal ()
  (interactive)
  (if (get-buffer "*vterm*")
	  (pop-to-buffer "*vterm*")
	(vterm)))
(defun emma/toggle-terminal ()
  (interactive)
  (if (string= (buffer-name) "*vterm*")
	  (popper-toggle)
	(emma/open-terminal)))

(define-key terminal-prefix-map (kbd "t") '("Toggle terminal" . emma/toggle-terminal))


(provide 'emma-ui-term)
