(use-package vterm
  :ensure t
  :defer t
  :init
  :bind (:map terminal-prefix-map
			  ("t" . #'emma/toggle-terminal))
  :commands (emma/toggle-terminal vterm)
  :config
  ;; (add-to-list 'evil-emacs-state-modes 'vterm-mode)
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

(defun open-kitty-linux (&rest args)
  (apply 'call-process "kitty" nil 0 nil "--working-directory" default-directory  args))
(defun open-kitty-mac (&rest args)
  (apply 'call-process "open" nil 0 nil "-a" "kitty" "--args" "--working-directory" default-directory args))
(defun open-kitty (&rest args)
  (interactive)
  (if (eq system-type 'darwin)
	  (apply 'open-kitty-mac args)
	(apply 'open-kitty-linux args)))

(defun open-konsole (&rest args)
  (interactive)
  (apply 'call-process "konsole" nil 0 nil "--workdir" default-directory args))

(defvar emma/terminal-macos-function #'open-kitty-mac "Function that opens the terminal on Mac")
(defvar emma/terminal-linux-function #'open-kitty-linux "Function that opens the terminal on Linux")
(defvar emma/terminal--function
  (if (eq system-type 'darwin)
	  emma/terminal-macos-function
	emma/terminal-linux-function)
  "Internal variable representing the terminal function to use in current session")
(defun open-terminal (&rest args)
  (interactive)
  (apply emma/terminal--function args))
(defun open-lazygit ()
  (interactive)
  (let ((args (when (member emma/terminal--function '(open-kitty open-kitty-mac open-kitty-linux))
				'("-o" "initial_window_width=180c" "-o" "initial_window_height=50c"))))
	(apply emma/terminal--function (append args '("lazygit")))))

(keymap-set git-prefix-map  "g" #'open-lazygit)

(provide 'emma-ui-term)
