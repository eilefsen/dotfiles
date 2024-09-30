(use-package evil
  :straight t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;;; Leader
  (evil-set-leader nil (kbd "SPC") nil)

  ;; buffer
  (setq buffer-prefix-map (make-sparse-keymap))
  (evil-define-key 'normal 'global (kbd "<leader>b") `("buffer" . ,buffer-prefix-map))
  (define-key buffer-prefix-map (kbd "b") '("Switch buffer" . switch-to-buffer))
  (evil-define-key 'normal 'global (kbd "<leader>,") '("Switch bufffer" . switch-to-buffer))
  (define-key buffer-prefix-map (kbd "d") '("Kill current buffer" . kill-current-buffer))

  ;; file
  (setq file-prefix-map (make-sparse-keymap))
  (evil-define-key 'normal 'global (kbd "<leader>f") `("file" . ,file-prefix-map))
  (define-key file-prefix-map (kbd "r") '("Recent file" . recentf))
  (define-key file-prefix-map (kbd "f") '("Find file" . find-file))

  ;; Override record macro command to disable 
  (evil-define-command evil-record-macro (register)
    "OVERRIDDEN BY EMMA - Record a keyboard macro into REGISTER. :, /, and ? are not valid, and therefore will not open their respective command windows"
    :keep-visual t
    :suppress-operator t
    (interactive
     (list (unless (and evil-this-macro defining-kbd-macro)
	     (or evil-this-register (evil-read-key)))))
    (let (last-macro)
      (cond
       ((eq register ?\C-g)
	(keyboard-quit))
       ((and evil-this-macro defining-kbd-macro)
	(setq evil-macro-buffer nil
	      last-macro (ignore-errors (evil-end-and-return-macro)))
	(when last-macro
	  (evil-set-register evil-this-macro last-macro))
	(setq evil-this-macro nil))
       ((or (<= ?0 register ?9)
	    (<= ?a register ?z)
	    (<= ?A register ?Z))
	(when defining-kbd-macro (end-kbd-macro))
	(setq evil-this-macro register
	      evil-last-recorded-register register)
	(evil-set-register evil-this-macro nil)
	(kmacro-start-macro nil)
	(setq evil-macro-buffer (current-buffer)))
       (t (error "Invalid register `%s'" register)))))
  )

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(provide 'emma-evil)
