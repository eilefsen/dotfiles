(use-package evil
  :straight t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; Override record macro command to disable 
  ;;; Leader
  (define-prefix-command 'my-leader-map)

  (keymap-set evil-motion-state-map "SPC" 'my-leader-map)
  (keymap-set evil-normal-state-map "SPC" 'my-leader-map)

  (evil-define-key nil my-leader-map
    ;; add your bindings here:
    "b"  'switch-to-buffer
    "B"  'project-switch-to-buffer
    "pf" 'project-find-file
    "ps" 'project-shell-command
    ;; etc.
    )

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
