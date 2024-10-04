(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t
		evil-want-C-d-scroll t
		evil-want-C-w-delete nil
		evil-want-Y-yank-to-eol t)
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; if the current state is obvious from the cursor's color/shape, then
        ;; we won't need superfluous indicators to do it instead.
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        ;; It's infuriating that innocuous "beginning of line" or "end of line"
        ;; errors will abort macros, so suppress them:
        evil-kbd-macro-suppress-motion-error t)
  :config
  (evil-mode 1)

  (setq evil-undo-system 'undo-redo)


  ;; Dont update the clipboard on selection. Improves performance and compatiblity
  (setq evil-visual-update-x-selection-p nil)

  ;;; esc quits
  (defun minibuffer-keyboard-quit ()
	"Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
	(interactive)
	(if (and delete-selection-mode transient-mark-mode mark-active)
		(setq deactivate-mark  t)
	  (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
	  (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (evil-define-key 'motion 'global (kbd "K") #'eldoc-doc-buffer)

  ;;; Leader
  (evil-set-leader 'normal (kbd "SPC") nil)

  ;; buffer
  (setq buffer-prefix-map (make-sparse-keymap))
  (evil-define-key 'normal 'global (kbd "<leader>u") `("universal" . ,universal-argument-map))
  (evil-define-key 'normal 'global (kbd "<leader>b") `("buffer" . ,buffer-prefix-map))
  (evil-define-key 'normal 'global (kbd "<leader>,") '("Switch bufffer" . switch-to-buffer))
  (define-key buffer-prefix-map (kbd "b") '("Switch buffer" . switch-to-buffer))
  (define-key buffer-prefix-map (kbd "p") '("Switch project buffer" . consult-project-buffer))
  (define-key buffer-prefix-map (kbd "d") '("Kill current buffer" . kill-current-buffer))

  ;; file
  (setq file-prefix-map (make-sparse-keymap))
  (evil-define-key 'normal 'global (kbd "<leader>f") `("file" . ,file-prefix-map))
  (evil-define-key 'normal 'global (kbd "<leader>.") '("Find file" . find-file))
  (define-key file-prefix-map (kbd "r") '("Recent file" . recentf))
  (define-key file-prefix-map (kbd "f") '("Find file" . find-file))

  ;; terminal
  (setq terminal-prefix-map (make-sparse-keymap))
  (evil-define-key 'normal 'global (kbd "<leader>t") `("terminal" . ,terminal-prefix-map))

  ;; help
  (evil-define-key 'normal 'global (kbd "<leader>h") `("help" . ,help-map))

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
  :ensure t
  :custom
  (evil-collection-magit-use-z-for-folds t)
  :config
  (setq evil-collection-mode-list (delq 'vterm evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :ensure t
  :init
  (evil-commentary-mode))

(provide 'emma-evil)
