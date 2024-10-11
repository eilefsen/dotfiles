;; Enable vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  :config
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete)

  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args))))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :defer t
  :config (marginalia-mode))

(use-package consult
  :ensure t
  :defer t
  :config
  (setq consult-buffer-sources
		'(consult--source-modified-buffer
		  consult--source-buffer
		  consult--source-hidden-buffer)
		consult-project-buffer-sources
		'(consult--source-project-buffer
		  consult--source-project-buffer-hidden))
  (add-to-list 'consult-buffer-filter "^\\*")
  (add-to-list 'consult-buffer-filter "^\\magit:")


  (define-key minibuffer-local-map (kbd "C-SPC") #'my-embark-preview)
  (defun my-embark-preview ()
	"Previews candidate in vertico buffer, unless it's a consult command"
	(interactive)
	(unless (bound-and-true-p consult--preview-function)
      (save-selected-window
		(let ((embark-quit-after-action nil))
          (embark-dwim)))))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))


  :init
  (define-key global-map [remap bookmark-jump]                 #'consult-bookmark)
  (define-key global-map [remap evil-show-marks]               #'consult-mark)
  (define-key global-map [remap evil-show-jumps]               #'+vertico/jump-list)
  (define-key global-map [remap evil-show-registers]           #'consult-register)
  (define-key global-map [remap goto-line]                     #'consult-goto-line)
  (define-key global-map [remap imenu]                         #'consult-imenu)
  (define-key global-map [remap Info-search]                   #'consult-info)
  (define-key global-map [remap locate]                        #'consult-locate)
  (define-key global-map [remap load-theme]                    #'consult-theme)
  (define-key global-map [remap man]                           #'consult-man)
  (define-key global-map [remap recentf-open-files]            #'consult-recent-file)
  (define-key global-map [remap switch-to-buffer]              #'consult-buffer)
  (define-key global-map [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (define-key global-map [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame)
  (define-key global-map [remap yank-pop]                      #'consult-yank-pop)
  (define-key global-map [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  )

(use-package consult-xref
  :after consult
  :config
  (defun consult-xref-without-jump (fetcher &optional alist)
	"Show xrefs with preview in the minibuffer. Do not jump if only one candidate.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments."
	(let* ((consult-xref--fetcher fetcher)
           (candidates (consult-xref--candidates))
           (display (alist-get 'display-action alist))
           (this-command #'consult-xref))
      (unless candidates
		(user-error "No xref locations"))
      (xref-pop-to-location
       (consult--read
		candidates
		:prompt "Go to xref: "
		:history 'consult-xref--history
		:require-match t
		:sort nil
		:category 'consult-xref
		:group #'consult--prefix-group
		:state
		;; do not preview other frame
		(when-let (fun (pcase-exhaustive display
						 ('frame nil)
						 ('window #'switch-to-buffer-other-window)
						 ('nil #'switch-to-buffer)))
          (consult-xref--preview fun))
		:lookup (apply-partially #'consult--lookup-prop 'consult-xref))
       display)))

  (consult-customize
   consult-xref consult-xref-without-jump
   :preview-key '(:debounce 0.4 any))
  (setq xref-show-definitions-function 'consult-xref-without-jump)
  (setq xref-show-xrefs-function 'consult-xref-without-jump))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :defer 2
  :init
  (savehist-mode))

(use-package corfu
  ;; Optional customizations
  :ensure t
  :defer t
  :custom
  (corfu-quit-at-boundary t)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.24)
  (corfu-auto-prefix 2)
  (corfu-preselect 'prompt)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-on-exact-match nil)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. Global mode will not play nice with vertico
  :hook ((prog-mode . corfu-mode)
		 (shell-mode . corfu-mode)
		 (eshell-mode . corfu-mode)))


(use-package marginalia
  :defer t
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
				  (replace-regexp-in-string
				   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
				   crm-separator)
				  (car args))
		  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t
		completion-ignore-case t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(provide 'emma-utils-completion)
