(use-package vue-ts-mode
  :mode "\\.vue\\'"
  :vc (:url "https://github.com/theschmocker/vue-ts-mode" :rev :newest)
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
  ;; (add-hook 'vue-ts-mode-hook #'eglot-ensure)
  (add-hook 'vue-ts-mode-hook #'lsp)
  )

(use-package lsp-vue
  :load-path "lisp/"
  :custom
  (lsp-vue-hybrid-mode t)
  (lsp-vue-take-over-mode nil)
  (lsp-vue-add-on-mode t)
  ;; these below are necessary becaues lsp-volar will register and set settings for lsp-vue somehow
  (lsp-volar-hybrid-mode t)
  (lsp-volar-take-over-mode nil))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode))
  :config
  (add-hook 'typescript-ts-mode-hook #'lsp)
  (defvar emma/ts-font-lock-settings
	(treesit-font-lock-rules
	 :language 'typescript
	 :feature 'nullish
	 :override t
     `([(undefined) (null)] @font-lock-keyword-face)

	 :language 'typescript
	 :feature 'constant
	 :override t
     `([(false) (true)] @font-lock-constant-face)

	 ))

  (add-hook 'typescript-ts-mode-hook
			(lambda ()
			  (setf (nth 1 treesit-font-lock-feature-list) (append (nth 1 treesit-font-lock-feature-list) '(nullish)))
			  (treesit-font-lock-recompute-features '(nullish))
			  (treesit-add-font-lock-rules emma/ts-font-lock-settings :before nil)))
  )
(use-package tsx-ts-mode
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook 'tsx-ts-mode-hook #'lsp)
  )


(use-package lsp-javascript
  :config
  (defun set-lsp-activation-fn! (client fn)
	"Change the activation function of lsp CLIENT."
	(if-let (client (gethash client lsp-clients))
		(setf (lsp--client-activation-fn client)
			  fn)
	  (error "No LSP client named %S" client)))
  (set-lsp-activation-fn!
   'ts-ls
   (lambda (filename &optional _)
	 "Check if the js-ts lsp server should be enabled based on FILENAME."
	 (or (string-match-p "\\.vue\\|\\.[cm]js\\|\\.[jt]sx?\\'" filename)
		 (and (derived-mode-p 'js-mode 'js-ts-mode 'typescript-mode 'typescript-ts-mode 'vue-ts-mode)
			  (not (derived-mode-p 'json-mode))))))
  (defun emma/set-typescript-lsp-faces ()
	(setq-default lsp-semantic-token-modifier-faces
				  (cons '("readonly" . emma/lsp-face-semh-modifier-readonly)
						(assoc-delete-all "readonly" lsp-semantic-token-modifier-faces))))
  (add-hook 'lsp-mode-hook #'emma/set-typescript-lsp-faces)

  
   (setq
	lsp-clients-typescript-prefer-use-project-ts-server nil
	lsp-clients-typescript-plugins
	(vector
	 (list
	  :name "@vue/typescript-plugin"
	  :location "/usr/local/lib/node_modules/"
	  :enableForWorkspaceTypeScriptVersions t
	  :languages (vector "vue")
	  ))))

(use-package flymake-eslint
  :ensure t
  :custom
  (flymake-eslint-executable-name "eslint_d")
  :config
  (add-hook 'vue-ts-mode-hook #'flymake-eslint-enable)
  (add-hook 'typescript-ts-mode-hook #'flymake-eslint-enable)
  (add-hook 'tsx-ts-mode-hook #'flymake-eslint-enable))

;(after! apheleia
;  (add-to-list 'apheleia-formatters '(eslint_d . ("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath)))
;  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . eslint_d))
;  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . eslint_d))
;  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . eslint_d))
;  )

(use-package lsp-tailwindcss
  :ensure t
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode
             vue-ts-mode
             vue-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;; (with-eval-after-load 'eglot
;;   (put 'vue-ts-mode 'eglot-language-id "vue")
;;   (defun vue-eglot-init-options ()
;;     (let ((tsdk-path "/usr/local/lib/node_modules/typescript/lib/"))
;;       `(:typescript (:tsdk ,tsdk-path
;;                      :languageFeatures (:completion
;;                                         (:defaultTagNameCase "both"
;;                                          :defaultAttrNameCase "kebabCase"
;;                                          :getDocumentNameCasesRequest nil
;;                                          :getDocumentSelectionRequest nil)
;;                                         :diagnostics
;;                                         (:getDocumentVersionRequest nil))
;;                      :documentFeatures (:documentFormatting
;;                                         (:defaultPrintWidth 100
;;                                          :getDocumentPrintWidthRequest nil)
;;                                         :documentSymbol t
;;                                         :documentColor t)))))
;;   (add-to-list 'eglot-server-programs
;;                `(vue-ts-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options)))))


(provide 'emma-lang-typescript)
