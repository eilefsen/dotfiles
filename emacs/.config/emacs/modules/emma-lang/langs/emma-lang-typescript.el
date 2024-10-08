(defun emma/apply-typescript-font-lock-rules ()
  (setf (nth 1 treesit-font-lock-feature-list) (append (nth 1 treesit-font-lock-feature-list) '(nullish)))
  (treesit-font-lock-recompute-features '(nullish))
  (treesit-add-font-lock-rules (treesit-font-lock-rules
								:language 'typescript
								:feature 'nullish
								:override t
								`([(undefined) (null)] @font-lock-keyword-face)

								:language 'typescript
								:feature 'constant
								:override t
								`([(false) (true)] @font-lock-constant-face)

								) :before nil))

(use-package vue-ts-mode
  :mode "\\.vue\\'"
  :vc (:url "https://github.com/theschmocker/vue-ts-mode" :rev :newest)
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
  ;; (add-hook 'vue-ts-mode-hook #'eglot-ensure)
  (add-hook 'vue-ts-mode-hook #'lsp)
  (add-hook 'vue-ts-mode-hook #'emma/apply-typescript-font-lock-rules)
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
  (add-hook 'typescript-ts-mode-hook #'emma/apply-typescript-font-lock-rules)
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
  (defun emma/set-lsp-modifier-faces (facename face)
	(setq-default lsp-semantic-token-modifier-faces
				  (cons `(,facename . ,face)
						(assoc-delete-all facename lsp-semantic-token-modifier-faces))))
  (defun emma/set-typescript-lsp-faces ()
	(emma/set-lsp-modifier-faces "readonly" 'emma/lsp-face-semh-modifier-readonly)
	(emma/set-lsp-modifier-faces "declaration" 'emma/lsp-face-semh-modifier-declaration))
  (add-hook 'lsp-mode-hook #'emma/set-typescript-lsp-faces)

  
   (setq
	lsp-clients-typescript-prefer-use-project-ts-server t
	lsp-clients-typescript-plugins
	(vector
	 (list
	  :name "@vue/typescript-plugin"
	  :location ""
	  :enableForWorkspaceTypeScriptVersions t
	  :languages (vector "vue")
	  ))))

(use-package flymake-eslint
  :defer t
  :ensure t
  :custom
  (flymake-eslint-executable-name "eslint_d")
  :config
  (add-hook 'vue-ts-mode-hook #'flymake-eslint-enable)
  (add-hook 'typescript-ts-mode-hook #'flymake-eslint-enable)
  (add-hook 'tsx-ts-mode-hook #'flymake-eslint-enable))

(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-formatters '(eslint_d . ("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath)))
  (add-to-list 'apheleia-mode-alist '(vue-ts-mode . eslint_d))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . eslint_d))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . eslint_d))
  (apheleia-global-mode +1)
 )

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

;;; compilation-mode support

;; tsc supports formatting errors in two general ways: plain and
;; pretty. ("Plain" is our term for "not pretty".) In tsc versions
;; prior to 2.7, the plain and pretty formats both used the same
;; format for references into files. `typescript-tsc-error-regexp`
;; covers both plain and pretty for those versions.
;;
;; Version 2.7 changed the pretty format so as to format source code
;; references differently. This required the introduction of
;; `typescript-tsc-pretty-error-regexp`. The format of plain error
;; messages did not change. So from that version onwards,
;; `typescript-tsc-error-regexp` covers plain error messages and
;; `typescript-tsc-pretty-error-regexp` covers pretty error messages.

;; handle plain compiler-errors like the following when doing M-x compile<ret>tsc<ret>
;;
;; greeter.ts(24,9): error TS2362: The left-hand side of an arithmetic operation must be of type 'any', 'number' or an enum type.
;; greeter.ts(30,12): error TS2339: Property 'indexOf' does not exist on type 'number'.
(defconst typescript-tsc-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\),\\([0-9]+\\)):[[:blank:]]+"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;; handle pretty compiler-errors like the following when doing M-x compile<ret>tsc<ret>
;; test.ts:2:7 - error TS2322: Type '2' is not assignable to type 'string'.
(defconst typescript-tsc-pretty-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\):\\([0-9]+\\):\\([0-9]+\\) - [[:blank:]]*"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;;
;; Should handle output like:
;; src/modules/authenticator.ts[1, 83]: ' should be "
;; (quotemarks) src/modules/authenticator.ts[2, 26]: ' should be "
;; ERROR: (quotemarks) src/modules/authenticator.ts[2, 26]: ' should be "
;; WARNING: src/modules/authenticator.ts[2, 26]: ' should be "
;;
;; "(quotemarks)" it the rule name. It is produced when using the
;; "verbose" formatter. The "verbose" formatter is identical to the
;; default ("prose") formatter, except for the additional rule name.
;;
;; "ERROR:" and "WARNING:" are the severity. This was added in tslint
;; 5.0. Prior versions have no notion of severity and simply omit this
;; part.
;;
(defconst typescript-tslint-report-regexp
  (concat
   "^[[:blank:]]*"
   ;; severity ("type" in Emacs' parlance)
   "\\(?:\\(?:ERROR\\|\\(WARNING\\)\\):[[:blank:]]+\\)?"
   ;; rule name
   "\\((.*)[[:blank:]]+\\)?"
   ;; filename
   "\\([^(\r\n)]+\\)"
   "\\["
   ;; line
   "\\([[:digit:]]+\\)"
   ", "
   ;; column
   "\\([[:digit:]]+\\)"
   "\\]: "
   ;; message
   ".*$")
  "Regexp to match reports generated by tslint.")

(defconst typescript-nglint-error-regexp
  (concat
   ;; severity ("type" in Emacs' parlance)
   "ERROR:[[:blank:]]+"

   ;; filename
   "\\([^(\r\n)]+\\)"
   ":"
   ;; line
   "\\([[:digit:]]+\\)"
   ":"
   ;; column
   "\\([[:digit:]]+\\)"

   " - "
   ;; message
   ".*$"))

(defconst typescript-nglint-warning-regexp
  (concat
   ;; severity ("type" in Emacs' parlance)
   "WARNING:[[:blank:]]+"

   ;; filename
   "\\([^(\r\n)]+\\)"
   ":"
   ;; line
   "\\([[:digit:]]+\\)"
   ":"
   ;; column
   "\\([[:digit:]]+\\)"

   " - "
   ;; message
   ".*$"))

(dolist
    (regexp
     `((typescript-tsc
        ,typescript-tsc-error-regexp
        1 2 3 2)

       (typescript-tsc-pretty
        ,typescript-tsc-pretty-error-regexp
        1 2 3 2)

       (typescript-tslint
        ,typescript-tslint-report-regexp
        3 4 5 (1))

       (typescript-nglint-error
        ,typescript-nglint-error-regexp
        1 2 3 2)

       (typescript-nglint-warning
        ,typescript-nglint-warning-regexp
        1 2 3 1)))
  (add-to-list 'compilation-error-regexp-alist-alist regexp)
  (add-to-list 'compilation-error-regexp-alist (car regexp)))


(defun emma/set-tsc-as-compiler ()
  (setq-local compile-command "tsc --noEmit --incremental --noErrorTruncation false "))
(defun emma/set-vue-tsc-as-compiler ()
  (setq-local compile-command "npx vue-tsc --noEmit --incremental --noErrorTruncation false "))

(add-hook 'typescript-ts-mode-hook #'emma/set-tsc-as-compiler)
(add-hook 'tsx-ts-mode-hook #'emma/set-tsc-as-compiler)
(add-hook 'vue-ts-mode-hook #'emma/set-vue-tsc-as-compiler)

(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
	(ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))



(provide 'emma-lang-typescript)
