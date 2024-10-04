(use-package vue-ts-mode
  :vc (:url https://github.com/8uff3r/vue-ts-mode)
  :config
  ;; (add-hook 'vue-ts-mode-hook #'eglot-ensure)
  (add-hook 'vue-ts-mode-hook #'lsp)
  )

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


(provide 'emma-lang-vue)
