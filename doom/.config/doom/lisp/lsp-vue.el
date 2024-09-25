;;; lsp-vue.el --- A lsp-mode client for Vue3 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 JadeStrong
;;
;; Author: JadeStrong <https://github.com/jadestrong>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: November 08, 2021
;; Modified: November 08, 2024 (Emma Eilefsen Glenna)
;; Version: 0.0.2
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-vue
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;
;;; Commentary:
;;
;; provide the connection to lsp-mode and vue language server
;;
;;; Code:
(require 'lsp-mode)
(require 'json)

(defgroup lsp-vue nil
  "Lsp support for vue3."
  :group 'lsp-mode
  :link '(url-link "https://github.com/vuejs/language-tools")
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-vue-take-over-mode nil
  "Enable Take Over Mode."
  :type 'boolean
  :group 'lsp-vue
  :package-version '(lsp-mode . "9.0.0"))

(defcustom lsp-vue-hybrid-mode t
  "Enable Hybrid Mode."
  :type 'boolean
  :group 'lsp-vue
  :package-version '(lsp-mode . "9.0.1"))

;; Add custom for setting the add-on? attr
(defcustom lsp-vue-add-on-mode t
  "Enable Hybrid Mode."
  :type 'boolean
  :group 'lsp-vue
  :package-version '(lsp-mode . "9.0.1"))

(defcustom lsp-vue-activate-file ".volarrc"
  "A file with a custom name placed in WORKSPACE-ROOT is used to force enable
 vue when there is no package.json in the WORKSPACE-ROOT."
  :type 'string
  :group 'lsp-vue
  :package-version '(lsp-mode . "9.0.0"))

(defconst lsp-vue--is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defun lsp-vue-get-typescript-tsdk-path ()
  "Get tsserver lib*.d.ts directory path."
  (if-let ((package-path (lsp-package-path 'typescript))
           (system-tsdk-path (f-join (file-truename package-path)
                                     (if lsp-vue--is-windows
                                         "../node_modules/typescript/lib"
                                       "../../lib")))
           ((file-exists-p system-tsdk-path)))
      system-tsdk-path
    (prog1 ""
      (lsp--error "[lsp-vue] Typescript is not detected correctly. Please ensure the npm package typescript is installed in your project or system (npm install -g typescript), otherwise open an issue"))))

(lsp-dependency 'typescript
                '(:system "tsserver")
                '(:npm :package "typescript"
                  :path "tsserver"))

(lsp-dependency 'vue-language-server
                '(:system "vue-language-server")
                '(:npm :package "@vue/language-server" :path "vue-language-server"))

(lsp-register-custom-settings
 '(("typescript.tsdk"
    (lambda ()
      (if-let ((project-root (lsp-workspace-root))
               (tsdk-path (f-join project-root "node_modules/typescript/lib"))
               ((file-exists-p tsdk-path)))
          tsdk-path
        (lsp-vue-get-typescript-tsdk-path)))
    t)))

(lsp-register-custom-settings
 '(("vue.hybridMode" lsp-vue-hybrid-mode t)))

(defun lsp-vue--vue-project-p (workspace-root)
  "Check if the `Vue' package is present in the package.json file
in the WORKSPACE-ROOT."
  (if-let ((package-json (f-join workspace-root "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json))
           (dependencies (alist-get 'dependencies config)))
      (alist-get 'vue (append dependencies (alist-get 'devDependencies config)))
    nil))

(defun lsp-vue--activate-p (filename &optional _)
  "Check if the vue-language-server should be enabled base on FILENAME."
  (if lsp-vue-take-over-mode
      (or (or
           (and (lsp-workspace-root) (lsp-vue--vue-project-p (lsp-workspace-root)))
           (and (lsp-workspace-root) lsp-vue-activate-file (f-file-p (f-join (lsp-workspace-root) lsp-vue-activate-file))))
          (or (or (string-match-p "\\.mjs\\|\\.[jt]sx?\\'" filename)
                  (and (derived-mode-p 'js-mode 'typescript-mode 'typescript-ts-mode)
                       (not (derived-mode-p 'json-mode))))
              (string= (file-name-extension filename) "vue")))
    (string= (file-name-extension filename) "vue")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'vue-language-server) "--stdio")))
  :activation-fn 'lsp-vue--activate-p
  :priority 0
  :add-on? lsp-vue-add-on-mode
  :multi-root nil
  :server-id 'vue-ls
  :initialization-options (lambda () (ht-merge (lsp-configuration-section "typescript")
                                               (lsp-configuration-section "vue")
                                               (ht ("serverMode" 0)
                                                   ("diagnosticModel" 1)
                                                   ("textDocumentSync" 2))))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--server-register-capability
                       (lsp-make-registration
                        :id "random-id"
                        :method "workspace/didChangeWatchedFiles"
                        :register-options? (lsp-make-did-change-watched-files-registration-options
                                            :watchers
                                            `[,(lsp-make-file-system-watcher :glob-pattern "**/*.js")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.ts")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.vue")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.jsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.tsx")
                                              ,(lsp-make-file-system-watcher :glob-pattern "**/*.json")])))))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'vue-language-server
                                            callback error-callback))))

(provide 'lsp-vue)
;;; lsp-vue.el ends here
