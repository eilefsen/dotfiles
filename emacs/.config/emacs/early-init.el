(setq use-package-compute-statistics t)

(defvar emma-cache-directory "~/.local/state/emacs/")
(setq native-comp-eln-load-path
      (list (concat emma-cache-directory  "eln-cache")))

(setq package-enable-at-startup nil)

;;; optimize startup 
;;
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-percentage 0.6
      gc-cons-threshold (* 50 1000 1000))

;; Decrease garbage collection threshold after startup for better interactive performance
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024))))

;; lsp needs lots of memory
(setq read-process-output-max (* 1024 1024)) ; 1mb

(setq inhibit-startup-message t)

;; no menu bar, toolbar, scroll bar
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  )
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq native-comp-async-report-warnings-errors 'silent)
