(setq use-package-compute-statistics t)

(defvar emma-cache-directory "~/.local/state/emacs/")
(setq native-comp-eln-load-path
      (list (concat emma-cache-directory  "eln-cache")))

(setq package-enable-at-startup nil)

;;; optimize startup 
;;
;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6
      gc-cons-threshold most-positive-fixnum)

;; Decrease garbage collection threshold after startup for better interactive performance
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024))))

(run-with-idle-timer 4 nil
                     (lambda ()
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1) ; original value
                       (garbage-collect)))
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
  (tooltip-mode -1))



(setq native-comp-async-report-warnings-errors 'silent)
