;;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; set backup file directory, so backups are not dumped in current dir
(let ((backup-files-directory
       (file-name-concat emma-cache-directory "backup/")))
  (make-directory backup-files-directory :parents)

  (setq backup-directory-alist
	`(("." . ,backup-files-directory))))
;; same as above, but with autosave
(let ((save-files-directory
       (file-name-concat emma-cache-directory
                         "autosave/")))
  (make-directory save-files-directory :parents)
  (setq auto-save-file-name-transforms
	`(("\\(?:[^/]*/\\)*\\(.*\\)" ,save-files-directory t))))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      create-lockfiles nil              ; disable annoying lockfiles (starting with ".#"
      )

(add-to-list 'desktop-locals-to-save 'buffer-undo-list)

(setq enable-recursive-minibuffers t)
(recentf-mode 1)
(global-auto-revert-mode t)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;; elide GNU propaganda
(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)
(setq inhibit-startup-screen t)

;; dont wrap lines, scroll with trackpad
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
(setq-default truncate-lines t)

;; lsp needs lots of memory
(setq read-process-output-max (* 1024 1024)) ; 1mb


(use-package emma-evil :load-path "modules")
(use-package emma-ui :load-path "modules/emma-ui")
(use-package emma-utils :load-path "modules/emma-utils")
(use-package emma-lang :load-path "modules/emma-lang")

(use-package config :load-path "modules")
