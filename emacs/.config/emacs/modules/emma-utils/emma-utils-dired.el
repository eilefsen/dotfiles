(use-package dired
  :commands dired-jump
  :init
  (setq
   ;; do what i mean :)
   dired-dwim-target t
   ;; don't prompt to revert, just do it
   dired-auto-revert-buffer #'dired-buffer-stale-p
   ;; Always copy/delete recursively
   dired-recursive-copies  'always
   dired-recursive-deletes 'top
   ;; Ask whether destination dirs should get created when copying/removing files.
   dired-create-destination-dirs 'ask
   ;; Screens are larger nowadays, we can afford slightly larger thumbnails
   image-dired-thumb-size 150
   ;; human readable file size
   dired-listing-switches "-alh"
   ;; avoid having a billion trillion dired buffers polluting the buffer list
   dired-kill-when-opening-new-dired-buffer t)
  :config
  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (when (or (featurep :system 'bsd) (featurep :system 'darwin)
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
	  (setq insert-directory-program gls)
	;; BSD ls doesn't support -v or --group-directories-first
	(setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))))
  )

(provide 'emma-utils-dired)
