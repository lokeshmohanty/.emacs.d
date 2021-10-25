;; Use straight.el instead of package.el
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(use-package org
  :straight org-contrib)

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;; Required if using package.el
;; (setq custom-file (concat user-emacs-directory "custom.el"))
;; (when (file-exists-p custom-file)
;;   (load custom-file))
