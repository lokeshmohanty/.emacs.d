;; get latest version
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install package using straight if not installled
(setq straight-use-package-by-default 't)
;; integrate with use-package
(straight-use-package 'use-package)
;; to change git protocol
;; (straight-vc-git-default-protocol 'ssh)

;; display position on modeline
(column-number-mode)
;; wrap lines
(global-visual-line-mode t)
;; update buffers on file change
(global-auto-revert-mode)
;; enable relative line numbering
(setq display-line-numbers-type 'relative)
;; enable line numbers globally
(global-display-line-numbers-mode 1)
;; persist history over emacs restarts
(savehist-mode)

(use-package doom-themes
  :config 
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-org-config))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after all-the-icons
  :config (all-the-icons-completion-mode))

;; run the below command to install fonts
;; (all-the-icons-install-fonts)

(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; required by evil-collection
  :custom
  (evil-shift-width 2)
  (evil-want-find-undo t) ;; insert mode undo steps as per emacs
  (evil-undo-system 'undo-redo) ;; use native commands in emacs 28
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-mc
  :config (global-evil-mc-mode 1))

(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))
;; (use-package evil-owl
;;   :config
;;   (setq evil-owl-display-method 'posframe
;;         evil-owl-extra-posframe-args '(:width 50 :height 20)
;;         evil-owl-max-string-length 50)
;;   (evil-owl-mode))

(use-package evil-lion
  :config (evil-lion-mode))

(use-package expand-region)

(use-package embrace
  :commands embrace-commander)

(use-package helpful
  :commands (helpful-callable	; for functions and macros
            helpful-function	; for functions only
            helpful-macro
            helpful-command		; for interactive functions
            helpful-key
            helpful-variable
            helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap Info-goto-emacs-command-node] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap display-local-help] . helpful-at-point))

(use-package which-key
  :config (which-key-mode))

(use-package org
  :custom
  (org-startup-folded t)
  (org-startup-indented t)
  (org-confim-babel-evaluate nil)
  (org-pretty-entities t))		; "C-c C-x \" to toggle

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; latex fragments preview, toggle with "C-c C-x C-l"
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)	; sub/super scripts
  (org-appear-autokeywords t)	; keywords in org-hidden-keywords
  ;; (org-appear-delay t)
)

(org-babel-do-load-languages
  'org-babel-load-languages
      '((C          . t)
        (python     . t)
        (shell      . t)
        (latex      . t)
        (js         . t)
        (sql        . t)
        (haskell    . t)))

(use-package evil-org
  :after org
  ;; :hook (org-mode . (lambda () evil-org-mode))
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/Documents/Org-Roam"))
  (org-roam-db-autosync-mode))

(use-package tex
  :straight auctex
  :bind (:map TeX-mode-map ("<f2>" . preview-document))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "xdg-open")))
  :config
  (setq-default TeX-master nil))

(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (org-mode . org-cdlatex-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package evil-markdown
  :straight '(evil-markdown
               :host github
               :repo "Somelauw/evil-markdown")
  :after markdown-mode
  :hook (markdown-mode . evil-markdown-mode))

(use-package cmake-mode)

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (require 'dap-python))

(use-package pyvenv)

(use-package haskell-mode)
(use-package markdown-mode)

;; (use-package smartparens
;;   :config
;;   (smartparens-global-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-l")
  :config (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (cmake-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package dap-mode)

(use-package company
  :custom (company-minimum-prefix-length 1)
  :config (global-company-mode))

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package orderless
  :config (setq orderless-component-separator "[ &]") ; to search with multiple components in company
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode)
  ;; :config (add-hook 'marginalia-mode-hook
  ;;                   #'all-the-icons-completion-marginalia-setup)
  )

(use-package embark
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package magit)

(use-package vterm
  :custom (vterm-shell "fish"))

(use-package pdf-tools)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(evil-define-key 'normal 'global
  (kbd "<leader>e") 'embrace-commander)
(evil-define-key 'visual 'global
  (kbd "<leader>e") 'embrace-commander)

;; org-roam keybindings
(evil-define-key 'normal 'global
  (kbd "<leader>nf") 'org-roam-node-find)
(evil-define-key 'normal 'global
  (kbd "<leader>nd") 'org-roam-dailies-goto-today)
(evil-define-key 'normal 'global
  (kbd "<leader>nt") 'org-roam-dailies-goto-tomorrow)

;; embark keybindings
(evil-define-key 'normal 'global
  (kbd "C-.") 'embark-act)
(evil-define-key 'visual 'global
  (kbd "C-.") 'embark-act)
(evil-define-key 'insert 'global
  (kbd "C-.") 'embark-act)
(evil-define-key 'normal 'global
  (kbd "C-;") 'embark-dwim)
(evil-define-key 'visual 'global
  (kbd "C-;") 'embark-dwim)
(evil-define-key 'insert 'global
  (kbd "C-;") 'embark-dwim)

;; marginalia keybindings
(evil-define-key 'normal 'minibuffer-local-map
  (kbd "M-A") 'marginalia-cycle)
(evil-define-key 'insert 'minibuffer-local-map
  (kbd "M-A") 'marginalia-cycle)

;; vertico keybindings
(evil-define-key 'insert 'vertico-map
  (kbd "C-k") 'vertico-previous)
(evil-define-key 'insert 'vertico-map
  (kbd "C-j") 'vertico-next)
