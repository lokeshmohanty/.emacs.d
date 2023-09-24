(context-menu-mode)											              ; show context menu on right click
(column-number-mode)                                  ; display position on modeline
;; (global-visual-line-mode t)                           ; wrap lines
(global-auto-revert-mode)
(setq blink-cursor-mode nil)
(add-hook 'prog-mode-hook 'hs-minor-mode)             ; enable folding
(add-hook 'TeX-mode-hook 'hs-minor-mode)              ; enable folding for latex mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; enable line numbers for all programming modes
(add-hook 'TeX-mode-hook  'display-line-numbers-mode) ; enable line numbers for latex mode
(add-hook 'org-mode-hook  'display-line-numbers-mode) ; enable line numbers for org mode

(setq-default tab-width 2
              ;; display-line-numbers-type 'relative
              use-short-answers t                     ; Replace yes/no prompts with y/n
              confirm-nonexistent-file-or-buffer nil) ; Ok to visit non existent files

(setq visible-bell '1)                                ; use visible bell instead of beep

(add-hook 'after-init-hook 'recentf-load-list)
(recentf-mode 1)                                      ; Allow storing of recent files list
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 200)

(undelete-frame-mode)										; allows recovering a deleted frame (emacs 29)

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

;; (require 'package)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "https://melpa.org/packages/") t)

;; (package-refresh-contents)
;; M-x package-install RET use-package RET

(use-package hydra)
(use-package general
  :init
  (setq general-override-states '(insert emacs hybrid normal
                                  visual motion operator replace))
  :config
  (general-create-definer my/leader
    ;; :keymaps '(normal insert visual emacs override)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer my/ctrl-c
    :prefix "C-c"))

(use-package which-key
  :config (which-key-mode))

(my/leader :states 'normal :keymaps 'override
  "p"    (general-simulate-key "C-x p" :which-key "project")
  "."    '(find-file :which-key "find file")
  "s"    '(:ignore t                    :wk "shortcuts")
  "s0"   '(0x0-dwim                     :wk "0x0 share")
  "sa"   '(org-agenda                   :wk "org-agenda")
  "sc"   '(org-capture                  :wk "org-capture")
  "sd"   '(dirvish-dwim                 :wk "dirvish dwim")
  "se"   '(eshell                       :wk "eshell")
  "sg"   '(general-describe-keybindings :wk "general keybindings")
  "sm"   '(notmuch                      :wk "mail")
  "so"   '(org-present                  :wk "org present")
  "sr"   '(consult-recent-file          :wk "recent files")
  "ss"   '(dirvish-side                 :wk "dirvish side")
  "sp"   '(multi-vterm-project          :wk "vterm")
  "st"   '(multi-vterm-dedicated-toggle :wk "vterm"))

(general-def :states 'normal
  "j"   'evil-next-visual-line
  "k"   'evil-previous-visual-line)

(my/leader :states 'visual :keymaps 'override
  "s"    '(:ignore t          :wk "shortcuts")
  "s0"   '(0x0-dwim           :wk "0x0 share"))

(general-def :states 'normal :keymaps 'Info-mode-map
  "?" 'hydra-info/body)

(my/leader :states 'normal
  "h"   '(:ignore t :wk "help/hydra")
  "he"  '(hydra-expand/body :wk "expand")
  "ht"  '(hydra-tab-bar/body :wk "tab-bar")
  "hm"  '(hydra-mu4e-headers/body :wk "mu4e")
  "hi"  '(hydra-info/body :wk "info")
  "hp"  '(hydra-pdftools/body :wk "pdftooks")
  "hc"  '(hydra-org-clock/body :wk "org-clock")
  "hs"  '(hydra-smartparens/body :wk "smartparens")
  "hw"  '(hydra-window/body :wk "window")
  "hr"  '((lambda () (interactive) (load-file (expand-file-name "init.el" user-emacs-directory))) :wk "Reload emacs config")
  "hc"  '((lambda () (interactive) (find-file (expand-file-name "README.org" user-emacs-directory))) :wk "Goto emacs config"))

(defhydra hydra-info (:color blue
                      :hint nil)
      "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
      ("]"   Info-forward-node)
      ("["   Info-backward-node)
      ("n"   Info-next)
      ("p"   Info-prev)
      ("s"   Info-search)
      ("S"   Info-search-case-sensitively)

      ("l"   Info-history-back)
      ("r"   Info-history-forward)
      ("H"   Info-history)
      ("t"   Info-top-node)
      ("<"   Info-top-node)
      (">"   Info-final-node)

      ("u"   Info-up)
      ("^"   Info-up)
      ("m"   Info-menu)
      ("g"   Info-goto-node)
      ("b"   beginning-of-buffer)
      ("e"   end-of-buffer)

      ("f"   Info-follow-reference)
      ("i"   Info-index)
      (","   Info-index-next)
      ("I"   Info-virtual-index)

      ("T"   Info-toc)
      ("d"   Info-directory)
      ("c"   Info-copy-current-node-name)
      ("C"   clone-buffer)
      ("a"   info-apropos)

      ("1"   Info-nth-menu-item)
      ("2"   Info-nth-menu-item)
      ("3"   Info-nth-menu-item)
      ("4"   Info-nth-menu-item)
      ("5"   Info-nth-menu-item)
      ("6"   Info-nth-menu-item)
      ("7"   Info-nth-menu-item)
      ("8"   Info-nth-menu-item)
      ("9"   Info-nth-menu-item)

      ("?"   Info-summary "Info summary")
      ("h"   Info-help "Info help")
      ("q"   Info-exit "Info exit")
      ("C-g" nil "cancel" :color blue))

(my/leader :states 'normal :keymaps 'override
  "b"    '(:ignore t        :wk "buffer")
  "bi"   '(ibuffer          :wk "ibuffer")
  "bf"   '(consult-buffer-other-frame    :wk "in other window")
  "bg"   '(revert-buffer    :wk "revert")
  "bw"   '(consult-buffer-other-window    :wk "in other window")
  "bs"   '(consult-buffer   :wk "switch")
  "bk"   '(kill-current-buffer :wk "kill"))

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(use-package ibuffer :straight (:type built-in))
(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

(defhydra hydra-window (:color blue :hint nil)
  "
                                                               ╭─────────┐
   Move to               Size            Split           Do    │ Windows │
╭──────────────────────────────────────────────────────────────┴─────────╯
      ^_k_^           ^_K_^       ╭─┬─┐^ ^        ╭─┬─┐^ ^         ↺ [_u_] undo layout
      ^^↑^^           ^^↑^^       │ │ │_v_ertical ├─┼─┤_b_alance   ↻ [_r_] restore layout
  _h_ ←   → _l_   _H_ ←   → _L_   ╰─┴─╯^ ^        ╰─┴─╯^ ^         ✗ [_d_] close window
      ^^↓^^           ^^↓^^       ╭───┐^ ^        ╭───┐^ ^         ⇋ [_w_] cycle window
      ^_j_^           ^_J_^       ├───┤_s_tack    │   │_z_oom
      ^^ ^^           ^^ ^^       ╰───╯^ ^        ╰───╯^ ^       
--------------------------------------------------------------------------------
          "
  ("<ESC>" nil "quit")
  ("b" balance-windows)
  ("d" delete-window)
  ("H" shrink-window-horizontally :color red)
  ("h" windmove-left :color red)
  ("J" shrink-window :color red)
  ("j" windmove-down :color red)
  ("K" enlarge-window :color red)
  ("k" windmove-up :color red)
  ("L" enlarge-window-horizontally :color red)
  ("l" windmove-right :color red)
  ("r" winner-redo :color red)
  ("s" split-window-vertically :color red)
  ("u" winner-undo :color red)
  ("v" split-window-horizontally :color red)
  ("w" other-window)
  ("z" delete-other-windows))

(my/leader :states 'normal :keymaps 'override
  "t"    '(:ignore t :wk "tab")
  "tb"   '(switch-to-buffer-other-tab :wk "buffer")
  "tc"   '(tab-close                  :wk "close")
  "tf"   '(find-file-other-tab        :wk "file")
  "tr"   '(tab-rename                 :wk "close"))

(defhydra hydra-tab-bar (:color amaranth)
  "Tab Bar Operations"
  ("n" tab-new "Create a new tab" :column "Creation")
  ("d" dired-other-tab "Open Dired in another tab")
  ("f" find-file-other-tab "Find file in another tab")
  ("0" tab-close "Close current tab")
  ("m" tab-move "Move current tab" :column "Management")
  ("r" tab-rename "Rename Tab")
  ("<return>" tab-bar-select-tab-by-name "Select tab by name" :column "Navigation")
  ("l" tab-next "Next Tab")
  ("h" tab-previous "Previous Tab")
  ("q" nil "Exit" :exit t))

(my/leader :states 'normal :keymaps 'override
  "f"    '(:ignore t                  :wk "frame")
  "fc"   '(clone-frame                :wk "clone")
  "fc"   '(other-frame                :wk "other")
  "fd"   '(delete-frame               :wk "delete")
  "fu"   '(undelete-frame             :wk "undelete")
  "fb"   '(consult-buffer-other-frame :wk "buffer")
  "ff"   '(find-file-other-frame      :wk "file"))

(my/leader :states 'normal :keymaps 'override
  "z"   '(:ignore t                       :wk "toggle")
  "zl"  '(custom/toggle-line-numbers-type :wk "relative line number")
  "zw"  '(custom/toggle-tab-width         :wk "tab width")
  "zi"  '(custom/toggle-indent-mode       :wk "tab indent")
  "zo"  '(org-toggle-inline-images        :wk "toggle inline images")
  "zt"  '(toggle-truncate-lines           :wk "toggle truncate lines"))

(defun custom/toggle-line-numbers-type ()
    "Toggle line numbers type between relative and absolute"
    (interactive)
    (setq display-line-numbers-type (if (eq display-line-numbers-type t) 'relative 't))
    (display-line-numbers-mode)
    (display-line-numbers-mode))
(defun custom/toggle-tab-width ()
    "Toggle setting tab widths between 2, 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 8) 2 (if (= tab-width 4) 8 4)))
    (redraw-display))
(defun custom/toggle-indent-mode ()
    "toggle indenting modes"
    (interactive)
    (setq indent-tabs-mode (if (eq indent-tabs-mode t) nil t))
    (message "Indenting using %s." (if (eq indent-tabs-mode t) "tabs" "spaces")))
;; Change opacity from input with empty as 100
(defun custom/change-opacity (opacity)
    "Change the opacity of the frame"
    (interactive "nOpacity: ")
    (set-frame-parameter (selected-frame) 'alpha
                         (list (if (equal opacity 0)
                               100
                               (/ opacity 100.0)))))

(set-face-attribute 'default nil :family "Iosevka Fixed SS07" :height 135)
(set-face-attribute 'font-lock-comment-face nil
                    :family "Iosevka Fixed SS07"
                    :height 135
                    :slant 'italic)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after all-the-icons
  :config (all-the-icons-completion-mode))

;; required as during daemon initialization, there are no frames
;; (use-package modus-themes
;; 	:config
;; 	(load-theme 'modus-vivendi-tinted t))
;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))
(use-package doom-themes
  :config
    (load-theme 'doom-palenight t))

;; very minimal modeline
;; (use-package mood-line
;;   :config
;;   (mood-line-mode))

;; run (nerd-icons-install-fonts) to install fonts
(use-package doom-modeline
  :init
	;; If the actual char height is larger, it respects the actual height.
	(setq doom-modeline-height 25)
	(setq doom-modeline-bar-width 4)
	(setq doom-modeline-window-width-limit 85)

	;; Whether display icons in the mode-line.
	;; While using the server mode in GUI, should set the value explicitly.
	(setq doom-modeline-icon t)

	(setq doom-modeline-unicode-fallback t)
	(setq doom-modeline-minor-modes t)

	;; If non-nil, a word count will be added to the selection-info modeline segment.
	(setq doom-modeline-enable-word-count nil)

	;; Major modes in which to display word count continuously.
	;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
	;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
	;; remove the modes from `doom-modeline-continuous-word-count-modes'.
	(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

	;; Whether display the indentation information.
	(setq doom-modeline-indent-info nil)

	;; Whether display the total line number。
	(setq doom-modeline-total-line-number nil)

	;; When non-nil, always show the register name when recording an evil macro.
	(setq doom-modeline-always-show-macro-register t)

	;; By default, almost all segments are displayed only in the active window. To
	;; display such segments in all windows, specify e.g.
	(setq doom-modeline-always-visible-segments '(mu4e irc))
	(doom-modeline-mode 1))

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; required by evil-collection
  :custom
  (evil-shift-width 2)
  (evil-want-find-undo t) ;; insert mode undo steps as per emacs
  (evil-undo-system 'undo-redo) ;; use native commands in emacs 28
  (evil-symbol-word-search t)		; */# search the symbol under the cursor instead of the word
  :config
  (evil-mode 1)
  ;; replace <C-z> with <C-x C-z> to use <C-z> to suspend frame instead
  (define-key evil-motion-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-motion-state-map (kbd "C-x C-z") 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-emacs-state-map (kbd "C-x C-z") 'evil-exit-emacs-state)
  ;; make <C-z> emulate vim in insert/replace mode 
  (define-key evil-insert-state-map (kbd "C-z") (kbd "C-q C-z"))
  (define-key evil-insert-state-map (kbd "C-x C-z") 'evil-emacs-state)
  (define-key evil-replace-state-map (kbd "C-z") (kbd "C-q C-z"))
  )

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-mc
	:demand t
  :config (global-evil-mc-mode 1))

(use-package posframe)
(use-package evil-owl
  :after posframe
  :config
  ;; (setq evil-owl-max-string-length 500)
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*evil-owl*"
  ;;                (display-buffer-in-side-window)
  ;;                (side . bottom)
  ;;                (window-height . 0.3)))
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode))

(use-package evil-lion
  :config (evil-lion-mode))

(use-package avy
  :general (:states 'normal
                    "K" 'avy-goto-char-timer))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package embrace
  :commands embrace-commander
  :general (:states 'normal
                    ;; "ys"   '(embrace-add    :wk "add surrounding")
                    ;; "cs"   '(embrace-change :wk "change surrounding")
                    ;; "ds"   '(embrace-delete :wk "delete surrounding")
                    "s" 'embrace-commander))

(use-package expand-region)

(defhydra hydra-expand ()
  "Zoom/Expand Region"
  ("n" er/expand-region    "expand-region")
  ("p" er/contract-region  "contract-region")
  ("h" text-scale-increase "zoom in ")
  ("l" text-scale-decrease "zoom out"))

(use-package org
	:custom
	(org-startup-folded 'content)
	(org-startup-indented t)
	(org-confim-babel-evaluate nil)
	(org-hide-emphasis-markers t)
	(org-hidden-keywords nil)			; enabling it couases fontification error and problem with org-appear
	;; (org-pretty-entities t)		; "C-c C-x \" to toggle
	(org-image-actual-width nil)
	:config
	;; open pdfs with okular
	;; (setq org-preview-latex-default-process 'dvisvgm)
	(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
	;; (setf (alist-get "\\.pdf\\'" org-file-apps nil nil #'equal) "okular %s")
	;; (setf (alist-get "\\.pdf::\\([0-9]+\\)?\\'" org-file-apps nil nil #'equal) "okular %s -p %1")
	(org-add-link-type "xdg-open" (lambda (path) (browse-url-xdg-open path)))
	(setq org-export-backends '(ascii html icalendar latex md odt)))

;; from https://stackoverflow.com/a/47850858/6479297 to littering due to org export
;; issue: doesn't respect "#+export_file_name" property
(defun my/org-export-to-customized-location (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir (setq pub-dir ".output")
					(unless (file-directory-p pub-dir)
						(make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'my/org-export-to-customized-location)

(setq org-directory "~/Documents/Org")

(setq org-agenda-files '("calendar.org" "tasks.org"))

(setq org-todo-keywords 
      '((sequence "TODO(t@/!)" "ACTIVE(a!)" "BACKLOG(b!)" "HOLD(h@/!)" "ATTEND(A!)" "|" "DONE(D!)" "CANCELED(C!)" "MISSED(M!)")))

(setq org-capture-templates 
      `(("t" "Tasks")
        ("tt" "General" entry 
         (file+olp "tasks.org" "Inbox")
         "* TODO %? %^G\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i" 
         :empty-lines 1)
        ("ts" "Scheduled" entry 
         (file+olp "tasks.org" "Inbox")
         "* TODO %? %^G\nSCHEDULED: %^t\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i" 
         :empty-lines 1)
        ("td" "With a deadline" entry 
         (file+olp "tasks.org" "Inbox")
         "* TODO %? %^G\nDEADLINE: %^t\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i" 
         :empty-lines 1)
        ("tl" "Links to visit" entry 
         (file+olp "tasks.org" "Links")
         "* TODO [[%c][%^{Link Title}]] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n  %i" 
         :empty-lines 1)

        ("p" "Project Task")
        ("pt" "General" entry 
         (file+olp "tasks.org" "Projects")
         "* TODO %? %^G\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i" 
         :empty-lines 1)
        ("ps" "Scheduled" entry 
         (file+olp "tasks.org" "Projects")
         "* TODO %? %^G\nSCHEDULED: %^t\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i" 
         :empty-lines 1)
        ("pd" "With a deadline" entry 
         (file+olp "tasks.org" "Projects")
         "* TODO %? %^G\nDEADLINE: %^t\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i" 
         :empty-lines 1)

        ("n" "Notes")
        ("nn" "General" entry 
         (file "notes.org")
         "* %? %^G\n:PROPERTIES:\n:Created: %U\n:LOCATION: %a\n:END:\n  %i")
        ("np" "Project" entry 
         (file+olp "notes.org" "Project")
         "* %? :@work\n:PROPERTIES:\n:CATEGORIES: %^{Categories}\n:Created: %U\n:LOCATION: %a\n:END:\n  %i")
        ("nv" "Vocabulary" entry 
         (file+olp+datetree "notes.org" "Vocabulary")
         "\n* %<%I:%M %p>\n\n%?\n"
         :clock-in :clock-resume :empty-lines 1)

        ;; ("j" "Journal Entries")
        ("j" "Journal" entry
         (file+olp+datetree "journal.org")
         "\n* %<%I:%M %p> - %? :journal:\n"
         :clock-in :clock-resume :empty-lines 1)

        ("h" "Habit Entries")
        ("hd" "Daily Habit" entry
         (file+olp "tasks.org" "Repeat Tasks")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:STYLE:    habit\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("hw" "Weekly Habit" entry
         (file+olp "tasks.org" "Repeat Tasks")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1w>>\n:PROPERTIES:\n:STYLE:    habit\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("hm" "Monthly Habit" entry
         (file+olp "tasks.org" "Repeat Tasks")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1m>>\n:PROPERTIES:\n:STYLE:    habit\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("hy" "Yearly Habit" entry
         (file+olp "tasks.org" "Repeat Tasks")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1y>>\n:PROPERTIES:\n:STYLE:    habit\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("hr" "Repeat Tasks" entry 
         (file+olp "tasks.org" "Repeat Tasks")
         "* REPEAT %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:Created: %U\n:STYLE: habit\n:REPEAT_TO_STATE: REPEAT\n:LOGGING: DONE(!)\n:ARCHIVE: %%s_archive::* Habits\n:END:\n")

        ))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)	; sub/super scripts
  (org-appear-autokeywords t)	; kkywords in org-hidden-keywords
  (org-appear-delay 0.3))

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
  (setq org-roam-directory (file-truename "~/Documents/Org/Roam"))
  (org-roam-db-autosync-mode))

(my/ctrl-c
  "l"   '(org-store-link                 :wk "org store link")
  "n"   '(:ignore t                      :wk "org roam")
  "nt"  '(org-roam-buffer-toggle         :wk "toggle backlinks")
  "nf"  '(org-roam-node-find             :wk "find node")
  "nd"  '(:ignore t                      :wk "dailies")
  "nd1" '(org-roam-dailies-goto-today    :wk "today")
  "nd2" '(org-roam-dailies-goto-tomorrow :wk "tomorrow")
  "ng"  '(org-roam-graph                 :wk "node graph"))

(my/ctrl-c :keymaps 'org-mode-map
  "ni" '(org-roam-node-insert      :wk "insert")
  "nI" '(org-roam-insert-immediate :wk "insert immediate"))

(defhydra hydra-org-clock (:color blue :hint nil)
   "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    _i_n         _e_dit   _g_oto entry | (_z_)     _r_elative      ti_m_e
 ^ ^     _c_ontinue   _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
 ^ ^     _o_ut        ^ ^      _r_eport     |  ^ ^      _p_ause toggle
 ^ ^     ^ ^          ^ ^      ^ ^          |  ^ ^      _s_top
"
   ("i" org-clock-in)
   ("c" org-clock-in-last)
   ("o" org-clock-out)
 
   ("e" org-clock-modify-effort-estimate)
   ("q" org-clock-cancel)

   ("g" org-clock-goto)
   ("d" org-clock-display)
   ("r" org-clock-report)
   ("?" (org-info "Clocking commands"))

  ("r" org-timer-start)
  ("n" org-timer-set-timer)
  ("p" org-timer-pause-or-continue)
  ("s" org-timer-stop)

  ("m" org-timer)
  ("t" org-timer-item)
  ("z" (org-info "Timers")))

(setq treesit-extra-load-path '("/usr/local/lib/tree-sitter"))

(setq major-mode-remap-alist
 '((yaml-mode       . yaml-ts-mode)
   (bash-mode       . bash-ts-mode)
   (js2-mode        . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode       . json-ts-mode)
   (css-mode        . css-ts-mode)
   (cmake-mode      . cmake-ts-mode)
   (python-mode     . python-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))

(use-package tex
  :straight auctex
  :general
  (:states '(normal insert visual emacs) :keymaps 'TeX-mode-map
           "C-c C-g" '(pdf-sync-forward-search)
           "<f2>" 'preview-document)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (preview-auto-cache-preamble t)
  ;; (TeX-view-program-selection '((output-pdf "xdg-open")))
  (TeX-source-correlate-method (quote synctex))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-output-dir "output")
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  ;; (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer)
  (setq-default TeX-master nil))

(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  ;; (LaTeX-mode . cdlatex-mode)
  (org-mode . org-cdlatex-mode)
  :bind (:map cdlatex-mode-map ("<tab>" . cdlatex-tab))
  :config
  (dolist (cmd '(("vc" "Insert \\vect{}" "\\vect{?}"
                  cdlatex-position-cursor nil nil t)
                 ("equ*" "Insert equation* env"
                  "\\begin{equation*}\n?\n\\end{equation*}"
                  cdlatex-position-cursor nil t nil)
                 ("sn*" "Insert section* env"
                  "\\section*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("ss*" "Insert subsection* env"
                  "\\subsection*{?}"
                  cdlatex-position-cursor nil t nil)
                 ("sss*" "Insert subsubsection* env"
                  "\\subsubsection*{?}"
                  cdlatex-position-cursor nil t nil)))
    (push cmd cdlatex-command-alist))

  (setq cdlatex-math-symbol-alist '((?F ("\\Phi"))
                                    (?o ("\\omega" "\\mho" "\\mathcal{O}"))
                                    (?6 ("\\partial"))
                                    (?v ("\\vee" "\\forall"))
                                    (?^ ("\\uparrow" "\\Updownarrow" "\\updownarrow"))))
  (setq cdlatex-math-modify-alist '((?B "\\mathbb" "\\textbf" t nil nil)
                                    ;; (?t "\\text" nil t nil nil)
                                    ))
  (setq cdlatex-paired-parens "$[{(")
  (cdlatex-reset-mode))

(use-package reftex
  :after latex
  :defer 2
  :commands turn-on-reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :config
  (setq reftex-insert-label-flags '("sf" "sfte"))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-use-multiple-selection-buffers t))

;; (use-package consult-reftex
;;   :straight (:type git :host github :repo "karthink/consult-reftex")
;;   :after (reftex consult embark)
;;   :bind (:map reftex-mode-map
;;          ("C-c )"   . consult-reftex-insert-reference)
;;          ("C-c M-." . consult-reftex-goto-label))
;;   :config (setq consult-reftex-preview-function
;;                 #'consult-reftex-make-window-preview))

(defun my/tikzit-make-figure ()
  "Prompt for file name, insert tikzit boilerplate, and start the tikzit process."
  (interactive)
  (let* ((name (read-string "Enter filename: "))
         (filename (concat "figures/" name ".tikz")))
    (make-directory "figures" t)
    (insert (concat "\\ctikzfig{" name "}"))
    (make-process :name "tikzit"
                  :command (list "tikzit" filename))))

(defun my/tikzit-edit-figure ()
  "Get the file name from the word under the cursor, and start the tikzit process."
  (interactive)
  (let* ((name (thing-at-point 'symbol))
         (filename (concat "figures/" name ".tikz")))
    (make-directory "figures" t)
    (make-process :name "tikzit"
                  :command (list "tikzit" filename))))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package evil-markdown
  :straight '(evil-markdown
               :host github
               :repo "Somelauw/evil-markdown")
  :after markdown-mode
  :hook (markdown-mode . evil-markdown-mode))

;; (use-package cmake-mode) ; facing git errors
(use-package cuda-mode)

(use-package conda
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.conda"))
  (setq conda-env-home-directory (expand-file-name "~/.conda/envs"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  ;; :config
  ;; (require 'dap-python)
  ;; these hooks can't go in the :hook section since lsp-restart-workspace
  ;; is not available if lsp isn't active
  ;; (add-hook 'conda-postactivate-hook (lambda () (lsp-restart-workspace)))
  ;; (add-hook 'conda-postdeactivate-hook (lambda () (lsp-restart-workspace)))
)

;; (use-package pyvenv)

(use-package haskell-mode)

(use-package citre
  :defer t
  :init (require 'citre-config)
  :general
  (:states 'normal :keymaps '(citre-mode-map override)
           "gd"  'citre-jump
           "gD"  'citre-jump-back
           "gp"  'citre-peek
           "gP"  'citre-ace-peek
           "gc"  '(:ignore t :which-key "citre")
           "gcj"  'citre-peek-next-line
           "gck"  'citre-peek-prev-line
           "gcc"  'citre-create-tags-file
           "gcu"  'citre-update-this-tags-file
           "gcU"  'citre-update-tags-file)
  :config (setq citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package eglot
  :hook ((LaTeX-mode . eglot-ensure)
         (c-mode     . eglot-ensure)
         (c++-mode   . eglot-ensure)))

;; (add-to-list 'eglot-server-programs '((c++-mode c++-ts-mode c-mode c-ts-mode) "clangd"))

(use-package consult-eglot
  :commands consult-eglot-symbols)

(my/leader :states 'normal :keymaps 'eglot-mode-map
  "l"    '(:ignore t :wk "language server")
  "lfn"  '(flymake-goto-next-error :wk "buffer")
  "lfp"  '(flymake-goto-prev-error :wk "close")
  "lr"   '(eglot-rename            :wk "close"))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init (setq lsp-keymap-prefix "C-l")
;;   :config (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
;;   :hook
;;   ;; (c-mode . lsp-deferred)
;;   ;; (c++-mode . lsp-deferred)
;;   ;; (cmake-mode . lsp-deferred)
;;   (lsp-mode . lsp-enable-which-key-integration))

;; (use-package lsp-bridge)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :config (require 'dap-cpptools))

;; (use-package gdb-mi
;;   :straight (:host github :repo "weirdNox/emacs-gdb" :files ("*.el" "*.c" "*.h" "Makefile"))
;;   :init
;;   (fmakunbound 'gdb)
;;   (fmakunbound 'gdb-enable-debug))

(use-package company
  :custom (company-minimum-prefix-length 1)
  :config (global-company-mode)
  :custom (company-idle-delay 0.5))

;; company front-end with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :general
  (:states 'insert :keymaps 'copilot-mode-map
           "M-h"  'copilot-complete
           "M-n"  'copilot-next-completion
           "M-p"  'copilot-previous-completion
           "M-l"  'copilot-accept-completion-by-word
           "M-j"  'copilot-accept-completion-by-line
           "M-<return>"  'copilot-accept-completion))

(add-hook 'prog-mode-hook 'copilot-mode)

(use-package vertico
  :straight (:files (:defaults "extensions/*")) ; load the extensions as well
  :init (vertico-mode)
  :custom (vertico-cycle t)
  :config (vertico-mouse-mode)					; enable mouse extension
  ;; vertico-directory extension: delete parent directory on backspace
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; got bored after some time
;; (use-package vertico-posframe
;;   :after posframe
;;   :config (vertico-posframe-mode))

; it needs to be set after no-littering to prevent issues
(use-package savehist
  :after no-littering
  :init (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :config (setq orderless-component-separator "[ &]") ; to search with multiple components in company
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :general (:states '(normal insert) :keymaps 'minibuffer-local-map
                    "M-a"   '(marginalia-cycle :wk "marginalia-cycle"))
  :init (marginalia-mode)
  ;; :config (add-hook 'marginalia-mode-hook
  ;;                   #'all-the-icons-completion-marginalia-setup)
  )

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
)

(use-package embark
  :general
  (:states '(normal visual insert) :keymaps 'override
           "C-,"   '(embark-act  :wk "embark-act")
           "C-;"   '(embark-dwim :wk "embark-dwim"))
  :init
  (setq prefix-help-command #'embark-prefix-help-command) ; supposed to replace which-key in the future
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package yasnippet
	:hook (prog-mode . yas-minor-mode)
	:config
	(setq yas-snippet-dirs
				(append yas-snippet-dirs
								(list (expand-file-name "snippets" user-emacs-directory))))
	(yas-reload-all))

(use-package yasnippet-snippets)

(my/ctrl-c
	"y" '(yas-insert-snippet :wk "insert snippet"))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :general
  (:states 'normal :keymaps 'dired-mode-map
    "SPC" 'nil
    "l"   'dired-find-file
    "h"   'dired-up-directory)
  (:states 'normal :keymaps 'dirvish-mode-map
    "g?"  'dirvish-dispatch
    "a"   'dirvish-quick-access
    "f"   'dirvish-file-info-menu
    "o"   'dirvish-quicksort
    "q"   'dirvish-quit
    "z"   'dirvish-layout-toggle
    "v"   'dirvish-vc-menu
    "y"   'dirvish-yank-menu
    "N"   'dirvish-narrow
    "H"   'dirvish-history-last
    "L"   'dirvish-history-jump
    "TAB" 'dirvish-subtree-toggle
    "F" 'dirvish-history-go-forward
    "B" 'dirvish-history-go-backward
    "M-l" 'dirvish-ls-switches-menu
    "M" 'dirvish-mark-menu
    "S" 'dirvish-setup-menu
    "E" 'dirvish-emerge-menu
    "J" 'dirvish-fd-jump)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("c" "~/Documents/Courses/Aug23/"  "Courses")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("p" "~/Documents/Projects/"       "Projects")
     ("s" "~/.local/src"                "Sources")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (dirvish-peek-mode) ; Preview files listed in minibuffer
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group"))

(setq dired-auto-revert-buffer t)
(setq dired-mouse-drag-files t)                   ; added in Emacs 29
(setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29


(setq mouse-1-click-follows-link nil)
(define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
(define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
(define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)

(use-package burly)

(my/leader :states 'normal :keymaps 'override
  "r"    '(:ignore t              :wk "register/bookmark")
  "ri"   '(:ignore t              :wk "insert")
  "rib"  '(bookmark-set           :wk "buffer")
  "rif"  '(burly-bookmark-frames  :wk "frames")
  "riw"  '(burly-bookmark-windows :wk "windows")
  "rl"   '(consult-bookmark       :wk "list")
  "rs"   '(bookmark-save          :wk "save"))

(use-package no-littering)

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

(use-package magit)

(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively #'magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))))

(use-package smerge-mode
  :config
  (defhydra hydra-smerge (:color pink :hint nil :post (smerge-auto-leave))
    "
  ^Move^       ^Keep^               ^Diff^                 ^Other^
  ^^-----------^^-------------------^^---------------------^^-------
  _n_ext       _b_ase               _<_: upper/base        _C_ombine
  _p_rev       _u_pper              _=_: upper/lower       _r_esolve
  ^^           _l_ower              _>_: base/lower        _k_ill current
  ^^           _a_ll                _R_efine
  ^^           _RET_: current       _E_diff
  "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (hydra-smerge/body)))))

(use-package sudo-edit)

(use-package vterm
  :custom (vterm-shell "fish"))

(use-package multi-vterm
  :general
  (:states 'normal :keymaps 'vterm-mode-map
           ",c"    'multi-vterm
           ",n"    'multi-vterm-next
           ",p"    'multi-vterm-prev
           ",d"    'multi-vterm-dedicated-toggle
           ",q"    'kill-this-buffer)
  :config (setq multi-vterm-dedicated-window-height-percent 30))

(setq message-kill-buffer-on-exit t)
(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
;;      message-sendmail-f-is-evil t
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
(setq mail-user-agent 'gnus-user-agent)

;; (with-eval-after-load 'mu4e
;;   (defun my/make-mu4e-context (address &rest args)
;;     (let* ((name (if (plist-member args :name) (plist-get args :name) "Lokesh Mohanty"))
;;            (context (if (plist-member args :context) (plist-get args :context) address))
;;            (type (if (plist-member args :type) (plist-get args :type) 'other))
;;            (dir (concat "/" address))
;;            (signature (if (plist-member args :signature) (plist-get args :signature) (concat "Thanks & Regards\n" name)))
;;            (prefix (concat dir (pcase type ('gmail "/[Gmail]") (_ "")))))
;;       (make-mu4e-context
;;        ;; first letter of context is used to switch contexts
;;        :name context
;;        ;; :match-func `(lambda (msg) (when msg (string-match-p ,(concat "^" dir) (mu4e-message-field msg :maildir))))
;;        ;; :match-func (lambda (msg) (when msg (string-prefix-p dir (mu4e-message-field msg :maildir))))
;;        :enter-func (lambda () (mu4e-message (concat "Entering context: " "hi")))
;;        :leave-func (lambda () (mu4e-message (concat "Leaving context: " "hi")))
;;        :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to address)))
;;        :vars
;;        `((user-mail-address    . ,address)
;;          (user-full-name       . ,name)
;;          (mu4e-sent-folder     . ,(concat prefix (pcase type ('gmail "/Sent Mail") ('outlook "/Sent Items") (_ "/Sent"))))
;;          (mu4e-trash-folder    . ,(concat prefix (pcase type ('outlook "/Deleted Items") (_ "/Trash"))))
;;          (mu4e-drafts-folder   . ,(concat prefix "/Drafts"))
;;          (mu4e-refile-folder   . ,(concat prefix "/Archive"))
;;          (mu4e-compose-signature . ,signature)))))

;;   (setq mu4e-contexts `(,(my/make-mu4e-context "lokesh1197@yahoo.com" :context "home")
;;                         ,(my/make-mu4e-context "lokesh1197@gmail.com" :context "personal" :type 'gmail)
;;                         ,(my/make-mu4e-context "lokeshm@iisc.ac.in"   :context "work"     :type 'outlook))))

(use-package notmuch)

(use-package gnus-alias
	:config
	(setq gnus-alias-identity-alist
				'(("home"
					 nil ;; parent identity
					 "Lokesh Mohanty <lokesh1197@gmail.com>" ;; from
					 nil ;; organization
					 nil ;; extra headers
					 nil ;; body
					 "Thanks & Regards\nLokesh Mohanty\n\n") ;; signature
					("personal" nil
					 "Lokesh Mohanty <lokesh1197@yahoo.com>" nil
					 (("Bcc" . "lokesh1197@gmail.com")) nil
					 "Thanks & Regards\nLokesh Mohanty\n\n")
					("work" nil
					 "Lokesh Mohanty <lokeshm@iisc.ac.in>"
					 nil nil nil
					 "Thanks & Regards\nLokesh Mohanty\n\n")))
	(setq gnus-alias-default-identity "home")
  (setq gnus-alias-identity-rules
				'(("work" ("any" "lokeshm@iisc\\.ac\\.in" both) "work")
					("personal" ("any" "lokesh1197@yahoo\\.com" both) "personal")))
	;; Determine identity when message-mode loads
	(add-hook 'message-setup-hook 'gnus-alias-determine-identity))

(setq message-directory "Drafts") ; stores postponed messages to the specified directory
(setq notmuch-fcc-dirs "Sent") ; sent mail directory
;; (setq notmuch-hello-hide-tags (quote ("killed"))) ; settings for main screen

(setq message-kill-buffer-on-exit t) ; kill buffer after sending mail)
(setq mail-specify-envelope-from t) ; Settings to work with msmtp

;; Crypto Settings
(setq notmuch-crypto-process-mime t) ; Automatically check signatures
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
(setq epg-gpg-program "/usr/bin/gpg2")

(use-package org-msg
  :after org
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi%s,\n\n"
        org-msg-recipient-names '(("lokeshm@iisc.ac.in" . "Lokesh Mohanty"))
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        org-msg-convert-citation t
        org-msg-signature (concat
                            "#+begin_signature\n"
                            "Regards,\n"
                            "*Lokesh Mohanty*\n"
                            "#+end_signature"))
  (org-msg-mode))

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :custom (use-shr-fonts nil))

(use-package elfeed-org
  :config (elfeed-org)
  :custom (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory))))

(use-package elfeed-goodies
  :config (elfeed-goodies/setup))

(use-package elfeed-tube
  :after elfeed
  :demand t
  :config
  (setq elfeed-tube-auto-save-p t) ; default value: nil
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(use-package elfeed-dashboard
  :config
  (setq elfeed-dashboard-file "elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

;; auth-sources
(setq auth-source-debug t)
(setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))
;; (setq auth-sources '((:source "~/.authinfo.gpg")))
(setq password-cache-expiry nil)
(customize-set-variable 'ange-ftp-netrc-filename "~/.authinfo.gpg")

;; access unix password store
(use-package password-store)

(use-package ledger-mode
  :ensure-system-package ledger)

(use-package evil-ledger
  :after ledger-mode
  :config
  (setq evil-ledger-sort-key "S")
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))

(use-package org-present)
(use-package visual-fill-column
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(defun my/org-present-prepare-slide (buffer-name heading)
  (org-overview) ; show only top-level headlines
  (org-show-entry) ; unfold the current entry
  (org-show-children)) ; show only direct subheadings of the slide but don't expand them

(defun my/org-present-start ()
  (setq header-line-format " ") ; set a blank header line string to create blank space at the top
  (org-display-inline-images)
  (display-line-numbers-mode 0)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(defun my/org-present-end ()
  (setq header-line-format nil) ; clear the header line string so that it isn't displayed
  (org-remove-inline-images)
  (display-line-numbers-mode 1)
  (visual-line-mode 0)
  (visual-fill-column-mode 0))

(add-hook 'org-present-mode-hook #'my/org-present-start)
(add-hook 'org-present-mode-quit-hook #'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(use-package pdf-tools
  :hook (pdf-view-mode . (lambda () (cua-mode 0))) ; turn off cua mode to make copy work
  ;; :hook ((pdf-view-mode . (lambda () (cua-mode 0))) ; turn off cua mode to make copy work
  ;;        (pdf-view-mode . (setq mode-line-format nil))) ; hide mode-line
  :demand t
  :general
  (:states 'normal :keymaps 'pdf-view-mode-map
           "C-s" 'isearch-forward)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1))               ; finer zooming

(defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y"  pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-dark-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red))

(use-package emms
  ;; :init (add-hook 'emms-player-started-hook 'emms-show)
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/"))

(use-package 0x0
  :commands (0x0-shorten-uri 0x0-dwim 0x0-upload-kill-ring 0x0-popup))

(my/ctrl-c
  "0"  '(:ignore t :wk "0x0")
  "0d"  '(0x0-dwim :wk "dwim") ; upload file in dired buffer, upload text in buffer
  "0p"  '(0x0-popup :wk "popup")
  "0s"  '(0x0-shorten-uri :wk "shorten")
  "0c"  '(0x0-upload-kill-ring :wk "clipboard"))

(use-package emacs-everywhere)
