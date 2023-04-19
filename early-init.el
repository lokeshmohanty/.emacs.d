;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

(when (featurep 'native-compile)
  ;; silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil)
  ;; asynchronous native compilation
  (setq native-comp-deferred-compilation t)
  ;; Set the right directory to store the native compilation cache
  ;; NOTE: the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" user-emacs-directory)))))
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Don't enable package.el at startup
  (setq package-enable-at-startup nil)

  ;; Make the initial buffer load faster by setting its mode to fundamental-mode
  (customize-set-variable 'initial-major-mode 'fundamental-mode)

  (setq-default
   inhibit-startup-screen t            ; Disable start-up screen
   inhibit-startup-message t           ; Disable startup message
   inhibit-startup-echo-area-message t ; Disable initial echo message
   initial-scratch-message nil         ; Empty the initial *scratch* buffer
   initial-buffer-choice nil)          ; Open *scratch* buffer at init, make it 't' for using nano-splash

  ;; Set some default frame parameters
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))   ; disable the toolbar
  (add-to-list 'default-frame-alist '(tooltip-lines . 0))    ; disable the toolbar
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))   ; disalbe the menu bar
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)) ; disable scroll bars
  (add-to-list 'default-frame-alist '(alpha 90 . 90))				 ; transparency

  ;; Example of setting a frame parameter manually
  ;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))

(setq
  gc-cons-threshold most-positive-fixnum                    ; Inhibit garbage collection during startup
  byte-compile-warnings '(cl-functions)                     ; hide cl package deprecation warning
  auto-mode-case-fold nil                                   ; Use case-sensitive `auto-mode-alist' for performance
  fast-but-imprecise-scrolling t                            ; More performant rapid scrolling over unfontified regions
  ffap-machine-p-known 'reject                              ; Don't ping things that look like domain names
  frame-inhibit-implied-resize t                            ; Inhibit frame resizing for performance
  idle-update-delay 1.0                                     ; slow down UI updates down
  inhibit-compacting-font-caches t                          ; Inhibit frame resizing for performance
  read-process-output-max (* 1024 1024)                     ; Increase how much is read from processes in a single chunk.
  redisplay-skip-fontification-on-input t                   ; Inhibits it for better scrolling performance.
  command-line-x-option-alist nil                           ; Remove irreleant command line options for faster startup
  select-active-regions 'only                               ; Emacs hangs when large selections contain mixed line endings.
  auto-save-list-file-prefix nil                            ; Disable auto-save
  create-lockfiles nil                                      ; Disable lockfiles
  make-backup-files nil                                     ; Disable backup files
  custom-file (concat user-emacs-directory "custom.el")     ; Place all "custom" code in a temporary file
  vc-follow-symlinks t)                                      ; Do not ask about symlink following
