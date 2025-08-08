;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal, modern Emacs configuration.

;;; Code:

;;; Phase 1: Core Settings & Package Management

;; Performance optimization during startup
(setq gc-cons-threshold (* 50 1000 1000))  ; 50MB

;; Package management with straight.el
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

;; Use straight.el for use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Basic UI improvements

;; Clean up the interface
(setq inhibit-startup-message t)      ; No splash screen
(scroll-bar-mode -1)                  ; Disable visible scrollbar
(tool-bar-mode -1)                    ; Disable the toolbar
(tooltip-mode -1)                     ; Disable tooltips
(set-fringe-mode 10)                  ; Give some breathing room
(menu-bar-mode -1)                    ; Disable the menu bar

;; Font configuration (keeping your existing setup)
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono-12")

;; Line numbers
(column-number-mode)                  ; Show column number in mode line
(global-display-line-numbers-mode t)  ; Show line numbers in all buffers

;; Configure line numbers to have consistent width with leading zeros
(setq display-line-numbers-width-start t)  ; Calculate width based on file size
(setq display-line-numbers-width 4)        ; Minimum width of 4 digits

;; Custom function to format line numbers with leading zeros
(defun my/format-line-numbers ()
  "Configure line numbers to show with leading zeros."
  (when display-line-numbers
    (setq-local display-line-numbers-width
                (max 4 (length (number-to-string
                              (count-lines (point-min) (point-max))))))))

;; Apply formatting to all buffers
(add-hook 'find-file-hook #'my/format-line-numbers)
(add-hook 'after-change-functions
          (lambda (&rest _)
            (when (and display-line-numbers
                       (zerop (% (line-number-at-pos) 100)))
              (my/format-line-numbers)))
          nil t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight current line
(global-hl-line-mode t)

;; Disable bell completely
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Better scrolling
(setq scroll-conservatively 101)      ; Scroll one line at a time
(setq scroll-margin 5)                ; Keep 5 lines above/below cursor
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;; Essential editing features

;; Better selection behavior
(delete-selection-mode t)             ; Typing replaces selected text

;; Auto-save and backup management
(setq auto-save-default t)
(setq make-backup-files t)

;; Put all backups in one directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))

;; Put auto-save files in a separate directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Create directories if they don't exist
(make-directory (expand-file-name "backups" user-emacs-directory) t)
(make-directory (expand-file-name "auto-saves" user-emacs-directory) t)
(make-directory (expand-file-name "undo" user-emacs-directory) t)

;; Recent files
(use-package recentf
  :straight nil  ; Built-in package
  :config
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; Remember cursor position in files
(save-place-mode 1)

;; Refresh buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Smoother rendering
(setq-default truncate-lines t)      ; Don't wrap lines by default
(setq-default indent-tabs-mode nil)  ; Use spaces instead of tabs
(setq-default tab-width 4)           ; Set width of tabs
(setq-default fill-column 90)       ; Set default text wrap width to 90 columns

;; Show matching parentheses
(show-paren-mode 1)

;; Electric pair mode - auto-close brackets
(electric-pair-mode 1)

;; Add more pairs for electric-pair-mode
(setq electric-pair-pairs
      '((?\" . ?\")     ; double quotes
        (?\' . ?\')     ; single quotes
        (?\` . ?\`)     ; backticks
        (?\{ . ?\})     ; curly braces
        (?\[ . ?\])     ; square brackets
        (?\( . ?\))     ; parentheses
        (?\" . ?\")     ; fancy left/right double quotes
        (?\' . ?\')     ; fancy left/right single quotes
        (?\« . ?\»)))   ; guillemets

;; Also add text mode pairs
(setq electric-pair-text-pairs
      '((?\" . ?\")     ; double quotes
        (?\' . ?\')     ; single quotes
        (?\` . ?\`)     ; backticks
        (?\" . ?\")     ; fancy quotes
        (?\' . ?\')))

;; Better help
(setq help-window-select t)          ; Focus help window when opened

;;; Phase 2: Modern Completion System

;; Vertico - Modern minibuffer completion UI
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-count 15)             ; Show more candidates
  (setq vertico-cycle t))             ; Enable cycling for `vertico-next/previous'

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight nil
  :init
  (savehist-mode))

;; Orderless - Flexible completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Rich annotations in the minibuffer
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult - Enhanced search and navigation commands
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
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
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; Corfu - In-buffer completion popup
(use-package corfu
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-no-match nil)      ; Never quit, even if there is no match
  (corfu-preview-current nil)    ; Disable current candidate preview
  (corfu-preselect 'prompt)      ; Preselect the prompt
  (corfu-on-exact-match nil)     ; Configure handling of exact matches
  (corfu-scroll-margin 5)        ; Use scroll margin
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

;; Cape - Additional completion backends
(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point) ; capf
         ("C-c p t" . complete-tag)        ; etags
         ("C-c p d" . cape-dabbrev)        ; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)   ; Use elisp-specific symbol completion
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  ;; Remove any problematic cape functions that might be lingering
  (setq completion-at-point-functions
        (cl-remove-if (lambda (f)
                        (and (symbolp f)
                             (string-match-p "^cape-" (symbol-name f))))
                      completion-at-point-functions))
  ;; Add only the cape functions we want
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  ;; Remove cape-symbol from completion functions if it exists
  (setq completion-at-point-functions
        (delq 'cape-symbol completion-at-point-functions))
  ;; Ensure cape-symbol is never called
  (defalias 'cape-symbol 'ignore))

;; Which-key - Shows available keybindings in a popup
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;;; Phase 3: Programming Support

;; Magit - Git integration
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g" . magit-file-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Built-in Eglot configuration (LSP support)
(use-package eglot
  :straight nil  ; Built-in package
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l f" . eglot-format)
         ("C-c l F" . eglot-format-buffer)
         ("C-c l d" . xref-find-definitions)
         ("C-c l D" . xref-find-references)
         ("C-c l h" . eldoc)
         ("C-c l i" . eglot-find-implementation))
  :config
  ;; Optimize performance
  (setq eglot-autoshutdown t)  ; Shutdown language server after closing last file
  (setq eglot-sync-connect 1)  ; Wait 1 second for server to start
  (setq eglot-connect-timeout 10)  ; 10 second connection timeout

  ;; Disable specific features for performance
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider))  ; Can be slow in large files

  ;; Configure logging
  (setq eglot-events-buffer-size 0))  ; Disable event logging for performance

;; Configure completion for programming modes
(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;; Tree-sitter configuration (built-in for Emacs 29+)
(use-package treesit
  :straight nil  ; Built-in package
  :config
  ;; Configure tree-sitter languages
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Remap major modes to use tree-sitter variants when available
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (rust-mode . rust-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (java-mode . java-ts-mode))))

;; Configure auto-mode-alist for file extensions
;; These will be automatically remapped to tree-sitter variants via major-mode-remap-alist
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

;; Load advanced tree-sitter installation functions
(load (expand-file-name "install-tree-sitter-grammars" user-emacs-directory) t)

;; Additional programming utilities
(use-package electric
  :straight nil  ; Built-in
  :config
  (electric-indent-mode 1))  ; Auto-indent on newline

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

;;; Phase 4: Quality of Life

;; Doom Themes - Beautiful color schemes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one is a nice dark theme)
  (load-theme 'doom-ir-black t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)  ; Disabled - visual bell is distracting

  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; Doom Modeline - A fancy and fast mode-line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-icon t)  ; Enable icons (requires nerd fonts)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-github-timer nil)
  (setq doom-modeline-gnus-timer nil))

;; Rainbow Delimiters - Colorize matching parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Helpful - Better help system
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)
  :init
  ;; Suppress native compiler warning about shortdoc-function-groups
  (declare-function shortdoc-function-groups "shortdoc" nil))

;; All the icons (required by doom-modeline)
;; Note: Run M-x all-the-icons-install-fonts on first install
(use-package all-the-icons
  :if (display-graphic-p))

;; All the icons support for dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Nerd icons (required by doom-modeline for proper icon display)
(use-package nerd-icons
  :if (display-graphic-p))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-enable-wiki-links t)
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-make-gfm-checkboxes-buttons t)
  (setq markdown-gfm-uppercase-checkbox t)
  (setq markdown-fontify-code-blocks-natively t)
  ;; Clean up completion functions for markdown
  :hook ((markdown-mode . (lambda ()
                           (setq-local completion-at-point-functions
                                     (cl-remove 'cape-symbol completion-at-point-functions))))))

;; Org mode - Replace built-in with latest version
(use-package org
  :straight (:type git :repo "https://code.orgmode.org/bzg/org-mode.git"
             :local-repo "org" :depth full
             :pre-build (straight-recipes-org-elpa--build)
             :build (:not autoloads)
             :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")))
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o b" . org-switchb))
  :config
  ;; Configure TODO states
  (setq org-todo-keywords
        '((sequence "READY(r)" "RUNNING(n)" "|" "RETURNED(d)")
          (sequence "BLOCKED(b)" "|" "ERRORED(e)")))

  ;; Configure TODO keyword faces
  (setq org-todo-keyword-faces
        '(("READY" . (:foreground "orange" :weight bold))
          ("RUNNING" . (:foreground "cyan" :weight bold))
          ("BLOCKED" . (:foreground "red" :weight bold))
          ("ERRORED" . (:foreground "red" :weight bold :underline t))
          ("RETURNED" . (:foreground "green" :weight bold))))

  ;; Basic org settings
  (setq org-log-done 'time)  ; Log time when task is done
  (setq org-log-into-drawer t)  ; Log into LOGBOOK drawer
  (setq org-hide-leading-stars t)  ; Hide extra stars in headings
  (setq org-adapt-indentation nil)  ; Don't indent text with heading level
  (setq org-startup-folded 'content)  ; Show only top-level headings initially
  (setq org-ellipsis " ")  ; Just a space, no visual indicator

  ;; Org directory and files
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-agenda-files (list org-directory))

  ;; Create org directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))

  ;; Configure capture templates
  (setq org-capture-templates
        '(("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
           "* READY %?\n  %u\n  %a")
          ("c" "CODE" entry (file+headline org-default-notes-file "Code Review")
           "* CODE %?\n  %u\n  From: [[file:%F::%l][%f:%l]]\n  #+begin_src %m\n%i\n  #+end_src")))

  ;; Enable speed keys at beginning of headings
  (setq org-use-speed-commands t)

  ;; Configure some useful speed keys
  (setq org-speed-commands-user
        '(("s" . org-narrow-to-subtree)
          ("w" . widen)
          ("u" . org-up-element)
          ("j" . org-next-heading)
          ("k" . org-previous-heading)
          ("n" . org-todo)  ; Cycle through TODO states
          ("r" . (lambda () (interactive) (org-todo "READY")))
          ("R" . (lambda () (interactive) (org-todo "RUNNING")))
          ("b" . (lambda () (interactive) (org-todo "BLOCKED")))
          ("e" . (lambda () (interactive) (org-todo "ERRORED")))
          ("d" . (lambda () (interactive) (org-todo "RETURNED")))))

  ;; Better code block editing
  (setq org-src-fontify-natively t)  ; Syntax highlighting in code blocks
  (setq org-src-tab-acts-natively t)  ; Tab works as in major mode
  (setq org-src-window-setup 'current-window)  ; Edit in current window
  (setq org-src-preserve-indentation t)  ; Preserve indentation

  ;; Enable some org modules
  (setq org-modules '(org-tempo))  ; Enable <s TAB for src blocks
  (org-load-modules-maybe t)

  ;; Quick insertion of code blocks
  (with-eval-after-load 'org
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("sh" . "src shell")))

  ;; Auto-sort TODO items by status
  (defun my/org-sort-todos-by-status ()
    "Sort TODO items in the current subtree by their TODO status."
    (interactive)
    (save-excursion
      ;; First ensure we're at a heading
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      ;; Check if we have children to sort
      (let ((has-children (save-excursion
                           (org-goto-first-child))))
        (if has-children
            ;; We have children, sort them
            (org-sort-entries nil ?o)
          ;; No children, try parent
          (when (org-up-heading-safe)
            (org-sort-entries nil ?o))))))

  ;; Auto-sort function that works with narrowed buffers
  (defun my/org-auto-sort-todos ()
    "Automatically sort TODOs when changing TODO state."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (let ((current-pos (point))
              (current-heading (org-get-heading t t t t)))
          ;; Ensure we're at a heading
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          ;; Always go to parent to sort siblings
          (if (org-up-heading-safe)
              ;; We have a parent, sort its children
              (progn
                (message "Sorting children under: %s" (org-get-heading t t t t))
                (org-sort-entries nil ?o))
            ;; No parent, we're at top level
            ;; Go to first heading and sort all top-level entries
            (progn
              (goto-char (point-min))
              ;; Find first heading
              (if (re-search-forward "^\\*+\\s-" nil t)
                  (progn
                    (beginning-of-line)
                    (message "Sorting top-level entries")
                    (org-sort-entries nil ?o))
                (message "No headings found to sort"))))
          ;; Try to return to the original heading
          (goto-char (point-min))
          (when (search-forward current-heading nil t)
            (beginning-of-line))))))

  ;; Hook to auto-sort after TODO state change
  (add-hook 'org-after-todo-state-change-hook #'my/org-auto-sort-todos)

  ;; Optional: Auto-sort when opening org files
  (add-hook 'find-file-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode)
                         buffer-file-name
                         (file-exists-p buffer-file-name))
                (save-excursion
                  (goto-char (point-min))
                  ;; Only sort if there are headings
                  (when (re-search-forward org-heading-regexp nil t)
                    (goto-char (point-min))
                    (org-sort-entries nil ?o)))))))

;; Additional visual enhancements
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-delay 0))

;; Smooth scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

;; Better window management
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Vundo - Visual undo tree
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Better terminal emulation
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Restore gc threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))  ; 2MB

;; Start Emacs server for emacsclient
(use-package server
  :straight nil  ; Built-in package
  :config
  (unless (server-running-p)
    (server-start)))

;; Custom file (don't clutter init.el)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Force markdown-mode association after all other configurations
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(provide 'init)
;;; init.el ends here
