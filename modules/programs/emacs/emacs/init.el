;; -*- lexical-binding: t; -*-

;; Performance optimizations during startup
(defvar neshtea/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Init straight.el
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defun neshtea/report-startup-time ()
  (message
   "Emacs startup took %s with %d garbage collections"
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(defun neshtea/restore-post-init-settings ()
  "Reset GC and file handlers after startup for better runtime performance."
  (setq file-name-handler-alist neshtea/file-name-handler-alist)
  (setq gc-cons-threshold (* 2 1000 1000))
  (setq gc-cons-percentage 0.1)
  (neshtea/report-startup-time))

(add-hook 'emacs-startup-hook #'neshtea/restore-post-init-settings)

;; UI stuff - disable early to prevent flashing
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(menu-bar-mode -1)
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)

;; Where to write stuff.
(setq custom-file (expand-file-name "~/.config/emacs/custom.el"))
(setq backup-directory-alist `(("." . "~/.saves")))
(savehist-mode)
(setq savehist-file (expand-file-name "history" user-emacs-directory))

;; Remap some mac-specific keys.
(setq ns-alternate-modifier 'none)
(setq ns-command-modifier 'meta)
(setq ns-function-modifier 'super)

(setq load-prefer-newer t)
(setq max-lisp-eval-depth 5000)
;; isearch
(setq isearch-allow-scroll t)
(setq isearch-lazy-count t)
(save-place-mode 1)
;; MacOS (disable menubar/scrollbar/toolbar).
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq-default cursor-type 'hbar)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(load custom-file 'no-error)

;; Answer y or n to yes-or-no questions.
;; http://pragmaticemacs.com/emacs/make-all-prompts-y-or-n/
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

(setq neshtea/font-alist    ; TODO copy the latest version from kenranunderscore
      '((jetbrains-mono . (:family "JetBrains Mono"
                                   :height 120))
	(iosevka-fixed . (:family "Iosevka Fixed"))
	(sf-mono . (:family
		    "SF Mono"
		    :width))
        (comic-mono . (:family "Comic Mono"))
        (victor-mono . (:family
                        "Victor Mono"))))

(setq neshtea/current-font 'jetbrains-mono)

(defun neshtea/switch-font (font)
  "Select one of the fonts configured in 'neshtea/font-alist' as
the face-font."
  (interactive
   (list (intern (completing-read "Font: " (mapcar #'car (copy-alist neshtea/font-alist))))))
  ;; If the selected font is not the currently active font, switch.
  (let* ((attrs (alist-get font neshtea/font-alist))
	 (family (plist-get attrs :family))
	 (height (plist-get attrs :height))
	 (width (plist-get attrs :width)))
    (setq neshtea/current-font family)
    (set-face-attribute 'default nil
			:family family
			:height (or height
				    120)
			:width (or width
				   'normal))))

;; Set the font to the default.
(neshtea/switch-font neshtea/current-font)

(global-set-key (kbd "C-. s f") #'neshtea/switch-font)
(global-set-key (kbd "C-. s t") #'neshtea/switch-theme)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Some packages where I specifically want to built-in version.
(use-package eldoc :straight (:type built-in))
(use-package project :straight (:type built-in))
(use-package flymake :straight (:type built-in))
(use-package xref :straight (:type built-in))

;; Especially on MacOS, the exec path is always wrong.  This package
;; tries to fix that.
;; SEE https://github.com/purcell/exec-path-from-shell#usage
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-shell-name "~/.nix-profile/bin/fish")
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package which-key
  :defer 1
  :custom
  (which-key-idle-delay 0.3)
  :config
  (which-key-mode))

;; Taken from Johannes init.el
;; https://github.com/kenranunderscore/dotfiles/blob/main/modules/programs/emacs/emacs.d/init.el#L80
(defun neshtea/switch-theme (name)
  "Switch themes interactively.  Similar to `load-theme' but also
disables all other enabled themes."
  (interactive
   (list (intern (completing-read "Theme: "
				  (mapcar #'symbol-name
					  (seq-difference (custom-available-themes)
							  custom-enabled-themes))))))
  (progn
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme name t)))

(defun neshtea/random-theme (&optional themes)
  "Select a random theme out of all available themes and load
it. Optionally, you can supply a list of themes to select from."
  (interactive)
  (let* ((themes (or themes (custom-available-themes)))
	 (but-active-themes (seq-difference themes custom-enabled-themes))
	 (next-theme (nth (random (length but-active-themes)) but-active-themes)))
    (message "Selected theme %s." next-theme)
    (neshtea/switch-theme next-theme)))

;; Collection of themes.
(use-package base16-theme)
(use-package doom-themes)
(use-package kanagawa-themes
  :config
  (setq kanagawa-themes-comment-italic nil)
  (setq kanagawa-themes-keyword-italic nil))
(use-package gruvbox-theme)
;; (neshtea/switch-theme 'base16-gruvbox-material-dark-medium)
(neshtea/switch-theme 'base16-everforest-dark-hard)

(use-package doom-modeline
  :defer 0.1
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-minor-modes nil))

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t))

(use-package company
  :defer 2
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  :config
  (global-company-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

;; consult provides a huge array of cap based searches.
(use-package consult
  :config
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :bind
  (("C-c c m" . consult-mode-command)
   ("C-c c h" . consult-history)
   ("C-c c b" . consult-bookmark)
   ("C-c c l" . consult-line)
   ("C-c c o" . consult-outline)
   ("C-c c b" . consult-buffer)
   ("C-c c r" . consult-ripgrep)
   ;; Use consult-buffer instead of default buffer switcher.
   ("C-x b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package marginalia
  :defer 0.5
  :config
  (marginalia-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h p" . helpful-at-point)))

(use-package cider
  :bind (:map clojure-mode-map
	      ("C-. h d" . cider-clojure-docs)
	      ("C-. h h" . cider-doc)
	      ("C-. t t" . cider-test-run-test)
	      ("C-. t a" . cider-test-run-ns-test))
  :config
  (setq cider-repl-display-help-banner nil))

(use-package eglot
  :straight (:type built-in)
  :hook ((clojure-mode
          clojurescript-mode
          typescript-ts-mode
          tsx-ts-mode
          nix-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0) ;; Disable event logging for performance
  (eglot-sync-connect nil) ;; Don't block on LSP connection
  :config
  (setq eglot-code-action-indications '(eldoc-hint))
  (setq eglot-connect-timeout 120)
  ;; https://whatsapp.github.io/erlang-language-platform/docs/get-started/editors/emacs/
  (add-to-list 'eglot-server-programs '(erlang-mode . ("elp" "server")))
  (add-to-list 'eglot-server-programs
               '((elixir-mode heex-mode elixir-ts-mode) . ("elixir-ls")))
  (setq-default eglot-workspace-configuration
                ;; Run `elp config` to see that options can be used here
                ;; Use `eglot-show-workspace-configuration` to see what is sent
                '(:elp (:highlightDynamic (:enable t)
                                          :typesOnHover (:enable t) ))

                eglot-semantic-token-modifiers
                '("bound" "exported_function" "exported_type" "deprecated_function" "type_dynamic"))

  ;; Each face name arises as a template from the modifiers as
  ;; "eglot-semantic-%s-face"
  (defface eglot-semantic-bound-face
    '((t :underline t))
    "The face modification to use for bound variables in patterns."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-exported_function-face
    '((t :underline t))
    "The face modification to use for exported functions."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-exported_type-face
    '((t :underline t))
    "The face modification to use for exported types."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-deprecated_function-face
    '((t :strike-through t))
    "The face modification to use for deprecated functions."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-type_dynamic-face
    '((t (:weight bold)))
    "The face modification to use for dynamic types."
    :group 'eglot-semantic-semantic-tokens)

  ;; Bare eglot makes the refresh a no-op. Provide our own version for
  ;; when Eqwalizer gets its results.
  (cl-defmethod eglot-handle-request
    (server (_method (eql workspace/semanticTokens/refresh)) &rest args)
    "Handle workspace/semanticTokens/refresh by refreshing font-lock."
    (dolist (buffer (eglot--managed-buffers server))
      (eglot--when-live-buffer buffer
                               (eglot--widening (font-lock-flush)))))

  ;; Astro stuff
  ;; https://medium.com/@jrmjrm/configuring-emacs-and-eglot-to-work-with-astro-language-server-9408eb709ab0
  (add-to-list 'eglot-server-programs
               `(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk ,(expand-file-name "~/.nix-profile/lib/node_modules/typescript/lib")))))))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))

(use-package reformatter)

(reformatter-define prettier-format
  :program "npx"
  :args (list "prettier" "--stdin-filepath" (buffer-file-name))
  :lighter " Prettier")

(defun neshtea/typescript-mode-hook ()
  (prettier-format-on-save-mode))

(use-package typescript-ts-mode
  :hook ((typescript-ts-mode . neshtea/typescript-mode-hook)
         (tsx-ts-mode . neshtea/typescript-mode-hook)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;;;; Haskell language support.
(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (:map haskell-mode-map
	      ("C-. i i" . haskell-navigate-imports-go)
	      ("C-. i r" . haskell-navigate-imports-return))
  :config
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-starter-offset 4)
  (setq haskell-process-type 'cabal-repl)
  (setq haskell-interactive-popup-errors nil))

;; A list of all modes I want lispy modes hooked to.  Add to this list
;; if new modes join the lispy gang.
(setq neshtea/lispy-modes '(emacs-lisp-mode
			    clojure-mode
			    ielm-mode
			    lisp-interaction-mode
			    lisp-mode
			    scheme-mode
			    racket-mode))

(defun neshtea/symbol-join (symbols sep)
  "Similar to 'string-join' but joins 'symbols' using 'sep' as
the separator."
 (intern (string-join (mapcar #'symbol-name symbols) sep)))

(defun neshtea/hook-lispy-modes (mode-name)
  "Add paredit-mode to 'mode-name'."
  (let* ((mode-hook (neshtea/symbol-join (list mode-name 'hook) "-")))
    (add-hook mode-hook #'enable-paredit-mode)))

(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (mapcar #'neshtea/hook-lispy-modes neshtea/lispy-modes))

(use-package docker
  :bind ("C-c d" . docker))
(use-package yaml-mode)
(use-package lua-mode)
(use-package envrc
  :defer 1
  :config
  (envrc-global-mode))
(use-package clj-refactor)
(use-package clojure-mode)

;; More info on why I (have to) do this:
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
(use-package jarchive
  :after eglot
  :config
  (jarchive-setup))

(use-package magit
  :defer t
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

(use-package org
  :preface
  (setq neshtea/org-agenda-file "~/org/tasks.org")
  (defun neshtea/open-gtd-file ()
    (interactive)
    (find-file neshtea/org-agenda-file))
  :bind (("C-c o a" . org-agenda)
         ("C-c o f" . neshtea/open-gtd-file)
         ("C-c o c" . org-capture))
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-agenda-files '("~/org"))
  (setq org-capture-templates
        '(("c" "Capture Todo" entry (file+headline "~/org/tasks.org" "INBOX")
           "* TODO %?\n  CAPTURED: %U"
           :prepend t)))
  (setq org-refile-targets '((org-agenda-files :maxlevel 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")))
  (setq org-agenda-custom-commands
        '(("z" "Zen Daily View"
           ((tags-todo "PRIORITY=\"A\""
                       ((org-agenda-overriding-header "Big Rocks")))
            (agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "+0d")
                        (org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Inbox/Unprocessed")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled 'deadline)))))))))

(use-package adoc-mode)
(use-package eat
  :config
  (setq eat-term-name "xterm-256color")
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:")
                     "remote-shell" "/bin/sh")))

(use-package tuareg)

;; Elixir language support.
(use-package elixir-mode)

(use-package csv-mode
  ;; https://www.emacswiki.org/emacs/CsvMode
  :hook (csv-mode . csv-align-mode))

;; https://wmealing.github.io/erlang-emacs-2025.html
(use-package erlang
  :hook (erlang-mode . erlang-format-on-save-mode)
  :config
  (setq inferior-erlang-machine "rebar3")
  (setq inferior-erlang-machine-options '("shell"))
  (setq inferior-erlang-shell-type nil))

(reformatter-define erlang-format
  :program "erlfmt"
  :args (list "-")
  :lighter " erlfmt")

(use-package agent-shell
  :config
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         :load-env "~/.env")))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#getting-started
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(use-package web-mode)
(define-derived-mode astro-mode web-mode "astro")
;; (setq auto-mode-alist
;;       (append '((".*\\.astro\\'" . astro-mode)
;;                 auto-mode-alist)))

(provide 'init)
;;; init.el ends here
