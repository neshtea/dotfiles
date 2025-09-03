;; -*- lexical-binding: t; -*-
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
(setq straight-use-package-by-default t)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs startup took %s with %d garbage collections"
    (format
     "%.2f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))

;;;; GENERAL
(setq
 inhibit-splash-screen t  ; Don't show the standart Emacs startup screen.
 make-backup-files nil  ; Don't make backup files the first time it is saved.
 custom-file (expand-file-name "~/.config/emacs/custom.el"); Don't clutter init.el with custom but instead write it to  ~/.emacs.d/custom.el.
 ;; Remap some mac-specific keys.
 ns-alternate-modifier 'none
 ns-command-modifier 'meta
 ns-function-modifier 'super
 ring-bell-function 'ignore  ; Turn off all alarms completely. See https://www.emacswiki.org/emacs/AlarmBell.
 load-prefer-newer t  ; Always prefer the "newer" version of a file.
 max-lisp-eval-depth 5000
 ;; isearch
 isearch-allow-scroll t  ; don't cancel isearch on scroll
 isearch-lazy-count t  ; show number of matches
 )

;; MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq-default cursor-type 'hbar)
(setq-default indent-tabs-mode nil)

;; "When you visit a file, point goes to the last place where it was when you
;; previously visited the same file."  https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)
(toggle-frame-maximized)  ; Startup with a maximized window
(setq-default fill-column 80)  ; Set a more sensible default for the maximum width of a column.
(load custom-file 'no-error)  ; Don't show errors when loading the custom file.

(setq neshtea/font-alist  ; TODO copy the latest version from kenranunderscore
      '((jetbrains-mono . (:family "JetBrains Mono"))
	(iosevka . (:family "Iosevka"))
	(sf-mono . (:family
		    "SF Mono"
		    :width))))

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

;; Disable menubar/scrollbar/toolbar.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Answer y or n to yes-or-no questions.
;; http://pragmaticemacs.com/emacs/make-all-prompts-y-or-n/
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)  ; Alwas show matching parens.
(setq display-line-numbers-type 't)  ; regular line numbers by default.

(use-package ibuffer :defer t)

;; Especially on MacOS, the exec path is always wrong.  This package
;; tries to fix that.
;; SEE https://github.com/purcell/exec-path-from-shell#usage
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-shell-name "zsh")
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package which-key :defer t
  :custom
  (which-key-idle-delay 0.3)
  :init
  (add-hook 'after-init-hook #'which-key-mode))

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
    (mapcar #'disable-theme
            custom-enabled-themes)
    (load-theme name t)))

(defun neshtea/random-theme (&optional themes)
  "Select a random theme out of all available themes and load
it. Optionally, you can supply a list of themes to select from."
  (interactive)
  (let* ((themes (or themes (custom-available-themes)))
	 (but-active-themes (seq-difference themes
					    custom-enabled-themes))
	 (next-theme (nth (random (length but-active-themes))
			  but-active-themes)))
    (message "Selected theme %s." next-theme)
    (neshtea/switch-theme next-theme)))

(use-package base16-theme :defer)
(use-package doom-themes :defer)
(use-package nerd-icons :defer)

(neshtea/switch-theme 'base16-gruvbox-material-dark-medium)

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t))

(use-package savehist :defer
  :init (savehist-mode))

(use-package company :defer)
(use-package company-box :defer
  :hook (company-mode . company-box-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package project)

;; consult provides a huge array of cap based searches.
(use-package consult :defer t
  :init
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :bind
  (("C-c c m" . consult-mode-command)
   ("C-c c h" . consult-history)
   ("C-c c b" . consult-bookmark)
   ("C-c c l" . consult-line)
   ("C-c c o" . consult-outline)
   ("C-c c b" . consult-buffer)
   ("C-c c r" . consult-ripgrep))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package marginalia :defer
  :init (marginalia-mode))

(use-package helpful :defer
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h p" . helpful-at-point)))

(use-package cider
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  :bind (:map clojure-mode-map
	      ("C-. h d" . cider-clojure-docs)
	      ("C-. h h" . cider-doc)
	      ("C-. t t" . cider-test-run-test)
	      ("C-. t a" . cider-test-run-ns-test)))

(use-package eglot
  :ensure t
  :hook (((clojure-mode clojurescript-mode typescript-ts-mode nix-mode) . eglot-ensure))
  :custom
  (eglot-code-action-indications '(eldoc-hint)))

(use-package eldoc)
(use-package flymake)

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))

(use-package merlin
  :defer t
  :hook ((tuareg-mode . merlin-mode)
	 (caml-mode . merlin-mode))
  :custom
  (merlin-command "ocamlmerlin"))

(use-package merlin-company :defer)
(add-to-list 'auto-mode-alist '("\\.mlx\\'" . tuareg-mode))
(use-package tuareg :defer
  :hook ((tuareg-mode . ocaml-format-on-save-mode)))
(use-package reason-mode :defer
  :straight (:host github :github "reasonml-editor/reason-mode"))

(use-package dune :defer
  :hook ((dune-mode . dune-format-on-save-mode)))

(use-package reformatter :defer)

(reformatter-define ocaml-format
  :program "ocamlformat"
  :args (list "--name" (buffer-file-name) "-"))

(reformatter-define ocaml-mlx-format
  :program "ocamlformat-mlx"
  :args (list "--name" (buffer-file-name) "--impl" "-"))

(reformatter-define dune-format
  :program "dune"
  :args '("format-dune-file")
  :lighter " DuneFmt")

(reformatter-define prettier-format
  :program "npx"
  :args (list "prettier" "--stdin-filepath" (buffer-file-name))
  :lighter " Prettier")

(use-package typescript-ts-mode :defer
  :hook ((typescript-ts-mode . prettier-format-on-save-mode)
         (tsx-ts-mode . prettier-format-on-save-mode)))

;;;; Haskell language support.
(use-package haskell-mode :defer
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-interactive-popup-errors nil)
  :config
  (setq haskell-indentation-left-offset 4
	haskell-indentation-layout-offset 4
	haskell-indentation-starter-offset 4)
  :bind (:map haskell-mode-map
	      ("C-. i i" . haskell-navigate-imports-go)
	      ("C-. i r" . haskell-navigate-imports-return))
  :hook ((haskell-mode . interactive-haskell-mode)))

(use-package yasnippet :defer
  :config
  (yas-global-mode 1))

(use-package sly :defer
  :config (setq inferior-lisp-program "sbcl"))

;; A list of all modes I want lispy modes hooked to.  Add to this list
;; if new modes join the lispy gang.
(setq neshtea/lispy-modes '(emacs-lisp-mode
			    eval-expression-minibuffer-setup
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

(use-package docker :defer)
(use-package dockerfile-mode :defer)
(use-package yaml-mode :defer)
(use-package markdown-toc :defer)
(use-package rustic :defer)
(use-package lua-mode :defer)
(use-package elixir-mode :defer)
(use-package erlang :defer)
(use-package sly-quicklisp :defer)
(use-package envrc :init (envrc-global-mode))
(use-package clj-refactor :defer)
(use-package clojure-mode :defer)
(use-package magit :defer)

(use-package markdown-mode :defer
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package org :defer
  :hook ((org-mode . org-indent-mode)))

(use-package org-modern :defer
  :hook ((org-mode . org-modern-mode))
  :config
  (setq org-auto-align-tags nil
	org-tags-column 0
	org-fold-catch-invisible-edits 'show-and-error
	org-special-ctrl-a/e t
	org-insert-heading-respect-content t
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(use-package go-mode :defer)
(use-package adoc-mode :defer)
(use-package hledger-mode :defer
  :config
  (setq hledger-jfile "~/.hledger.journal"))

(provide 'init)
;;; init.el ends here
