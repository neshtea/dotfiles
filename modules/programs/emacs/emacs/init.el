;; -*- lexical-binding: t; -*-
(setq use-package-always-ensure t)

(defun neshtea/report-startup-time ()
  (message
   "Emacs startup took %s with %d garbage collections"
   (format
    "%.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))
   gcs-done))

(use-package emacs
  :hook (emacs-startup-hook . (lambda ()
                                (toggle-frame-maximized)
                                (neshtea/report-startup-time)))
  :config
  ;; UI stuff.
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (setq inhibit-splash-screen t)
  (setq ring-bell-function 'ignore)
  
  ;; Where to write stuff.
  (setq custom-file (expand-file-name "~/.config/emacs/custom.el"))
  (setq user-emacs-directory
        (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache/")))

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
  (show-paren-mode 1))

(setq neshtea/font-alist    ; TODO copy the latest version from kenranunderscore
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

(use-package which-key
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

(use-package base16-theme)
(use-package doom-themes)

(neshtea/switch-theme 'base16-gruvbox-material-dark-medium)

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t))

(use-package savehist
  :init (savehist-mode 1)
  :config
  (setq savehist-file (expand-file-name "history" user-emacs-directory)))

(use-package company
  :init (global-company-mode))

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
   ("C-c c r" . consult-ripgrep))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package marginalia
  :config (marginalia-mode))

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
  :hook (((clojure-mode
           clojurescript-mode
           typescript-ts-mode
           tsx-ts-mode
           nix-mode) . eglot-ensure))
  :config
  (setq eglot-code-action-indications '(eldoc-hint))
  (setq eglot-connect-timeout 120))

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
  :config (envrc-global-mode))
(use-package clj-refactor)
(use-package clojure-mode)

;; More info on why I (have to) do this:
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
(use-package jarchive
  :after eglot
  :config
  (jarchive-setup))

(use-package magit)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

(use-package org
  :hook (org-mode . org-indent-mode))

(use-package adoc-mode)
(use-package eat)

;; (defun neshtea/emux/phoenix-ng ()
;;   (interactive)
;;   (delete-other-windows)
;;   (let* ((root-window (selected-window))
;;          (right-window (split-window-right)))
;;     (select-window right-window)    
;;     (let ((bottom-right-window (split-window-below)))
;;       (eat-project)
;;       (rename-buffer "phoenix-ng-client")
;;       (select-window bottom-right-window)
;;       (eat-project)
;;       (rename-buffer "phoenix-ng-server")
;;       (select-window root-window))))

(provide 'init)
;;; init.el ends here
