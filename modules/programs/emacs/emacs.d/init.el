;;; init.el --- Summary

;; Commentary:
;;; Configuration for Emacs

;;;; GENERAL
(setq
 ;; Don't show the standart Emacs startup screen.
 inhibit-splash-screen t
 ;; Don't make backup files the first time it is saved.
 make-backup-files nil
 ;; Don't clutter init.el with custom but instead write it to
 ;; ~/.emacs.d/custom.el.
 custom-file (expand-file-name "~/.config/emacs/custom.el")
 ;; Remap some mac-specific keys.
 ns-alternate-modifier 'none
 ns-command-modifier 'meta
 ns-function-modifier 'super
 ;; Turn off all alarms completely.
 ;; See https://www.emacswiki.org/emacs/AlarmBell.
 ring-bell-function 'ignore
 ;; Always prefer the "newer" version of a file.
 load-prefer-newer t
 max-lisp-eval-depth 5000)

;; "When you visit a file, point goes to the last place where it was
;; when you previously visited the same file."
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Set a more sensible default for the maximum width of a column.
(setq-default fill-column 80)

;; Don't show errors when loading the custom file.
(load custom-file 'no-error)

;; Set the font.
(setq neshtea/font-alist
      '((jetbrains-mono . (:font   "JetBrains Mono"
			   :height 140))
	(iosevka        . (:font   "Iosevka"
			   :height 160))))

(setq neshtea/current-font 'jetbrains-mono)

(defun neshtea/switch-font (font)
  "Select one of the fonts configured in 'neshtea/font-alist' as
the face-font."
  (interactive
   (list (intern (completing-read "Font: " (mapcar #'car (copy-alist neshtea/font-alist))))))
  ;; If the selected font is not the currently active font, switch.
  (let* ((attrs (alist-get font neshtea/font-alist))
	 (font (plist-get attrs :font))
	 (height (plist-get attrs :height)))
    (setq neshtea/current-font font)
    (set-face-attribute 'default nil
			:font font
			:height height)))

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

;; Alwas show matching parens.
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Sometimes, I need relative line numbers.  `display-line-numbers`
;; has this built in.  This function makes it easier to toggle.

					; Default to relative
(setq display-line-numbers-type 't)  ; regular line numbers by default.
(defun neshtea/toggle-display-line-numbers-relative ()
  "Toggle between relative and 'regular' line numbers."
  (interactive)
  (if (equal 't display-line-numbers)
      (setq display-line-numbers 'relative)
      (setq display-line-numbers 't)))

;; We install packages via nix home-manager, but we still configure
;; them via use-package
(require 'use-package)

;; Especially on MacOS, the exec path is always wrong.  This package
;; tries to fix that.
;; SEE https://github.com/purcell/exec-path-from-shell#usage
(use-package exec-path-from-shell)

(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
  (add-to-list 'exec-path-from-shell-variables var))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Display possible keyboard shortcut completions.
(use-package which-key
  :custom
  (which-key-idle-delay 0.3)  ; Show completions relatively fast.
  :init
  ;; We always want to display completions.
  (add-hook 'after-init-hook #'which-key-mode))

;; Taken from Johannes init.el
;; https://github.com/kenranunderscore/dotfiles/blob/main/modules/programs/emacs/emacs.d/init.el#L80
(defun neshtea/switch-theme (name)
  "Switch themes interactively.  Similar to `load-theme' but also
disables all other enabled themes."
  (interactive
   (list (intern
	  (completing-read
	   "Theme: "
	   (mapcar #'symbol-name
                   (-difference (custom-available-themes)
                                custom-enabled-themes))))))
  (progn
    (mapcar #'disable-theme
            custom-enabled-themes)
    (load-theme name t)))

(setq modus-themes-mode-line '(accented borderless (padding . 5)))
(setq modus-themes-italic-constructs t)
(setq modus-themes-syntax '(yellow-comments green-strings))
(setq modus-themes-paren-match '(bold))
(setq modus-themes-headings '((t . (monochrome))))
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-fringes nil)

;; Set the theme to gruvbox
(neshtea/switch-theme 'gruvbox-dark-hard)

(use-package all-the-icons)

;;;; Generic, non-mode specific helpers.o
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun neshtea/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-. t r") #'neshtea/toggle-display-line-numbers-relative)

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(defun neshtea/projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package project
  :config
  (add-hook 'project-find-functions #'neshtea/projectile-project-find-function))

;; consult provides a huge array of cap based searches.
(use-package consult
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
  
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

;; marginalia annotates completion candidates in the completion at
;; point buffer. Plays nicely with consult, etc.
(use-package marginalia :init (marginalia-mode))

;; Easily find projects and files within projects.
(use-package project)

(use-package projectile
  :init (projectile-mode +1))

;; Complete anything -- auto completion framework.
(use-package company
  :hook
  (after-init . global-company-mode)
  :diminish company-mode)

;;;; Org mode configuration
(use-package org-indent
  :defer t
  :after org)

(defun neshtea/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode))

(defun neshtea/org-gtd-file ()
  (interactive)
  (find-file (expand-file-name "~/Dropbox/Brain/org/gtd.org")))

(defun neshtea/org-projects-file ()
  (interactive)
  (find-file (expand-file-name "~/Dropbox/Brain/org/projects.org")))

(use-package org
  :hook (org-mode . neshtea/org-mode-setup)

  :bind (("C-c o a" . org-agenda-list)
	 ("C-c o t" . org-todo-list)
	 ("C-c o f" . neshtea/org-gtd-file)
	 ("C-c o p" . neshtea/org-projects-file)
	 ("C-c o c c" . org-capture))
  
  :custom
  ;; (org-ellipsis " ▾")
  (org-edit-src-content-indentation 0)  ; Don't indent in src blocks.
  (org-hide-emphasis-markers t)
  (org-adapt-indentation nil)
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-return-follows-link t)
  (org-startup-folded 'content)
  (org-agenda-files '("~/Dropbox/Brain/org/gtd.org"))
  (org-agenda-span 10)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "-3d")
  (org-capture-templates '(("t" "Todo [inbox/work]" entry
			    (file+headline "~/Dropbox/Brain/org/gtd.org" "INBOX")
			    "* TODO %i%? \n%U")))
  (org-refile-targets '(("~/Dropbox/Brain/org/gtd.org" :maxlevel . 2)
			("~/Dropbox/Brain/org/lists.org" :maxlevel . 2)
			("~/Dropbox/Brain/org/projects.org" :maxlevel . 1)))
	;; When the state of a section headline changes, log the
	;; transition into the headlines drawer.
  ;; When the state of a section headline changes, log the
  ;; transition into the headlines drawer.
  (org-log-into-drawer 'LOGBOOK)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED"))))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-trigger 'always))

(defun neshtea/org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (let ((markers org-hide-emphasis-markers))
    (if markers
	(setq-local org-hide-emphasis-markers nil)
      (setq-local org-hide-emphasis-markers t))))

;; Roam inspired mode for my zettelkasten using org mode.
(use-package org-roam
  :defer t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/Brain/org/zettelkasten")
  (org-roam-dailies-directory "dailies/")
  (org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
			 "#+title: %<%Y-%m-%d>\n"))))

  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-setup))

;; I need to have these keys available everywhere
(global-set-key (kbd "C-c o r c") #'org-roam-caputre)
(global-set-key (kbd "C-c o r f") #'org-roam-node-find)
(global-set-key (kbd "C-c o d t") #'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c o d p") #'org-roam-dailies-goto-previous-note)
(global-set-key (kbd "C-c o d n") #'org-roam-dailies-goto-next-note)
(global-set-key (kbd "C-c o d c") #'org-roam-dailies-capture-today)

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package default-text-scale
  :defer t
  :bind (("C-c t =" . default-text-scale-increase)
	 ("C-c t -" . default-text-scale-decrease)
	 ("C-c t 0" . default-text-scale-reset))
  :config
  (default-text-scale-mode))

(use-package diff-hl
  :init (global-diff-hl-mode))

;; Magit are the very best tools for interacting with git.
(use-package magit
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))  
  :after diff-hl
  :config
  (setq-default git-magit-status-fullscreen t))

;; Used for golden-ration mode.
(use-package zoom
  :config
  ;; Resize the selected window using the golden ratio:
  ;; https://github.com/cyrus-and/zoom#example-configurations
  (setq zoom-size '(0.618 . 0.618)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h p" . helpful-at-point)))

(use-package hledger-mode
  :defer t
  :hook (hledger-view-mode . #'hl-line-mode)
  :custom
  (hledger-jfile (expand-file-name "~/Dropbox/Brain/Finance/ledger2023.journal"))
  :config
  (add-to-list 'company-backends 'hledger-company))

(use-package hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

(use-package envrc
  :defer t
  :init (envrc-global-mode))

(use-package reformatter)

;;;; Programming language support

;;; Clojure language support.

;; Working Clojure needs almost no configuration, just some nice
;; packages (cider, clj-refactor, clojure-mode).
(use-package clj-refactor
  :defer t)

(defun neshtea/clojure-mode-hook ()
  "Hooks everything important for 'clojure-mode'."
  (interactive)
  (clj-refactor-mode 1)
  (add-hook 'before-save-hook
	    'cider-format-buffer
	    nil
	    t))

(use-package clojure-mode
  ;; https://docs.cider.mx/cider/usage/misc_features.html#formatting-code-with-cljfmt
  :hook (clojure-mode . neshtea/clojure-mode-hook)
  :defer t)

(use-package cider
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  :bind (:map clojure-mode-map
	      ("C-. h d" . cider-clojure-docs)
	      ("C-. h h" . cider-doc)
	      ("C-. t t" . cider-test-run-test)
	      ("C-. t a" . cider-test-run-ns-test)))

;;; Nix language support

;; Work with nix files (syntax highlighting and indentation). 
(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))
;;; OCaml language support
(use-package merlin
  :hook ((tuareg-mode . merlin-mode)
	 (caml-mode . merlin-mode))
  :custom
  (merlin-command "ocamlmerlin"))

(use-package utop
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package merlin-company
  :defer t)

(use-package ocp-indent
  :defer t)

(use-package tuareg
  :defer t)

;; Based on https://github.com/ludwigpacifici/ocamlreformat.el/blob/master/ocamlreformat.el
(reformatter-define ocaml-format
  :program "ocamlformat"
  :args (list "--name" (buffer-file-name) "-"))

;;;; Haskell language support.
(use-package haskell-mode
  :defer t
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-interactive-popup-errors nil)
  ;; TODO Remove, use dir-locals
  :bind (:map haskell-mode-map
	      ("C-. i i" . haskell-navigate-imports-go)
	      ("C-. i r" . haskell-navigate-imports-return))
  :hook (haskell-mode . interactive-haskell-mode))

(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
	      ("C-c <tab>" . company-complete)
	      ("C-c l a" . elgot-code-actions)
	      ("C-c l d" . eldoc-doc-buffer)
	      ("C-c l r" . eglot-rename)
	      ("C-c l g d" . xref-find-definitions)
	      ("C-c l g r" . xref-find-references)
	      ("C-c l e n" . flymake-goto-next-error)
	      ("C-c l e p" . flymake-goto-previous-error))
  :config
  ;; don't ask before lsp intiated writes.
  (setq eglot-confirm-server-initiated-edits nil))

;;; Common Lisp language support.
(use-package sly
  :config
  ;; default to sbcl
  (setq inferior-lisp-program "sbcl"))

;;; Scheme language support
(use-package geiser)

(use-package geiser-guile)

(use-package geiser-chicken)

;;; Generic s-expression based languages support

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

;;; Docker language support
(use-package dockerfile-mode
  :defer t)

;;; YAML language support
(use-package yaml-mode
  :defer t)

;;; Markdown language support
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init)
;;; init.el ends here
