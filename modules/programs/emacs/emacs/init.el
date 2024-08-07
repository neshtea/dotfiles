;; init.el --- Summary

;; Commentary:
;;; Configuration for Emacs

;; Bootstrap straight.el
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

;; (use-package benchmark-init
;;   :demand
;;   :config (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))

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

(setq-default cursor-type 'hbar)

;; "When you visit a file, point goes to the last place where it was
;; when you previously visited the same file."
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Startup with a maximized window
(toggle-frame-maximized)

;; Set a more sensible default for the maximum width of a column.
(setq-default fill-column 80)

;; Don't show errors when loading the custom file.
(load custom-file 'no-error)

;; Set the font.
(setq neshtea/font-alist
      '((jetbrains-mono . (:font "JetBrains Mono"
				 :height 140))
	(iosevka . (:font "Iosevka"
			  :height 150))
	(sf-mono . (:font "SF Mono" :height 150))
	))

;; (setq neshtea/current-font 'jetbrains-mono)
(setq neshtea/current-font 'iosevka)

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

;; windmove
(global-set-key (kbd "C-c h") #'windmove-left)
(global-set-key (kbd "C-c j") #'windmove-down)
(global-set-key (kbd "C-c k") #'windmove-up)
(global-set-key (kbd "C-c l") #'windmove-right)

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

(use-package ibuffer :defer t)

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
   (list (intern (completing-read "Theme: "
				  (mapcar #'symbol-name
					  (seq-difference (custom-available-themes)
							  custom-enabled-themes))))))
  (progn
    (mapcar #'disable-theme
            custom-enabled-themes)
    (load-theme name t)))

(defun neshtea/random-theme (&optional themes)
  ;; NOTE Motivated by watching kenranunderscores use something like this
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

(setq neshtea/favourite-dark-themes '(base16-gruvbox-dark-medium
				      base16-tokyo-night-dark
				      base16-horizon-dark
				      base16-default-dark
				      base16-onedark
				      base16-catppuccin-mocha))

(setq neshtea/favourite-light-themes '(base16-default-light
				       base16-ia-light
				       base16-catppuccin-latte))

(defun neshtea/random-favourite-dark-theme ()
  (interactive)
  (neshtea/random-theme neshtea/favourite-dark-themes))

(defun neshtea/random-favourite-light-theme ()
  (interactive)
  (neshtea/random-theme neshtea/favourite-light-themes))

(global-set-key (kbd "C-. s r r") #'neshtea/random-theme)
(global-set-key (kbd "C-. s r d") #'neshtea/random-favourite-dark-theme)
(global-set-key (kbd "C-. s r l") #'neshtea/random-favourite-light-theme)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package solaire-mode :init (solaire-global-mode +1))

(use-package base16-theme
  :defer t)

(use-package nerd-icons :defer t)

(neshtea/switch-theme 'base16-tokyo-night-dark)

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

(use-package savehist :init (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package project)

;; consult provides a huge array of cap based searches.
(use-package consult
  :defer t
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

(use-package projectile :init (projectile-mode +1))

;; Complete anything -- auto completion framework.
(use-package company
  :hook
  (after-init . global-company-mode)
  :diminish company-mode)

(use-package company-box
  :defer t
  :after company
  :hook (company-mode . company-box-mode))

(use-package default-text-scale
  :defer t
  ;; NOTE: Moved to hydra
  ;; :bind (("C-c t =" . default-text-scale-increase)
  ;; 	 ("C-c t -" . default-text-scale-decrease)
  ;; 	 ("C-c t 0" . default-text-scale-reset))
  :config
  (default-text-scale-mode))

(use-package hydra
  :defer t
  :after default-text-scale)

(defhydra hyrda-zoom (global-map "C-c t")
  "zoom"
  ("=" default-text-scale-increase)
  ("-" default-text-scale-decrease)
  ("0" default-text-scale-reset))

(use-package eshell :defer t)

(use-package eshell-syntax-highlighting
  :defer t
  :after eshell-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package casual-dired
  :straight (casual-dired :type git
			  :host github
			  :repo "kickingvegas/casual-dired")
  :ensure t
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

;;;; Org mode configuration
;; (use-package org-indent
;;   :ensure nil
;;   :after org)

(defun neshtea/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode))

(defvar icloud-base-dir (file-name-as-directory (expand-file-name "~/Library/Mobile Documents/com~apple~CloudDocs/")))

(defun icloud-file (relative-dir)
  (concat icloud-base-dir relative-dir))

(defun icloud-org-file (relative-file-name)
  (concat (icloud-file "Documents/org/") relative-file-name))

(icloud-org-file "gtd.org")

(defun neshtea/org-gtd-file ()
  (interactive)
  (find-file (icloud-org-file "gtd.org")))

(defun neshtea/org-projects-file ()
  (interactive)
  (find-file (icloud-org-file "projects.org")))

(use-package org
  ;; :straight (:type built-in)
  :hook (org-mode . neshtea/org-mode-setup)
  :bind (("C-c o a" . org-agenda-list)
	 ("C-c o t t" . org-todo-list)
	 ("C-c o t l" . org-tags-view)
	 ("C-c o f" . neshtea/org-gtd-file)
	 ("C-c o p" . neshtea/org-projects-file)
	 ("C-c o c c" . org-capture))
  :config
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  :custom
  (org-edit-src-content-indentation 0)  ; Don't indent in src blocks.
  (org-return-follows-link t)
  (org-startup-folded 'content)
  (org-agenda-files `(,(icloud-org-file "gtd.org")
		      ,(icloud-org-file "zettelkasten/dailies/")))
  (org-ellipsis "…")
  (org-pretty-entities t)
  (org-hide-emphasis-markers nil)
  (org-adapt-indentation nil)
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-agenda-span 7)
  (org-agenda-sorting-strategy
   '((agenda time-up priority-down todo-state-up category-keep)
     (todo priority-down todo-state-up category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
  (org-agenda-start-on-weekday 1)
  (org-capture-templates `(("t" "Todo [inbox/work]" entry
			    (file+headline ,(icloud-org-file "gtd.org") "INBOX")
			    "* TODO %?\n%U")
			   ("s" "scheduled Todo [inbox/work]" entry
			    (file+headline ,(icloud-org-file "gtd.org") "INBOX")
			    "* TODO %?\nSCHEDULED: %t\n%U")))
  (org-refile-targets `((,(icloud-org-file "gtd.org") :maxlevel . 3)))
  ;; When the state of a section headline changes, log the
  ;; transition into the headlines drawer.
  (org-log-into-drawer 'LOGBOOK)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED"))))

(use-package org-appear
  :after org
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
  :after org
  :bind (("C-c o r c" . #'org-roam-capture)
	 ("C-c o r f" . #'org-roam-node-find)
	 ("C-c o r i" . #'org-roam-node-insert)
	 ("C-c o d t" . #'org-roam-dailies-goto-today)
	 ("C-c o d p" . #'org-roam-dailies-goto-previous-note)
	 ("C-c o d n" . #'org-roam-dailies-goto-next-note)
	 ("C-c o d c" . #'org-roam-dailies-capture-today))
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (icloud-org-file "zettelkasten/"))
  (org-roam-dailies-directory "dailies/")
  (org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
			 ; :todo: tag means the note has not been scanned for
			 ; long term noteworthiness yet.
			 "#+title: %<%Y-%m-%d>\n#+filetags: :todo:\n"))))

  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("e" "exzerpt" plain
      "%?" :target
      (file+head "exzerpte/${slug}.org" "#+title ${title}\n#+filetags: :exzerpt:\n")
      :unnarrowed t)))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :defer t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package diff-hl
  :defer t
  :init (global-diff-hl-mode))

;; Magit are the very best tools for interacting with git.
(use-package magit
  :defer t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :after diff-hl
  :config
  (setq-default git-magit-status-fullscreen t))

;; Used for golden-ration mode.
(use-package zoom
  :defer t
  :config
  ;; Resize the selected window using the golden ratio:
  ;; https://github.com/cyrus-and/zoom#example-configurations
  (setq zoom-size '(0.618 . 0.618)))

(use-package helpful
  :defer t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h p" . helpful-at-point)))

(use-package hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

(use-package envrc :init (envrc-global-mode))

(use-package reformatter)

;;;; Programming language support

;;; Clojure language support.

;; Working Clojure needs almost no configuration, just some nice
;; packages (cider, clj-refactor, clojure-mode).
(use-package clj-refactor :defer t)

(use-package clojure-mode)

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

(use-package merlin
  :defer t
  :hook ((tuareg-mode . merlin-mode)
	 (caml-mode . merlin-mode))
  :custom
  (merlin-command "ocamlmerlin"))

(use-package utop  ;; maybe reconsider this package
  :defer t
  :hook
  (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam exec -- dune utop . -- -emacs"))

(use-package merlin-company :defer t)

(use-package ocp-indent :defer t)

(use-package tuareg
  :defer t
  :hook (tuareg-mode . ocaml-format-on-save-mode))

(use-package reason-mode
  :straight (:host github :github "reasonml-editor/reason-mode"))

(use-package dune :hook ((dune-mode . dune-format-on-save-mode)))

;; Based on https://github.com/ludwigpacifici/ocamlreformat.el/blob/master/ocamlreformat.el
(reformatter-define ocaml-format
  :program "ocamlformat"
  :args (list "--name" (buffer-file-name) "-"))

(reformatter-define dune-format
  :program "dune"
  :args '("format-dune-file")
  :lighter " DuneFmt")

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
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l d" . eldoc-doc-buffer)
	      ("C-c l r" . eglot-rename)
	      ("C-c l g d" . xref-find-definitions)
	      ("C-c l g r" . xref-find-references)
	      ("C-c l e n" . flymake-goto-next-error)
	      ("C-c l e p" . flymake-goto-previous-error)
	      ("C-c l f" . eglot-format-buffer))
  :config
  ;; don't ask before lsp intiated writes.
  (setq eglot-confirm-server-initiated-edits nil))

;;; Common Lisp language support.
(use-package sly
  :defer t
  :config
  ;; default to sbcl
  (setq inferior-lisp-program "sbcl"))

(use-package sly-quicklisp :defer t)

;;; Racket language support
(use-package racket-mode :defer t)

;;; Scheme language support
(use-package geiser :defer t)

(use-package geiser-guile :defer t)

(use-package geiser-chicken :defer t)

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

;;; Docker support
(use-package docker :defer t)

(use-package dockerfile-mode :defer t)

;;; YAML language support
(use-package yaml-mode :defer t)

;;; Markdown language support
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-toc :defer t)

;;; Rust language support
(use-package rustic :defer t)

;;; Lua language support
(use-package lua-mode :defer t)

(defun neshtea/reload-dir-locals (proj)
  (interactive (list (project-current)))
  (unless proj
    (user-error "There doesn't seem to be a project here"))
  ;; Load the variables; they are stored buffer-locally, so...
  (hack-dir-local-variables)
  ;; Hold onto them...
  (let ((locals dir-local-variables-alist))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (equal proj (project-current))
                   buffer-file-name)
          ;; transfer the loaded values to this buffer...
          (setq-local dir-local-variables-alist locals)
          ;; and apply them.
          (hack-local-variables-apply))))))

;;; PureScript language support
(defun neshtea/purescript-mode-hook ()
  (psc-ide-mode)
  (turn-on-purescript-indentation))

(use-package purescript-mode
  :defer t
  :hook (purescript-mode . neshtea/purescript-mode-hook))

(use-package psc-ide
  :defer t
  :config
  (setq psc-ide-use-npm t))

(reformatter-define purescript-format
  :program "npx purs-tidy"
  :args (list "format-in-place" (buffer-file-name)))

(provide 'init)
;;; init.el ends here
