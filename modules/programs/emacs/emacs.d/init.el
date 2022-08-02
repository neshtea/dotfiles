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

;; Don't show errors when loading the custom file.
(load custom-file 'no-error)

;; Set the font.
(setq neshtea/font-alist
      '((jetbrains-mono . (:font   "JetBrains Mono"
			   :height 140))
	(iosevka        . (:font   "Iosevka"
			   :height 140))))

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

;; General is used to define keybindings (replaces previously used
;; evil-leader).
(require 'general)

;;    We define two general definers here:
;; 1. def-with-leader: Define "global" keys, prefixed by "SPC"
;; 2. def-local-with-leader: Define "local" keys (keys for
;;    specific modes, i.e. org-mode, clojure, ...)
(general-create-definer def-with-leader
  :keymaps 'override
  :states '(normal insert emacs visual motion)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(general-create-definer def-local-with-leader
  :keymaps 'override
  :states '(normal insert emacs visual motion)
  :prefix ","
  :non-normal-prefix "C-,")

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

(use-package doom-themes
  :defer t
  :init
  (neshtea/switch-theme 'doom-gruvbox)
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-modes's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :init
  (solaire-global-mode +1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25))

(use-package all-the-icons)

;; Code folding, also used by evil.
;; https://github.com/gregsexton/origami.el
(use-package origami
  :init
  (global-origami-mode))

;;;;  evil
;; Make editing files a little saner (that is, make it behave like
;; vim).
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-undo-system 'emacs))

;; evil-collection contains a large repository of behaviours that make
;; lots of buffers behave the way you would expect in evil-mode.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Same as evil-collection, but specific to org-mode.
;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :defer t
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
	 (org-agenda-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Make commenting a little easier.
(use-package evil-nerd-commenter)

;;;; Generic, non-mode specific helpers.
;; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; Some global keys, not specific to any one particular mode.
(def-with-leader
  ";"   #'evilnc-comment-or-uncomment-lines
  ;; "e"   #'find-file
  "k"   #'kill-buffer
  "TAB" #'er-switch-to-previous-buffer
  "t f" #'display-fill-column-indicator-mode
  "s h" #'eshell
  "f s" #'toggle-fullscreen
  ;; "t t" #'modus-themes-toggle
  "q r" #'restart-emacs
  "SPC" '(execute-extended-command :which-key "M-x")
  "s f" '(neshtea/switch-font :which-key "switch theme")
  "s t" '(neshtea/switch-theme :which-key "switch theme"))

;; Paredit allows to easily work with parens. Especially useful in
;; LISP-like languages.

;; A list of all modes I want lispy modes hooked to.  Add to this list
;; if new modes join the lispy gang.
(setq neshtea/lispy-modes '(emacs-lisp-mode
			    eval-expression-minibuffer-setup
			    clojure-mode
			    ielm-mode
			    lisp-interaction-mode
			    lisp-mode
			    scheme-mode))

(defun neshtea/symbol-join (symbols sep)
  "Similar to 'string-join' but joins 'symbols' using 'sep' as
the separator."
 (intern (string-join (mapcar #'symbol-name symbols) sep)))

(defun neshtea/hook-lispy-modes (mode-name)
  "Add paredit- and rainbow-delimeters-mode to 'mode-name'."
  (let* ((mode-hook (neshtea/symbol-join (list mode-name 'hook) "-")))
    (progn
      (add-hook mode-hook #'enable-paredit-mode)
      (add-hook mode-hook #'rainbow-delimiters-mode))))

(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (mapcar #'neshtea/hook-lispy-modes neshtea/lispy-modes))

;; Syntax highlighting for markdown files. Requires multimarkdown to
;; be installed on the system.
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Selectrum is a better solution for incremental narrowing in Emacs, 
;; replacing Helm, Ivy, and Ido.
;; https://github.com/raxod502/selectrum
(use-package selectrum :init (selectrum-mode +1))

;; selectrum-prescient helps with surfacing frequently used
;; completions.
(use-package selectrum-prescient
  :after selectrum
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; consult provides a huge array of cap based searches.
(use-package consult
  :init
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(def-with-leader
  "c m" #'consult-mode-command
  "c h" #'consult-history
  "c b" #'consult-bookmark
  "c l" #'consult-line
  "c o" #'consult-outline
  "b b" #'consult-buffer
  "/"   #'consult-ripgrep)

;; marginalia annotates completion candidates in the completion at
;; point buffer. Plays nicely with consult, etc.
(use-package marginalia :init (marginalia-mode))

;; Easily find projects and files within projects.
(use-package projectile :init (projectile-mode +1))

(def-with-leader
  "p p" #'projectile-switch-project
  "p f" #'project-find-file)

;; Highlights docker files and provides some basic commands (none of
;; which I use).
(use-package dockerfile-mode
  :defer t)

;; Highighting and indentation for yaml.
(use-package yaml-mode
  :defer t)

;; Complete anything -- auto completion framework.
(use-package company
  :hook
  (after-init . global-company-mode)
  :diminish company-mode)

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
  (cider-repl-display-help-banner nil))

(def-local-with-leader
  :keymaps 'clojure-mode-map
  "= =" #'clojure-align
  "h a" #'cider-apropos
  "h c" #'cider-cheatsheet
  "h d" #'cider-clojuredocs
  "h j" #'cider-javadoc
  "h h" #'cider-doc
  "h n" #'cider-browse-ns
  "h s" #'cider-browser-spec
  "e ," #'cider-eval-sexp-at-point
  "e b" #'cider-eval-buffer
  "e e" #'cider-eval-last-sexp
  "e m" #'cider-macroexpand-1
  "e n a" #'cider-ns-reload-all
  "t a" #'cider-test-run-ns-tests
  "t t" #'cider-test-run-test)

(use-package org-indent
  :defer t
  :after org)

(use-package org
  :hook (org-mode . auto-fill-mode)

  :custom
  (org-hide-emphasis-markers t)
  (org-adapt-indentation nil)
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-return-follows-link t)
  (org-startup-folded 'fold)
  (org-agenda-files '("~/Dropbox/Brain/Tasks/gtd.org"))
  (org-capture-templates '(("t" "Todo [inbox/work]" entry
			    (file+headline "~/Dropbox/Brain/Tasks/gtd.org" "INBOX")
			    "* TODO %i%? \n  %U")
			   ("c" "Capture [inbox]" entry
			    (file+headline "~/Dropbox/Brain/Tasks/gtd.org" "INBOX")
			    "* TODO %i%? \n  %a")))
  (org-refile-targets '(("~/Dropbox/Brain/Tasks/gtd.org" :maxlevel . 2)
			("~/Dropbox/Brain/Tasks/lists.org" :maxlevel . 2)
			("~/Dropbox/Brain/Tasks/someday.org" :level . 1)))
	;; When the state of a section headline changes, log the
	;; transition into the headlines drawer.
  (org-log-into-drawer 'LOGBOOK)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)"))))

(defun neshtea/org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))

;; org-present
(defun neshtea/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun neshtea/org-present-hook ()
  (setq header-line-format " ")
  (org-display-inline-images)
  (neshtea/org-present-prepare-slide))

(defun neshtea/org-present-quit-hook ()
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun neshtea/org-present-prev ()
  (interactive)
  (org-present-prev)
  (neshtea/org-present-prepare-slide))

(defun neshtea/org-present-next ()
  (interactive)
  (org-present-next)
  (neshtea/org-present-prepare-slide))

(use-package org-present
  :defer t
  :after org
  :bind (:map org-present-mode-keymap
	      ("C-c C-j" . neshtea/org-present-next)
	      ("C-c C-k" . neshtea/org-present-prev))
  :hook ((org-present-mode . neshtea/org-present-hook)
	 (org-present-mode-quit . neshtea/org-present-quit-hook)))

;; Roam inspired mode for my zettelkasten using org mode.
(use-package org-roam
  :defer t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/Brain/Knowledge/")
  (org-roam-index-file "index.org")
  :config
  (org-roam-setup))

(defun neshtea/org-gtd-file ()
  (interactive)
  (find-file (expand-file-name "~/Dropbox/Brain/Tasks/gtd.org")))

(def-with-leader
  "a o a" #'org-agenda-list
  "a o t" #'org-todo-list
  "a o c" #'consult-org-agenda
  "a o f" '(neshtea/org-gtd-file :which-key "open GTD file")
  "C c" #'org-capture

  "r n f" #'org-roam-node-find
  "r n c" #'org-roam-capture
  "r n i" #'org-roam-node-insert
  "r n t" #'org-roam-buffer-toggle
  "r n a a" #'org-roam-alias-add
  "r n a r" #'org-roam-alias-remove)

(def-local-with-leader
  :keymaps '(org-mode-map org-agenda-mode-map)
  "d d" #'org-deadline
  "d s" #'org-schedule
  "s A" #'org-archive-subtree
  "s l" #'org-demote-subtree
  "s h" #'org-promote-subtree
  "s k" #'org-move-subtree-up
  "s j" #'org-move-subtree-down
  "s r" #'org-refile
  "s s" #'org-sparse-tree
  "s S" #'org-sort-entries

  "i d" #'org-insert-drawer
  "i l" #'org-insert-link
  "i n" #'org-add-note
  "i t" #'org-set-tags-command
  
  "T c" #'org-toggle-checkbox
  "T i" #'org-toggle-inline-images
  "T t" #'org-todo
  "T e" '(neshtea/org-toggle-emphasis :which-key "toggle emphasis")

  "x o" #'org-open-at-point)

;; Work with nix files (syntax highlighting and indentation). 
(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))

(use-package default-text-scale
  :defer t
  :config
  (default-text-scale-mode))

(def-with-leader
  ;; '+' and '=': When switching between german and us layout, this
  ;; always bugs me.
  "f +" #'default-text-scale-increase
  "f =" #'default-text-scale-increase
  "f -" #'default-text-scale-decrease
  "f 0" #'default-text-scale-reset)

(use-package diff-hl
  :init (global-diff-hl-mode))

;; Magit (and Neogit for Neovim) are the very best tools for
;; interacting with git.
(use-package magit
  :hook ((git-commit-mode . evil-insert-state)  ; Start commit messages in insert mode.
	 ;; https://github.com/dgutov/diff-hl#magit
	 (magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))  
  :after diff-hl
  :config
  (setq-default git-magit-status-fullscreen t))

(def-with-leader
  "g i" #'magit-init
  "g s" #'magit
  "g b" #'magit-blame
  "g i" #'magit-gitignore)

(def-with-leader
  "t c i" #'timeclock-in
  "t c o" #'timeclock-out
  "f f t" #'timeclock-visit-timelog)

;; Used for golden-ration mode.
(use-package zoom
  :config
  ;; Resize the selected window using the golden ratio:
  ;; https://github.com/cyrus-and/zoom#example-configurations
  (setq zoom-size '(0.618 . 0.618)))

(def-with-leader "z z" #'zoom-mode)

(use-package helpful
  :after evil)

(def-with-leader
  "h f" #'helpful-callable
  "h v" #'helpful-variable
  "h k" #'helpful-key
  "h p" #'helpful-at-point)

;;;; Elixir
(use-package elixir-mode
  :hook (elixir-mode . (lambda () 
			 (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package alchemist
  ;:custom
  ;; See https://alchemist.readthedocs.io/en/latest/configuration/
  ;; NOTE I don't want to pollute my global environment with those
  ;; specific tools but rather have a nix-shell with these things
  ;; installed.
  ;(alchemist-mix-command (expand-file-name "~/.nix-profile/bin/mix"))
  ;(alchemist-iex-program-name (expand-file-name "~/.nix-profile/bin/iex"))
  ;(alchemist-execute-command (expand-file-name "~/.nix-profile/bin/elixir"))
  ;(alchemist-compile-command (expand-file-name "~/.nix-profile/bin/elixirc"))
  )

(def-local-with-leader
  :keymaps 'elixir-mode-map
  "a x" #'alchemist-mix
  "a c" #'alchemist-mix-compile
  "a r" #'alchemist-mix-run

  ;; hex
  "a X i" #'alchemist-hex-info-at-point
  "a X I" #'alchemist-hex-info
  "a X s" #'alchemist-hex-search

  ;; tests
  "a t"     #'alchemist-mix-test
  "a m t f" #'alchemist-mix-test-file
  "a m t b" #'alchemist-mix-test-this-buffer
  "a m t ." #'alchemist-mix-test-at-point

  ;; documentation
  "a h h" #'alchemist-help
  "a h e" #'alchemist-help-search-at-point

  ;; jump to definition
  "a g d" #'alchemist-goto-definition-at-point)

(use-package hl-todo
  :init
  (add-hook 'after-init-hook 'global-hl-todo-mode))

;; (def-local-with-leader
;;   :keymaps 'clojure-mode-map
;;   "u i" #'lsp-ui-imenu
;;   "f r" #'lsp-find-references
;;   "l a" #'lsp-execute-code-action
;;   "l r" #'lsp-rename
;;   "l s" #'lsp-rust-analyzer-status
;;   "l w r" #'lsp-workspace-restart
;;   "l w s" #'lsp-workspace-shutdown)

;; (use-package lsp-mode
;;   :defer t
;;   :commands lsp
;;   :custom
;;   (lsp-rust-analyzer-cargo-watch-command "check")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   :hook ((elixir-mode . lsp)
;; 	 (tuareg-mode . lsp)
;; 	 (lsp-mode . lsp-ui-mode)))

;; (use-package lsp-ui
;;   :defer t
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))

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

(use-package envrc
  :defer t
  :init (envrc-global-mode))

;;; Haskell
(use-package haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-interactive-popup-errors nil)
  (haskell-compiler-type 'cabal)
  (haskell-process-type 'cabal)
  (haskell-stylish-on-save t)
  :hook (haskell-mode . interactive-haskell-mode))

;; (use-package lsp-haskell
;;   :custom
;;   (lsp-haskell-server-path "haskell-language-server")
;;   (lsp-haskell-formatting-provider "stylish-haskell"))

(def-local-with-leader
  :keymaps 'interactive-haskell-mode-map
  "i i" '(haskell-navigate-to-imports-go :which-key "go to imports")
  "i r" '(haskell-navigate-imports-return :which-key "return from imports"))

(use-package hledger-mode
  :defer t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :hook (hledger-view-mode . #'hl-line-mode)
  :custom
  (hledger-jfile (expand-file-name "~/Dropbox/Brain/Finance/ledger2022.journal"))
  :config
  (add-to-list 'company-backends 'hledger-company))

(use-package tuareg
  :defer t)

(use-package racket-mode
  :defer t
  :hook (racket-mode . enable-paredit-mode))

(use-package rust-mode
  :defer t
  :custom
  (rust-format-on-save t))

;;; Purescript
(use-package psc-ide
  :defer t)

(defun purs-mode-hook ()
  (psc-ide-mode)
  ; (company-mode)  ; NOTE Globally enabled anyway
  (turn-on-purescript-indentation))

(use-package purescript-mode
  :defer t
  :hook (purescript-mode . purs-mode-hook))

(use-package eglot
  :defer t)

(def-with-leader
  ; Eglot Connect
  "e c" #'eglot
  ; Eglot Refactor Rename
  "e r" #'eglot-rename
  ; Eglot Code Actions
  "e a" #'eglot-code-actions)

(provide 'init)
;;; init.el ends here
