;;; init.el --- Summary
;;; hide certain modes in modeline

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
 load-prefer-newer t)

;; "When you visit a file, point goes to the last place where it was
;; when you previously visited the same file."
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Don't show errors when loading the custom file.
(load custom-file 'no-error)

;; Set the font.
(set-frame-font "Iosevka-14" t t)

;; Disable menubar/scrollbar/toolbar.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Answer y or n to yes-or-no questions.
;; http://pragmaticemacs.com/emacs/make-all-prompts-y-or-n/
(fset 'yes-or-no-p 'y-or-n-p)

;; Alwas show matching parens.
(show-paren-mode 1)

;; We install packages via nix home-manager, but we still configure
;; them via use-package
(require 'use-package)

;; Especially on MacOS, the exec path is always wrong.  This package
;; tries to fix that.
(use-package exec-path-from-shell
  :config
  ;; Point to the fish shell installed via home-manager/nix.
  (setenv "SHELL" (expand-file-name "~/.nix-profile/bin/fish"))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH")))

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
(defun snowcrash/switch-theme (name)
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

;; Some themes to choose from besides the basic ones.
(use-package doom-themes
  :defer t
  :init
  ;; Make doom-one the default.
  (snowcrash/switch-theme 'doom-opera))

;; Distinguish file-visiting buffers from other ones. Only works with
;; doom-themes (and maybe a few others).
(use-package solaire-mode
  :init
  (solaire-global-mode +1))

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
  :after (evil)
  :config
  (evil-collection-init))

;; Same as evil-collection, but specific to org-mode.
;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
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
  "e"   #'find-file
  "k"   #'kill-buffer
  "TAB" #'er-switch-to-previous-buffer
  "t l" #'global-display-line-numbers-mode
  "t f" #'display-fill-column-indicator-mode
  "s h" #'eshell
  "f s" #'toggle-fullscreen
  "q r" #'restart-emacs
  "SPC" '(execute-extended-command :which-key "M-x")
  "s t" '(snowcrash/switch-theme :which-key "change theme"))

;; Paredit allows to easily work with parens. Especially useful in
;; LISP-like languages.
(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  ;; TODO :hook
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'rainbow-delimiters-mode))

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

(use-package yasnippet
  :init (yas-global-mode 1)
  :diminish yas-minor-mode)

;; See https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
;; (use-package corfu
;;   :general
;;   (:keymaps 'corfu-map
;; 	    "C-n" #'corfu-next
;; 	    "C-p" #'corfu-previous
;; 	    "<escape>" #'corfu-quit
;; 	    "<return>" #'corfu-insert
;; 	    "M-d" #'corfu-show-documentation
;; 	    "M-l" #'corfu-show-location)
;;   (:keymaps '(corfu-map general-override-mode-map)
;; 	    :states 'insert
;; 	    "H-SPC" #'corfu-insert-seperator
;; 	    "SPC" #'corfu-insert-separator)
;;   :custom
;;   (corfu-auto nil)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.25)
;;   (corfu-min-width 80)
;;   (corfu-max-width corfu-min-width)  ; Always keep the same width
;;   (corfu-count 14)
;;   (corfu-scroll-margin 4)
;;   (corfu-cycle nil)
;;   (corfu-echo-documentation t)
;;   (corfu-preselect-first t)
  
;;   :config
;;   (corfu-global-mode)
;;   ;(tab-always-indent 'complete)
;;   ;(completion-cycle-threshold nil)
;;   )

;; (use-package no-littering)

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-ues-icons t)
;;   (kind-icon-default-face 'corfu-default)
;;   (kind-icon-blend-background nil)
;;   (kind-icon-blend-frac 0.08)

;;   (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Working Clojure needs almost no configuration, just some nice
;; packages (ciderm, clj-refactor, clojure-mode).
(use-package clj-refactor
  :defer t)
(use-package clojure-mode
  :defer t)
(use-package cider
  :defer t)

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

;; https://github.com/clojure-emacs/clj-refactor.el
(defun custom-clojure-mode-hook ()
  "Turn on clj-refactor in clojure-mode."
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(add-hook 'clojure-mode-hook #'custom-clojure-mode-hook)

(use-package org
  :hook (org-mode . #'auto-fill-mode)
  :custom
  (org-adapt-indentation t)
  (org-hide-leading-starts t)
  (org-return-follows-link t)
  (org-startup-folded 'content)
  (org-agenda-files '("~/Dropbox/Brain/Tasks/gtd.org"))
  (org-capture-templates '(("t" "Todo [inbox/work]" entry
			    (file+headline "~/Dropbox/Brain/Tasks/gtd.org" "INBOX")
			    "* TODO %i%? \n  %U")
			   ("c" "Capture [inbox]" entry
			    (file+headline "~/Dropbox/Brain/Tasks/gtd.org" "INBOX")
			    "* TODO %i%? \n  %a")))
  (org-refile-targets '(("~/Dropbox/Brain/Tasks/gtd.org" :maxlevel . 1)
			("~/Dropbox/Brain/Tasks/lists.org" :maxlevel . 2)
			("~/Dropbox/Brain/Tasks/someday.org" :level . 1)))
	;; When the state of a section headline changes, log the
	;; transition into the headlines drawer.
  (org-log-into-drawer 'LOGBOOK)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)"))))

;; Roam inspired mode for my zettelkasten using org mode.
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/Brain/Knowledge/")
  (org-roam-index-file "index.org")
  :config
  (org-roam-setup))

(def-with-leader
  "a o a" #'org-agenda-list
  "a o t" #'org-todo-list
  "a o c" #'consult-org-agenda
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

  "x o" #'org-open-at-point)

;; Work with nix files (syntax highlighting and indentation). 
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))

;; Nicer modeline with symbols, etc.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25))

(def-with-leader
  "f +" #'text-scale-increase
  "f -" #'text-scale-decrease
  "f 0" #'text-scale-adjust)

(use-package diff-hl :init (global-diff-hl-mode))

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
  "g s" #'magit)

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
  :config
  ;; See https://alchemist.readthedocs.io/en/latest/configuration/
  (setq alchemist-mix-command (expand-file-name "~/.nix-profile/bin/mix")
	alchemist-iex-program-name (expand-file-name "~/.nix-profile/bin/iex")
	alchemist-execute-command (expand-file-name "~/.nix-profile/bin/elixir")
	alchemist-compile-command (expand-file-name "~/.nix-profile/bin/elixirc")))

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

;;;; Rust
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(use-package rustic
  :defer t
  :hook (rustic-mode . #'snowcrash/rustic-mode-hook)
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(def-local-with-leader
  :keymaps 'clojure-mode-map
  "u i" #'lsp-ui-imenu
  "f r" #'lsp-find-references
  "l a" #'lsp-execute-code-action
  "l r" #'lsp-rename
  "l s" #'lsp-rust-analyzer-status
  "l w r" #'lsp-workspace-restart
  "l w s" #'lsp-workspace-shutdown)

(defun snowcrash/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "check")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :hook ((elixir-mode . lsp)
	 (rustic-mode . lsp)
	 (tuareg-mode . lsp)
	 (lsp-mode . #'lsp-ui-mode)))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package envrc
  :defer t
  :init (envrc-global-mode))

;;; Haskell
(use-package haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-interactive-popup-errors nil)
  :hook (haskell-mode . interactive-haskell-mode))

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

(provide 'init)
;;; init.el ends here
