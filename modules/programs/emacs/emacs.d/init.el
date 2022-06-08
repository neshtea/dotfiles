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
 max-lisp-eval-depth 10000)

;; "When you visit a file, point goes to the last place where it was
;; when you previously visited the same file."
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Don't show errors when loading the custom file.
(load custom-file 'no-error)

;; Set the font.
(set-frame-font "Iosevka-14" t t)
;; (set-face-attribute 'default nil
;;                     :font "Iosevka"
;;                     :weight 'regular
;;                     :height 140)
;; (set-face-attribute 'fixed-pitch nil
;;                     :font "Iosevka"
;;                     :weight 'regular
;;                     :height 140)

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
  (snowcrash/switch-theme 'doom-gruvbox))

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
  :after evil
  :config
  (evil-collection-init))

;; Same as evil-collection, but specific to org-mode.
;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :defer t
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

(use-package org-indent
  :defer t
  :after org)

(use-package org
  :hook (org-mode . auto-fill-mode)

  :custom
  (org-adapt-indentation nil)
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-return-follows-link t)
  (org-startup-folded 'content)
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
  :defer t
  :mode "\\.nix\\'"
  :hook (before-save . nix-format-before-save))

;; Nicer modeline with symbols, etc.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25))

(use-package all-the-icons)

(def-with-leader
  "f +" #'text-scale-increase
  "f =" #'text-scale-increase
  "f -" #'text-scale-decrease
  "f 0" #'text-scale-adjust)

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

(def-local-with-leader
  :keymaps 'clojure-mode-map
  "u i" #'lsp-ui-imenu
  "f r" #'lsp-find-references
  "l a" #'lsp-execute-code-action
  "l r" #'lsp-rename
  "l s" #'lsp-rust-analyzer-status
  "l w r" #'lsp-workspace-restart
  "l w s" #'lsp-workspace-shutdown)

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "check")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :hook ((elixir-mode . lsp)
	 (tuareg-mode . lsp)
	 (lsp-mode . #'lsp-ui-mode)))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

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
  (haskell-stylish-on-save 't)
  :hook (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "haskell-language-server")
  (lsp-haskell-formatting-provider "stylish-haskell"))

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

(use-package lsp-rescript
  :defer t)

(use-package racket-mode
  :defer t
  :hook (racket-mode . enable-paredit-mode))

;; (use-package mu4e
;;   :defer t
;;   :commands (mu4e)
;;   :custom
;;   (mail-user-agent 'mu4e-user-agent)
;;   (mu4e-drafts-folder "/drafts")
;;   (mu4e-attachment-dir "~/Downloads")
;;   (mu4e-contexts
;;    `(,(make-mu4e-context
;;        :name "posteo"
;;        :match-func (lambda (msg)
;; 		     (when msg
;; 		       (string-prefix-p "/posteo"
;; 					(mu4e-message-field msg :maildir)
;; 					t)))
;;        :vars '((user-email-address  . "marco.schneider@posteo.de")
;; 	       (mu4e-compose-signature . nil)
;; 	       ;; See
;; 	       ;; https://posteo.de/en/help/how-do-i-set-up-posteo-in-an-email-client-pop3-imap-and-smtp
;; 	       ;; for special folder names.
;; 	       (mu4e-sent-folder . "/posteo/Sent")
;; 	       (mu4e-trash-folder . "/posteo/Trash")
;; 	       (mu4e-drafts-folder . "/posteo/Drafts")))))
;;   (mu4e-bookmarks '((:name "Inbox" :query "maildir:/posteo/Inbox" :key ?i)
;; 		    (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
;; 		    (:name "Sent" :query "maildir:/posteo/Sent" :key ?s )))
;;   (mu4e-get-mail-command "mbsync -a")
;;   ;; General email stuff, nothing to do with mu4e in particular.
;;   (message-send-mail-function #'message-send-mail-with-sendmail)
;;   (send-mail-function #'message-send-mail-with-sendmail)
;;   (message-sendmail-envelope-from 'header))

(provide 'init)
;;; init.el ends here
