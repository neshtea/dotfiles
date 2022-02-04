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
(defun my--switch-theme (name)
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
  (my--switch-theme 'doom-one))

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
  "s t" '(my--switch-theme :which-key "change theme"))

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
  "/"   #'consult-git-grep)

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
(use-package dockerfile-mode)

;; Highighting and indentation for yaml.
(use-package yaml-mode)

;; Complete anything -- auto completion framework.
(use-package company :hook ((after-init . global-company-mode)))

;; Working Clojure needs almost no configuration, just some nice
;; packages (ciderm, clj-refactor, clojure-mode).
(use-package clj-refactor)
(use-package clojure-mode)
(use-package cider)

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
  :config
  (add-hook 'org-mode-hook #'auto-fill-mode)

  ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  (setq org-adapt-indentation t
	org-hide-leading-stars t
	org-return-follows-link t
	;; Use org-mode for the initial *scratch* buffer.
	initial-major-mode 'org-mode
	;; Start any org-file in "overview"-mode.
	org-startup-folded t
	org-agenda-files '("~/Dropbox/Brain/Tasks/gtd.org")
	org-capture-templates '(("t" "Todo [inbox/work]" entry
				 (file+headline "~/Dropbox/Brain/Tasks/gtd.org" "INBOX")
				 "* TODO %i%? \n  %U")
				("c" "Capture [inbox]" entry
				 (file+headline "~/Dropbox/Brain/Tasks/gtd.org" "INBOX")
				 "* TODO %i%? \n  %a"))
	org-refile-targets '(("~/Dropbox/Brain/Tasks/gtd.org" :maxlevel . 1)
			     ("~/Dropbox/Brain/Tasks/lists.org" :maxlevel . 2)
			     ("~/Dropbox/Brain/Tasks/someday.org" :level . 1))
	;; When the state of a section headline changes, log the
	;; transition into the headlines drawer.
	org-log-into-drawer 'LOGBOOK
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "INPROGRESS(p!)" "|" "DONE(d!)" "CANCELLED(c!)"))))

;; Roam inspired mode for my zettelkasten using org mode.
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/Dropbox/Brain/Knowledge/"
	org-roam-index-file "index.org")
  (add-hook 'after-init-hook 'org-roam-mode)
  (require 'org-roam-protocol)
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
(use-package nix-mode :mode "\\.nix\\'")

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
  :hook (git-commit-mode . evil-insert-state)  ; Start commit messages in insert mode.
  :after diff-hl
  :config
  (setq-default git-magit-status-fullscreen t)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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

(use-package helpful)

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

(setq user-full-name "Marco Schneider")

(let ((mu4epath
       (concat
        (f-dirname
         (file-truename
          (executable-find "mu")))
        "/../share/emacs/site-lisp/mu4e")))
  (when (and
         (string-prefix-p "/nix/store/" mu4epath)
         (file-directory-p mu4epath))
    (add-to-list 'load-path mu4epath)))

(use-package mu4e
  :ensure nil
  :defer t
  :commands (mu4e)
  ;; Automatically break (and support M-q) in message mode.
  :hook (message-mode . #'auto-fill-mode)
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-confirm-quit nil)
  ;; I don't sync drafts, so I don't care.
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-fields '((:human-date . 12)
			      (:flags . 6)
			      (:maildir . 15)
			      (:mailing-list . 10)
			      (:from . 22)
			      (:subject)))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-policy 'ask)
  (setq mu4e-get-mail-command "mbsync -a")
  (setq message-send-mail-function #'message-send-mail-with-sendmail)
  (setq send-mail-function #'message-send-mail-with-sendmail)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq mail-specify-envelope-from 'header)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-contexts
	`(,(make-mu4e-context
	    :name "posteo"
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/posteo"
					     (mu4e-message-field msg :posteo)
					     t)))
	    :vars '((user-mail-address "marco.schneider@posteo.de")
		   (mu4e-compose-signature . nil)
		   (mu4e-sent-folder . "/posteo/Sent")
		   (mu4e-trash-folder . "/posteo/Trash")
		   (mu4e-refile-folder . (lambda (msg)
					   (let* ((date (mu4e-message-field-at-point :date))
						  (year (decoded-time-year (decode-time date))))
					     (concat "/posteo/Archive/"
						     (number-to-string year)))))))
	  ,(make-mu4e-context
	    :name "ag"
	    :match-func (lambda (msg)
			  (when msg
			    (string-prefix-p "/ag"
					     (mu4e-message-field msg :maildir)
					     t)))
	    :vars `((user-mail-address . "marco.schneider@active-group.de")
		   (mu4e-compose-signature . ,(concat "Marco Schneider\n"
						      "marco.schneider@active-group.de\n"
						      "+49 7071 70896 81\n"
						      "Hechinger Str. 12/1\n"
						      "72072 Tübingen\n"
						      "Registergericht: Amtsgericht Stuttgart, HRB 224404\n"
						      "Geschäftsführer: Dr. Michael Sperber"))
		   (mu4e-sent-folder "/ag/Sent")
		   (mu4e-trash-folder . "/ag/Trash")
		   (mu4e-refile-folder . (lambda (msg)
					   (let* ((date (mu4e-message-field-at-point :date))
						  (year (decoded-time-year (decode-time date))))
					     (concat "/ag/Archives/"
						     (number-to-string year))))))))))

(def-with-leader
  "m m" #'mu4e)

(provide 'init)
;;; init.el ends here
