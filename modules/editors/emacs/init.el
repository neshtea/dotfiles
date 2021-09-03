;;; init.el --- Summary
;;; hide certain modes in modeline

;; Commentary:
;;; Configuration for Emacs

;;;; GENERAL

;; Don't show the standart Emacs startup screen.
(setq inhibit-splash-screen t)

;; Don't make backup files the first time it is saved.
(setq make-backup-files nil)

;; Don't clutter init.el with custom but instead write it to
;; ~/.emacs.d/custom.el.
(setq custom-file (expand-file-name "~/.config/emacs/custom.el"))

;; Remap some mac-specific keys.
(setq ns-alternate-modifier 'none
      ns-command-modifier 'meta
      ns-function-modifier 'super)

;; Turn off all alarms completely.
;; See https://www.emacswiki.org/emacs/AlarmBell.
(setq ring-bell-function 'ignore)

;; Always prefer the "newer" version of a file.
(setq load-prefer-newer t)

;; "When you visit a file, point goes to the last place where it was
;; when you previously visited the same file."
;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Don't show errors when loading the custom file.
(load custom-file 'no-error)

;; Set the font.
(set-frame-font "Roboto Mono-14" t t)

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

;; General is used to define keybindings (replaces evil-leader).
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

;; Very nice thems in terms of readability and contrast, perhaps a
;; little dull.
(use-package modus-themes
  :defer t)

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
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; Easily restart emacs. Sadly useful from time to time.
(use-package restart-emacs)

;; Perhaps this is a good point to make a shortcut to my home-manager
;; config file (and some more files, too).
(defun home-manager-visit-config ()
  (interactive)
  (find-file (expand-file-name "~/.config/nixpkgs/home.nix")) )

(defun emacs-visit-init-el ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

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
  "f f n" #'home-manager-visit-config
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

;;; selectrum-prescient
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
  "/" #'consult-git-grep)

;; marginalia annotates completion candidates in the completion at
;; point buffer. Plays nicely with consult, etc.
(use-package marginalia :init (marginalia-mode))

;; Easily find projects and files within projects.
(use-package projectile
  :init
  (projectile-mode +1))

(def-with-leader
  "p p" #'projectile-switch-project
  "p f" #'project-find-file)

;; Highlights docker files and provides some basic commands (none of
;; which I use).
(use-package dockerfile-mode
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; Highighting and indentation for yaml.
(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Complete anything -- auto completion framework.
(use-package company
  :hook ((after-init . global-company-mode)))

;; Lets us define several aliases to use in message-mode (and
;; notmuch).
(use-package gnus-alias
  :config
  ;; SEE https://notmuchmail.org/emacstips/#index13h2
  (setq gnus-alias-identity-alist
	'(("posteo"
	   nil ;; Does not refer to any other identity
	   "Marco Schneider <marco.schneider@posteo.de>" ;; Sender address
	   nil ;; No organization header
	   nil ;; No extra headers
	   nil ;; No extra body text
	   "~/.signature")
	  ("ag"
	   nil
	   "Marco Schneider <marco.schneider@active-group.de>"
	   "Active Group GmbH"
	   nil
	   nil
	   "~/.signature.work")))
  ;; Use "home" identity by default
  (setq gnus-alias-default-identity "posteo")
  ;; Define rules to match work identity
  (setq gnus-alias-identity-rules
	'(("ag" ("any" "@active-group.de" both) "ag")))
  ;; Determine identity when message-mode loads
  (add-hook 'message-setup-hook #'gnus-alias-determine-identity))

;; notmuch
;; The HEY-part is mostly a straight copy of
;; https://gist.github.com/vedang/26a94c459c46e45bc3a9ec935457c80f

;; Must point to your maildir root.
(defvar notmuch-mail-dir (concat (getenv "HOME") "/Mail"))

(use-package notmuch
  :config
  (require 'ol-notmuch)
  (setq user-mail-address (notmuch-user-primary-email)
	user-full-name (notmuch-user-name)

	message-send-mail-function 'message-send-mail-with-sendmail
	message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote:\n"
	message-citation-line-function 'message-insert-formatted-citation-line

	notmuch-show-logo nil
	notmuch-show-imenu-indent nil
	notmuch-always-prompt-for-sender 't

	;; Replying
	notmuch-mua-cite-function 'message-cite-original-without-signature
	;; we substitute sendmail with msmtp
	sendmail-program (expand-file-name "~/.nix-profile/bin/msmtp")
	message-sendmail-envelope-from 'header
	message-kill-buffer-on-exit t
	mail-specify-envelope-from 'header
	mail-envelope-from 'header
	mail-user-agent 'message-user-agent

	;; Move sent messages to the corrent sent folders for later sync to the
	;; IMAP server (and set the correct tags).
	notmuch-fcc-dirs '(("marco.schneider@posteo.de" . "posteo/Sent -unread -inbox +sent +personal")
			   ("marco.schneider@active-group.de" . "ag/Sent -unread -inbox +sent +work"))

	notmuch-hello-thousands-separator ""
	notmuch-archive-tags '("-inbox" "-unread" "+archived")
	notmuch-show-mark-read-tags '("-inbox" "-unread" "+archived")
	;; appearently, this only only works when set via custom...
	;; notmuch-search-oldest-first nil
	notmuch-show-indent-content nil
	notmuch-hooks-dir (expand-file-name ".notmuch/hooks" notmuch-mail-dir))
  ;; My Notmuch start screen:
  (progn
    (setq notmuch-saved-searches nil)
    (push '(:name "Unread"
		  :query "tag:unread"
		  :key "u"
		  :sort-order newest-first)
	  notmuch-saved-searches)
    (push '(:name "Unscreened"
		  :query "tag:inbox AND NOT tag:screened"
		  :key "s"
		  :sort-order newest-first)
	  notmuch-saved-searches)
    (push '(:name "The Papertrail"
		  :query "tag:/ledger/"
		  :key "p"
		  :sort-order newest-first)
	  notmuch-saved-searches)
    (push '(:name "The Feed"
		  :query "tag:thefeed"
		  :key "f"
		  :search-type 'tree
		  :sort-order newest-first)
	  notmuch-saved-searches)
    (push '(:name "Previously Seen"
		  :query "tag:screened AND NOT tag:unread"
		  :key "I"
		  :sort-order newest-first)
	  notmuch-saved-searches)
    (push '(:name "Inbox"
		  :query "tag:inbox AND tag:screened"
		  :key "i"
		  :search-type 'tree
		  :sort-order newest-first)
	  notmuch-saved-searches)
    (push '(:name "Sent"
		  :query "tag:sent"
		  :key "t"
		  :sort-order newest-first)
	  notmuch-saved-searches)))

(defun hey/notmuch-add-addr-to-db (nmaddr nmdbfile)
  "Add the email address NMADDR to the db-file NMDBFILE."
  (append-to-file (format "%s\n" nmaddr) nil nmdbfile))

(defun hey/notmuch-tree-get-from ()
  "A helper function to find the email address for the given email.
  Assumes `notmuch-tree-mode'."
  (plist-get (notmuch-tree-get-prop :headers) :From))

(defun hey/notmuch-search-get-from ()
  "A helper function to find the email address for the given email."
  (let ((notmuch-addr-sexp (car
                             (notmuch-call-notmuch-sexp "address"
                                                        "--format=sexp"
                                                        "--format-version=1"
                                                        "--output=sender"
                                                        (notmuch-search-find-thread-id)))))
    (plist-get notmuch-addr-sexp :name-addr)))

(defun hey/notmuch-get-from ()
  "Find the From email address for the email at point."
  (car (notmuch-clean-address (cond
                                ((eq major-mode 'notmuch-show-mode)
                                 (notmuch-show-get-from))
                                ((eq major-mode 'notmuch-tree-mode)
                                 (hey/notmuch-tree-get-from))
                                ((eq major-mode 'notmuch-search-mode)
                                 (hey/notmuch-search-get-from))))))

(defun hey/notmuch-search-by-from (&optional no-display)
  "Show all emails sent from the sender of the current thread.
  NO-DISPLAY is sent forward to `notmuch-search'."
  (interactive)
  (notmuch-search (concat "from:" (hey/notmuch-get-from))
                  notmuch-search-oldest-first
                  nil
                  nil
                  no-display))

(defun hey/notmuch-tag-by-from (tag-changes &optional beg end refresh)
  "Apply TAG-CHANGES to all emails from the sender of the current thread.
  BEG and END provide the region, but are ignored. They are defined
  since `notmuch-search-interactive-tag-changes' returns them. If
  REFRESH is true, refresh the buffer from which we started the
  search."
  (interactive (notmuch-search-interactive-tag-changes))
  (let ((this-buf (current-buffer)))
    (hey/notmuch-search-by-from t)
    ;; This is a dirty hack since I can't find a way to run a
    ;; temporary hook on `notmuch-search' completion. So instead of
    ;; waiting on the search to complete in the background and then
    ;; making tag-changes on it, I will just sleep for a short amount
    ;; of time. This is generally good enough and works, but is not
    ;; guaranteed to work every time. I'm fine with this.
    (sleep-for 0.5)
    (notmuch-search-tag-all tag-changes)
    (when refresh
      (set-buffer this-buf)
      (notmuch-refresh-this-buffer))))

(defun hey/notmuch-move-sender-to-spam ()
  (interactive)
  (hey/notmuch-add-addr-to-db (hey/notmuch-get-from)
                              (format "%s/spam.db" notmuch-hooks-dir))
  (hey/notmuch-tag-by-from '("+spam" "+deleted" "+archived" "-inbox" "-unread" "-screened")))

(defun hey/notmuch-move-sender-to-screened ()
  (interactive)
  (hey/notmuch-add-addr-to-db (hey/notmuch-get-from)
                              (format "%s/screened.db" notmuch-hooks-dir))
  (hey/notmuch-tag-by-from '("+screened" "-unscreened")))

(defun hey/notmuch-move-sender-to-papertrail (tag-name)
  (interactive "sTag Name: ")
  (hey/notmuch-add-addr-to-db (format "%s %s"
                                      tag-name
                                      (hey/notmuch-get-from))
                              (format "%s/ledger.db" notmuch-hooks-dir))
  (let ((tag-string (format "+ledger/%s" tag-name)))
    (hey/notmuch-tag-by-from (list tag-string "+archived" "-inbox" "-unread"))))

(defun hey/notmuch-move-sender-to-thefeed ()
  (interactive)
  (hey/notmuch-add-addr-to-db (hey/notmuch-get-from)
                              (format "%s/thefeed.db" notmuch-hooks-dir))
  (hey/notmuch-tag-by-from '("+thefeed" "+archived" "-inbox")))

(defun hey/notmuch-reply-later ()
  "Capture this email for replying later."
  (interactive)
  (org-capture nil "r")
  ;; The rest of this function is just a nice message in the modeline.
  (let* ((email-subject (format "%s..."
                                (substring (notmuch-show-get-subject) 0 15)))
         (email-from (format "%s..."
                             (substring (notmuch-show-get-from) 0 15)))
         (email-string (format "%s (From: %s)" email-subject email-from)))
    (message "Noted! Reply Later: %s" email-string)))

(def-with-leader
  "a e m" #'notmuch-hello)

(def-local-with-leader
  :keymaps 'notmuch-search-mode-map
  "S" #'hey/notmuch-move-sender-to-spam
  "I" #'hey/notmuch-move-sender-to-screened
  "P" #'hey/notmuch-move-sender-to-papertrail
  "F" #'hey/notmuch-move-sender-to-thefeed
  "C" #'hey/notmuch-reply-later)

(def-local-with-leader
  :keymaps 'notmuch-show-mode-map
  "r r" #'notmuch-tree-reply-sender
  "r a" #'notmuch-tree-reply)

;; Automatically break (and support M-q) in message mode.
(add-hook 'message-mode-hook #'auto-fill-mode)

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

;; Not much to say here, just a lot to configure.
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
	org-agenda-files '("~/Box/Brain/Tasks/inbox.org"
			   "~/Box/Brain/Tasks/gtd.org"
			   "~/Box/Brain/Tasks/tickler.org")
	org-capture-templates '(("t" "Todo [inbox/work]" entry
				 (file "~/Box/Brain/Tasks/inbox.org")
				 "* TODO %i%? \n  %U")
				("c" "Capture [inbox]" entry
				 (file "~/Box/Brain/Tasks/inbox.org")
				 "* TODO %i%?\n  %a")
				("r" "Respond to email" entry
				 (file "~/Box/Brain/Tasks/inbox.org")
				 "* TODO Respond to %:from on %:subject  :email: \nSCHEDULED: %t\n%U\n%a\n"
				 :immediate-finish t))
	org-refile-targets '(("~/Box/Brain/Tasks/gtd.org" :maxlevel . 3)
			     ("~/Box/Brain/Tasks/lists.org" :maxlevel . 2)
			     ("~/Box/Brain/Tasks/someday.org" :level . 1))
	;; When the state of a section headline changes, log the
	;; transition into the headlines drawer.
	org-log-into-drawer 'LOGBOOK
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "INPROGRESS(p!)" "|" "DONE(d!)" "CANCELLED(c!)"))))

(use-package org-contrib
  :config
  (require 'ol-notmuch))

;; Roam inspired mode for my zettelkasten using org mode.
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/Box/Brain/Knowledge/"
	org-roam-index-file "index.org")
  (add-hook 'after-init-hook 'org-roam-mode)
  (require 'org-roam-protocol)
  (org-roam-setup))

;; TODO Maybe use =notdeft=? But it's harder to install and configure...
;; TODO Perhaps consult-git-grep is enough?
;; Deft makes it easy to do full-text search on a certain directory
;; (and it's children).
(use-package deft
  :after org
  :hook (deft-mode . evil-insert-state)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(def-with-leader
  "a o a" #'org-agenda-list
  "a o t" #'org-todo-list
  "a o c" #'consult-org-agenda
  "C c" #'org-capture

  "r n f" #'org-roam-node-find
  "r n c" #'org-roam-capture
  "r n i" #'org-roam-insert
  "r n t" #'org-roam-buffer-toggle
  "r n g" #'org-id-get-create
  "r n a a" #'org-roam-alias-add
  "r n a r" #'org-roam-alias-remove
  )

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

  "x o" #'org-open-at-point
  )

;; Work with nix files (syntax highlighting and indentation). 
(use-package nix-mode
  :mode "\\.nix\\'")

;; Nicer modeline with symbols, etc.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25))

;; Define little states that stay in the minibuffer.
(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "C-#")
    "zoom"
    ("+" text-scale-increase "increase")
    ("-" text-scale-decrease "decrease")
    ("0" text-scale-adjust "reset")))

(def-with-leader
  "f z" #'hydra-zoom/body)

(use-package diff-hl
  :init (global-diff-hl-mode))

;; Magit (and Neogit for Neovim) are the very best tools for
;; interacting with git.
(use-package magit
  :hook (git-commit-mode . evil-insert-state)
  :after diff-hl
  :init
  ;; (evil-leader/set-key
  ;;   "gi" #'magit-init
  ;;   "gs" #'magit)
  :config
  (setq-default git-magit-status-fullscreen t)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(def-with-leader
  "g i" #'magit-init
  "g s" #'magit)

(use-package hledger-mode
  :mode "\\.journal\\'"
  :commands hledger-enable-reporting
  :load-path "straight/repos/hledger-mode"
  :hook (hledger-view-mode . #'hl-line-mode)
  :config
  (setq hledger-jfile "~/.hledger.journal")
  (add-to-list 'company-backends 'hledger-company))

(defun hledger-visit-jfile ()
  (interactive)
  (find-file "~/Box/Brain/Finance/ledger.journal"))

(defun hledger/next-entry ()
  "Move to next entry and pulse."
  (interactive)
  (hledger-next-or-new-entry)
  (hledger-pulse-momentary-current-entry))

(defun hledger/prev-entry ()
  "Move to last entry and pulse."
  (interactive)
  (hledger-backward-entry)
  (hledger-pulse-momentary-current-entry))

(def-with-leader
  "f f e" #'hledger-visit-jfile)

(def-local-with-leader
  :keymaps 'hledger-mode-map
  "r" #'hledger-run-command
  "e" #'hledger-jentry
  "p" #'hledger/prev-entry
  "n" #'hledger/next-entry)

;; Timeclock allows me to clock in and out of projects and store it
;; in a format that hledger understands.
(use-package timeclock
  :config
  (setq timeclock-file (expand-file-name  "~/Box/Brain/Timetracking/tracking.timeclock")
	;; 32h/Woche
	;; 384min/Tag
	;; 23040sec/tag
	timeclock-workday 23040
	timeclock-project-list '(general  ; General work for Active Group
				             phoenix)))

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

(def-with-leader
  "z z" #'zoom-mode)

(use-package helpful)

(def-with-leader
  "h f" #'helpful-callable
  "h v" #'helpful-variable
  "h k" #'helpful-key
  "h p" #'helpful-at-point)

;;; LATEX Support
(use-package tex-mik
  ;; :hook (org-mode . evil-org-mode)
  :hook ((LaTeX-mode . auto-fill-mode)
	 (LaTeX-mode . LaTeX-math-mode))
  :config
  ;; Automatically compile to PDF.
  (setq TeX-PDF-mode t))


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

;; lua-mode
;; https://github.com/immerrr/lua-mode
(use-package lua-mode)

(use-package windmove)

(def-with-leader
  "<left>"  #'windmove-left
  "<right>" #'windmove-right
  "<up>"    #'windmove-up
  "<down>"  #'windmove-down)

(provide 'init)
;;; init.el ends here
