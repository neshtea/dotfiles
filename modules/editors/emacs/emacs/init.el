;; Load up Org Mode and Babel and load the configuration file
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(require 'org-install)
(org-babel-load-file (expand-file-name "configuration.org" dotfiles-dir))

;; (expand-file-name "configuration.org" (buffer-file-name))
