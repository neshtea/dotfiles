;; -*- lexical-binding: t; -*-
;; Emacs startup can be significantly sped up by reducing the number  
;; of garbage collections that take place during initialization.  The
;; default `gc-cons-threshold' of 80 kilobytes is way too low for any
;; more recent system.  Still it's beneficial to reset this temporary
;; value back to a lower number after initialization.  That way the GC
;; pause won't be as long when working within Emacs.  `gcmh-mode' will
;; be started at the end of my init file, which will then control the
;; GC behavior.
(setq gc-cons-threshold most-positive-fixnum)

;; Users of Emacs versions >= 27 will want to add:
(setq package-enable-at-startup nil)
;; to their early init-file to prevent package.el loading packages prior to
;; their init-file loading.
