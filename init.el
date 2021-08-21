;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "profiles" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-sitepkgs) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)


;; Framework
(require 'init-frame-hooks)


;; Misc
(require 'init-misc)
(require 'init-osx-keys)


;; User Session
(require 'init-recentf)
(require 'init-sessions)


;; Window & Buffer
(require 'init-mmm)
(require 'init-xterm)
(require 'init-themes)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-uniquify)
(require 'init-gui-frames)
(require 'init-minibuffer)
(require 'init-hippie-expand)


;; Navigate & Search
(require 'init-grep)
(require 'init-dired)
(require 'init-isearch)
(require 'init-projectile)


;; Vim Mode
(require 'init-vim-mode)


;; Spelling & Auto Completion
(require 'init-company)
(require 'init-flycheck)
(require 'init-whitespace)
(require 'init-editing-utils)
(when *spell-check-support-enabled*
  (require 'init-spelling))


;; Code Version Control
(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)


;; Basic DevOps
(require 'init-sql)
(require 'init-docker)
(require 'init-crontab)
(require 'init-terraform)
(maybe-require-package 'nginx-mode)


;; Markup Language
(require 'init-csv)
(require 'init-org)
(require 'init-toml)
(require 'init-yaml)
(require 'init-nxml)
(require 'init-textile)
(require 'init-markdown)


;; Web Develop Language
(require 'init-css)
(require 'init-html)
(require 'init-http)
(require 'init-purescript)
(require 'init-javascript)


;; Generic Language
(require 'init-nix)
(require 'init-rust)
(require 'init-lisp)
(require 'init-python)
(require 'init-haskell)
(require 'init-paredit)
(require 'init-compile)


;; Note Book
(require 'init-ledger)


;; Extra packages
(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)

(when *is-a-mac*
  (require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

(require 'init-direnv)


;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
