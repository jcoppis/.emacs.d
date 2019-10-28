(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))


  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(package-refresh-contents)

(defvar my-packages '(use-package
                      restclient
                      auto-yasnippet
                      emmet-mode
                      magit
                      counsel
		                  hydra
		                  lispy
		                  company
                      rg
                      flycheck
                      js2-mode
                      rainbow-delimiters
                      clojure-mode
                      cider
                      slime
                      sql-indent))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; Set default lisp
(setq inferior-lisp-program "/bin/sbcl")

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Keep themes in separate directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(ivy-mode 1)

(setq company-idle-delay .2)
(setq company-dabbrev-downcase nil)
(global-company-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)

;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

;; Ivy-resume and other commands
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" (or user-login-name "jcoppis")))
(add-to-list 'load-path user-settings-dir)

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
(mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
