(setq user-full-name "Coppis Javier"
      user-mail-address "javier.coppis@gmail.com")

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)

;; Disallow scrolling with mouse wheel
(when window-system
  (mouse-wheel-mode -1))

;; No more scrolling surprises
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))

;; No graphics please o.O
(setq speedbar-use-images nil)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; theme
(load-theme 'zenburn t)

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 2) ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; highlight the current line
(global-hl-line-mode +1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
