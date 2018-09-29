;;; Code:
;; Start off with giant gc threshold
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

;; Use command as meta on mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; Don't dump custom variables into init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Be fast - from aaron bieber
(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Bootstrap `use-package'
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Fix path on mac
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; Visual stuff
(set-scroll-bar-mode 'nil)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(column-number-mode 1)
(set-face-font 'default "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )

;; Nice start screen
(use-package dashboard
  :after page-break-lines
  :config
  (dashboard-setup-startup-hook))

;; Required for dashboard
(use-package page-break-lines
  :config (global-page-break-lines-mode))

;; Modeline
(use-package doom-modeline
  :config
  (doom-modeline-init)
  )

;; Fix some defaults
(setq-default
 make-backup-files                nil ;; I don't want directory pollution
 ring-bell-function               'ignore ;; Stop ringing bell
 sentence-end-double-space        nil ; I prefer single space
 )

(defalias 'yes-or-no-p #'y-or-n-p)

;; Introduce keybindings
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-show-operator-state-maps t)
  (setq which-key-idle-delay 0.1))

(use-package key-chord
  :config
  (key-chord-mode +1))

(use-package modalka
  :after key-chord
  :config
  (setq-default cursor-type '(bar . 1))
  (setq modalka-cursor-type 'box)
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)
  )

;; A super light and incomplete implementation of vi using modalka
(key-chord-define-global "fd"
   '(lambda () (interactive) (modalka-mode 1) (overwrite-mode -1)))
(define-key modalka-mode-map "i" '(lambda () (interactive) (modalka-mode -1)))
(define-key modalka-mode-map "a"
   '(lambda () (interactive) (forward-char 1) (modalka-mode -1)))
(define-key modalka-mode-map "A"
  '(lambda () (interactive)
     (move-end-of-line 1)
     (modalka-mode -1)))
(modalka-define-kbd "h" "C-b")
(modalka-define-kbd "j" "C-n")
(modalka-define-kbd "k" "C-p")
(modalka-define-kbd "l" "C-f")
(modalka-define-kbd "w" "M-f")
(modalka-define-kbd "b" "M-b")
(modalka-define-kbd "H" "C-a")
(modalka-define-kbd "L" "C-e")
(modalka-define-kbd "C-d" "C-v")
(modalka-define-kbd "C-u" "M-v")
(modalka-define-kbd "x" "C-d")
(modalka-define-kbd "v" "C-SPC")
(modalka-define-kbd "e" "C-x C-e")
(modalka-define-kbd "d" "C-w")
(define-key modalka-mode-map "c"
  '(lambda () (interactive)
     (kill-region (region-beginning) (region-end))
     (modalka-mode -1)))
(modalka-define-kbd "D" "C-k")
(modalka-define-kbd "y" "M-w")
(modalka-define-kbd "p" "C-y")
(modalka-define-kbd "u" "C-/")
(modalka-define-kbd "," "C-x C-s")
;; This one needs to be done like this
;; because modalka doesn't deal with recursive mappings well
(define-key modalka-mode-map "x" 'delete-char)

(modalka-define-kbd "0" "C-0")
(modalka-define-kbd "1" "C-1")
(modalka-define-kbd "2" "C-2")
(modalka-define-kbd "3" "C-3")
(modalka-define-kbd "4" "C-4")
(modalka-define-kbd "5" "C-5")
(modalka-define-kbd "6" "C-6")
(modalka-define-kbd "7" "C-7")
(modalka-define-kbd "8" "C-8")
(modalka-define-kbd "9" "C-9")
(define-key modalka-mode-map "/" 'swiper)
(define-key modalka-mode-map "J"
  '(lambda () (interactive) (join-line 1)))
(define-key modalka-mode-map "R"
  '(lambda () (interactive)
     (overwrite-mode 1)
     (modalka-mode -1)))
(define-key modalka-mode-map "o"
  '(lambda (&optional count) (interactive "P")
     (move-end-of-line nil)
     (newline count)
     (modalka-mode -1)))
(define-key modalka-mode-map "O"
  '(lambda (&optional count) (interactive "P")
     (move-beginning-of-line nil)
     (newline count)
     (previous-line count)
     (modalka-mode -1)))

;; Ivy (taken from "How to make your own Spacemacs")
(use-package ivy-hydra)
(use-package wgrep)

(use-package ivy
  :after ivy-hydra
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 10)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (define-key ivy-minibuffer-map "\C-j" 'ivy-next-line)
  (define-key ivy-minibuffer-map "\C-k" 'ivy-previous-line)
  )

;; Counsel (same as Ivy above)
(use-package counsel
  :after (ivy ivy-hydra)
  :commands          ; Load counsel when any of these commands are invoked
  (counsel-M-x       ; M-x use counsel
   counsel-find-file ; C-x C-f use counsel-find-file
   counsel-recentf   ; search recently edited files
   counsel-git       ; search for files in git repo
   counsel-git-grep  ; search for regexp in git repo
   counsel-ag        ; search for regexp in git repo using ag
   counsel-locate)   ; search for files or else using locate
  :config
  (setq counsel-rg-base-command
	"rg -i -M 120 --follow --glob \"!.git/*\" --no-heading --ignore-case\
      --line-number --column --color never %s .")
  )

(use-package smex
  :after (counsel ivy swiper))

;; Swiper
(use-package swiper
  :after (ivy ivy-hydra)
  :commands swiper
  )

;; Company
(use-package company
  :config
  (global-company-mode 1)
  (company-tng-configure-default)
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map "\C-j" 'company-select-next)
  (define-key company-active-map "\C-k" 'company-select-previous)
  ;; This is apparently how you bind tab...
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  )

;; Add fuzzy backend to company
(use-package company-flx
  :after company
  :config
  (with-eval-after-load 'company
    (company-flx-mode +1))
  (setq company-flx-limit 250)
  )

;; Yasnipet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Add yasnippets to a company mode BACKEND."
  (if (or (not company-mode/enable-yas)
	  (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(use-package avy
  :commands
  (avy-goto-char-timer
   avy-goto-char-in-line
   avy-ivy)
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  )

(use-package magit
  :commands magit-status
  )

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Code"))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  )

(use-package counsel-projectile
  :commands (counsel-projectile projectile-find-file)
  :after (projectile counsel)
  :config (counsel-projectile-mode)
  )

(use-package flycheck
  :init (global-flycheck-mode)
  )

(use-package expand-region)

;; Smartparens is very heavy and weird. This stays more or less out of the way
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  )

;; Language specific
(use-package haskell-mode
  :config
  (setq haskell-process-type 'stack-ghci))

(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hasky-stack)
(use-package shakespeare-mode)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Revert garbage collection to default after loading init
(setq gc-cons-threshold 1000000)

(provide 'init)
;;; init.el ends here
