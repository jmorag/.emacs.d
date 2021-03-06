;; Code:

;;; Startup improvements
(setq package-enable-at-startup nil)
(setq-default cursor-type '(bar . 2))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;; Fonts and scrolling
(column-number-mode 1)
(when (eq system-type 'darwin)
  (set-face-font 'default "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
;; Make scrolling work more like vim's
(scroll-lock-mode 1)

;;;; Make long lines not kill emacs
(global-so-long-mode 1)

;;;; Start off with giant gc threshold
(setq gc-cons-threshold most-positive-fixnum)

;;;; Don't dump custom variables into init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;; Bootstrap straight.el
(setq straight-recipes-gnu-elpa-use-mirror t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Core editor facilities
;;;; Fix keyboard layout
(use-package colemak-mode
  :straight (colemak-mode :local-repo "~/emacs/colemak/")
  :bind (("C-c c" . colemak-mode)))
;;;; I only use GUI mode emacs so keys should do sane things
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-\[] [C-\[])
(define-key special-mode-map (kbd "j") #'next-line)
(define-key special-mode-map (kbd "k") #'previous-line)
;;;; Fancy text editing
(use-package key-chord
  :config (key-chord-mode 1))
(use-package key-seq)
(use-package ryo-modal
  :straight (ryo-modal :host github :repo "Kungsgeten/ryo-modal"
                       :fork (:host github :repo "jmorag/ryo-modal"))
  :config
  (defun ryo-enter () (interactive) (ryo-modal-mode 1))
  (key-seq-define-global "fd" 'ryo-enter)
  (global-set-key (kbd "<escape>") 'ryo-enter))

(use-package kakoune
  :straight (kakoune :local-repo "~/Projects/kakoune.el/" :files ("*.el"))
  :demand t
  :bind (("C-z" . ryo-modal-mode)
         ("C-<left>" . windmove-left)
         ("C-<down>" . windmove-down)
         ("C-<up>" . windmove-up)
         ("C-<right>" . windmove-right)
         ("<XF86Tools>" . er/expand-region))
  :init
  (require 'kakoune)
  (defun my/goto-init () (interactive)
         (find-file (expand-file-name "init.el" user-emacs-directory)))
  (setq ryo-modal-cursor-type 'box)
  (add-hook 'prog-mode-hook #'ryo-modal-mode)
  (define-key ryo-modal-mode-map (kbd "SPC h") 'help-command)
  ;; Try using this for bookmarks instead
  ;; Access all C-x bindings easily
  ;; (define-key ryo-modal-mode-map (kbd "z") ctl-x-map)
  (kakoune-setup-keybinds)
  (ryo-modal-keys
   (:mc-all 0)
   ("," save-buffer)
   ("m" mc/mark-next-like-this)
   ("M" mc/skip-to-next-like-this)
   ("n" mc/mark-previous-like-this)
   ("N" mc/skip-to-previous-like-this)
   ("M-m" mc/edit-lines)
   ("*" mc/mark-all-like-this)
   ("v" er/expand-region)
   ("C-v" rectangle-mark-mode)
   ("M-s" mc/split-region)
   (";" (("q" delete-window)
         ("v" split-window-horizontally)
         ("s" split-window-vertically)
         ("i" my/goto-init)))
   ("C-h" windmove-left)
   ("C-j" windmove-down)
   ("C-k" windmove-up)
   ("C-l" windmove-right)
   ("g r" revert-buffer)
   ("C-u" scroll-down-command :first '(deactivate-mark))
   ("C-d" scroll-up-command :first '(deactivate-mark))))

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  :ryo
  ("f" avy-goto-char-in-line :first '(deactivate-mark))
  ("F" avy-goto-char-in-line :first '(set-mark-if-inactive))
  ("C-f" avy-goto-char-timer :first '(deactivate-mark)))
(use-package embrace
  :ryo
  ("S" embrace-commander))
(use-package visual-regexp
  :ryo
  ("s" vr/mc-mark)
  ("?" vr/replace)
  ("M-/" vr/query-replace))
(use-package phi-search
  :bind (("C-s" . phi-search)
	     ("C-r" . phi-search-backward)))

;;;; Sane undo and redo
(use-package undo-fu
  :custom
  ((undo-fu-ignore-keyboard-quit t)
   (undo-fu-allow-undo-in-region t))
  :ryo
  ("u" undo-fu-only-undo)
  ("U" undo-fu-only-redo))

;;; Mac specific
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (progn
      (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
      (exec-path-from-shell-initialize))))

;; Use command as meta on mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;;; Visual improvements
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Required for dashboard
(use-package page-break-lines
  :config (global-page-break-lines-mode))

;; Nice start screen
(use-package dashboard
  :straight (dashboard :host github :repo "rakanalh/emacs-dashboard")
  :after page-break-lines
  :demand t
  :config
  (dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
	          ("j" . widget-forward)
	          ("k" . widget-backward)))

;; Theme
(use-package solarized-theme
  :custom
  (solarized-use-mode-italic t)
  (solarized-scale-org-mode-headlines nil))

(use-package doom-themes
  :custom (doom-themes-padded-modeline t)
  :config
  (defvar current-theme "doom-solarized-dark-high-contrast" "Which doom theme is active.")
  (defun reload-theme ()
    (interactive)
    (straight-rebuild-package "doom-themes")
    (counsel-load-theme-action current-theme))
  (defun toggle-theme ()
    "Toggle between doom-one and doom-solarized-light themes."
    (interactive)
    (if (string-equal current-theme "doom-solarized-dark-high-contrast")
        (progn (counsel-load-theme-action "doom-solarized-light")
               (setq current-theme "doom-solarized-light"))
      (progn (counsel-load-theme-action "doom-solarized-dark-high-contrast")
             (setq current-theme "doom-solarized-dark-high-contrast"))))
  (doom-themes-org-config)
  :ryo
  ("SPC a l" toggle-theme)
  ("SPC a L" reload-theme))

;; Modeline
(use-package doom-modeline
  :straight (doom-modeline :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-height 36))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1))

;; Window niceties
(use-package windower
  :straight (windower :host gitlab :repo "ambrevar/emacs-windower"))

(use-package hydra
  :ryo
  ("SPC s" :hydra
   '(hydra-smerge ()
                  "A hydra for smerge"
                  ("j" smerge-next "next conflict")
                  ("k" smerge-prev "previous conflict")
                  ("u" smerge-keep-upper "keep upper conflict")
                  ("l" smerge-keep-lower "keep lower conflict")
                  ("q" nil "cancel" :color blue)))
  ("C-w" :hydra
   '(hydra-windower (:hint nil)
                    "
^Move^                      ^Swap^
^^^^^^^^------------------------------------------------
_h_: move border left       _H_: swap border left
_j_: move border up         _J_: swap border above
_k_: move border down       _K_: swap border below
_l_: move border right      _L_: swap border right
                          _t_: toggle vertical or horizontal split
"
                    ("h" windower-move-border-left)
                    ("j" windower-move-border-below)
                    ("k" windower-move-border-above)
                    ("l" windower-move-border-right)
                    ("<left>" windower-move-border-left)
                    ("<down>" windower-move-border-below)
                    ("<up>" windower-move-border-above)
                    ("<right>" windower-move-border-right)
                    ("H" windower-swap-left)
                    ("J" windower-swap-below)
                    ("K" windower-swap-above)
                    ("L" windower-swap-right)
                    ("<S-left>" windower-swap-left)
                    ("<S-down>" windower-swap-below)
                    ("<S-up>" windower-swap-above)
                    ("<S-right>" windower-swap-right)
                    ("t" windower-toggle-split :exit t)
                    ("s" windower-swap :exit t)
                    ("q" nil "cancel" :color blue)
                    ("<return>" nil nil))))

;;; Saner defaults
(setq-default
 ring-bell-function              'ignore ;; Stop ringing bell
 sentence-end-double-space        nil	  ; I prefer single space
 tab-width                        4       ; 8 is enormous
 initial-scratch-message          nil     ; I know what the scratch buffer is for
 vc-follow-symlinks               t       ; I never don't want to follow them
 )

(defalias 'yes-or-no-p #'y-or-n-p)

;; Lifted from technomancy's better-defaults package
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(show-paren-mode 0)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline nil
      load-prefer-newer t
      echo-keystrokes 0.1
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      save-place-file (concat user-emacs-directory "places")
      auto-save-default nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(global-auto-revert-mode t)
(use-package crux
  :ryo
  ("g U" crux-view-url)
  ("g u" browse-url-chromium))

;;; Interface management
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-show-operator-state-maps t)
  (setq which-key-idle-delay 0.3)
  ;; Fix ryo commands' names
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;; Ivy (taken from "How to make your own Spacemacs")
(use-package ivy-hydra)
(use-package wgrep
  :commands
  (wgrep-change-to-wgrep-mode))

(use-package ivy
  :init (ivy-mode 1)               ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t) ; extend searching to bookmarks and …
  (setq ivy-height 10)             ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (define-key ivy-minibuffer-map "\C-j" 'ivy-next-line)
  (define-key ivy-minibuffer-map "\C-k" 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-toggle-fuzzy)
  :ryo
  ("SPC" (("b" ivy-switch-buffer)
          ("r" ivy-resume))))

(use-package ivy-rich
  :after (counsel)
  :config (ivy-rich-mode 1))

;; Counsel (same as Ivy above)
(use-package counsel
  :init
  (counsel-mode 1)
  :ryo
  (":" counsel-M-x)
  ("P" counsel-yank-pop)
  ("SPC" (("f f" counsel-find-file)
          ("f r" counsel-recentf)
          ("/" counsel-rg)
          ("." counsel-find-file))))

;; Swiper
(use-package swiper
  :after (ivy ivy-hydra)
  :commands swiper
  :ryo ("/" swiper)
  :custom
  (swiper-goto-start-of-match t))

;; Remeber searches
(use-package prescient
  :after counsel
  :config (prescient-persist-mode))
(use-package ivy-prescient
  :after counsel
  :config (ivy-prescient-mode))

;; Workspaces
(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-new-workspace t)
  :ryo
  ("gt" eyebrowse-next-window-config)
  ("gT" eyebrowse-prev-window-config)
  ("; w" eyebrowse-close-window-config)
  ("SPC" (("0" eyebrowse-switch-to-window-config-0)
          ("1" eyebrowse-switch-to-window-config-1)
          ("2" eyebrowse-switch-to-window-config-2)
          ("3" eyebrowse-switch-to-window-config-3)
          ("4" eyebrowse-switch-to-window-config-4)
          ("5" eyebrowse-switch-to-window-config-5)
          ("6" eyebrowse-switch-to-window-config-6)
          ("7" eyebrowse-switch-to-window-config-7)
          ("8" eyebrowse-switch-to-window-config-8)
          ("9" eyebrowse-switch-to-window-config-9))))
(winner-mode 1)
(ryo-modal-key "] w" 'winner-redo)
(ryo-modal-key "[ w" 'winner-undo)

;; Bookmarks - https://github.com/joodland/bm
(use-package bm
  :custom
  ((bm-cycle-all-buffers t)
   (bm-marker 'bm-marker-right))
  :ryo
  ("z" bm-toggle)
  ("] z" bm-next)
  ("[ z" bm-previous)
  :config
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

;;; Autocompletion
;;;; Company
(use-package company
  :hook (prog-mode . company-mode)
  :ryo
  ("SPC a c" company-mode)
  :config
  (company-tng-configure-default)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  (define-key company-active-map "\C-j" 'company-select-next)
  (define-key company-active-map "\C-k" 'company-select-previous)
  (define-key company-active-map (kbd "<down>") 'company-select-next)
  (define-key company-active-map (kbd "<up>") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

;; quite slow :(
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; Add fuzzy backend to company
(use-package company-flx
  :after company
  :config
  (company-flx-mode +1)
  (setq company-flx-limit 250))

;; Remember completions
(use-package company-prescient
  :config (company-prescient-mode))

;; Add help to company
(use-package company-quickhelp
  :after company
  :config
  (use-package pos-tip)
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 1.0))

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

;; helpful help
(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-symbol #'helpful-symbol))

;;;; Snippets
(use-package yasnippet
  :commands
  (yas/expand)
  :config
  (yas-global-mode 1)
  (global-set-key (kbd "C-c s") 'company-yasnippet))

;;; In buffer navigation
(use-package outshine
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode))

(use-package navi-mode
  :bind (:map navi-mode-map
	          ("j" . occur-next)
	          ("k" . occur-prev)
	          ("d" . navi-kill-thing-at-point)
	          ("C-d" . scroll-up-command)
	          ("C-u" . scroll-down-command)
	          ("SPC" . occur-mode-display-occurrence)))

(use-package goto-last-change
  :ryo ("g ;" goto-last-change))

;;; Project management
;;;; Dired
(use-package dired-hacks-utils
  :custom
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-dwim-target t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  :ryo ("SPC d" dired-jump)
  :bind (:map dired-mode-map
              (";" . wdired-change-to-wdired-mode)
              ("h" . dired-up-directory)
              ("j" . dired-hacks-next-file)
              ("k" . dired-hacks-previous-file)
              ("l" . dired-find-alternate-file)
              ("K" . dired-do-kill-lines)))
(use-package dired-filter
  :config (define-key dired-mode-map (kbd "F") dired-filter-map))
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("f" . dired-narrow)
              ("/" . dired-narrow)))
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)
              ("C-j" . dired-subtree-next-sibling)
              ("C-k" . dired-subtree-previous-sibling)))
(use-package peep-dired
  :custom
  (peed-dired-cleanup-on-disable t)
  (peep-dired-enable-on-directories t)
  :bind (:map dired-mode-map
              ("p" . peep-dired)
              :map peep-dired-mode-map
              ("j" . peep-dired-next-file)
              ("k" . peep-dired-prev-file)))
(use-package dired+
  :straight (dired+ :host github :repo "emacsmirror/dired-plus")
  :custom
  (dired-listing-switches "-alh"))

;;;; Treemacs
;; I never use this...
;; (use-package treemacs
;;   :ryo ("\\" treemacs)
;;   :config (treemacs-follow-mode)
;;   :bind (:map treemacs-mode-map
;;               ("j" . treemacs-next-line)
;;               ("k" . treemacs-previous-line)
;;               ("\\" . treemacs-quit)))
;;;; Disk Usage
(use-package disk-usage
  :bind (:map disk-usage-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("H" . disk-usage-toggle-human-readable)
              ("h" . disk-usage-up)
              :map dired-mode-map
              ("," . disk-usage-here)))
;;;; Direnv
(use-package direnv
  :if (executable-find "direnv")
  :config
  (direnv-mode))

;;;; Magit
(use-package magit
  :ryo
  ("SPC g" magit-status)
  :bind
  (:map magit-status-mode-map
        ("j" . magit-section-forward)
        ("k" . magit-section-backward)
        ("C-j" . magit-section-forward-sibling)
        ("C-k" . magit-section-backward-sibling)
        ("g" . magit-status-jump)
        ("p" . magit-push)
        ("P" . magit-pull)
        ("M-k" . magit-discard)
        ;; It seems as if we will have to repeat ourselves
        ;; or learn how macros work...
        :map magit-file-section-map
        ("C-j" . magit-section-forward-sibling)
        ("C-k" . magit-section-backward-sibling)
        :map magit-hunk-section-map
        ("C-j" . magit-section-forward-sibling)
        ("C-k" . magit-section-backward-sibling))
  :config
  ;; https://github.com/magit/magit/issues/1953
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-timemachine
  :ryo
  ("SPC G" git-timemachine)
  :bind
  (:map git-timemachine-mode-map
        ("j" . git-timemachine-show-next-revision)
        ("k" . git-timemachine-show-previous-revision)
        ("," . write-file)))

;; WIP git gutter fringe deleted
(use-package fringe-helper)
(require 's)
(defun make-triangle-bitmap (n-rows)
  (apply 'fringe-helper-convert
         (cl-loop for i from 0 to n-rows collect
                  (s-concat (s-repeat i "X") (s-repeat (- n-rows i) ".")))))

(use-package git-gutter-fringe
  :hook (prog-mode . git-gutter-mode)
  :init
  (require 'git-gutter-fringe)
  (if (fboundp 'fringe-mode) (fringe-mode '4))
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (advice-add #'magit-stage-file   :after #'git-gutter)
  (advice-add #'magit-unstage-file :after #'git-gutter)
  :ryo
  ("SPC a g" git-gutter-mode)
  ("] g" git-gutter:next-hunk)
  ("[ g" git-gutter:previous-hunk)
  ("g R" git-gutter:revert-hunk))

(use-package forge
  :after magit
  :config
  (transient-append-suffix 'magit-status-jump '(0 -1)
    [("i" "Issues" forge-jump-to-issues)
     ("P" "Pullreqs" forge-jump-to-pullreqs)]))

(use-package git-link)

(use-package gitignore
  :straight (gitignore :host github :repo "syohex/emacs-gitignore"))

;;;; Projectile
(use-package projectile
  :init
  (define-key ryo-modal-mode-map (kbd "SPC p") 'projectile-command-map)
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  :ryo
  ("SPC SPC" projectile-find-file))

;;; General programming concerns
;;;; Parentheses
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character)
  :hook (shakespeare-hamlet-mode . highlight-indent-guides-mode))

;;;; Linting
(use-package flycheck
  :config
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pylint-executable "pylint")
  :ryo
  ("SPC" (("a t" flycheck-mode :name "Flycheck toggle")))
  ("] e" flycheck-next-error :first '(deactivate-mark))
  ("[ e" flycheck-previous-error :first '(deactivate-mark)))

;;;; Jumping
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg)
  :ryo
  (:norepeat t)
  ("g" (("d" dumb-jump-go)
        ("b" dumb-jump-back)
        ("o" dumb-jump-other-window)
        ("p" dumb-jump-go-prompt))))

;;;; Formatting
(use-package format-all
  :ryo
  ("SPC =" format-all-buffer))
(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

;;;; Commenting
(use-package evil-nerd-commenter
  :ryo
  ("'" evilnc-comment-or-uncomment-lines)
  ("SPC '" evilnc-comment-or-uncomment-paragraphs))

;;;; Eshell
(setq explicit-shell-file-name "fish")
(use-package eshell-toggle
  :straight (:host github :repo "4DA/eshell-toggle")
  :bind
  ("C-t" . eshell-toggle)
  ("M-t" . transpose-chars)
  :config
  (setq et-use-projectile-root t)
  (setq et-eshell-height-fraction 3))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))
;; (use-package eshell-autojump)
(use-package fish-completion
  :config (global-fish-completion-mode))

(defun display-ansi-colors ()
  "Function to display ansi color codes in read-only buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;; LSP
(setq lsp-keymap-prefix "M-l")
(use-package lsp-mode
  :straight (lsp-mode :host github :repo "emacs-lsp/lsp-mode" :files (:defaults "clients/*.el"))
  :custom ((lsp-auto-configure t)
           (lsp-idle-delay 0.500))
  :hook ((go-mode . lsp)
         (lua-mode . lsp)
         (rust-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (lsp-mode . flycheck-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-to-list 'lsp-language-id-configuration '(shakespeare-julius-mode . "javascript"))
  (setq lsp-rust-analyzer-server-command '("/home/joseph/.cargo/bin/rust-analyzer"))
  (setq lsp-pyls-plugins-pycodestyle-ignore t))
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil))
(use-package lsp-ivy
  :straight (lsp-ivy :host github :repo "emacs-lsp/lsp-ivy")
  :commands lsp-ivy-workspace-symbol)

;;;; Dash Documentation
;; (use-package dash-docs
;;   :straight (dash-docs :host github :repo "dash-docs-el/dash-docs"))
;; (use-package counsel-dash
;;   :after dash-docs)
;;; Language specific programming concerns
;;;; Haskell
(use-package haskell-mode
  :custom (haskell-literate-default 'tex)
  :hook ((haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . interactive-haskell-mode))
  :ryo
  (:mode 'haskell-mode)
  ("," save-buffer)
  )
(use-package lsp-haskell
  :hook (haskell-mode . lsp)
  ;; :custom
  ;; ((lsp-haskell-process-path-hie "haskell-language-server")
  ;;  (lsp-haskell-process-args-hie '()))
  :config
  (setq lsp-log-io nil)
  (setq lsp-haskell-set-hlint-on t))

(use-package ormolu
  :custom (ormolu-extra-args '("--ghc-opt" "-XTypeApplications"))
  :straight (ormolu :host github :repo "vyorkin/ormolu.el")
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-region)))

(use-package shakespeare-mode)
;; (use-package shm
;;   :after haskell-mode
;;   :bind (:map shm-map
;;               ("C-0" . shm/goto-last-point))
;;   :ryo
;;   (:mode 'haskell-mode)
;;   ("SPC m s" structured-haskell-mode))

;;;; Idris
(use-package idris-mode
  :defer t
  :bind
  (:map idris-repl-mode-map
        ("C-c C-k" . idris-repl-clear-buffer))
  :ryo
  (:mode 'idris-mode)
  ("," idris-load-file)
  ("SPC m r" idris-load-file)
  ("SPC m t" idris-type-at-point)
  ("SPC m d" idris-add-clause)
  ("SPC m l" idris-make-lemma)
  ("SPC m c" idris-case-split)
  ("SPC m w" idris-make-with-block)
  ("SPC m m" idris-add-missing)
  ("SPC m p" idris-proof-search)
  ("SPC m h" idris-docs-at-point))

;;;; Rust
(use-package rust-mode
  :after (projectile)
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                    :compile "cargo build"
                                    :test "cargo test"
                                    :run "cargo run"))
(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)))

;;;; Go
(use-package go-mode)

;;;; Javascript
;; emacs 27 natively parses jsx correctly
(when (< emacs-major-version 27)
  (use-package rjsx-mode
    :mode ("\\.js\\'" . rjsx-mode)
    :config
    (add-hook 'rjsx-mode-hook #'flycheck-mode)
    (add-hook 'rjsx-mode-hook #'company-mode)
    (setq js2-strict-missing-semi-warning nil)))
(use-package json-mode)
(use-package typescript-mode
  :mode ("\\.tsx\\'" . typescript-mode))
(use-package rainbow-mode)

(use-package web-mode)

;;;; Purescript
(use-package purescript-mode
  :hook (purescript-mode . purescript-decl-scan-mode))
(use-package psc-ide
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

;;;; Dhall
(use-package dhall-mode
  :hook (dhall-mode . lsp))

;;;; Yaml
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :hook (yaml-mode . ryo-modal-mode))

;;;; i3
(use-package i3wm-config-mode
  :straight (:host github :repo "Alexander-Miller/i3wm-Config-Mode"))

;;;; Lisps
(defconst lisp-modes
  '(racket-mode emacs-lisp-mode clojure-mode scheme-mode lisp-mode lisp-interaction-mode))

(use-package aggressive-indent
  :config
  (dolist (mode lisp-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) 'aggressive-indent-mode)))

(use-package lispy
  :config
  (add-hook 'ryo-modal-mode-hook
            (lambda ()
              (when (find major-mode lisp-modes)
                (if (bound-and-true-p ryo-modal-mode)
                    (lispy-mode -1)
                  (lispy-mode 1))))))

(use-package parseedn
  :straight (parseedn :host github :repo "clojure-emacs/parseedn"))
(use-package clojure-mode)
(use-package cider)

(use-package racket-mode)
(use-package geiser)

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;; Could be fun... write python in lisp
(use-package hy-mode)

;;;; Nix
(use-package nix-mode
  :hook (nix-mode . lsp))
(use-package nix-env-install
  :straight (nix-env-install :host github :repo "akirak/nix-env-install")
  :custom (nix-env-install-npm-node2nix-options "--nodejs-14"))

;;;; Elm
(use-package elm-mode)

;;;; Ocaml
(use-package tuareg)
(use-package dune)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share"))))
      (ocamlmerlin-command (ignore-errors (car (process-lines "which" "ocamlmerlin")))))
  (when (and opam-share ocamlmerlin-command (file-directory-p opam-share))
    (use-package merlin
      :straight (merlin
                 :local-repo (expand-file-name "emacs/site-lisp" opam-share)
                 :files ("merlin*.elc"))
      :config
      (add-hook 'tuareg-mode-hook 'merlin-mode)
      (add-hook 'caml-mode-hook 'merlin-mode)
      (setq merlin-error-after-save nil)
      (setq merlin-command ocamlmerlin-command))))

(use-package flycheck-ocaml
  :config
  (flycheck-ocaml-setup)
  (add-hook 'tuareg-mode-hook 'flycheck-mode))

;;;; Python
(use-package lsp-pyright
  :straight (lsp-pyright :host github :repo "emacs-lsp/lsp-pyright")
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))
(use-package lpy
  :straight (lpy :host github :repo "abo-abo/lpy")
  :ryo
  (:mode 'python-mode)
  ("e" lispy-eval))

;;;; Lua
(use-package lua-mode)
;;;; C
(use-package cc-mode
  :custom (c-basic-offset 2)
  :mode (("\\.mc\\'" . c-mode)))

;;;; LLVM - for when we need to look at llvm
(use-package llvm-mode
  :straight (llvm-mode :local-repo "~/emacs/llvm/"
                       :files ("*.el")))

;;;; Graphviz
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4)
  (require 'company-graphviz-dot))

;;;; Fish
(use-package fish-mode)
;;;; Zig
(use-package zig-mode)
;;;; Salt
(use-package salt-mode)

;;;; Org
(straight-use-package '(org :type built-in))
(use-package org
  :straight nil
  :hook (org-mode . ryo-modal-mode)
  :bind ("C-c a" . org-agenda-list)
  :config
  (setq org-agenda-files '("~/Dropbox (Maestral)/Agenda/"))
  (setq org-agenda-span 'week)
  (require 'ox-md nil t)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  :ryo
  (:mode 'org-mode)
  (">" org-demote-subtree)
  ("<" org-promote-subtree))

(use-package org-agenda
  :straight nil
  :bind
  (:map org-agenda-mode-map
        ("h" . org-agenda-earlier)
        ("j" . org-agenda-next-line)
        ("k" . org-agenda-previous-line)
        ("l" . org-agenda-later)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package ox-moderncv
  :commands (org-mode)
  :straight (ox-moderncv :host gitlab :repo "Titan-C/org-cv"
                         :fork (:host github :repo "jmorag/org-cv"))
  :config
  (require 'ox-moderncv))

;;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook ((markdown-mode gfm-mode) . ryo-modal-mode))
(use-package apib-mode
  :mode ("\\.apib\\'" . apib-mode))

;;;; Asciidoc
(use-package adoc-mode)

;;;; Lilypond
(use-package lilypond-mode
  :straight nil
  :custom
  (LilyPond-pdf-command "zathura")
  :hook (LilyPond-mode . ryo-modal-mode)
  :mode ("\\.ly\\'" . LilyPond-mode))

;;;; Vimscript (lol)
(use-package vimrc-mode)
;;; Kitchen sink
;;;; Restclient
(use-package restclient)
(use-package company-restclient)

;;;; Pdf
;; Stolen from doom
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook #'(lambda () (ryo-modal-mode -1)))
  (add-hook 'pdf-view-mode-hook #'pdf-isearch-minor-mode)
  :bind (:map pdf-view-mode-map
              ("q" . kill-this-buffer)
              ("j" . pdf-view-next-page-command)
              ("k" . pdf-view-previous-page-command)
              ("l" . image-forward-hscroll)
              ("h" . image-backward-hscroll)
              ("x" . other-window)
              ("C-s" . isearch-forward)
              ("C-r" . isearch-backward)
              ("O" . pdf-outline)
              )
  :ryo
  (:mode 'pdf-view-mode)
  ("j" pdf-view-next-page-command)
  ("k" pdf-view-previous-page-command)
  ("l" image-forward-hscroll)
  ("h" image-backward-hscroll)
  ("O" pdf-outline))

(use-package pdf-outline
  :straight nil
  :bind (:map pdf-outline-buffer-mode-map
              ("j" . next-line)
              ("k" . previous-line)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;;;; Gif recording
(use-package gif-screencast
  :bind ("<f9>" . gif-screencast-start-or-stop)
  :custom ((gif-screencast-scale-factor 2)
           ;; Probably not great to hardcode the nix path like this
           (gif-screencast-convert-program "/nix/store/s9zgyg06sqlnk6mss6ixwc5n40bk3rf9-imagemagick-6.9.10-71/bin/convert")
           (gif-screencast-program "scrot")))
;;;; 2048
(use-package 2048-game
  :commands (2048-game)
  :bind
  (:map 2048-mode-map
        ("h" . 2048-left)
        ("j" . 2048-down)
        ("k" . 2048-up)
        ("l" . 2048-right)))

;;;; tetris
(advice-add 'tetris :before
            (lambda ()
              (setq gamegrid-xpm (with-temp-buffer
                                   (insert-file-contents-literally (concat user-emacs-directory "tetris.xpm"))
                                   (buffer-string)))
              (setq gamegrid-glyph-height 64)))

;;;; xkcd
(use-package xkcd
  :commands (xkcd xkcd-get xkcd-get-latest)
  :bind
  (:map xkcd-mode-map
        ("h" . xkcd-prev)
        ("j" . xkcd-next)
        ("k" . xkcd-prev)
        ("l" . xkcd-next)
        ("r" . xkcd-rand)
        ("SPC" . xkcd-alt-text)))

;;;; Music
;; Surprisingly, this works, but it can use some tweaking
;; (use-package emms
;;   :commands (emms)
;;   :straight (:repo "git://git.sv.gnu.org/emms.git")
;;   :config
;;   (require 'emms-setup)
;;   (emms-all)
;;   (emms-default-players)
;;   (setq emms-source-file-default-directory "~/Music/")
;;   (require 'emms-info-libtag)
;;   (setq emms-info-functions '(emms-info-libtag)))

;;;; Fake word processor
(use-package flyspell-correct
  :config
  (when (executable-find "aspell")
    (setq-default ispell-program-name "aspell")
    (setq ispell-really-aspell t))
  (setq flyspell-correct-interface #'flyspell-correct-dummy)
  :ryo
  ("SPC a s" flyspell-mode)
  ("g s" flyspell-auto-correct-word)
  ("g S" flyspell-correct-wrapper)
  ("[ s" flyspell-correct-previous)
  ("] s" flyspell-correct-next))

(use-package darkroom
  :init
  (defun jm/toggle-prose-mode ()
    "Toggle distraction free writing mode for prose."
    (interactive)
    (if (bound-and-true-p darkroom-mode)
        (progn (display-line-numbers-mode 1)
               (darkroom-mode 0)
               (visual-line-mode 0)
               (flyspell-mode 0))
      (progn (display-line-numbers-mode 0)
             (darkroom-mode 1)
             (visual-line-mode 1)
             (flyspell-mode 1)
             (text-scale-adjust -2))))
  :custom
  (darkroom-margins 0.1)
  :hook
  (org-mode-hook . jm/toggle-prose-mode)
  (markdown-mode-hook . jm/toggle-prose-mode)
  :ryo
  ("SPC a p" jm/toggle-prose-mode))

(use-package tex-site
  :straight auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; Turn on RefTeX in AUCTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'company-mode)
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (require 'preview)
  :ryo
  (:mode 'tex-mode)
  ("=" preview-at-point)
  ("+" preview-clearout))
;; (use-package calctex
;;   :straight (calctex :host github :repo "johnbcoughlin/calctex"))
(use-package company-auctex
  :config
  (company-auctex-init))
(use-package company-reftex
  :after (company)
  :config
  (add-to-list 'company-backends '(company-reftex-citations company-reftex-labels)))

;;;; Passwords
(use-package ivy-pass)

;;;; Email
(use-package org-mime)
(use-package notmuch
  :straight nil
  :config
  (setq notmuch-search-oldest-first nil
        send-mail-function 'sendmail-sent-it
        sendmail-program "/run/current-system/sw/bin/msmtp"
        message-send-mail-function 'message-send-mail-with-sendmail
        mail-host-address "josephmorag.com"
        user-full-name "Joseph Morag"
        notmuch-poll-script "/home/joseph/Mail/checkmail.sh"
        notmuch-show-logo nil
        mm-text-html-renderer 'links
        notmuch-multipart/alternative-discouraged '()
        notmuch-always-prompt-for-sender t)
  ;; The following stolen from evil-collection
  (defun notmuch-toggle-tag (tag mode &optional next-function)
    "Toggle TAG tag for message in MODE."
    (let ((get (intern (format "notmuch-%s-get-tags" mode)))
          (set (intern (format "notmuch-%s-tag" mode)))
          (next (or next-function (intern (format "notmuch-%s-next-message" mode)))))
      (funcall set (list (concat (if (member tag (funcall get))
                                     "-" "+")
                                 tag)))
      (funcall next)))
  (defun notmuch-show-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (notmuch-toggle-tag "deleted" "show"))

  (defun notmuch-tree-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (notmuch-toggle-tag "deleted" "tree"))

  (defun notmuch-search-toggle-delete ()
    "Toggle deleted tag for message."
    (interactive)
    (notmuch-toggle-tag "deleted" "search" 'notmuch-search-next-thread))

  (defun notmuch-tree-toggle-unread ()
    "Toggle unread tag for message."
    (interactive)
    (notmuch-toggle-tag "unread" "tree"))

  (defun notmuch-search-toggle-unread ()
    "Toggle unread tag for message."
    (interactive)
    (notmuch-toggle-tag "unread" "search" 'notmuch-search-next-thread))

  (defun notmuch-tree-toggle-flagged ()
    "Toggle flagged tag for message."
    (interactive)
    (notmuch-toggle-tag "flagged" "tree"))

  (defun notmuch-search-toggle-flagged ()
    "Toggle flagged tag for message."
    (interactive)
    (notmuch-toggle-tag "flagged" "search"))
  :bind
  (("C-c m" . notmuch)
   :map notmuch-search-mode-map
   ("u" . notmuch-search-toggle-unread)
   ("f" . notmuch-search-toggle-flagged)
   ("j" . notmuch-search-next-thread)
   ("k" . notmuch-search-previous-thread)
   (";" . notmuch-jump-search)
   ("K" . notmuch-tag-jump)
   :map notmuch-tree-mode-map
   ("u" . notmuch-tree-toggle-unread)
   ("f" . notmuch-tree-toggle-flagged)
   ("j" . notmuch-tree-next-matching-message)
   ("k" . notmuch-tree-prev-matching-message)
   (";" . notmuch-jump-search)
   ("K" . notmuch-tag-jump)))

;;;; Twitter
(use-package twittering-mode
  :commands twit
  :config
  (setq twittering-icon-mode t))

;;;; Restart Emacs from Emacs!
(use-package restart-emacs)
;;;; Services
(use-package prodigy
  :bind
  (("C-c p" . prodigy)
   :map prodigy-mode-map
   ("j" . prodigy-next)
   ("k" . prodigy-prev)
   ("g g" . revert-buffer)
   ("g d" . prodigy-jump-file-manager)
   ("g m" . prodigy-jump-magit))
  :config
  (prodigy-define-service
    :name "lantern-desktop-ui"
    :command "yarn"
    :args '("start")
    :stop-signal 'sigint
    :cwd "~/Projects/getlantern/lantern-desktop-ui"
    :tags '(yarn lantern))
  (prodigy-define-service
    :name "flashlight"
    :path '("~/Projects/go/src/github.com/getlantern/flashlight")
    :command "lantern"
    :args '("-uiaddr" ":16823" "-headless")
    :stop-signal 'sigint
    :cwd "~/Projects/go/src/github.com/getlantern/flashlight"
    :tags '(go lantern))
  (prodigy-define-service
    :name "yinbi-server"
    :cwd "~/Projects/go/src/github.com/getlantern/yinbi-server"
    :command "bash"
    :args '("run-admin.bash")
    :tags '(go)
    :stop-signal 'sigkill)
  (prodigy-define-tag
    :name 'yinbi-web
    :command "yarn"
    :cwd "~/Projects/getlantern/yinbi-web")
  (prodigy-define-service
    :name "yinbi-web"
    :tags '(yinbi yarn yinbi-web)
    :args '("dev"))
  (prodigy-define-service
    :name "yinbi-web-admin"
    :tags '(yinbi yarn yinbi-web)
    :args '("start:admin")))

;;; End
;; Revert garbage collection to default after loading init
;; or maybe don't
(setq gc-cons-threshold 1000000)

(provide 'init)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
(load-theme 'doom-solarized-dark-high-contrast t)
;; init.el ends here
