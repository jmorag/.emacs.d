;; Code:

;;; Startup improvements
;;;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;; Fonts and scrolling
(column-number-mode 1)
(when (eq system-type 'darwin)
  (set-face-font 'default "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
;; Make scrolling work more like vim's
(scroll-lock-mode 1)

;;;; Start off with giant gc threshold
(setq gc-cons-threshold most-positive-fixnum)

;;;; Don't dump custom variables into init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Core editor facilities

;;;; Turn off the mouse on linux
(when (string= (system-name) "jnix")
  (use-package disable-mouse
    :config (global-disable-mouse-mode)))

;;;; Key chord
(use-package use-package-chords
  :config
  (key-chord-mode 1))

;;;; General utilities
(defun goto-init-file () (interactive) (find-file user-init-file))

;;;; Fancy text editing
(require 'kakoune)

;;;; Sane undo and redo
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ryo
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
  ("SPC u" undo-tree-visualize)
  :bind (:map undo-tree-visualizer-mode-map
	      ("h" . undo-tree-visualize-switch-branch-left)
	      ("j" . undo-tree-visualize-redo)
	      ("k" . undo-tree-visualize-undo)
	      ("l" . undo-tree-visualize-switch-branch-right)))

;;; Mac specific
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    ;; (setenv "SHELL" "/bin/bash")
    (exec-path-from-shell-initialize)
    ))

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

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Required for dashboard
(use-package page-break-lines
  :demand t
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

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
(use-package doom-modeline
  :straight (doom-modeline :host github :repo "seagle0128/doom-modeline")
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 70)
  (setq doom-modeline-bar-width 4))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1))

;;; Saner defaults
(setq-default
 ring-bell-function               'ignore ;; Stop ringing bell
 sentence-end-double-space        nil	  ; I prefer single space
 )

(defalias 'yes-or-no-p #'y-or-n-p)

;; Lifted from technomancy's better-defaults package
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(show-paren-mode 0)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      save-place-file (concat user-emacs-directory "places")
      auto-save-default nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(global-auto-revert-mode t)

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
  :init (ivy-mode 1)                  ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t) ; extend searching to bookmarks and …
  (setq ivy-height 10)             ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (define-key ivy-minibuffer-map "\C-j" 'ivy-next-line)
  (define-key ivy-minibuffer-map "\C-k" 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-toggle-fuzzy)
  :ryo
  ("SPC" (("b" ivy-switch-buffer))))

(use-package ivy-rich
  :after (counsel)
  :config (ivy-rich-mode 1))

;; Counsel (same as Ivy above)
(use-package counsel
  :init
  (counsel-mode 1)
  :commands      ; Load counsel when any of these commands are invoked
  (counsel-M-x   ; M-x use counsel
   counsel-find-file          ; C-x C-f use counsel-find-file
   counsel-recentf            ; search recently edited files
   counsel-git                ; search for files in git repo
   counsel-git-grep           ; search for regexp in git repo
   counsel-ag                 ; search for regexp in git repo using ag
   counsel-locate             ; search for files or else using locate
   counsel-rg)                ; search for regexp in git repo using
  :config
  (setq counsel-rg-base-command
	"rg -i -M 120 --follow --glob \"!.git/*\" --no-heading --ignore-case\
      --line-number --column --color never %s .")
  :ryo
  (":" counsel-M-x)
  ("SPC" (("f f" counsel-find-file)
          ("f r" counsel-recentf)
          ("/" counsel-rg))))

;; Swiper
(use-package swiper
  :after (ivy ivy-hydra)
  :commands swiper
  :ryo ("/" swiper))

;; Remeber searches
(use-package prescient
  :config (prescient-persist-mode))
(use-package ivy-prescient
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

;;; Autocompletion
;;;; Company
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (company-tng-configure-default)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (define-key company-active-map "\C-j" 'company-select-next)
  (define-key company-active-map "\C-k" 'company-select-previous)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

;; Add fuzzy backend to company
(use-package company-flx
  :after company
  :config
  (with-eval-after-load 'company
    (company-flx-mode +1))
  (setq company-flx-limit 250))

;; Remember completions
(use-package company-prescient
  :config (company-prescient-mode))

;; Add help to compnay
(use-package company-quickhelp
  :after company
  :config
  (use-package pos-tip)
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 1))

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

;;;; Snippets
;; Yasnipet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (global-set-key (kbd "C-c s") 'company-yasnippet))

;;; In buffer navigation
(use-package avy
  :config
  (setq avy-background t)
  (setq avy-all-windows t)
  :ryo
  ("f" avy-goto-char-in-line :first '(deactivate-mark))
  ("F" avy-goto-char-in-line :first '(set-mark-if-inactive))
  ("C-f" avy-goto-char-timer :first '(deactivate-mark)))

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
;;;; Magit
(use-package magit
  :ryo
  ("SPC g" magit-status)
  :config
  ;; Stolen from magnars whattheemacs.d
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (defun with-editor-finish-if-ryo ()
    (interactive)
    (if (bound-and-true-p ryo-modal-mode)
	(with-editor-finish nil)
      (newline)))
  :bind
  (:map magit-status-mode-map
	("j" . magit-section-forward)
	("k" . magit-section-backward)
	("C-j" . magit-section-forward-sibling)
	("C-k" . magit-section-backward-sibling)
	("g r" . magit-refresh)
	("q" . magit-quit-session)
	("g n" . magit-jump-to-untracked)
	("g s" . magit-jump-to-staged)
	("g t" . magit-jump-to-tracked)
	("g u" . magit-jump-to-unstaged)
	("g z" . magit-jump-to-stashes)
	("g p p" . magit-jump-to-unpushed-to-pushremote)
	("g p u" . magit-jump-to-unpushed-to-upstream)
	("g f p" . magit-jump-to-unpulled-from-pushremote)
	("g f u" . magit-jump-to-unpulled-from-upstream)
	("p" . magit-push-popup)
	("P" . magit-pull-popup)
        ("M-k" . magit-discard)
	;; It seems as if we will have to repeat ourselves
	;; or learn how macros work...
	:map magit-file-section-map
	("C-j" . magit-section-forward-sibling)
	("C-k" . magit-section-backward-sibling)
	:map magit-hunk-section-map
	("C-j" . magit-section-forward-sibling)
	("C-k" . magit-section-backward-sibling)
        :map with-editor-mode-map
        ("RET" . with-editor-finish-if-ryo)))

(use-package git-timemachine
  :defer t
  :ryo
  ("SPC G" git-timemachine)
  :bind
  (:map git-timemachine-mode-map
        ("j" . git-timemachine-show-next-revision)
        ("k" . git-timemachine-show-previous-revision)
        ("," . write-file)))

;;;; Projectile
(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-require-project-root nil)
  (define-key ryo-modal-mode-map (kbd "SPC p") 'projectile-command-map)
  :ryo
  ("SPC SPC" projectile-find-file))

(use-package counsel-projectile
  :commands (couns
             el-projectile projectile-find-file)
  :after (projectile counsel)
  :config (counsel-projectile-mode))

;;; File management
(use-package ranger
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t)
  (setq ranger-show-hidden t)
  :ryo
  ("SPC d" ranger))

;;; General programming concerns
;;;; Parentheses
(electric-pair-mode 1)
;; I don't like how smartparens deals with actual pairs, but its
;; navigation commands are very good
(use-package smartparens
  :ryo
  ("e" sp-forward-sexp :first '(set-mark-here))
  ("E" sp-forward-sexp :first '(set-mark-if-inactive))
  ("M-e" sp-backward-sexp :first '(set-mark-here))
  ("M-E" sp-backward-sexp :first '(set-mark-if-inactive)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package emacs-surround
  :straight (emacs-surround :host github :repo "ganmacs/emacs-surround")
  :config
  (add-to-list 'emacs-surround-alist '("<" . ("<" . ">")))
  (add-to-list 'emacs-surround-alist '(">" . ("< " . " >")))
  (add-to-list 'emacs-surround-alist '(")" . ("( " . " )")))
  (add-to-list 'emacs-surround-alist '("}" . ("{ " . " }")))
  :ryo
  ("S" emacs-surround))

;;;; Indentation
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;;; Linting
(use-package flycheck
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

;;;; Commenting
(use-package evil-nerd-commenter
  :ryo
  ("'" evilnc-comment-or-uncomment-lines)
  ("SPC '" evilnc-comment-or-uncomment-paragraphs))

;;;; Eshell
(use-package eshell-toggle
  :straight (:host github :repo "4DA/eshell-toggle")
  :bind ("C-t" . eshell-toggle)
  :config
  (setq et-use-projectile-root t)
  (setq et-eshell-height-fraction 3))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))
(use-package eshell-autojump)

;;; Language specific programming concerns
;;;; Haskell
(use-package haskell-mode
  :preface
  (defun haskell-backward-sexp (count)
    (interactive "p") (haskell-forward-sexp (- count)))
  :hook (haskell-mode . haskell-decl-scan-mode)
  :ryo
  (:mode 'haskell-mode)
  ("e" haskell-forward-sexp :first '(set-mark-here))
  ("E" haskell-forward-sexp :first '(set-mark-if-inactive))
  ("M-e" haskell-backward-sexp :first '(set-mark-here))
  ("M-E" haskell-backward-sexp :first '(set-mark-if-inactive))
  ("[ [" haskell-ds-backward-decl :first '(set-mark-here))
  ("{ [" haskell-ds-backward-decl :first '(set-mark-if-inactive))
  ("] ]" haskell-ds-forward-decl :first '(set-mark-here))
  ("} ]" haskell-ds-forward-decl :first '(set-mark-if-inactive))
  )

;; (use-package intero
;;   :after haskell-mode
;;   ;; :hook (haskell-mode . intero-mode)
;;   :config (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
;;   :ryo
;;   (:mode 'haskell-mode)
;;   ("SPC m t" intero-type-at)
;;   ("SPC m i" intero-info)
;;   ("SPC m c" intero-repl-eval-region)
;;   ("SPC m e" intero-expand-splice-at-point)
;;   ("SPC m l" intero-repl-load)
;;   ("SPC m r" intero-apply-suggestions)
;;   ("SPC m z" intero-repl)
;;   )

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  :config (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
  :bind
  (:map dante-mode-map
        ("C-c C-c" . dante-eval-block))
  :ryo
  (:mode 'haskell-mode)
  ("SPC m t" dante-type-at)
  ("SPC m i" dante-info)
  ("SPC m e" dante-eval-block)
  )

(use-package shakespeare-mode)
(use-package shm
  :after haskell-mode
  :bind (:map shm-map
              ("C-0" . shm/goto-last-point))
  :ryo
  (:mode 'haskell-mode)
  ("SPC m s" structured-haskell-mode)
  )


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
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                    :compile "cargo build"
                                    :test "cargo test"
                                    :run "cargo run"))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :after (rust-mode company)
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

;;;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook
  (yaml-mode . ryo-modal-mode))

;;;; Lisps
;; (since all of my time is spent in emacs, I should have a lisp plugin)
(use-package lispy
  :config
  (add-hook 'ryo-modal-mode-hook
            (lambda ()
              (when (eq major-mode 'emacs-lisp-mode)
        	(if (bound-and-true-p ryo-modal-mode)
                    (lispy-mode -1)
                  (lispy-mode 1))))))

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;;;; Nix
(use-package nix-mode
  )

;;;; Elm
(use-package reformatter
  :straight (reformatter :host github :repo "purcell/reformatter.el"))
(use-package elm-mode
  :after reformatter
  :straight (elm-mode :host github :repo "jcollard/elm-mode"))

;;;; Org mode install
;; Installing org mode with straight is annoying
;; https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe" "--match=release\*" "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;;;; Org config
(use-package org
  :hook (org . ryo-modal-mode)
  :config
  (require 'ox-md nil t)
  :ryo
  (:mode 'org-mode)
  (">" org-demote-subtree)
  ("<" org-promote-subtree))

(use-package org-bullets
  :hook (org . org-bullets-mode)
  :after org)

;;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown . ryo-modal-mode))

;;;; Lilypond
(when (and (executable-find "lilypond") (eq system-type 'darwin))
  (use-package lilypond-mode
    :straight
    (:local-repo "/usr/local/Cellar/lilypond/2.18.2/share/emacs/site-lisp/lilypond/"
                 :files ("lilypond*.el"))
    :config
    (add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
    :hook
    (lilypond . ryo-modal-mode)))

;;; Kitchen sink
;;;; Pdf
;; Stolen from doom
(use-package pdf-tools
  :preface
  (defun +pdf|cleanup-windows ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive (pdf-tools-install))
  (add-hook 'pdf-view-mode-hook
            (add-hook 'kill-buffer-hook #'+pdf|cleanup-windows nil t))
  (setq-default pdf-view-display-size 'fit-page)
  :bind (:map pdf-view-mode-map
              ("q" . kill-this-buffer))
  )

;;;; 2048
(use-package 2048-game
  :commands (2048-game)
  :bind
  (:map 2048-mode-map
        ("h" . 2048-left)
        ("j" . 2048-down)
        ("k" . 2048-up)
        ("l" . 2048-right)))

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
(use-package emms
  :straight (:repo "git://git.sv.gnu.org/emms.git")
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  ;; (setq emms-source-file-default-directory "~/Music/iTunes/iTunes Media/Music/")
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag)))

;;;; Fake word processor
(use-package flyspell-correct
  :config
  (when (executable-find "aspell")
    (setq-default ispell-program-name "aspell")
    (setq ispell-really-aspell t))
  (setq flyspell-correct-interface #'flyspell-correct-dummy)
  :ryo
  ("g s" flyspell-auto-correct-word)
  ("g S" flyspell-correct-wrapper))

(use-package darkroom
  :preface
  (defun jm/toggle-prose-mode ()
    "Toggle distraction free writing mode for prose."
    (interactive)
    (if (bound-and-true-p darkroom-mode)
        (progn (display-line-numbers-mode 1)
               (darkroom-mode 0)
               (visual-line-mode 0)
               (flyspell-mode 0)
               (text-scale-increase 1))
      (progn (display-line-numbers-mode 0)
             (darkroom-mode 1)
             (visual-line-mode 1)
             (flyspell-mode 1)
             (text-scale-decrease 1))))
  :commands (darkroom-mode darkroom-tentative-mode))

;;;; Email (still a mess)
;; (require 'smtpmail)
;; (when (and (executable-find "mu") (string= (system-name) "jnix"))
;;   (use-package mu4e
;;     :defer t
;;     :straight
;;     (:local-repo "/run/current-system/sw/share/emacs/site-lisp/mu4e/")
;;     :bind
;;     (:map mu)
;;     :custom
;;     (mu4e-maildir "~/Mail/gmail")
;;     (user-mail-address "sefim96@gmail.com")
;;     (user-full-name "Joseph Morag")
;;     ( mu4e-compose-signature nil)
;;     ( message-send-mail-function 'smtpmail-send-it )
;;     ( smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)) )
;;     ( smtpmail-default-smtp-server "smtp.gmail.com" )
;;     ( smtpmail-smtp-server "smtp.gmail.com" )
;;     ( smtpmail-smtp-service 587 )
;;     ( smtpmail-debug-info t )
;;     ( mu4e-sent-folder "/[Gmail]/Sent Mail" )
;;     ( mu4e-drafts-folder "/[Gmail]/Drafts" )
;;     ( mu4e-trash-folder "/[Gmail]/Trash" )
;;     ( mu4e-get-mail-command "mbsync -c ~/.emacs.d/.mbsyncrc gmail" )

;;     ;; :config
;;     ;; (setq mu4e-contexts `( ,(make-mu4e-context
;;     ;;                          :name "gmail"
;;     ;;                          ;; we match based on the contact-fields of the message
;;     ;;                          ;; :match-func (lambda (msg)
;;     ;;                          ;;               (when msg
;;     ;;                          ;;                 (string-match-p "*" (mu4e-message-field msg :maildir))))
;;     ;;                          :vars '( ( user-mail-address      . "sefim96@gmail.com"  )
;;     ;;                                   ( user-full-name         . "Joseph Morag" )
;;     ;; ( mu4e-compose-signature . nil)
;;     ;; ( message-send-mail-function . 'smtpmail-send-it )
;;     ;; ( smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)) )
;;     ;; ( smtpmail-default-smtp-server . "smtp.gmail.com" )
;;     ;; ( smtpmail-smtp-server . "smtp.gmail.com" )
;;     ;; ( smtpmail-smtp-service . 587 )
;;     ;; ( smtpmail-debug-info . t )
;;     ;; ( mu4e-sent-folder . "/[Gmail]/Sent Mail" )
;;     ;; ( mu4e-drafts-folder . "/gmail/[Gmail]/Drafts" )
;;     ;; ( mu4e-trash-folder . "/gmail/[Gmail]/Trash" )
;;     ;; ( mu4e-get-mail-command . "mbsync -c ~/.emacs.d/.mbsyncrc gmail" )
;;     ;;                                   ))
;;     ;;                        ,(make-mu4e-context
;;     ;;                          :name "lionmail"
;;     ;;                          ;; Match on the maildir
;;     ;;                          ;; :match-func (lambda (msg)
;;     ;;                          ;;               (when msg
;;     ;;                          ;;                 (string-match-p "^/lionmail" (mu4e-message-field msg :maildir))))
;;     ;;                          :vars '( ( user-mail-address      . "jm4157@lionmail.com"  )
;;     ;;                                   ( user-full-name         . "Joseph Morag" )
;;     ;;                                   ( mu4e-compose-signature . nil)
;;     ;;                                   ( message-send-mail-function . 'smtpmail-send-it )
;;     ;;                                   ( smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)) )
;;     ;;                                   ( smtpmail-default-smtp-server . "smtp.gmail.com" )
;;     ;;                                   ( smtpmail-smtp-server . "smtp.gmail.com" )
;;     ;;                                   ( smtpmail-smtp-service . 587 )
;;     ;;                                   ( smtpmail-debug-info . t )
;;     ;;                                   ( mu4e-sent-folder . "/lionmail/[Gmail]/Sent Mail" )
;;     ;;                                   ( mu4e-drafts-folder . "/lionmail/[Gmail]/Drafts" )
;;     ;;                                   ( mu4e-trash-folder . "/lionmail/[Gmail]/Trash" )
;;     ;;                                   ( mu4e-get-mail-command . "mbsync -c ~/.emacs.d/.mbsyncrc lionmail" )
;;     ;;                                   ))

;;     ;;                        ,(make-mu4e-context
;;     ;;                          :name "ipower"
;;     ;;                          ;; :match-func (lambda (msg)
;;     ;;                          ;;               (when msg
;;     ;;                          ;;                 (string-match-p "^/ipower" (mu4e-message-field msg :maildir))))
;;     ;;                          :vars '( ( user-mail-address      . "jm@josephmorag.com"  )
;;     ;;                                   ( user-full-name         . "Joseph Morag" )
;;     ;;                                   ( mu4e-compose-signature . nil)
;;     ;;                                   ( message-send-mail-function . 'smtpmail-send-it )
;;     ;;                                   ( smtpmail-starttls-credentials . '(("smtp.ipower.com" 587 nil nil)) )
;;     ;;                                   ( smtpmail-default-smtp-server . "smtp.ipower.com" )
;;     ;;                                   ( smtpmail-smtp-server . "smtp.ipower.com" )
;;     ;;                                   ( smtpmail-smtp-service . 587 )
;;     ;;                                   ( smtpmail-debug-info . t )
;;     ;;                                   ( mu4e-sent-folder . "/ipower/Sent Messages" )
;;     ;;                                   ( mu4e-drafts-folder . "/ipower/Drafts" )
;;     ;;                                   ( mu4e-trash-folder . "/ipower/Trash" )
;;     ;;                                   ( mu4e-get-mail-command . "mbsync -c ~/.emacs.d/.mbsyncrc ipower" )
;;     ;;                                   ))
;;     ;;                        ))
;;     ))

;; (use-package notmuch
;;   :straight (notmuch :local-repo "~/.nix-profile/share/emacs/site-lisp/" :files ("notmuch*.elc"))
;;   :custom
;;   (notmuch-search-oldest-first . nil))

;;;; Wifi management
(when (executable-find "nmcli")
  (use-package nm
    :commands (nm/list-access-points
               nm/list-active-connections
               nm/show-wifi-status
               nm/set-interface
               nm/connect-basic
               nm/connect-with-profile
               nm/connect-vpn-profile)
    :straight (emacs-nm :host github :repo "Kodkollektivet/emacs-nm")
    ))

;;;; Package management
(use-package system-packages
  :commands (system-pacakges-search
             system-pacakges-install
             system-pacakges-uninstall))

;;; End
;; Revert garbage collection to default after loading init
(setq gc-cons-threshold 1000000)

(provide 'init)
;; init.el ends here
