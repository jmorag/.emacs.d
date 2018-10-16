;; Code:

;;; Startup improvements
;;;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;;; Fonts and scrolling 
(column-number-mode 1)
(set-face-font 'default "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; Make scrolling work more like vim's
(scroll-lock-mode 1)

;;;; Start off with giant gc threshold
(setq gc-cons-threshold most-positive-fixnum)

;;;; Don't dump custom variables into init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; Bootstrap package management
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

;;;; Bootstrap `use-package' -- deprecated package.el
;; (package-initialize)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t)

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

;;; Core editor facilities
;; This sets up basic text editing commands
;;;; Key chord
(use-package use-package-chords
  :config
  (key-chord-mode 1))

;;;; General utilities
(defun ryo-enter () (interactive) (ryo-modal-mode +1))
(defun ryo-leave () (interactive) (ryo-modal-mode -1))
(defun ryo-tbd () (interactive) (message "Key not assigned"))
(defun set-mark-if-inactive () (interactive)
       (unless (use-region-p) (set-mark (point))))
(defun set-mark-here () (interactive) (set-mark (point)))
(defun unset-mark () (interactive) (deactivate-mark))

(defun backward-same-syntax (count)
  (interactive "p")
  (forward-same-syntax (- count)))

(defun kak/select-to-char (arg char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (progn
      (forward-char direction)
      (unwind-protect
	  (search-forward (char-to-string char) nil nil arg)
	(backward-char direction))
      (point))))

(defun kak/select-up-to-char (arg char)
  "Select up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
		     (read-char "Select to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (search-forward (char-to-string char) nil nil arg)
  (point))

(defun kak/x (&optional count)
  "Select COUNT lines from the current line.

Note that kakoune's x doesn't behave exactly like this,
but I like this behavior better."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line count))

(defun kak/X (&optional count)
  "Extend COUNT lines from the current line."
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (set-mark (point)))
  (forward-line count))

(defun kak/d ()
  "Kill selected text."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char 1 t)))

(defun kak/p (&optional count)
  "Yank COUNT times after the point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (yank)))
  )

(defun jm/comment-region-or-line (count) (interactive "p")
       (if (use-region-p)
	   (comment-or-uncomment-region (region-beginning) (region-end))
	 (save-excursion (comment-line count))))

(defun insert-line-below (&optional count)
  "Insert COUNT empty lines below the current line."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line count)))

(defun insert-line-above (&optional count)
  "Insert COUNT empty lines above the current line."
  (interactive "p")
  (save-excursion
    (end-of-line 0)
    (open-line count)))

(defun paste-above (&optional count)
  "Paste (yank) COUNT times above the current line."
  (interactive "p")
  (save-excursion
    (dotimes (_ count) (end-of-line 0)
	     (newline)
	     (yank))))

(defun paste-below (&optional count)
  "Paste (yank) COUNT times below the current line."
  (interactive "p")
  (save-excursion
    (dotimes (_ count) (end-of-line)
	     (newline)
	     (yank))))

(defun kak/downcase ()
  "Downcase region."
  (interactive)
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (+ 1 (point)))
    ))

(defun kak/upcase ()
  "Upcase region."
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-region (point) (1+ (point)))
    ))

(defun kak/replace-char (char)
  "Replace selection with CHAR."
  (interactive "cReplace with char: ")
  (if (use-region-p)
      (progn (let ((region-size (- (region-end) (region-beginning))))
	       (delete-region (region-beginning) (region-end))
	       (save-excursion
		 (insert-char char region-size t))))
    (progn (delete-region (point) (1+ (point)))
	   (save-excursion
	     (insert-char char)))))

(defun kak/replace-selection ()
  "Replace selection with killed text."
  (interactive)
  (if (use-region-p)
      (progn (delete-region (region-beginning) (region-end))
	     (yank))
    (progn (delete-region (point) (1+ (point)))
	   (yank))))

(defun kak/o (count)
  (interactive "p")
  (end-of-line)
  (dotimes (_ count)
    (electric-newline-and-maybe-indent)
    (indent-for-tab-command)))

(defun kak/O (count)
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)
    (indent-for-tab-command)))

(defun kak/join ()
  "Join the next line to the current one."
  (interactive) (join-line 1))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

;; Macros - https://www.emacswiki.org/emacs/KeyboardMacros
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;;;; Basic keybindings
(use-package ryo-modal
  :straight (ryo-modal :type git :host github :repo "Kungsgeten/ryo-modal"
		       :fork (:host github :repo "jmorag/ryo-modal"))
  :chords ("fd" . ryo-enter)
  :config
  (setq-default cursor-type '(bar . 1))
  (setq ryo-modal-cursor-type 'box)
  (add-hook 'prog-mode-hook 'ryo-modal-mode)
  (ryo-modal-keys
   ("a" forward-char :exit t)
   ("A" move-end-of-line :exit t)
   ("b" backward-same-syntax :first '(set-mark-here))
   ("B" backward-same-syntax :first '(set-mark-if-inactive))
   ("c" kak/d :exit t)
   ("C" kill-line :exit t)
   ("d" kak/d)
   ("D" kill-line)
   ("e" ryo-tbd)
   ("E" ryo-tbd)
   ("g" (("h" beginning-of-line)
	 ("j" end-of-buffer)
	 ("k" beginning-of-buffer)
	 ("g" beginning-of-buffer)
	 ("l" end-of-line)
	 ("i" back-to-indentation)) :first '(deactivate-mark))
   ("G" (("h" beginning-of-line)
	 ("j" end-of-buffer)
	 ("k" beginning-of-buffer)
	 ("g" beginning-of-buffer)
	 ("l" end-of-line)
	 ("i" back-to-indentation)) :first '(set-mark-if-inactive))
   ("h" backward-char :first '(deactivate-mark))
   ("H" backward-char :first '(set-mark-if-inactive))
   ("i" ryo-leave)
   ("I" back-to-indentation :exit t)
   ("j" next-line :first '(deactivate-mark))
   ("J" next-line :first '(set-mark-if-inactive))
   ("k" previous-line :first '(deactivate-mark))
   ("K" previous-line :first '(set-mark-if-inactive))
   ("l" forward-char :first '(deactivate-mark))
   ("L" forward-char :first '(set-mark-if-inactive))
   ("o" kak/o :exit t)
   ("O" kak/O :exit t)
   ("p" kak/p)
   ("P" yank-pop)
   ("q" kmacro-call-macro)
   ("Q" toggle-kbd-macro-recording-on)
   ("r" kak/replace-char)
   ("R" kak/replace-selection)
   ("t" kak/select-to-char :first '(set-mark-here))
   ("T" kak/select-to-char :first '(set-mark-if-inactive))
   ("w" forward-same-syntax :first '(set-mark-here))
   ("W" forward-same-syntax :first '(set-mark-if-inactive))
   ("x" kak/x)
   ("X" kak/X)
   ("y" kill-ring-save)
   ("Y" ryo-tbd)
   ("z" ryo-tbd)
   ("Z" ryo-tbd)
   ("." ryo-modal-repeat)
   ("," save-buffer)
   ("'" jm/comment-region-or-line)
   (";" unset-mark)
   ("M-;" exchange-point-and-mark)
   ("*" ryo-tbd)
   ("`" kak/downcase)
   ("~" kak/upcase)
   ("%" mark-whole-buffer)
   ("\\"  ryo-tbd)
   ("M-`" xah-toggle-letter-case)
   ("M-j" kak/join)
   ("C-e" eval-last-sexp)
   ("[" (("SPC" insert-line-above)
	 ("b" previous-buffer)
	 ("p" paste-above)))
   ("]" (("SPC" insert-line-below)
	 ("b" next-buffer)
	 ("p" paste-below)))
   ("C-u" scroll-down-command :first '(deactivate-mark))
   ("C-d" scroll-up-command :first '(deactivate-mark))
   ("0" "M-0" :norepeat t)
   ("1" "M-1" :norepeat t)
   ("2" "M-2" :norepeat t)
   ("3" "M-3" :norepeat t)
   ("4" "M-4" :norepeat t)
   ("5" "M-5" :norepeat t)
   ("6" "M-6" :norepeat t)
   ("7" "M-7" :norepeat t)
   ("8" "M-8" :norepeat t)
   ("9" "M-9" :norepeat t)))
;; Register bindings
(define-key ryo-modal-mode-map (kbd "\"") ctl-x-r-map)

(defun vimlike-navigation (keymap)
  "Add basic navigation bindings to a KEYMAP."
  (progn
    (define-key keymap "j" 'next-line)
    (define-key keymap "k" 'previous-line)
    (define-key keymap "\C-d" 'scroll-up-command)
    (define-key keymap "\C-u" 'scroll-down-command)
    (define-key keymap "o" 'other-window)
    ))

;; Help mode bindings
(vimlike-navigation help-mode-map)
(define-key ryo-modal-mode-map (kbd "SPC h") 'help-command)
;; Package mode bindings
(eval-after-load 'package '(vimlike-navigation package-menu-mode-map))
;; Ibuffer mode bindings
(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
     (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
     (define-key ibuffer-mode-map (kbd "K") 'ibuffer-do-kill-lines)))

;; ;; Dired mode bindings
;; (use-package dired
;;   :straight nil
;;   :bind (:map dired-mode-map
;; 	      ("j" . dired-next-line)
;; 	      ("k" . dired-previous-line)
;; 	      ("K" . dired-do-kill-lines)))

;;;; Multiple cursors and expand region
(use-package multiple-cursors
  :ryo
  ("m" mc/mark-next-like-this)
  ("M" mc/skip-to-next-like-this)
  ("n" mc/mark-previous-like-this)
  ("N" mc/skip-to-previous-like-this)
  ("C-v" set-rectangular-region-anchor)
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package expand-region
  :ryo
  ("v" er/expand-region))

(use-package visual-regexp
  :ryo
  ("s" vr/mc-mark)
  ("?" vr/replace)
  ("M-/" vr/query-replace))

(use-package phi-search
  :bind (("C-s" . phi-search)
	 ("C-r" . phi-search-backward)))

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
    (exec-path-from-shell-initialize)))

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
  (global-display-line-numbers-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Required for dashboard
(use-package page-break-lines
  :config (global-page-break-lines-mode))

;; Nice start screen
(use-package dashboard
  :straight (dashboard :type git :host github :repo "rakanalh/emacs-dashboard")
  :after page-break-lines
  :defer nil
  :config
  (dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
	      ("j" . widget-forward)
	      ("k" . widget-backward)))

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 4))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1))

;;; Saner defaults
;; Fix some defaults
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

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;;; Interface management with ivy and which-key
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
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 10)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  (define-key ivy-minibuffer-map "\C-j" 'ivy-next-line)
  (define-key ivy-minibuffer-map "\C-k" 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-toggle-fuzzy))

(use-package ivy-rich
  :config (ivy-rich-mode 1))

;; Counsel (same as Ivy above)
(use-package counsel
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
  (counsel-mode 1)
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

;;; Autocompletion
;;;; Company
(use-package company
  :config
  (global-company-mode 1)
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

;; Make company prettier
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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
  (yas-global-mode 1))

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
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'prog-mode-hook 'outline-minor-mode))

(use-package navi-mode
  :ryo ("-" outshine-navi)
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
    (if (ryo-modal-mode)
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

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Code"))

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
  :commands (counsel-projectile projectile-find-file)
  :after (projectile counsel)
  :config (counsel-projectile-mode))

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
  :ryo
  (:mode 'org-mode)
  (">" org-demote-subtree)
  ("<" org-promote-subtree))

;;; File management
(use-package ranger
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t)
  (setq ranger-show-hidden t))

;;; General programming concerns
;;;; Parentheses 
;; Smartparens is very heavy and weird. This stays more or less out of the way
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package emacs-surround
  :straight (emacs-surround :type git :host github :repo "ganmacs/emacs-surround")
  :config
  (add-to-list 'emacs-surround-alist '("<" . ("<" . ">")))
  (add-to-list 'emacs-surround-alist '(">" . ("< " . " >")))
  (add-to-list 'emacs-surround-alist '(")" . ("( " . " )")))
  (add-to-list 'emacs-surround-alist '("}" . ("{ " . " }")))
  :ryo
  ("S" emacs-surround))

;;;; Indentation 
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  )

;;;; Linting 
(use-package flycheck
  :ryo
  ("SPC" (("a t" flycheck-mode :name "Flycheck toggle")))
  ("] e" flycheck-next-error :first '(deactivate-mark))
  ("[ e" flycheck-previous-error :first '(deactivate-mark)))

;;; Language specific programming concerns
;;;; Haskell - (should rethink to lsp)
(use-package haskell-mode
  :config
  (setq haskell-process-type 'stack-ghci))

;; (use-package intero
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hasky-stack)
(use-package shakespeare-mode)

;;;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;; Lisps 
;; (since all of my time is spent in emacs, I should have a lisp plugin)
;; There is a choice between lispy, paredit, smart-parens, and parinfer
(use-package lispy
  :config
  (add-hook 'ryo-modal-mode-hook
	    (lambda ()
	      (when (eq major-mode 'emacs-lisp-mode)
		(if ryo-modal-mode
                    (lispy-mode -1)
                  (lispy-mode 1))))))

(use-package elisp-slime-nav
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

;;; End
;; Revert garbage collection to default after loading init
(setq gc-cons-threshold 1000000)

(provide 'init)
;; init.el ends here
