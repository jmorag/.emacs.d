;;; Code:
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

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(use-package doom-themes
  :config (load-theme 'doom-one t))

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

;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-minibuffer t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

(evil-collection-swap-key nil 'evil-motion-state-map
  ";" ":")

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-indent-textobject)

;; General
(use-package general
  :after evil
  :config (general-evil-setup))

;; Bind fd escape
(general-imap "f"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "d" 'evil-normal-state))
(general-vmap "f"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "d" 'evil-normal-state))
(general-define-key :state 'replace "f"
		    (general-key-dispatch 'self-insert-command
		      :timeout 0.25
		      "d" 'evil-normal-state))

;; Comma to save
(general-nmap "," 'save-buffer)

;; vnoremap < <gv
(general-vmap "<"
  '(lambda ()
     (interactive)
     (evil-shift-left (region-beginning) (region-end))
     (evil-normal-state)
     (evil-visual-restore)))

;; vnoremap > >gv
(general-vmap ">"
  '(lambda ()
     (interactive)
     (evil-shift-right (region-beginning) (region-end))
     (evil-normal-state)
     (evil-visual-restore)))

(general-nmap "H" 'evil-first-non-blank)
(general-nmap "L" 'evil-end-of-line)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps t)
  (setq which-key-idle-delay 0.1))

;; Ivy (taken from "How to make your own Spacemacs")
(use-package ivy
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 10)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  :general
  (general-imap
    :keymap '(ivy-mode-map counsel-mode-map)
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
    "C-a" 'ivy-toggle-fuzzy)
  (general-nmap "SPC b" 'ivy-switch-buffer)
  )

;; Counsel (same as Ivy above)
(use-package counsel
  :commands          ; Load counsel when any of these commands are invoked
  (counsel-M-x       ; M-x use counsel
   counsel-find-file ; C-x C-f use counsel-find-file
   counsel-recentf   ; search recently edited files
   counsel-git       ; search for files in git repo
   counsel-git-grep  ; search for regexp in git repo
   counsel-ag        ; search for regexp in git repo using ag
   counsel-locate)   ; search for files or else using locate
  :general
  (general-nmap "SPC ;" 'counsel-M-x)
  (general-nmap "SPC f" 'counsel-find-file)
  (general-nmap "C-p" 'counsel-yank-pop)
  (general-nmap "SPC /" 'counsel-rg)
  :config
  (setq counsel-rg-base-command
	"rg -i -M 120 --follow --glob \"!.git/*\" --no-heading --ignore-case\
      --line-number --column --color never %s .")
  )

;; Swiper
(use-package swiper
  :commands swiper
  :general
  ;; cannibalize evil search command
  ;; for some reason, the search direction is set to backwards by default, so the behavior of
  ;; "n" and "N" are switched until you actually perform an evil forward search.
  ;; I literally never use backward search, so this behavior is useless to me.
  ;; Hence the following hack to fix this.
  ;; TODO: come up with a better use for ?
  (general-nmap "/" 'swiper
    "n" 'evil-search-previous
    "N" 'evil-search-next
    "?" nil)
  )

;; Company
(use-package company
  :config
  (global-company-mode 1)
  (setq company-minimum-prefix-length 2)
  ;; For some reason, these bindings don't work with general
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
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
  :general
  (general-imap "TAB" 'yas-expand-from-trigger-key)
  )

;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
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
  :general
  (general-mmap "f" 'avy-goto-char-in-line)
  (general-define-key :states '(normal insert emacs)
		      "C-f" 'avy-goto-char-timer))

;; Make "j" and "k" traverse visual lines
(general-mmap "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

;; Bind = to the upgraded emacs align regexp
(general-mmap "=" 'align-regexp)

;; Better window moving
(general-nmap "C-h" 'evil-window-left)
(general-nmap "C-j" 'evil-window-down)
(general-nmap "C-k" 'evil-window-up)
(general-nmap "C-l" 'evil-window-right)
(general-nmap "SPC w" 'evil-window-map)
;; Replace stolen C-h command
(general-nvmap :prefix "SPC" "h" 'help-command)

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode)
  :general
  (general-nvmap "'" 'evil-commentary-line)
  ;; nnoremap <Leader>' gcap
  (general-nmap "SPC '"
    (general-simulate-key "gcap" :state 'normal
      :which-key "Toggle paragraph comment")
    ))

(use-package magit
  :commands magit-status
  :general
  (general-nmap
    :prefix "SPC"
    "g" '(magit-status :which-key "Magit"))
  )

(use-package evil-magit
  :after magit)

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Code"))

(use-package projectile
  :config
  (projectile-mode 1)
  :general
  (general-nmap "SPC p" '(projectile-command-map :which-key "Projectile"))
  )

(use-package counsel-projectile
  :commands (counsel-projectile projectile-find-file)
  :after (projectile counsel)
  :config (counsel-projectile-mode)
  )

(use-package flycheck
  :init (global-flycheck-mode)
  :general
  (general-nmap "]e" 'flycheck-next-error)
  (general-nmap "[e" 'flycheck-previous-error)
  (general-nmap :prefix "SPC"
    "a" (general-simulate-key "C-c !" :which-key "Flycheck"))
  (general-nmap "C-c ! t" '(flycheck-mode :which-key "Toggle flycheck in current buffer"))
  )

(use-package evil-multiedit
  :config (evil-multiedit-default-keybinds))

(use-package expand-region
  :general
  (general-vmap "v" 'er/expand-region
    "V" 'er/contract-region))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (setq evil-goggles-blocking-duration 0.010))

(general-vmap "SPC e"
  '(lambda ()
     (interactive)
     (eval-region (region-beginning) (region-end))
     (evil-normal-state)))

(general-nmap "SPC e" 'eval-last-sexp)

;; Tabs
(use-package eyebrowse
  :init
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode))

(use-package evil-vimish-fold
  :config (evil-vimish-fold-mode 1))

;; Be strict about parens and indentation
(use-package smartparens
  :config
  (smartparens-mode))

(use-package evil-smartparens
  :after (smartparens evil)
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (setq evil-smartparens-threshold 1250))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;; Copied from evil-unimpaired which is not on melpa for some reason
(defun evil-unimpaired-paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired-paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired-insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired-insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(general-nmap "[p" 'evil-unimpaired-paste-above)
(general-nmap "]p" 'evil-unimpaired-paste-below)

(general-nmap "[ SPC" 'evil-unimpaired-insert-space-above)
(general-nmap "] SPC" 'evil-unimpaired-insert-space-below)

;; File navigation
(use-package ranger
  :config (ranger-override-dired-mode t))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :commands treemacs
  :general
  (general-mmap
    :keymaps '(global evil-treemacs-state-map)
    "-" 'treemacs))

(use-package treemacs-evil
  :after (treemacs evil)
  )

(use-package treemacs-projectile
  :after treemacs projectile)

(provide 'init)
;;; init.el ends here
