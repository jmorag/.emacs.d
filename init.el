;;; Code:
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
                        
(package-initialize)

;; Use command as meta on mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; Don't dump custom variables into init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Visual stuff
(set-scroll-bar-mode 'nil)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
  (setq doom-one-brighter-modeline t)
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
   "C-a" 'ivy-toggle-fuzzy))

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
  )

;; Swiper
(use-package swiper
  :commands swiper
  :general
  (general-nmap "/" 'swiper)
  )

;; Company
(use-package company
  :config
  (global-company-mode 1)
  (setq company-minimum-prefix-length 2)
  :general
  (general-define-key
   :states '(insert emacs)
   :keymaps '(company-active-map
	      company-filter-map
	      company-search-map
	      company-template-field-map
	      company-template-nav-map)
   "TAB" nil
  ))

;; Yasnipet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :general
  (general-imap "TAB" 'yas-expand-from-trigger-key)
  )

(use-package yasnippet-snippets
  :after yasnippet)

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

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode)
  :general
  (general-nvmap "'" 'evil-commentary-line)
  ;; nnoremap <Leader>' gcap
  (general-nmap "SPC '"
    (general-simulate-key ('evil-commentary "ap")
      :which-key "Toggle paragraph comment")
  ))

(use-package magit
  :commands magit-status
  :general
  (general-nmap
    :prefix "SPC"
    "g" '(magit-status :which-key "Magit"))
  )

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/Code"))

(use-package projectile
  :config (projectile-mode 1)
  :general
  (general-nmap "SPC p" '(projectile-command-map :which-key "Projectile"))
  )

(use-package counsel-projectile
  :commands (counsel-projectile projectile-find-file)
  :after (projectile counsel)
  :config (counsel-projectile-on)
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
  (general-vmap "]" 'er/expand-region
	        "[" 'er/contract-region))

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

;; Haskell config

(provide 'init)
;;; init.el ends here
