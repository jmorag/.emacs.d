(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix ";")
 '(c-basic-offset 2)
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(darkroom-margins 0.1 t)
 '(dired-auto-revert-buffer t)
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(doom-themes-padded-modeline t)
 '(electric-indent-mode nil)
 '(gif-screencast-convert-program
   "/nix/store/s9zgyg06sqlnk6mss6ixwc5n40bk3rf9-imagemagick-6.9.10-71/bin/convert" t)
 '(gif-screencast-program "scrot" t)
 '(gif-screencast-scale-factor 2 t)
 '(haskell-literate-default 'tex)
 '(lsp-ui-sideline-enable t)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "digest" :query "tag:digest")
     (:name "lists" :query "tag:lists")
     (:name "recurse" :query "to:@lists.community.recurse.com")))
 '(org-export-backends '(ascii html icalendar latex md odt moderncv))
 '(peed-dired-cleanup-on-disable t t)
 '(peep-dired-enable-on-directories t t)
 '(safe-local-variable-values
   '((dante-repl-command-line "nix-shell" "--pure" "--run" "cabal repl valor-test")
     (dante-repl-command-line "nix-shell" "--pure" "--run" "cabal repl  --builddir=dist/dante")
     (dante-command-line "nix-shell" "--pure" "--run" "cabal v2-repl  --builddir=dist/dante")
     (dante-target . "test:Testall")
     (projectile-project-compile-cmd . "nix-shell --run 'make lantern")
     (projectile-project-run-cmd . "nix-shell --run 'make lantern && ./lantern -uiaddr=\":58735\" -headless'")
     (checkdoc-package-keywords-flag)
     (projectile-project-run-cmd . "make lantern && ./lantern -uiaddr=\":58735\" -headless")
     (projectile-project-run-cmd . "yarn start")
     (projectile-project-compilation-cmd . "hugo server -D --disableFastRender")
     (lsp-enabled-clients quote
                          (mspyls))
     (lsp-enabled-clients quote
                          (lsp-python-ms))
     (dante-repl-command-line "nix-shell" "--pure" "--run" "cabal" "new-repl")
     (org-src-preserve-indentation)
     (eval and
           (featurep 'ox-extra)
           (ox-extras-activate
            '(ignore-headlines)))
     (eval require 'ox-texinfo+ nil t)
     (eval require 'ox-extra nil t)
     (dante-command-line "nix-shell" "--pure" "--run" "ghci")
     (dante-repl-command-line "stack" "repl")
     (dante-repl-command-line "stack repl")
     (dante-repl-command-line "nix-shell" "--run" "cabal new-repl")
     (dante-repl-command-line "nix-shell" "--pure" "--run"
                              (concat "cabal" "new-repl"))
     (dante-repl-command-line "nix-shell" "--pure" "--run" "cabal new-repl")
     (dante-repl-command-line "nix-shell" "--pure" "--run" "ghci")
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (package-build-minor-mode)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (haskell-indent-spaces . 2)
     (haskell-process-use-ghci . t)))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(solarized-scale-org-mode-headlines nil t)
 '(solarized-use-mode-italic t t)
 '(swiper-goto-start-of-match t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
