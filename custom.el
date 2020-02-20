(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix ";")
 '(c-basic-offset 2 t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(darkroom-margins 0.1 t)
 '(dired-auto-revert-buffer t)
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(electric-indent-mode nil)
 '(haskell-literate-default (quote tex))
 '(org-export-backends (quote (ascii html icalendar latex md odt moderncv)))
 '(peed-dired-cleanup-on-disable t t)
 '(peep-dired-enable-on-directories t t)
 '(safe-local-variable-values
   (quote
    ((org-src-preserve-indentation)
     (eval and
           (featurep
            (quote ox-extra))
           (ox-extras-activate
            (quote
             (ignore-headlines))))
     (eval require
           (quote ox-texinfo+)
           nil t)
     (eval require
           (quote ox-extra)
           nil t)
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
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (haskell-indent-spaces . 2)
     (haskell-process-use-ghci . t))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(swiper-goto-start-of-match t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
