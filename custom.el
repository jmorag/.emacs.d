(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix ";")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(electric-indent-mode nil)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(safe-local-variable-values
   (quote
    ((dante-repl-command-line "stack" "repl")
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
     (intero-targets "othaskell:lib" "othaskell:exe:othaskell-exe" "othaskell:test:othaskell-test")
     (intero-targets "mcc:lib" "mcc:exe:mcc" "mcc:test:testall")
     (haskell-indent-spaces . 2)
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (intero-stack-yaml . "/Users/josephmorag/Code/crossword-helper/stack.yaml"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
