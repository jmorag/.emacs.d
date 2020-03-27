;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'dash)
(require 'dash-functional)
(require 'cl-lib)
(require 's)
(require 'bind-key)
(defvar colemak-translations
  (-tree-map #'kbd '(("h" "n")
                     ("j" "e")
                     ("k" "i")
                     ("l" "o")
                     ("n" "j")
                     ("e" "k")
                     ("i" "l")
                     ("o" "h")
                     ("C-h" "C-n")
                     ("C-j" "C-e")
                     ("C-k" "<C-i>")
                     ("C-l" "C-o")
                     ("C-n" "C-j")
                     ("C-e" "C-k")
                     ("<C-i>" "C-l")
                     ("C-o" "C-h")
                     ("H" "N")
                     ("J" "E")
                     ("K" "I")
                     ("L" "O")
                     ("N" "J")
                     ("E" "K")
                     ("I" "L")
                     ("O" "H")
                     ;; ("g h" "g n")
                     ;; ("g j" "g e")
                     ;; ("g k" "g i")
                     ;; ("g l" "g o")
                     ;; ("g n" "g j")
                     ;; ("g e" "g k")
                     ;; ("g i" "g l")
                     ;; ("g o" "g h")
                     ;; ("g H" "g N")
                     ;; ("g J" "g E")
                     ;; ("g K" "g I")
                     ;; ("g L" "g O")
                     ;; ("g N" "g J")
                     ;; ("g E" "g K")
                     ;; ("g I" "g L")
                     ;; ("g O" "g H")
                     ;; ("G H" "G N")
                     ;; ("G J" "G E")
                     ;; ("G K" "G I")
                     ;; ("G L" "G O")
                     ;; ("G N" "G J")
                     ;; ("G E" "G K")
                     ;; ("G I" "G L")
                     ;; ("G O" "G H")
                     ))
  "Key translations between qwerty and colemak layouts.")

(defun apply-translations (translations keymap-symbol)
  "Apply TRANSLATIONS to keymap denoted by KEYMAP-SYMBOL."
  (let* ((keymap (symbol-value keymap-symbol))
         (backup-symbol (intern (concat (symbol-name keymap-symbol) "-backup")))
         (backup-map (if (boundp backup-symbol)
                         (symbol-value backup-symbol)
                       (set backup-symbol (copy-keymap keymap)))))
    (cl-loop for (from to) in translations
             do (define-key keymap to (lookup-key backup-map from)))))

(defun revert-translations (translations keymap-symbol)
  "Revert the effects of calling (apply-translations TRANSLATIONS KEYMAP-SYMBOL)."
  ;; The backup map should really exist already
  (let ((keymap (symbol-value keymap-symbol))
        (backup-map
         (symbol-value (intern (concat (symbol-name keymap-symbol) "-backup")))))
    (cl-loop for (from _) in translations
             do (define-key keymap from (lookup-key backup-map from)))))

(apply-translations colemak-translations 'ryo-modal-mode-map)
(revert-translations colemak-translations 'ryo-modal-mode-map)

(defvar colemak-translation-maps
  '(ryo-modal-mode-map
    lispy-mode-map
    navi-mode-map
    company-mode-map
    ivy-minibuffer-map
    dired-mode-map
    peep-dired-mode-map
    treemacs-mode-map
    disk-usage-mode-map
    magit-status-mode-map
    magit-file-section-map
    magit-hunk-section-map
    pdf-view-mode-map
    ryo-pdf-view-mode-map
    2048-mode-map
    xkcd-mode-map
    notmuch-search-mode-map
    notmuch-tree-mode-map
    special-mode-map
    )
  "Collection of keymaps for which to run colemak translations.")

(define-minor-mode colemak-mode
  "Toggle colemak-mode."
  :init-value nil
  :lighter " Colemak"
  :global t
  (progn
    (cl-loop for map in colemak-translation-maps
             do (funcall
                 (if colemak-mode 'apply-translations 'revert-translations)
                 colemak-translations map))))

(global-set-key (kbd "C-c c") 'colemak-mode)
(provide 'key-translation)
;;; key-translation.el ends here
