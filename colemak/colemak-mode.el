;;; colemak.el --- Minor mode for remapping keys to other keys.

;;; Commentary:

;; Global minor mode for remapping hjkl to neio, etc. to preserve muscle memory.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'f)
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
                     ("g h" "g n")
                     ("g j" "g e")
                     ("g k" "g i")
                     ("g l" "g o")
                     ("g n" "g j")
                     ("g e" "g k")
                     ("g i" "g l")
                     ("g o" "g h")
                     ("g H" "g N")
                     ("g J" "g E")
                     ("g K" "g I")
                     ("g L" "g O")
                     ("g N" "g J")
                     ("g E" "g K")
                     ("g I" "g L")
                     ("g O" "g H")
                     ("G h" "G n")
                     ("G j" "G e")
                     ("G k" "G i")
                     ("G l" "G o")
                     ("G n" "G j")
                     ("G e" "G k")
                     ("G i" "G l")
                     ("G o" "G h")
                     ("G H" "G N")
                     ("G J" "G E")
                     ("G K" "G I")
                     ("G L" "G O")
                     ("G N" "G J")
                     ("G E" "G K")
                     ("G I" "G L")
                     ("G O" "G H")))
  "Key translations between qwerty and colemak layouts.")

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
    special-mode-map)
  "Collection of keymaps for which to run colemak translations.")

(defun apply-or-revert-translations (translations keymap-symbol apply)
  "If APPLY then apply TRANSLATIONS to KEYMAP-SYMBOL. Otherwise revert them."
  (when (boundp keymap-symbol)
    (let* ((keymap (symbol-value keymap-symbol))
           (backup-symbol (intern (concat (symbol-name keymap-symbol) "-backup")))
           (backup-map (if (boundp backup-symbol)
                           (symbol-value backup-symbol)
                         (set backup-symbol (copy-keymap keymap)))))
      (cl-loop for (from to) in translations
               for command = (lookup-key backup-map from)
               when (commandp command)
               do (define-key keymap (if apply to from) command)))))


(defvar keyboard-layout "QWERTY")

;;;###autoload
(define-minor-mode colemak-mode
  "Toggle colemak-mode."
  :init-value nil
  :lighter " Colemak"
  :global t
  (add-to-list 'mode-line-misc-info '(keyboard-layout (" " keyboard-layout " ")))
  (setq keyboard-layout (if colemak-mode "Colemak" "QWERTY"))
  (f-delete (f-long "~/.config/i3/config") t)
  (f-symlink (f-long (if colemak-mode "~/.config/i3/config-colemak"
                       "~/.config/i3/config-qwerty"))
             (f-long "~/.config/i3/config"))
  (shell-command "i3-msg restart")
  (doom-modeline-refresh-frame)
  (cl-loop for map in colemak-translation-maps
           do (apply-or-revert-translations
               colemak-translations map colemak-mode)))

(provide 'colemak-mode)
;;; colemak-mode.el ends here
