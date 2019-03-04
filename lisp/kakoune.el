;;; kakoune.el --- A simulation, but not emulation, of kakoune, in emacs

;; Author: Joseph Morag <jm4157@columbia.edu>
;; Version: 0.1
;; Package-Requires: ((ryo-modal "0.4") (multiple-cursors "1.4") ())

;;; Commentary:
;; This package provides many, but not all of the editing primitives in the kakoune editor.
;; Unlike evil-mode for vim, this is very shallow emulation, and seeks to do as little
;; work as possible, leveraging Emacs native editing commmands and the work of other
;; packages wherever possible.
(require 'ryo-modal)

;;; Code:
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

(defvar kak/last-char-selected-to " " "kak/select-to-char updates this variable")

(defun kak/select-to-char (arg char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncSelect to char: ")
  (setq kak/last-char-selected-to char)
  (let ((direction (if (>= arg 0) 1 -1)))
    (progn
      (forward-char direction)
      (unwind-protect
	  (search-forward (char-to-string char) nil nil arg)
	(backward-char direction))
      (point))))

(defun kak/select-to-char-again (&optional count)
  "Expand the selection to whatever the last 't' command was."
  (interactive "p")
  (kak/select-to-char count kak/last-char-selected-to))

(defun kak/x (count)
  "Select COUNT lines from the current line.

Note that kakoune's x doesn't behave exactly like this,
but I like this behavior better."
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line count))

(defun kak/X (count)
  "Extend COUNT lines from the current line."
  (interactive "p")
  (beginning-of-line)
  (unless (use-region-p) (set-mark (point)))
  (forward-line count))

(defun kak/d (count)
  "Kill selected text or COUNT chars."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (delete-char count t)))

(defun kak/p (count)
  "Yank COUNT times after the point."
  (interactive "p")
  (dotimes (_ count) (save-excursion (yank)))
  )

(defun insert-line-below (count)
  "Insert COUNT empty lines below the current line."
  (interactive "p")
  (save-excursion
    (end-of-line)
    (open-line count)))

(defun insert-line-above (count)
  "Insert COUNT empty lines above the current line."
  (interactive "p")
  (save-excursion
    (end-of-line 0)
    (open-line count)))

(defun paste-above (count)
  "Paste (yank) COUNT times above the current line."
  (interactive "p")
  (save-excursion
    (dotimes (_ count) (end-of-line 0)
	     (newline)
	     (yank))))

(defun paste-below (count)
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
    (electric-newline-and-maybe-indent)))

(defun kak/O (count)
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ count)
    (newline)
    (forward-line -1)))

(defun kak/join ()
  "Join the next line to the current one."
  (interactive) (join-line 1))

(defun kak/Y (count)
  "Copy to the end of the line"
  (interactive "p")
  (save-excursion
    (let ((cur (point)))
      (move-end-of-line count)
      (kill-ring-save cur (point)))))

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

;;;; Exchanges - stolen from evil-exchange pacakge
(defcustom holy-exchange-highlight-face 'highlight
  "Face used to highlight marked area."
  :type 'sexp
  :group 'holy-exchange)

(defvar holy-exchange--position nil "Text position which will be exchanged.")

(defvar holy-exchange--overlays nil "Overlays used to highlight marked area.")

(defun holy-exchange--highlight (beg end)
  (let ((o (make-overlay beg end nil t nil)))
    (overlay-put o 'face holy-exchange-highlight-face)
    (add-to-list 'holy-exchange--overlays o)))

(defun holy-exchange--clean ()
  (setq holy-exchange--position nil)
  (mapc 'delete-overlay holy-exchange--overlays)
  (setq holy-exchange--overlays nil))

(defun holy-exchange (beg end)
  "Exchange two regions."
  (interactive "r")
  (let ((beg-marker (copy-marker beg t))
        (end-marker (copy-marker end nil)))
    (if (null holy-exchange--position)
        ;; call without holy-exchange--position set: store region
        (progn
          (setq holy-exchange--position (list (current-buffer) beg-marker end-marker))
          ;; highlight area marked to exchange
          (holy-exchange--highlight beg end))
      ;; secondary call: do exchange
      (cl-destructuring-bind
          (orig-buffer orig-beg orig-end) holy-exchange--position
        (holy-exchange--do-swap (current-buffer) orig-buffer
                                beg-marker end-marker
                                orig-beg orig-end
                                #'delete-and-extract-region #'insert)))))

(defun holy-exchange--do-swap (curr-buffer orig-buffer curr-beg curr-end orig-beg
                                           orig-end extract-fn insert-fn)
  "This function does the real exchange work. Here's the detailed steps:

1. call extract-fn with orig-beg and orig-end to extract orig-text.
2. call extract-fn with curr-beg and curr-end to extract curr-text.
3. go to orig-beg and then call insert-fn with curr-text.
4. go to curr-beg and then call insert-fn with orig-text.
After step 2, the two markers of the same beg/end pair (curr or orig)
will point to the same position. So if orig-beg points to the same position
of curr-end initially, orig-beg and curr-beg will point to the same position
before step 3. Because curr-beg is a marker which moves after insertion, the
insertion in step 3 will push it to the end of the newly inserted text,
thus resulting incorrect behaviour.
To fix this edge case, we swap two extracted texts before step 3 to
effectively reverse the (problematic) order of two `holy-exchange' calls."
  (if (eq curr-buffer orig-buffer)
      ;; in buffer exchange
      (let ((adjacent  (equal (marker-position orig-beg) (marker-position curr-end)))
            (orig-text (funcall extract-fn orig-beg orig-end))
            (curr-text (funcall extract-fn curr-beg curr-end)))
        ;; swaps two texts if adjacent is set
        (let ((orig-text (if adjacent curr-text orig-text))
              (curr-text (if adjacent orig-text curr-text)))
          (save-excursion
            (goto-char orig-beg)
            (funcall insert-fn curr-text)
            (goto-char curr-beg)
            (funcall insert-fn orig-text))))
    ;; exchange across buffers
    (let ((orig-text (with-current-buffer orig-buffer
                       (funcall extract-fn orig-beg orig-end)))
          (curr-text (funcall extract-fn curr-beg curr-end)))
      (save-excursion
        (with-current-buffer orig-buffer
          (goto-char orig-beg)
          (funcall insert-fn curr-text))
        (goto-char curr-beg)
        (funcall insert-fn orig-text))))
  (holy-exchange--clean))

(defun holy-exchange-cancel ()
  "Cancel current pending exchange."
  (interactive)
  (if (null holy-exchange--position)
      (message "No pending exchange")
    (holy-exchange--clean)
    (message "Exchange cancelled")))

;;;; Basic keybindings
(use-package ryo-modal
  :straight (ryo-modal :host github :repo "Kungsgeten/ryo-modal"
		       :fork (:host github :repo "jmorag/ryo-modal"))
  :chords ("fd" . ryo-enter)
  :bind ("C-z" . ryo-modal-mode) ;; In order to be useful in macros
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
   ("g x" holy-exchange)
   ("g X" holy-exchange-cancel)
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
   ("Q" ryo-tbd)
   ("r" kak/replace-char)
   ("R" kak/replace-selection)
   ("t" kak/select-to-char :first '(set-mark-here))
   ("T" kak/select-to-char :first '(set-mark-if-inactive))
   ("w" forward-same-syntax :first '(set-mark-here))
   ("W" forward-same-syntax :first '(set-mark-if-inactive))
   ("x" kak/x)
   ("X" kak/X)
   ("y" kill-ring-save)
   ("Y" kak/Y)
   ("Z" ryo-tbd)
   ("," save-buffer)
   ("." kak/select-to-char-again :first '(set-mark-if-inactive))
   (";" (("q" delete-window)
         ("v" split-window-horizontally)
         ("s" split-window-vertically)
         ("i" goto-init-file)))
   ("<tab>" (("h" windmove-left)
             ("j" windmove-down)
             ("k" windmove-up)
             ("l" windmove-right)))
   ("M-;" exchange-point-and-mark)
   ("*" ryo-tbd)
   ("`" kak/downcase)
   ("~" kak/upcase)
   ("%" mark-whole-buffer)
   ("\\" ryo-tbd)
   ("M-`" xah-toggle-letter-case)
   ("M-j" kak/join)
   ("C-e" eval-last-sexp)
   ("[" (("SPC" insert-line-above)
         ("b" previous-buffer)
         ("p" paste-above)))
   ("]" (("SPC" insert-line-below)
         ("b" next-buffer)
         ("p" paste-below)))
   ("[ [" backward-paragraph :first '(set-mark-here))
   ("{ [" backward-paragraph :first '(set-mark-if-inactive))
   ("] ]" forward-paragraph :first '(set-mark-here))
   ("} ]" forward-paragraph :first '(set-mark-if-inactive))
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
   ("9" "M-9" :norepeat t)
   ("-" "M--" :norepeat t)))

;; Register bindings
(define-key ryo-modal-mode-map (kbd "\"") ctl-x-r-map)

;; Access all C-x bindings easily
(define-key ryo-modal-mode-map (kbd "z") ctl-x-map)

;; Hippie expand
(global-set-key (kbd "C-w") 'hippie-expand)

;; We have better ways to open lines
(global-set-key (kbd "C-o") 'other-window)

(defun vimlike-navigation (keymap)
  "Add basic navigation bindings to a KEYMAP."
  (progn
    (define-key keymap "j" 'next-line)
    (define-key keymap "k" 'previous-line)
    (define-key keymap "\C-d" 'scroll-up-command)
    (define-key keymap "\C-u" 'scroll-down-command)
    (define-key keymap "o" 'other-window)))

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

;;;; Fancy text editing
(use-package multiple-cursors
  :ryo
  ("m" mc/mark-next-like-this)
  ("M" mc/skip-to-next-like-this)
  ("n" mc/mark-previous-like-this)
  ("N" mc/skip-to-previous-like-this)
  ("*" mc/mark-all-like-this)
  ("C-v" set-rectangular-region-anchor)
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package expand-region
  :straight (expand-region :host github :repo "magnars/expand-region.el"
                           :fork (:host github :repo "jmorag/expand-region.el"))
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

(provide 'kakoune)
;;; kakoune.el ends here
