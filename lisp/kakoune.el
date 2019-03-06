;;; kakoune.el --- A simulation, but not emulation, of kakoune, in emacs

;; Author: Joseph Morag <jm4157@columbia.edu>
;; Version: 0.1
;; Package-Requires: ((ryo-modal "0.4") (multiple-cursors "1.4") (expand-region "0.11.0") (visual-regexp "1.1") (phi-search "2.2.2"))

;;; Commentary:
;; This package provides many, but not all of the editing primitives in the kakoune editor.
;; Unlike evil-mode for vim, this is very shallow emulation, and seeks to do as little
;; work as possible, leveraging Emacs native editing commmands and the work of other
;; packages wherever possible.

;;; Code:
(require 'kakoune-utils)
(require 'ryo-modal)
(require 'expand-region)
(require 'multiple-cursors)
(require 'visual-regexp)

(setq-default cursor-type '(bar . 1))
(setq ryo-modal-cursor-type 'box)
(add-hook 'prog-mode-hook 'ryo-modal-mode)

;;;; Basic keybindings
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
 ("." kak/select-to-char-again :first '(set-mark-if-inactive))
 (";" deactivate-mark)
 ("M-;" exchange-point-and-mark)
 ("*" ryo-tbd)
 ("`" kak/downcase)
 ("~" kak/upcase)
 ("%" mark-whole-buffer)
 ("M-`" xah-toggle-letter-case)
 ("M-j" kak/join)
 ("[ [" backward-paragraph :first '(set-mark-here))
 ("{ [" backward-paragraph :first '(set-mark-if-inactive))
 ("] ]" forward-paragraph :first '(set-mark-here))
 ("} ]" forward-paragraph :first '(set-mark-if-inactive))
 )

(ryo-modal-keys  ("0" "M-0" :norepeat t)
                 ("1" "M-1" :norepeat t)
                 ("2" "M-2" :norepeat t)
                 ("3" "M-3" :norepeat t)
                 ("4" "M-4" :norepeat t)
                 ("5" "M-5" :norepeat t)
                 ("6" "M-6" :norepeat t)
                 ("7" "M-7" :norepeat t)
                 ("8" "M-8" :norepeat t)
                 ("9" "M-9" :norepeat t)
                 ("-" "M--" :norepeat t))

;; Multiple cursors
(ryo-modal-keys ("s" vr/mc-mark))
(provide 'kakoune)
;;; kakoune.el ends here
