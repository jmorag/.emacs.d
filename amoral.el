;;; amoral.el --- Kakoune in emacs - neither evil nor holy
;;
;;; Commentary:
;; If Emacs' editing style is holy, and vim's is evil, then kakoune's must be amoral
;; or something...
;;
;; This is an attempt to implement kakoune style modal editing in Emacs. It
;; _should_ be simpler than evil, just because kakoune seems to have only two modes,
;; whereas vim has many: insert, normal, visual, operator-pending. Given that
;; only normal and insert exist in kakoune, we will try and bind commands through
;; mkkrpv's modalka package. We'll probably depend on Magnar's multiple-cursors as
;; well, once we get there. Heck, we might even depend on evil's work for
;; text blocks and such, although initially we should try to use
;; Emacs's native editing commands. Unlike Evil, the goal of this project is not
;; perfect emulation. I haven't used Kakoune extensively at all, so I doubt that
;; I'd ever know enough about it to emulate it perfectly anyway.

;;; Code:
(require 'modalka)

(defun am-escape ()
  "Leave insert mode and return to normal mode.
Bind this to your favorite escape method. For example:

\(define-key modalka-mode-map \"[ESC]\" 'am-escape)
or
\(key-chord-define-global \"jk\" 'am-escape)"
       (interactive)
       (modalka-mode 1))

(defun am-insert ()
  "Go from normal mode to insert mode: i.e. default Emacs insert state.
This reverts Emacs back to its usual vanilla behavior."
  (interactive)
  (modalka-mode -1))


(provide 'amoral)
;;; amoral.el ends here
