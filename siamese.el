;;; siamese.el --- programmatically change key bindings using twin maps

;; Copyright (C) 2011  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Status: abandoned, was never usable, for inspiration / as a warning

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; **************************** WARNING ****************************
;; *                                                               *
;; *  This package is not usable at all (and never was) and since  *
;; *  I no longer attempt to use it this will probably never       *
;; *  change.                                                      *
;; *                                                               *
;; *  If you are interested feel free to adopt it - orphans lead   *
;; *  a sad live.                                                  *
;; *                                                               *
;; *****************************************************************

;; This package programmatically changes key bindings in most keymaps.
;; This is done by automatically attaching a "twin" map to each keymap
;; and defining key bindings according to some user defined rules.

;; This package has never finished, though I did try to use it for a
;; few weeks causing major damage to my muscle memory. What you see
;; here is stripped down version with most of my configuration removed
;; but still enough of it to demonstrate how it was supposed to be
;; used.

;; I intended to use it to automatically translate the basic motion
;; keys:

;;                  p
;; C- f
;;      b   n

;; to

;;              i
;; C-         j k l
;;

;; (That's the basic idea; of course my plan went beyond that.)

;; I am still planning to create such key bindings but it turned out
;; that automating the required changes is almost impossible.  I now
;; think it is easier to change all bindings manually instead of
;; trying to understand a mixture of pending, cannot-be-automated,
;; automatic-but-not-quite-right, and work-around changes ;-/

;; However if you want to do something much simpler than what I tried
;; then this package *might* at least serve as source of inspiration -
;; or at least serve as a warning.

;;; Code:

(require 'keymap-utils)

;; TODO the next three variables can probably be merged
;; even if this is not fully possible changing the order inside
;; the cons-cells would be good.

(defvar siamese-exchange-function-keys
  '(("\e[C-i" . "\C-i")
    ("\e[C-m" . "\C-m")
    (  "\C-i" . [tab])
    (  "\C-m" . [return])))

(defvar siamese-function-keys
  `((    [return])
    (  [M-return])
    (  [S-return])
    ([M-S-return])
    (  [kp-enter] . [return])
    (       [tab])
    (     [M-tab])
    (     [S-tab])
    (   [M-S-tab])
    (    [kp-tab] . [tab])))

(defvar siamese-keys
  `(;; move: char, word, sexp
    ("\C-b" . "\C-j")  ("\M-b"  . "\M-j")  ("\C-\M-b" . "\C-\M-j")
    ("\C-f" . "\C-l")  ("\M-f"  . "\M-l")  ("\C-\M-f" . "\C-\M-l")
    ;; move: line, -, list
    ("\C-p" . "\C-i")  ("\M-p"  . "\M-i")  ("\C-\M-p" . "\C-\M-i")
    ("\C-n" . "\C-k")  ("\M-n"  . "\M-k")  ("\C-\M-n" . "\C-\M-k")
    ;; move: of-line, sentense, of-defun
    ("\C-a" . "\C-u")  ("\M-a"  . "\M-u")  ("\C-\M-a" . "\C-\M-u")
    ("\C-e" . "\C-o")  ("\M-e"  . "\M-o")  ("\C-\M-e" . "\C-\M-o")
    ;; kill: char
    ("\d"   . "\C-s")  ("\M-\d" . "\M-s")  ;; ?
    ("\C-d" . "\C-f")  ("\M-d"  . "\M-f")  ;; ("\C-\M-d" . nil)
    ;; kill: line
    ("\C-k" . "\C-d")  ("\M-k"  . "\M-d")

    ;; TODO rearrange format so that we don't have to duplicate
    ;; siamese-function-keys here
    (,(kbd     "C-m") .     [return])
    (,(kbd   "M-C-m") .   [M-return])
    (,(kbd   "S-C-m") .   [S-return])
    (,(kbd "M-S-C-m") . [M-S-return])
    ;;(,(kbd     "C-m") .   [kp-enter])
    (,(kbd     "C-i") .        [tab])
    (,(kbd   "M-C-i") .      [M-tab])
    (,(kbd   "S-C-i") .      [S-tab])
    (,(kbd "M-S-C-i") .    [M-S-tab])
    ;;(,(kbd     "C-i") .     [kp-tab])
    ))

;; Bind the now unused keys to the now binding-less commands.  or make
;; use of now empty keys.  This can also be used to make bindings that
;; are not directly related to the remappings above.
;; ((MAPVAR (KEY . COMMAND)...)...)
;; TODO create `siamese-define-bindings' to append to this var
(defvar siamese-binding-alist
  `((siamese-global-map
     ("\C-p"    . universal-argument)
     ("\M-p"    . negative-argument)
     )))

;; TODO explicitly list all keymaps as they are handled differently
(defconst siamese-minibuffer-local-maps
  '(minibuffer-local-map))

(defvar siamese-global-map nil)

(defvar siamese-mode-map-alist nil)

(defvar siamese-local-mode-map-alist nil)
(make-variable-buffer-local 'siamese-local-mode-map-alist)

(define-minor-mode siamese-local-mode
  ""
  :init-value nil
  (cond
   (siamese-local-mode
    (unless siamese-global-map
      (siamese-setup-global-map))
    (setq emulation-mode-map-alists
          (cons 'siamese-local-mode-map-alist
                (delq 'siamese-local-mode-map-alist
                      emulation-mode-map-alists)))
    (setq siamese-local-mode-map-alist nil)
    (siamese-setup-major-mode-map)
    (siamese-setup-minor-mode-maps)
    (setq siamese-local-mode-map-alist
          (nreverse siamese-local-mode-map-alist)))
   (t
    ;; not an error because that hinders debugging
    (message "Reattaching ain't possible yet."))))

(defun siamese-initialize ()
  (unless (minibufferp)
    (siamese-local-mode 1)))

(define-globalized-minor-mode siamese-mode
  siamese-local-mode siamese-initialize)

(defadvice siamese-mode (after global-map activate)
  (cond
   (siamese-mode
    (siamese-setup-global-map)
    (if (functionp 'siamese-switch-minibuffer-maps)
        (funcall   'siamese-switch-minibuffer-maps)
      (siamese-setup-minibuffer-maps))
    (siamese-setup-function-key-map 'function-key-map)
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (siamese-setup-terminal-local-maps)))
    (eval-after-load "isearch"
      '(siamese-setup-isearch-mode-map))
    (add-hook 'term-setup-hook 'siamese-setup-terminal-local-maps))
   (t
    ;; not an error because that hinders debugging
    (message "Reattaching ain't possible yet."))))

;; TODO recreate `simese-global-map' when turning mode on *again*?
(defun siamese-setup-global-map ()
  (unless siamese-global-map
    (let ((map (make-sparse-keymap)))
      (siamese-bind-keys 'siamese-global-map siamese-keys map global-map)
      (set-keymap-parent map global-map)
      (setq siamese-global-map map)))
  (use-global-map siamese-global-map))

(defun siamese-setup-minibuffer-maps ()
  (dolist (var siamese-minibuffer-local-maps)
    (let ((map (symbol-value var)))
      (unless (siamese-matching-keymap-state-p map var)
        (siamese-attach-twin-map map)
        (siamese-bind-keys var siamese-keys map (keymap-parent map))))))

(defun siamese-setup-terminal-local-maps ()
  (siamese-setup-function-key-map 'local-function-key-map)
  (unless (window-system)
    (siamese-setup-input-decode-map input-decode-map)))

(defun siamese-setup-input-decode-map (map)
  (unless (siamese-matching-keymap-state-p map 'input-decode-map)
    (siamese-attach-twin-map map)
    (dolist (elt siamese-exchange-function-keys)
      (define-key map (car elt) (cdr elt)))
    (siamese-set-keymap-state map 'input-decode-map)))

(defun siamese-setup-function-key-map (var)
  (let ((map (symbol-value var)))
    (unless (siamese-matching-keymap-state-p map var)
      (siamese-attach-twin-map map)
      (dolist (elt siamese-function-keys)
        (define-key map (car elt) (cdr elt))) ; usually nil
      (siamese-set-keymap-state map var))))

(defun siamese-setup-isearch-mode-map ()
  (let ((map isearch-mode-map))
    (unless (siamese-matching-keymap-state-p map 'siamese)
      (siamese-attach-twin-map map)
      (siamese-bind-keys 'isearch-mode-map siamese-keys
                         map (keymap-parent map))
      (siamese-set-keymap-state map 'siamese))))

(defun siamese-setup-major-mode-map ()
  (let ((map (current-local-map)))
    (when map
      (siamese-setup-mode-map major-mode map 'siamese-local-mode))))

(defun siamese-setup-minor-mode-maps ()
  (dolist (elt minor-mode-map-alist)
    (siamese-setup-mode-map (car elt) (cdr elt))))

(defun siamese-setup-mode-map (mode map &optional idx)
  (let ((twin-map (cdr (assq mode siamese-mode-map-alist))))
    (unless twin-map
      (setq twin-map (make-sparse-keymap))
      (push (cons mode twin-map) siamese-mode-map-alist)
      (siamese-bind-keys mode siamese-keys twin-map map siamese-global-map))
    (push (cons (or idx mode) twin-map)
          siamese-local-mode-map-alist)))

(defun siamese-set-keymap-state (map state)
  (define-key map [siamese] state))

(defun siamese-matching-keymap-state-p (map state)
  (eq state (lookup-key map [siamese])))

(defun siamese-attach-twin-map (map) ; TODO &optional orig-map)
  (setcdr map (cons 'keymap (cdr map))))

(defun siamese-detach-twin-map (map)
  (setcdr map (cdr (keymap-parent map))))

;; TODO warn about lost bindings
(defun siamese-remap-key (twin-key orig-key twin-map orig-map
                                   &optional fallback-map)
  (let ((val (lookup-key orig-map orig-key)))
    (cond (val
           (define-key twin-map twin-key val)
           (unless (lookup-key twin-map orig-key)
             (define-key twin-map orig-key nil)))

          ((and fallback-map (kmu-lookup-local-key orig-map twin-key))
           (define-key twin-map twin-key
             (lookup-key fallback-map twin-key))))))

(defun siamese-bind-keys (idx key-alist twin-map orig-map
                              &optional fallback-map)
  (dolist (twins key-alist)
    (siamese-remap-key (cdr twins) (car twins)
                       twin-map orig-map fallback-map))
  (dolist (binding (cdr (assq idx siamese-binding-alist)))
    (define-key twin-map (car binding) (cdr binding))))

(provide 'siamese)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; siamese.el ends here
