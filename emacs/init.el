;;; init.el --- Emacs configuration file  -*- lexical-binding: t -*-

;;; Commentary:

;; My Emacs configuration file.

;;; Code:

(require 'package)
(package-initialize)

(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp #'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(global-hl-line-mode -1)
(global-font-lock-mode -1)
(transient-mark-mode -1)

;; Various shell programs like to send their stdout through a pager.
;; When those programs are run within emacs, pagination functionality
;; is unnecessary.
(setenv "PAGER" "cat")

(column-number-mode 1)
(show-paren-mode 1)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Override yes-or-no-p's definition by making yes-or-no-p an alias
;; for y-or-n-p.  This way, when yes-or-no-p is called, y-or-n-p will
;; actually be called, and I can reply with y/SPC or n/DEL instead of
;; `yes RET' or `no RET'.
(defalias 'yes-or-no-p #'y-or-n-p)

;; Remove undesirable key-bindings.
(define-key global-map (kbd "C-x C-c") nil)

;; A few convenient key-bindings are assigned to not-so-interesting
;; functions by default.  Reassign them to more interesting functions.
(define-key global-map (kbd "C-x C-b") #'ibuffer)
(define-key global-map (kbd "C-z") #'repeat)
(define-key global-map (kbd "C-x C-z") #'repeat)

;; Assign convenient key-bindings for useful functions that either
;; have no default key-bindings or have inconvenient default
;; key-bindings.
(define-key global-map (kbd "C-c H") #'hl-line-mode)
(define-key global-map (kbd "C-c J") #'join-line)
(define-key global-map (kbd "C-c P") #'font-lock-mode)
(define-key global-map (kbd "C-c t") #'toggle-truncate-lines)
(define-key global-map (kbd "C-c ;") #'comment-line)

;; Make emacs-windows navigation easier.  The arg to
;; windmove-default-keybindings is a symbol indicating the modifier to
;; use with the arrow keys to navigate windows.
(require 'windmove)
(windmove-default-keybindings 'control)

;; Useful for changing CRLF line terminators to LF line terminators.
(defun raghu/to-unix-utf-8-buffer (buf)
  "Convert file encoding system of buffer BUF to UNIX UTF-8.

If called interactively, read buffer name from minibuffer."
  (interactive "bBuffer")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (set-buffer-file-coding-system 'unix t)
    (set-buffer-file-coding-system 'utf-8 t)
    (message "To UNIX UTF-8: %S" (get-buffer buf))
    t))

(defun raghu/indent-buffer (buf)
  "Indent all or narrowed part of buffer BUF.

If called interactively, read buffer name from minibuffer."
  (interactive "bBuffer")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (indent-region (point-min) (point-max))
    (message "Indented: %S" (get-buffer buf))
    t))

;; async-shell-command runs the commands in buffers that are not
;; entirely uniquely named.  It has support for using different buffer
;; names, but for me, that's not good enough.  I want the buffers to
;; be named after the command that is run in them.
(defun raghu/async-shell-command (cmd)
  "Run shell command CMD asynchronously in buffer \"*Async: CMD*\"."
  (interactive (list (read-shell-command "Async shell command? ")))
  ;; Get rid of leading/trailing space from the command.
  (let ((trimmed-cmd (replace-regexp-in-string "^[ \t]+\\|[ \t]+$" "" cmd))
	(dir default-directory)
	(buf-name nil))
    (unless (> (length trimmed-cmd) 0)
      (user-error "%s" "Empty command"))
    (setq buf-name (concat "*Async: " trimmed-cmd "*"))
    (switch-to-buffer buf-name)
    (setq default-directory dir)
    (async-shell-command trimmed-cmd buf-name nil)))
(define-key global-map (kbd "C-c !") #'raghu/async-shell-command)

(defun raghu/visit-emacs-configuration-file ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
(define-key global-map (kbd "C-c C") #'raghu/visit-emacs-configuration-file)

;; Scroll while keeping point on original text-line (so long as the
;; original text-line is in the window, of course).

;; First, define a few scrolling-related functions.  These functions
;; will be eventually bound to keys.
(defun raghu--show-down-one ()
  "Scroll buffer to show next one line."
  (interactive)
  (scroll-up 1))
(defun raghu--show-up-one ()
  "Scroll buffer to show previous one line."
  (interactive)
  (scroll-down 1))
(defun raghu--show-left-one ()
  "Scroll buffer to show left one column."
  (interactive)
  (scroll-right 1))
(defun raghu--show-right-one ()
  "Scroll buffer to show right one column."
  (interactive)
  (scroll-left 1))

;; Next, define a map variable, and bind the above functions to one or
;; more keys in this map.  Make sure to keep the key-bindings and the
;; documentation below in agreement with each other.
(defvar raghu/scroll-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'raghu--show-up-one)
    (define-key map (kbd "i") #'raghu--show-up-one)
    (define-key map (kbd "<up>") #'raghu--show-up-one)
    (define-key map (kbd "d") #'raghu--show-down-one)
    (define-key map (kbd "k") #'raghu--show-down-one)
    (define-key map (kbd "<down>") #'raghu--show-down-one)
    (define-key map (kbd "s") #'raghu--show-left-one)
    (define-key map (kbd "j") #'raghu--show-left-one)
    (define-key map (kbd "<left>") #'raghu--show-left-one)
    (define-key map (kbd "f") #'raghu--show-right-one)
    (define-key map (kbd "l") #'raghu--show-right-one)
    (define-key map (kbd "<right>") #'raghu--show-right-one)
    map)
  "Keymap for scrolling one line/column at a time.

The following key-bindings are defined in this map:

e, i, <up>: show previous (scroll down)
d, k, <down>: show next (scroll up)
s, j, <left>: show left (scroll right)
f, l, <right>: show right (scroll left)

While scrolling, point remains with the original text-line (not
screen-line) so long as the original text-line is within the
window.")

;; Finally, define a function and a key-binding to activate the above
;; keymap in a transient fashion.
(defun raghu/scroll ()
  "Scroll in the current buffer.

Activate the keymap `raghu/scroll-map', which makes scrolling a
little easier.  When a key that is not in the map is pressed, it
will deactivate the keymap so usual editing operations can be
resumed.  A mark is set at point's original starting position."
  (interactive)
  (push-mark)
  (message "%s" "raghu/scroll-map ON")
  (set-transient-map
   raghu/scroll-map t (lambda () (message "%s" "raghu/scroll-map OFF"))))
(define-key global-map (kbd "C-c s") #'raghu/scroll)

(defun raghu/kill-backward-to-indentation (&optional arg)
  "Kill backward from point to first nonblank character on line.

With prefix argument ARG, kill backward to indentation of the
ARGth line above the current line.  Without a prefix argument,
work on current line only (ARG defaults to 0 in this case).
Return number of lines killed (count includes partially killed
lines, if any)."
  (interactive "*P")
  (cond ((null arg) (setq arg 0))
	((listp arg) (let ((z (car arg))) (when (natnump z) (setq arg z)))))
  (unless (natnump arg)
    (signal 'wrong-type-argument (list #'natnump arg)))
  (let ((prior-point (point)) (point-at-indent nil) (num-killed-lines 0))
    (back-to-indentation)
    (when (> arg 0)
      (when (> (point) prior-point) (setq prior-point (point)))
      (backward-to-indentation arg))
    (setq point-at-indent (point))
    (when (> prior-point point-at-indent)
      (setq num-killed-lines (count-lines prior-point point-at-indent))
      (kill-region prior-point point-at-indent))
    num-killed-lines))
(define-key global-map (kbd "C-c k") #'raghu/kill-backward-to-indentation)

(define-error 'raghu/incomplete-comment-syntax
  "Incomplete comment syntax"
  'error)

(defun raghu/duplicate-region-and-comment (beginning end)
  "Duplicate lines containing region and make them comments.

Take the lines necessary and sufficient to encapsulate the region
defined by BEGINNING and END, place a copy of these lines above
the first line of the region, and make those lines into comments.

Signal an error if comment syntax is not defined for buffer's
major mode.  This function considers comment syntax as defined if
the symbols `comment-start' and `comment-end' satisfy the
predicate functions `boundp' and `stringp'.

Return the number of lines copied."
  ;; See newcomment.el for `comment-start' and `comment-end'.
  (unless (boundp 'comment-start)
    (signal 'raghu/incomplete-comment-syntax '(boundp comment-start)))
  (unless (boundp 'comment-end)
    (signal 'raghu/incomplete-comment-syntax '(boundp comment-end)))
  (unless (stringp comment-start)
    (signal 'raghu/incomplete-comment-syntax
	    (list #'stringp comment-start 'comment-start)))
  (unless (stringp comment-end)
    (signal 'raghu/incomplete-comment-syntax
	    (list #'stringp comment-end 'comment-end)))
  (unless (integerp beginning)
    (signal 'wrong-type-argument (list #'integerp beginning 'beginning)))
  (unless (integerp end)
    (signal 'wrong-type-argument (list #'integerp end 'end)))
  ;; Ensure beginning <= end for ease of implementation.
  (when (> beginning end) (let (x) (setq x beginning beginning end end x)))
  ;; Ensure beginning and end are within bounds.
  (let ((pmin (point-min)) (pmax (point-max)))
    (if (< beginning pmin)
	(setq beginning pmin)
      (when (> beginning pmax) (setq beginning pmax)))
    (if (< end pmin)
	(setq end pmin)
      (when (> end pmax) (setq end pmax))))
  (if (= beginning end)
      0					; Return #lines copied.
    (let (beginning-bol end-eol copied-lines num-copied-lines)
      (save-excursion
	(goto-char beginning) (beginning-of-line) (setq beginning-bol (point))
	(goto-char end) (end-of-line) (setq end-eol (point))
	;; Use buffer-substring instead of kill-ring-save because we
	;; do not want the copied text to end up on the kill-ring.
	;; The idea is to duplicate the lines, not to save them
	;; anywhere for yanking later.
	(setq copied-lines (buffer-substring beginning-bol end-eol))
	(setq num-copied-lines (count-lines beginning-bol end-eol))
	(goto-char beginning-bol)
	(open-line 1)
	(insert copied-lines)
	(comment-region beginning-bol end-eol))
      ;; Account for save-excursion behavior at beginning of line.
      (when (and (bolp) (= beginning (point)))
	(forward-line num-copied-lines))
      num-copied-lines)))		; Return #lines copied.

(defun raghu/duplicate-line-and-comment (arg)
  "Duplicate and comment current line.

If ARG is a positive integer, duplicate and comment the current
line and ARG lines below it.  If ARG is a negative integer,
duplicate and comment the current line and (`abs' ARG) lines
above it.  If ARG is 0, duplicate and comment current line only.

Consider comment syntax as defined if the symbols `comment-start'
and `comment-end' satisfy the predicate functions `boundp' and
`stringp'.  Signal an error if comment syntax is not defined for
buffer's major mode.

Return the number of lines copied."
  ;; See newcomment.el for `comment-start' and `comment-end'.
  (unless (boundp 'comment-start)
    (signal 'raghu/incomplete-comment-syntax '(boundp comment-start)))
  (unless (boundp 'comment-end)
    (signal 'raghu/incomplete-comment-syntax '(boundp comment-end)))
  (unless (stringp comment-start)
    (signal 'raghu/incomplete-comment-syntax
	    (list #'stringp comment-start 'comment-start)))
  (unless (stringp comment-end)
    (signal 'raghu/incomplete-comment-syntax
	    (list #'stringp comment-end 'comment-end)))
  (unless (integerp arg)
    (signal 'wrong-type-argument (list #'integerp arg)))
  (let (original start end copied-lines num-copied-lines)
    (setq original (point))
    (save-excursion
      (forward-line arg)
      (if (> arg 0)
	  (progn (end-of-line)
		 (setq end (point))
		 (goto-char original)
		 (beginning-of-line)
		 (setq start (point)))
	(setq start (point))		; Already in col. 0 here.
	(goto-char original)
	(end-of-line)
	(setq end (point)))
      (setq copied-lines (buffer-substring start end))
      (setq num-copied-lines (count-lines start end))
      (goto-char start)
      (open-line 1)
      (insert copied-lines)
      (comment-region start end))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (bolp) (= start (point)))
      (forward-line num-copied-lines))
    num-copied-lines))			; Return #lines copied.

(defun raghu/duplicate-and-comment (&optional arg)
  "Duplicate lines and make them comments.

If region is active, call `raghu/duplicate-region-and-comment' on
the region, and ignore argument ARG.  If region is not active,
call `raghu/duplicate-line-and-comment' with argument ARG.  When
no prefix argument is provided, ARG defaults to 0.  Return the
number of lines copied.

Interactive only!  `raghu/duplicate-line-and-comment' and/or
`raghu/duplicate-region-and-comment' are for use in Lisp."
  (interactive "*P")
  (condition-case err
      (if (use-region-p)
	  (raghu/duplicate-region-and-comment (region-beginning) (region-end))
	(cond
	 ((null arg) (setq arg 0))
	 ((listp arg) (let ((x (car arg))) (when (integerp x) (setq arg x)))))
	(raghu/duplicate-line-and-comment arg))
    ((raghu/incomplete-comment-syntax wrong-type-argument)
     (progn (message "%s" (error-message-string err) 0)))))
(define-key global-map (kbd "C-c I") #'raghu/duplicate-and-comment)

(defun raghu/new-line-above (arg)
  "Above current line, insert new line and indentaion.

With prefix argument ARG, insert that many lines above current
line.  Move point to top-most line inserted, and insert
indentation according to mode."
  (interactive "*p")
  (unless (natnump arg) (signal 'wrong-type-argument (list #'natnump arg)))
  (when (> arg 0)			; (natnump 0) => t
    (beginning-of-line 1)
    (open-line arg)
    (indent-according-to-mode)))
(define-key global-map (kbd "C-c o") #'raghu/new-line-above)

(defun raghu/new-line-below (arg)
  "Below current line, insert new line and indentation.

With prefix argument ARG, insert that many lines below current
line.  Move point to top-most line inserted, and insert
indentation according to mode."
  (interactive "*p")
  (unless (natnump arg) (signal 'wrong-type-argument (list #'natnump arg)))
  (when (> arg 0)			; (natnump 0) => t
    (save-excursion
      (end-of-line 1)
      (newline arg nil))
    (forward-line)
    (indent-according-to-mode)))
(define-key global-map (kbd "C-c RET") #'raghu/new-line-below)

;; Functions meant solely for adding to mode hooks.
(defun raghu--enable-hl-line-mode-in-buffer ()
  "Highlight line containing point in current buffer."
  (hl-line-mode 1))
(defun raghu--enable-font-lock-mode-in-buffer ()
  "Enable font lock mode in buffer."
  (font-lock-mode 1))
(defun raghu--disable-line-wrap-in-buffer ()
  "Enable line truncation in current buffer."
  (set (make-local-variable 'truncate-lines) t))
(defun raghu--enable-word-wrap-in-buffer ()
  "Enable word-wrapping in current buffer."
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'word-wrap) 1))
(defun raghu--show-trailing-whitespace-in-buffer ()
  "Highlight trailing whitespace in the current buffer."
  (set (make-local-variable 'show-trailing-whitespace) t))

(with-eval-after-load 'simple
  ;; special-mode is a "parent" mode for various modes.
  (add-hook 'special-mode-hook #'raghu--enable-font-lock-mode-in-buffer))

(with-eval-after-load 'prog-mode
  ;; prog-mode is a "parent" mode for various programming modes.
  (add-hook 'prog-mode-hook #'raghu--disable-line-wrap-in-buffer)
  (add-hook 'prog-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'prog-mode-hook #'raghu--show-trailing-whitespace-in-buffer))

(with-eval-after-load 'ibuffer
  ;; font-lock-mode is helpful in ibuffer-mode, but there is no need
  ;; to enable it here separately.  ibuffer-mode inherits from
  ;; special-mode, and I have enabled font-lock-mode in special-mode
  ;; (see above), so ibuffer-mode gets font-lock-mode.
  (add-hook 'ibuffer-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'dired-mode-hook #'raghu--enable-font-lock-mode-in-buffer))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook #'raghu--enable-font-lock-mode-in-buffer))

(with-eval-after-load 'diff-mode
  (add-hook 'diff-mode-hook #'raghu--enable-font-lock-mode-in-buffer)
  (add-hook 'diff-mode-hook #'raghu--disable-line-wrap-in-buffer))

(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'shell-mode-hook #'raghu--enable-font-lock-mode-in-buffer))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'eshell-mode-hook #'raghu--enable-font-lock-mode-in-buffer))

(with-eval-after-load 'python
  (add-hook 'inferior-python-mode-hook #'raghu--enable-font-lock-mode-in-buffer))

;; text-mode does not provide a feature, so use "text-mode" below.
(with-eval-after-load "text-mode"
  (add-hook 'text-mode-hook #'raghu--enable-word-wrap-in-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(frame-background-mode (quote light))
 '(hscroll-step 1)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(package-archive-priorities (quote (("gnu" . 90) ("melpa-stable" . 70))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(ring-bell-function (quote ignore))
 '(scroll-conservatively 5)
 '(sh-basic-offset 2)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(sh-indentation 2)
 '(show-paren-delay 0)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x)) (:height 90 :family "DejaVu Sans Mono"))))
 '(eshell-prompt ((t nil)))
 '(fringe ((t (:inherit default))))
 '(isearch ((((type x ns) (class color) (background light)) (:background "plum2" :foreground "black"))))
 '(region ((((type x ns) (class color) (background light)) (:background "LightGoldenrod2")))))

;;; init.el ends here
