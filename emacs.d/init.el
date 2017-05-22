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
(defun raghu/dos2unix (buffer)
  "Convert BUFFER's file encoding system from DOS to UNIX."
  (interactive "*bApply to buffer")
  (with-current-buffer buffer
    (set-buffer-file-coding-system 'unix t)
    (set-buffer-file-coding-system 'utf-8 t)))

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
    (define-key map (kbd "i") #'raghu--show-up-one)
    (define-key map (kbd "<up>") #'raghu--show-up-one)
    (define-key map (kbd "k") #'raghu--show-down-one)
    (define-key map (kbd "<down>") #'raghu--show-down-one)
    (define-key map (kbd "j") #'raghu--show-left-one)
    (define-key map (kbd "<left>") #'raghu--show-left-one)
    (define-key map (kbd "l") #'raghu--show-right-one)
    (define-key map (kbd "<right>") #'raghu--show-right-one)
    map)
  "Keymap for scrolling one line/column at a time.

The following key-bindings are defined in this map:

i, <up>: show previous (scroll down)
k, <down>: show next (scroll up)
j, <left>: show left (scroll right)
l, <right>: show right (scroll left)

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
  (message "raghu/scroll-map activated.")
  (set-transient-map raghu/scroll-map
		     t
		     (lambda () (message "raghu/scroll-map deactivated."))))
(define-key global-map (kbd "C-c s") #'raghu/scroll)

(defun raghu/kill-backward-to-indentation (lines)
  "Kill backward from point to first nonblank character on line.

Starting from point, kill backward to indentation of the LINESth
line above.  The count LINES includes the current line.  So, to
kill from point backward to indentation on the same line, LINES
must be 1.  Signal an error if LINES < 1."
  (interactive "*p")
  (unless (> lines 0)
    (user-error "Expected non-zero positive integer; got %S" lines))
  (let ((prior-point (point))
	(point-at-indentation nil))
    (back-to-indentation)
    (when (> lines 1)
      (when (> (point) prior-point) (setq prior-point (point)))
      (backward-to-indentation (1- lines)))
    (setq point-at-indentation (point))
    (when (> prior-point point-at-indentation)
      (kill-region prior-point point-at-indentation))))
(define-key global-map (kbd "C-c k") #'raghu/kill-backward-to-indentation)

(define-error 'raghu/unsupported-in-major-mode
  "Function unsupported in buffer's major mode"
  'error)
(define-error 'raghu/invalid-region
  "Invalid region specified"
  'error)

(defun raghu/duplicate-region-and-comment (beginning end)
  "Duplicate lines containing region and make them comments.

Take the lines necessary and sufficient to encapsulate the region
defined by BEGINNING and END, place a copy of these lines above
the first line of the region, and make those lines into comments.

If the major mode of the buffer is not derived from `prog-mode',
signal an error: err on the side of caution because the concept
of comments may not be well-defined for non-programming-language
modes."
  (unless (derived-mode-p 'prog-mode)
    (signal 'raghu/unsupported-in-major-mode
	    (list (format-message "`%s' not derived from prog-mode"
				  major-mode))))
  (unless (and (integerp beginning) (integerp end))
    (signal 'wrong-type-argument
	    (list (format-message "Expected integers, got %S, %S"
				  beginning
				  end))))
  (unless (and (>= beginning 1) (>= end 1))
    (signal 'raghu/invalid-region
	    (list (format-message "(%d, %d) defines invalid region"
				  beginning
				  end))))
  ;; Ensure beginning <= end for ease of implementation.
  (when (> beginning end) (let (x) (setq x beginning beginning end end x)))
  (let (beginning-bol end-eol copied-lines)
    (save-excursion
      (goto-char beginning) (beginning-of-line) (setq beginning-bol (point))
      (goto-char end) (end-of-line) (setq end-eol (point))
      ;; Use buffer-substring instead of kill-ring-save because we
      ;; do not want the copied text to end up on the kill-ring.
      ;; The idea is to duplicate the lines, not to save them
      ;; anywhere for yanking later.
      (setq copied-lines (buffer-substring beginning-bol end-eol))
      (goto-char beginning-bol)
      (open-line 1)
      (insert copied-lines)
      (comment-region beginning-bol end-eol))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (bolp) (= beginning (point)))
      (forward-line (count-lines beginning-bol end-eol)))))

(defun raghu/duplicate-line-and-comment (arg)
  "Duplicate current line and make it a comment.

Starting from and including the current line, take ARG lines,
place a copy of them above the current line, and convert those
lines into comments.  If ARG is a non-zero positive integer,
perform this work on ARG lines below.  If ARG is a non-zero
negative integer, perform this work on ARG lines above.  In
either case, ARG includes the current line.  So, to perform the
work on the current line only, ARG must be either 1 or -1.  If
ARG is anything else, signal an error.

If the major mode of the buffer is not derived from `prog-mode',
signal an error: err on the side of caution because the concept
of comments may not be well-defined for non-programming-language
modes."
  (unless (derived-mode-p 'prog-mode)
    (signal 'raghu/unsupported-in-major-mode
	    (list (format-message
		   "`%s' not derived from prog-mode"
		   major-mode))))
  (unless (and (integerp arg) (not (= 0 arg)))
    (signal 'wrong-type-argument
	    (list (format-message "Expected non-zero integer, got %S" arg))))
  (let (original start end copied-lines)
    (setq original (point))
    (save-excursion
      (if (> arg 0)
	  (progn (forward-line (1- arg))
		 (end-of-line)
		 (setq end (point))
		 (goto-char original)
		 (beginning-of-line)
		 (setq start (point)))
	(forward-line (1+ arg))		; arg is never zero here.
	(setq start (point))
	(goto-char original)
	(end-of-line)
	(setq end (point)))
      (setq copied-lines (buffer-substring start end))
      (goto-char start)
      (open-line 1)
      (insert copied-lines)
      (comment-region start end))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (bolp) (= start (point)))
      (forward-line (count-lines start end)))))

(defun raghu/duplicate-and-comment (&optional arg)
  "Duplicate lines and make them comments.

This function is meant only for interactive use.  In Lisp, use
`raghu/duplicate-line-and-comment' and
`raghu/duplicate-region-and-comment'.

If region is active, call `raghu/duplicate-region-and-comment' on
region, and ignore ARG.  If region is not active and if ARG is
nil or not supplied, call `raghu/duplicate-line-and-comment' with
argument 1.  If region is not active and if ARG is an integer,
call `raghu/duplicate-line-and-comment' with argument ARG.  If
region is not active and ARG is a list, call
`raghu/duplicate-line-and-comment' with argument (`car' ARG).  If
region is not active and ARG is none of the above, print an error
message to the minibuffer, and perform no further work.

If the buffer's major mode is not derived from `prog-mode', print
an error message to the minibuffer, and take no further action:
err on the side of caution because the concept of comments may
not be well-defined for non-programming-language modes."
  (interactive "*P")
  (condition-case err
      (progn
	(unless (derived-mode-p 'prog-mode)
	  (signal 'raghu/unsupported-in-major-mode
		  (list (format-message
			 "`%s' not derived from prog-mode"
			 major-mode))))
	(cond ((use-region-p) (raghu/duplicate-region-and-comment
			       (region-beginning)
			       (region-end)))
	      ((integerp arg) (raghu/duplicate-line-and-comment arg))
	      ((null arg) (raghu/duplicate-line-and-comment 1))
	      ((listp arg) (raghu/duplicate-line-and-comment (car arg)))
	      (t (signal 'user-error (list (format-message
					    "Argument %S is unsupported"
					    arg))))))
    ((wrong-type-argument
      raghu/unsupported-in-major-mode
      raghu/invalid-region)
     (message "%s" (error-message-string err)))))
(define-key global-map (kbd "C-c I") #'raghu/duplicate-and-comment)

(defun raghu/insert-and-go-to-new-line-above (lines)
  "Insert LINES new lines above current line.

Point is moved to the top-most line inserted, and indentation
according to mode is inserted."
  (interactive "*p")
  (unless (> lines 0)
    (user-error "Expected non-zero positive integer; got %S" lines))
  (beginning-of-line 1)
  (open-line lines)
  (indent-according-to-mode))
(define-key global-map (kbd "C-c O") #'raghu/insert-and-go-to-new-line-above)

(defun raghu/insert-and-go-to-new-line-below (lines)
  "Insert LINES new lines below current line.

Point moves to the newly-inserted line immediately below the line
on which point originally was, and indentation according to mode
is inserted."
  (interactive "*p")
  (unless (> lines 0)
    (user-error "Expected non-zero positive integer; got %S" lines))
  (save-excursion
    (end-of-line 1)
    (newline lines nil))
  (forward-line)
  (indent-according-to-mode))
(define-key global-map (kbd "C-c o") #'raghu/insert-and-go-to-new-line-below)

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
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x)) (:height 90 :family "DejaVu Sans Mono"))))
 '(eshell-prompt ((t nil)))
 '(isearch ((((type x ns) (class color) (background light)) (:background "plum2" :foreground "black"))))
 '(region ((((type x ns) (class color) (background light)) (:background "LightGoldenrod2")))))

;;; init.el ends here
