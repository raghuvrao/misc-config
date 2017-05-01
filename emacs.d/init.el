;;; init.el --- Emacs configuration file  -*- lexical-binding: t -*-

;;; Commentary:

;; My Emacs configuration file.

;;; Code:

(require 'package)
(package-initialize)

;; Various shell programs like to send their stdout through a pager.
;; When those programs are run within emacs, pagination functionality
;; through an external program is unnecessary.
(setenv "PAGER" "cat")

(column-number-mode 1)
(show-paren-mode 1)

(global-hl-line-mode -1)
(global-font-lock-mode -1)
(transient-mark-mode -1)
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

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
(define-key global-map (kbd "C-c K") #'kill-whole-line)
(define-key global-map (kbd "C-c P") #'font-lock-mode)
(define-key global-map (kbd "C-c t") #'toggle-truncate-lines)

;; auto-complete is great when it stays out of the way.  In
;; custom-set-variables, I have disabled auto-complete from starting
;; automatically.  I will hit `S-TAB' to trigger auto-complete.  Also,
;; I am not enabling it globally.  I will enable it in specific
;; major-modes.
(require 'auto-complete)
(define-key ac-mode-map (kbd "<backtab>") #'auto-complete)

;; Make emacs-windows navigation easier.  The arg to
;; windmove-default-keybindings is a symbol indicating the modifier to
;; use with the arrow keys to navigate windows.
(require 'windmove)
(windmove-default-keybindings 'control)

;; async-shell-command runs the commands in buffers that are not
;; entirely uniquely named.  It has support for using different buffer
;; names, but for me, that's not good enough.  I want the buffers to
;; be named after the command that is run in them.
(defun raghu/async-shell-command (cmd)
  "Run shell command CMD asynchronously in buffer named after CMD.

The name of the buffer is \"*Async: CMD*\"."
  (interactive (list (read-shell-command "Async shell command? ")))
  ;; Get rid of leading/trailing space from the command.
  (let ((cmd (replace-regexp-in-string "^[ \t]+\\|[ \t]+$" "" cmd)))
    ;; Do work only if cmd is non-empty after trimming.
    (when (> (length cmd) 0)
      (let ((dir default-directory)
	    (buf-name (concat "*Async: " cmd "*")))
	(switch-to-buffer buf-name)
	(setq default-directory dir)
	(async-shell-command cmd buf-name nil)))))
(define-key global-map (kbd "C-c !") #'raghu/async-shell-command)

(defun raghu/visit-emacs-configuration-file ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
(define-key global-map (kbd "C-c C") #'raghu/visit-emacs-configuration-file)

;; Scroll while keeping point on original text-line (so long as the
;; original text-line is in the window, of course).
(setq raghu/scroll-map (make-sparse-keymap))
(define-key raghu/scroll-map (kbd "i") (lambda () (interactive) (scroll-up 1)))
(define-key raghu/scroll-map (kbd "k") (lambda () (interactive) (scroll-down 1)))
(define-key raghu/scroll-map (kbd "j") (lambda () (interactive) (scroll-left 1)))
(define-key raghu/scroll-map (kbd "l") (lambda () (interactive) (scroll-right 1)))
(defun raghu/scroll ()
  "Scroll in the current buffer.

Activates a keymap temporarily where `i' will scroll text up, `k'
will scroll text down, `j' will scroll text left, and `l' will
scroll text right.  Any other key will deactivate the keymap.
While scrolling, point remains with the original text-line (not
screen-line) so long as the original text-line is within the
window.  A mark is set at point's original starting position."
  (interactive)
  (push-mark)
  (message "Use i/j/k/l to scroll text.")
  (set-transient-map raghu/scroll-map
		     (lambda ()
		       (when (or (eq last-input-event ?i)
				 (eq last-input-event ?k)
				 (eq last-input-event ?j)
				 (eq last-input-event ?l))
			 (message "Use i/j/k/l to scroll text.")))
		     nil))
(define-key global-map (kbd "C-c s") #'raghu/scroll)

(defun raghu/kill-backward-to-indentation (&optional arg)
  "Kill backward from point to first nonblank character on line.

If optional argument ARG is a positive integer, then kill
backward from point to first nonblank character ARG lines above.
If ARG is 0, then kill backward from point to first nonblank
character on the same line.  If ARG is nil or not supplied,
perform the same action as when ARG is 0.  If ARG is none of the
above, perform no action."
  (interactive "P")
  (let ((prior-point (point)))		; No need to mess with mark, I think.
    (cond ((integerp arg)
	   (let ((lines (prefix-numeric-value arg)))
	     (cond ((= lines 0)
		    ;; (backward-to-indentation 0) will call
		    ;; (forward-line 0), which has a side-effect of
		    ;; moving point to column 0.  In places where a
		    ;; subset of the text is read-only (for example,
		    ;; the minibuffer), this side-effect will cause
		    ;; kill-region to fail.  back-to-indentation calls
		    ;; beginning-of-line, which does not have this
		    ;; side-effect.
		    (back-to-indentation)
		    (kill-region prior-point (point)))
		   ((> lines 0)
		    ;; back-to-indentation does not take arguments, so
		    ;; use backward-to-indentation when we are dealing
		    ;; with multiple lines.
		    (backward-to-indentation lines)
		    (kill-region prior-point (point))))))
	  ((null arg)
	   ;; We want to treat arg being nil the same as arg being 0.
	   ;; See reasoning above for using back-to-indentation here
	   ;; instead of backward-to-indentation.
	   (back-to-indentation)
	   (kill-region prior-point (point))))))
(define-key global-map (kbd "C-c k") #'raghu/kill-backward-to-indentation)

(defun raghu/copy-line (&optional lines)
  "Copy LINES lines.

Save LINES lines in the `kill-ring' without actually killing the
lines.  If LINES is a non-zero positive integer, save that many
lines below starting from the current line.  If LINES is a
non-zero negative integer, save that many lines above starting
from the current line.  If LINES is neither of the above, do
nothing."
  (interactive "p")
  (when (integerp lines)
    (unless (= 0 lines)
      (save-excursion
	(if (< 0 lines) (forward-line 0) (forward-line 1))
	(let ((end (point)))
	  (forward-line lines)
	  (kill-ring-save (point) end)
	  (message "Lines copied <= %d" (abs lines)))))))
(define-key global-map (kbd "C-c w") #'raghu/copy-line)

(defun raghu/yank-above-current-line (&optional arg)
  "Insert most recent kill above current line.

If ARG is supplied and is a non-negative integer, insert the
ARGth most recent kill above current line."
  (interactive "p")
  (when (integerp arg)
    (when (>= arg 0)
      (save-excursion
	(beginning-of-line)
	(open-line 1)
	(yank arg)
	;; If the yanked text already has a newline at the end, we
	;; will end up with an extra newline, so let's get rid of the
	;; extra newline if one exists.
	(when (and (eolp) (bolp))
	  (forward-line 0)
	  (let ((beginning (point)))
	    (forward-line 1)
	    (delete-region beginning (point)))))
      ;; Account for save-excursion behaving differently at the
      ;; beginning of the line.
      (when (bolp) (forward-line 1)))))
(define-key global-map (kbd "C-c Y") #'raghu/yank-above-current-line)

(defun raghu/yank-below-current-line (&optional arg)
  "Insert most recent kill below current line.

If ARG is supplied and is a non-negative integer, insert the
ARGth most recent kill below current line."
  (interactive "p")
  (when (integerp arg)
    (when (>= arg 0)
      (save-excursion
	(end-of-line)
	(if (eobp) (newline 1 nil) (forward-line 1))
	(yank arg)))))
(define-key global-map (kbd "C-c y") #'raghu/yank-below-current-line)

(defun raghu/insert-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

As new lines are inserted, point's position will remain constant
relative to the line on which point was located originally."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (beginning-of-line 1)
      (open-line lines))
    ;; Account for save-excursion behaving differently at the
    ;; beginning of the line.
    (when (bolp) (forward-line lines))))
(define-key global-map (kbd "C-c RET") #'raghu/insert-new-line-above)

(defun raghu/insert-new-line-below (&optional lines)
  "Insert LINES (default=1) new lines below current line.

As new lines are inserted, point's position will remain constant
relative to the line on which point was located originally."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (end-of-line 1)
      (newline lines nil))))
(define-key global-map (kbd "C-c M-RET") #'raghu/insert-new-line-below)

(defun raghu/insert-and-go-to-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

The point is moved to the top-most line inserted."
  (interactive "p")
  (when (> lines 0)
    (beginning-of-line 1)
    (open-line lines)
    (indent-according-to-mode)))
(define-key global-map (kbd "C-c O") #'raghu/insert-and-go-to-new-line-above)

(defun raghu/insert-and-go-to-new-line-below (&optional lines)
  "Insert LINES (default=1) new lines below current line.

Point moves to the newly-inserted line immediately below the line
on which point originally was."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (end-of-line 1)
      (newline lines nil))
    (forward-line)
    (indent-according-to-mode)))
(define-key global-map (kbd "C-c o") #'raghu/insert-and-go-to-new-line-below)

(defun raghu/enable-hl-line-mode-in-buffer ()
  "Highlight line containing point in current buffer.

Meant for adding to mode hooks."
  (interactive)
  (hl-line-mode 1))

(defun raghu/enable-font-lock-mode-in-buffer ()
  "Enable font lock mode in buffer.

Meant for adding to mode hooks."
  (interactive)
  (font-lock-mode 1))

(defun raghu/enable-truncate-long-lines-in-buffer ()
  "Enable line truncation in current buffer.

Meant for adding to mode hooks.  Line truncation is Emacs
parlance for not-line-wrapping."
  (interactive)
  (set (make-local-variable 'truncate-lines) t))

(defun raghu/enable-auto-complete-mode-in-buffer ()
  "Enable completion in current buffer.

Meant for adding to mode hooks."
  (interactive)
  (auto-complete-mode 1))

(with-eval-after-load 'simple
  ;; special-mode is a "parent" mode for various modes.
  (add-hook 'special-mode-hook #'raghu/enable-font-lock-mode-in-buffer))

(with-eval-after-load 'prog-mode
  ;; prog-mode is a "parent" mode for various programming modes.
  (add-hook 'prog-mode-hook #'raghu/enable-truncate-long-lines-in-buffer)
  (add-hook 'prog-mode-hook #'raghu/enable-hl-line-mode-in-buffer))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'raghu/enable-hl-line-mode-in-buffer)
  (add-hook 'dired-mode-hook #'raghu/enable-font-lock-mode-in-buffer))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook #'raghu/enable-font-lock-mode-in-buffer))

(with-eval-after-load 'diff-mode
  (add-hook 'diff-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
  (add-hook 'diff-mode-hook #'raghu/enable-truncate-long-lines-in-buffer))

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'raghu/enable-auto-complete-mode-in-buffer)
  (add-hook 'lisp-interaction-mode-hook #'raghu/enable-auto-complete-mode-in-buffer))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'raghu/enable-hl-line-mode-in-buffer)
  (add-hook 'eshell-mode-hook #'raghu/enable-font-lock-mode-in-buffer))

(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-mode-hook #'raghu/enable-hl-line-mode-in-buffer))

(with-eval-after-load 'python
  (add-hook 'inferior-python-mode-hook #'raghu/enable-font-lock-mode-in-buffer))

;; text-mode does not provide a feature, so use "text-mode" below.
(with-eval-after-load "text-mode"
  (add-hook 'text-mode-hook (lambda ()
			      (set (make-local-variable 'truncate-lines) nil)
			      (set (make-local-variable 'word-wrap) 1))))

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
 '(make-backup-files nil)
 '(package-archive-priorities (quote (("gnu" . 90) ("melpa-stable" . 70) ("melpa" . 50))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
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
