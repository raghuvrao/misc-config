(require 'package)
(package-initialize)

;; Various shell programs like to send their stdout through a pager.
;; When those programs are run within emacs, pagination functionality
;; through an external program is unnecessary.
(setenv "PAGER" "cat")

(column-number-mode 1)
(show-paren-mode 1)

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
;; automatically.  I will hit `S-TAB' to trigger auto-complete.
(require 'auto-complete)
(define-key ac-mode-map (kbd "<backtab>") #'auto-complete)

;; Make emacs-windows navigation easier.  The arg to
;; windmove-default-keybindings is a symbol indicating the modifier to
;; use with the arrow keys to navigate windows.
(require 'windmove)
(windmove-default-keybindings 'control)

(defun raghu--trim-space (text)
  "Strip leading/trailing space from TEXT."
  (when (stringp text)
    (replace-regexp-in-string "^[ \t]+\\|[ \t]+$" "" text)))

;; async-shell-command runs the commands in buffers that are not
;; entirely uniquely named.  It has support for using different buffer
;; names, but for me, that's not good enough.  I want the buffers to
;; be named after the command that is run in them.
(defun raghu/async-shell-command (cmd &optional buf-name)
  "Run shell command CMD asynchronously in buffer named after command.

If optional string argument BUF-NAME is supplied, the command's
output (stdout and stderr) will be placed in the buffer named
BUF-NAME.  If BUF-NAME is nil or not supplied or not a string or
an effectively empty string, the command's output is placed in a
buffer named \"*Async: CMD*\"."
  (interactive (list (read-shell-command "Async shell command? ")))
  (when (> (length cmd) 0)
    ;; Get rid of leading/trailing space from input.
    (let ((cmd (raghu--trim-space cmd)))
      ;; Do work only if cmd is non-empty after trimming.
      (when (> (length cmd) 0)
	(let ((dir default-directory)
	      ;; If buf-name is a string, first strip leading/trailing
	      ;; space from it, if it is not already just "".  If
	      ;; buf-name is "" after that, or if buf-name was not a
	      ;; string to begin with, give the new buffer a name
	      ;; constructed from the command to be run.  Otherwise,
	      ;; use the now-trimmed buf-name to name the new buffer.
	      (buf-name (or (when (stringp buf-name)
			      (unless (string= buf-name "")
				(let ((buf-name (raghu--trim-space buf-name)))
				  (unless (string= buf-name "") buf-name))))
			    (concat "*Async: " cmd "*"))))
	  (switch-to-buffer buf-name)
	  (setq default-directory dir)
	  (async-shell-command cmd buf-name nil))))))
(define-key global-map (kbd "C-c !") #'raghu/async-shell-command)

(defun raghu/visit-emacs-configuration-file ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
(define-key global-map (kbd "C-c C") #'raghu/visit-emacs-configuration-file)

;; Highlighting the current line is useful, but not everywhere.
(global-hl-line-mode -1)
(defun raghu/enable-hl-line-mode-in-buffer ()
  "Highlight line containing point in current buffer."
  (interactive)
  (hl-line-mode 1))
(add-hook 'ibuffer-mode-hook #'raghu/enable-hl-line-mode-in-buffer)
(add-hook 'prog-mode-hook #'raghu/enable-hl-line-mode-in-buffer)
(add-hook 'text-mode-hook #'raghu/enable-hl-line-mode-in-buffer)

;; Enable syntax higlighting / fontification only in some places.
(global-font-lock-mode -1)
(defun raghu/enable-font-lock-mode-in-buffer ()
  "Enable font lock mode in buffer."
  (interactive)
  (font-lock-mode 1))
(add-hook 'compilation-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
(add-hook 'diff-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
(add-hook 'dired-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
(add-hook 'eshell-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
(add-hook 'inferior-python-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
(add-hook 'shell-mode-hook #'raghu/enable-font-lock-mode-in-buffer)
(add-hook 'special-mode-hook #'raghu/enable-font-lock-mode-in-buffer)

;; Line-wrapping is sometimes annoying.
(defun raghu/enable-truncate-long-lines-in-buffer ()
  "Enable line truncation in current buffer.

Line truncation is Emacs parlance for not-line-wrapping."
  (interactive)
  (set (make-local-variable 'truncate-lines) t))
(add-hook 'prog-mode-hook #'raghu/enable-truncate-long-lines-in-buffer)
(add-hook 'diff-mode-hook #'raghu/enable-truncate-long-lines-in-buffer)

;; Sometimes I like to scroll text while not having to move point too
;; much, so define corresponding functions and key-bindings.  The
;; key-bindings I am using below are a little awkward, but with the
;; `repeat' function, they are not so bad.
(defun raghu/show-text-next (&optional lines)
  "Reveal next LINES lines of text by scrolling text up."
  (interactive "p")
  (scroll-up lines))
(define-key global-map (kbd "C-c s n") #'raghu/show-text-next)
(defun raghu/show-text-previous (&optional lines)
  "Reveal previous LINES lines of text by scrolling text down."
  (interactive "p")
  (scroll-down lines))
(define-key global-map (kbd "C-c s p") #'raghu/show-text-previous)
(defun raghu/show-text-right (&optional cols)
  "Reveal COLS columns of text on the right by scrolling text left."
  (interactive "p")
  (scroll-left cols))
(define-key global-map (kbd "C-c s r") #'raghu/show-text-right)
(defun raghu/show-text-left (&optional cols)
  "Reveal COLS columns of text on the left by scrolling text right."
  (interactive "p")
  (scroll-right cols))
(define-key global-map (kbd "C-c s l") #'raghu/show-text-left)

;; Functions and key-bindings to make line-killing easier.

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
		    ;; If arg is 0, backward-to-indentation calls
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

;; Functions and key-bindings to make line-insertion easier.

(defun raghu/insert-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

As new lines are inserted, point's position will remain constant
relative to the line on which point was located originally."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (beginning-of-line 1)
      (open-line lines))
    ;; account for save-excursion working differently when column is 0
    (when (= (current-column) 0)
      (forward-line lines))))
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
 '(isearch ((((type x ns) (class color) (background light)) (:background "plum2" :foreground "black"))))
 '(region ((((type x ns) (class color) (background light)) (:background "LightGoldenrod2")))))
