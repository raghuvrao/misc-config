(require 'package)
(package-initialize)

(setenv "PAGER" "cat")

(column-number-mode 1)
(show-paren-mode 1)

(transient-mark-mode -1)
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))

;; Remove a few undesirable key-bindings.  I will repurpose some of
;; them.
(define-key global-map (kbd "C-x C-c") nil)
(define-key global-map (kbd "C-z") nil)
(define-key global-map (kbd "C-x C-z") nil)
(define-key global-map (kbd "C-x C-b") nil)

;; Override yes-or-no-p's definition by making yes-or-no-p an alias
;; for y-or-n-p.  This way, when yes-or-no-p is called, y-or-n-p will
;; actually be called, and I can reply with y/SPC or n/DEL instead of
;; `yes RET' or `no RET'.
(defalias 'yes-or-no-p #'y-or-n-p)

(defun raghu/visit-emacs-configuration-file ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
(define-key global-map (kbd "C-c C") #'raghu/visit-emacs-configuration-file)

;; Highlighting the current line is useful mostly in programs, so keep
;; it disabled by default, enable it in programs, and make a
;; key-binding to toggle it.
(global-hl-line-mode -1)
(defun raghu/hl-line-mode-in-buffer ()
  "Highlight line containing point in current buffer.

Does nothing more than calling (hl-line-mode 1).  Meant for
adding to the mode hooks in whose modes I want current-line
highlighting, so as to avoid repeating the corresponding lambda
form over and over."
  (interactive)
  (hl-line-mode 1))
(add-hook 'ibuffer-mode-hook #'raghu/hl-line-mode-in-buffer)
(add-hook 'prog-mode-hook #'raghu/hl-line-mode-in-buffer)
(add-hook 'text-mode-hook #'raghu/hl-line-mode-in-buffer)
(define-key global-map (kbd "C-c H") #'hl-line-mode)

;; Enable syntax higlighting only in some places.  Define a
;; key-binding to toggle it.
(global-font-lock-mode -1)
(defun raghu/font-lock-mode-in-buffer ()
  "Enable font-lock-mode (syntax highlighting etc.) in current buffer.

Does nothing more than calling (font-lock-mode 1).  Meant for
adding to the few mode hooks in whose modes I want syntax
highlighting enabled, so as to avoid repeating the corresponding
lambda form over and over."
  (interactive)
  (font-lock-mode 1))
(add-hook 'compilation-mode-hook #'raghu/font-lock-mode-in-buffer)
(add-hook 'diff-mode-hook #'raghu/font-lock-mode-in-buffer)
(add-hook 'dired-mode-hook #'raghu/font-lock-mode-in-buffer)
(add-hook 'eshell-mode-hook #'raghu/font-lock-mode-in-buffer)
(add-hook 'inferior-python-mode-hook #'raghu/font-lock-mode-in-buffer)
(add-hook 'shell-mode-hook #'raghu/font-lock-mode-in-buffer)
(add-hook 'special-mode-hook #'raghu/font-lock-mode-in-buffer)
(define-key global-map (kbd "C-c P") #'font-lock-mode)

;; Line-wrapping is sometimes annoying, beneficial at other times.
;; Disable it in a few major modes so corresponding buffers are not
;; line-wrapped, and make a key-binding to toggle it.
(defun raghu/enable-truncate-long-lines-in-buffer ()
  "Enable line truncation (or, disable line-wrapping) in current buffer."
  (interactive)
  (set (make-local-variable 'truncate-lines) t))
(add-hook 'prog-mode-hook #'raghu/enable-truncate-long-lines-in-buffer)
(add-hook 'diff-mode-hook #'raghu/enable-truncate-long-lines-in-buffer)
(define-key global-map (kbd "C-c t") #'toggle-truncate-lines)

;; I do not believe I need a key-binding to suspend/iconify/minimize
;; emacs.  Instead, I will use those key-bindings for the `repeat'
;; function.
(define-key global-map (kbd "C-z") #'repeat)
(define-key global-map (kbd "C-x C-z") #'repeat)

;; Make emacs-windows navigation easier.  The arg to
;; windmove-default-keybindings is a symbol indicating the modifier to
;; use with the arrow keys to navigate windows.
(require 'windmove)
(windmove-default-keybindings 'control)

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

;; Joining lines is a function I use fairly frequently.  Define a
;; key-binding for it.
(define-key global-map (kbd "C-c J") #'join-line)

;; Functions and key-bindings to make line-killing easier.

(defun raghu/kill-to-beginning-of-line ()
  "Kill backward from point to beginning of the line."
  (interactive)
  (kill-line 0))
(define-key global-map (kbd "C-c M-k") #'raghu/kill-to-beginning-of-line)

(defun raghu/kill-backward-to-indentation (&optional arg)
  "Kill backward from point to first nonblank character ARG lines up.

If ARG is a positive integer, then kill backward from point to
first nonblank character ARG lines above.

If ARG is 0, then kill backward from point to first nonblank
character on the same line.

If ARG is nil or not supplied, perform the same action as when
ARG is 0.

If ARG is none of the above, perform no action."
  ;; When arg is >0 (i.e. more than one line), use
  ;; backward-to-indentation.  When arg is nil or 0, use
  ;; back-to-indentation.  backward-to-indentation, when arg is 0 or
  ;; nil, will call (forward-line 0), which has the side-effect of
  ;; relocating point to column 0.  In places where a subset of the
  ;; text is read-only (e.g. minibuffer), this side-effect will cause
  ;; problems with kill-region.  back-to-indentation calls
  ;; beginning-of-line, which does not have this side-effect.  Also,
  ;; back-to-indentation does not take arguments.
  (interactive "P")
  (setq prior-point (point))		; No need to mess with mark, I think.
  (cond ((integerp arg)
	 (setq lines (prefix-numeric-value arg))
	 (cond ((= lines 0)
		(back-to-indentation)
		(kill-region prior-point (point)))
	       ((> lines 0)
		(backward-to-indentation lines)
		(kill-region prior-point (point)))))
	((null arg)
	 (back-to-indentation)
	 (kill-region prior-point (point)))))
(define-key global-map (kbd "C-c k") #'raghu/kill-backward-to-indentation)

(define-key global-map (kbd "C-c K") #'kill-whole-line)

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
