;;; init.el --- Emacs configuration file  -*- lexical-binding: t -*-

;;; Commentary:

;; Raghu V. Rao's Emacs configuration file.

;;; Code:

(require 'package)
(package-initialize)

(global-font-lock-mode -1)
(global-hl-line-mode -1)
(transient-mark-mode -1)

(when (fboundp #'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp #'tooltip-mode) (tooltip-mode -1))

(column-number-mode 1)
(show-paren-mode 1)

;; Various shell programs like to send their stdout through a
;; pagination program (e.g. more, less).  When those programs are run
;; within Emacs, pagination is unnecessary.
(setenv "PAGER" "cat")
(setenv "GIT_PAGER" nil)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Reply with y/SPC or n/DEL instead of `yes RET' or `no RET'.
(defalias 'yes-or-no-p #'y-or-n-p)

(define-key global-map (kbd "C-c B") #'ibuffer)
(define-key global-map (kbd "C-c H") #'hl-line-mode)
(define-key global-map (kbd "C-c J") #'join-line)
(define-key global-map (kbd "C-c P") #'font-lock-mode)
(define-key global-map (kbd "C-c w") #'toggle-truncate-lines)
(define-key global-map (kbd "C-c ;") #'comment-line)

(require 'windmove)
(windmove-default-keybindings 'control)

(defun raghu/with-confirmation (fn &rest args)
  "Conditionally, call function FN with arguments ARGS."
  (when (y-or-n-p "Are you sure?") (apply fn args)))

(advice-add #'save-buffers-kill-terminal :around #'raghu/with-confirmation)

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
  "Indent buffer BUF.

If BUF is narrowed, indent only its accessible portion.  If
called interactively, read buffer name from minibuffer."
  (interactive "bBuffer")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (indent-region (point-min) (point-max))
    (message "Indented: %S" (get-buffer buf))
    t))

;; async-shell-command runs the commands in buffers that are not
;; entirely uniquely named.  I want the buffers to be named after the
;; command that is run in them.
(defun raghu/async-shell-command (cmd)
  "Run string CMD in buffer \"*Async: `CMD'*\".

First, remove spaces, line-feeds, carriage-returns, vertical tabs
and form-feeds from CMD even if escaped with `\\', if they are
leading or trailing.  To preserve any of these characters when
they are leading or trailing, they must be surrounded by single
or double quotes."
  (interactive (list (read-shell-command "Async shell command? ")))
  (let* ((space "\\\\?\\([[:space:]]\\|\n\\|\r\\|\v\\|\f\\)")
	 (lead-space (format "^\\(%s\\)+" space))
	 (trail-space (format "\\(%s\\)+$" space))
	 (lead-or-trail-space (format "%s\\|%s" lead-space trail-space)))
    (setq cmd (replace-regexp-in-string lead-or-trail-space "" cmd)))
  (unless (> (length cmd) 0) (user-error "%s" "Empty command"))
  (let ((dir default-directory)
	(buf-name nil))
    (setq buf-name (format-message "*Async: `%s'*" cmd))
    (switch-to-buffer buf-name)
    (setq default-directory dir)
    (async-shell-command cmd buf-name nil)))
(define-key global-map (kbd "C-c !") #'raghu/async-shell-command)

(defun raghu/visit-emacs-configuration-file ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
(define-key global-map (kbd "C-c I") #'raghu/visit-emacs-configuration-file)

;; Scroll while keeping point on original text-line (so long as the
;; original text-line is in the window, of course).
;;
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

;; Next, define a map variable, and in it, give key-bindings to the
;; above functions.
(defvar raghu/scroll-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'raghu--show-up-one)
    (define-key map "d" #'raghu--show-down-one)
    (define-key map "s" #'raghu--show-left-one)
    (define-key map "f" #'raghu--show-right-one)
    map)
  "Keymap for scrolling one line/column at a time.

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
resumed.  A mark is set at point's original starting position.

\\{raghu/scroll-map}"
  (interactive)
  (push-mark)
  (message "%s" "raghu/scroll-map ON")
  (set-transient-map raghu/scroll-map
		     (lambda ()
		       (when (assoc last-input-event raghu/scroll-map)
			 (message "%s" "raghu/scroll-map ON")))
		     (lambda ()
		       (message "%s" "raghu/scroll-map OFF"))))
(define-key global-map (kbd "C-c s") #'raghu/scroll)

(defun raghu/kill-backward-to-indentation (&optional arg)
  "Kill backward from point to first nonblank character on line.

When ARG is a non-zero integer, kill backward from point to the
indentation of the ARGth line above the current line.  When ARG
is 0, nil or t, kill backward from point to the indentation of
the current line.  Return number of lines killed (the count
includes partially killed lines, if any).

ARG can be supplied through \\[universal-argument]."
  (interactive "*P")
  (if (booleanp arg)
      (setq arg 0)
    (when (listp arg) (let ((z (car arg))) (when (natnump z) (setq arg z))))
    (unless (natnump arg)
      (signal 'wrong-type-argument (list (list #'natnump #'booleanp) arg))))
  (let ((starting-point (point)) (point-at-indent nil) (num-killed-lines 0))
    (back-to-indentation)
    (when (> arg 0)
      (when (> (point) starting-point) (setq starting-point (point)))
      (backward-to-indentation arg))
    (setq point-at-indent (point))
    (when (> starting-point point-at-indent)
      (setq num-killed-lines (count-lines starting-point point-at-indent))
      (kill-region point-at-indent starting-point))
    num-killed-lines))
(define-key global-map (kbd "C-c k") #'raghu/kill-backward-to-indentation)

(defun raghu/duplicate-and-comment (&optional lines)
  "Duplicate and comment lines.

If region is active, work on the lines necessary and sufficient
to encapsulate the region.  Ignore argument LINES.

If region is not active, work on the current line and LINES
additional lines.  If LINES is greater than 0, work on the
current line and LINES lines below it.  If LINES is lesser than
0, work on the current line and -LINES lines above it.  If LINES
is nil, t or equal to 0, work on the current line only.

LINES can be specified via prefix argument.  When no prefix
argument is specified, work on the current line only.

Return the number of lines copied.

NOTE: This function is meant for interactive use only: it calls
`comment-normalize-vars', which prompts the user for comment
syntax if comment syntax is undefined for the buffer's major
mode."
  (interactive "*P")
  (comment-normalize-vars)
  (let ((copied-lines nil) (num-copied-lines 0) (begin nil) (end nil))
    (if (use-region-p)
	(let ((rb (region-beginning)) (re (region-end)))
	  (save-excursion
	    (goto-char rb)
	    (beginning-of-line)
	    (setq begin (point))
	    (goto-char re)
	    (end-of-line)
	    (setq end (point))))
      (if (booleanp lines)
	  (setq lines 0)
	(when (listp lines)
	  (let ((z (car lines)) (when (integerp z) (setq lines z)))))
	(unless (integerp lines)
	  (signal 'wrong-type-argument
		  (list (list #'integerp #'booleanp) lines))))
      (let ((starting-point (point)))
	(save-excursion
	  (forward-line lines)
	  (if (> lines 0)
	      (progn (end-of-line)
		     (setq end (point))
		     (goto-char starting-point)
		     (beginning-of-line)
		     (setq begin (point)))
	    (setq begin (point))
	    (goto-char starting-point)
	    (end-of-line)
	    (setq end (point))))))
    (when (= begin end) (error "%s" "Nothing to comment"))
    (save-excursion
      (setq copied-lines (buffer-substring begin end)
	    num-copied-lines (count-lines begin end))
      (goto-char begin)
      (open-line 1)
      (insert copied-lines)
      (comment-region begin end))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (bolp) (= begin (point))) (forward-line num-copied-lines))
    ;; Return the number of lines copied+commented.
    num-copied-lines))
(define-key global-map (kbd "C-c C") #'raghu/duplicate-and-comment)

(defun raghu/new-line-above (arg)
  "Above current line, insert new line and indentation.

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

(defun raghu/mark-string (&optional activate-mark-p)
  "Mark the string containing point.

If ACTIVATE-MARK-P is non-nil, activate mark too."
  (interactive "P")
  (if (nth 3 (syntax-ppss))
      (progn
	(while (nth 3 (syntax-ppss)) (backward-char 1))
	(push-mark)
	(forward-char 1)
	(while (nth 3 (syntax-ppss)) (forward-char 1))
	(when activate-mark-p (activate-mark)))
    (error "Point is not in a string")))
(define-key global-map (kbd "C-c \"") #'raghu/mark-string)

(defun raghu/insert-current-date-time (&optional arg)
  "At point, insert current date and time in system timezone.

With one \\[universal-argument], insert current date and time in
Etc/UTC.  With another \\[universal-argument], insert current
UNIX timestamp."
  (interactive "*P")
  (let ((fmt "%Y-%m-%d %a %H:%M:%S %Z")
	(timezone nil))
    (pcase (prefix-numeric-value arg)
      (4  (setq timezone "Etc/UTC"))	; One `universal-argument'
      (16 (setq fmt "%s")))		; Two `universal-argument's
    (insert (format-time-string fmt nil timezone))))
(define-key global-map (kbd "C-c t") #'raghu/insert-current-date-time)

(defun raghu--enable-hl-line-mode-in-buffer ()
  "Highlight line containing point in current buffer.

Do so only when Emacs is running on a graphic display."
  (when (fboundp #'display-graphic-p)
    (when (display-graphic-p) (hl-line-mode 1))))

(defun raghu--enable-word-wrap-in-buffer ()
  "Enable word-wrapping in current buffer."
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'word-wrap) 1))

(defun raghu--do-line-wrap-in-buffer ()
  "Do line-wrapping in current buffer."
  (set (make-local-variable 'truncate-lines) nil))

(defun raghu--show-trailing-whitespace-in-buffer ()
  "Highlight trailing whitespace in the current buffer."
  (set (make-local-variable 'show-trailing-whitespace) t))

(with-eval-after-load 'simple
  (add-hook 'special-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'prog-mode-hook #'raghu--show-trailing-whitespace-in-buffer))

(with-eval-after-load 'outline
  (add-hook 'outline-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'outline-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'cus-edit
  (add-hook 'Custom-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'tabulated-list
  (add-hook 'tabulated-list-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'dired-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'compilation-mode-hook #'raghu--do-line-wrap-in-buffer)
  (add-hook 'compilation-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'diff-mode
  (add-hook 'diff-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'shell-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'eshell-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'python
  (add-hook 'inferior-python-mode-hook #'turn-on-font-lock))

(with-eval-after-load "text-mode"	; No `provide' in text-mode.el
  (add-hook 'text-mode-hook #'raghu--enable-word-wrap-in-buffer))

(with-eval-after-load 'log-edit
  (add-hook 'log-edit-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'log-edit-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'conf-mode
  (add-hook 'conf-mode-hook #'raghu--disable-line-wrap-in-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(frame-background-mode (quote dark))
 '(help-window-select t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(kill-whole-line t)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(mouse-yank-at-point t)
 '(package-archive-priorities (quote (("gnu" . 90) ("melpa-stable" . 70))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(ring-bell-function (quote ignore))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-delay 0)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x) (class color) (background dark)) (:background "gray10" :foreground "wheat3" :height 90)) (((type ns) (class color) (background dark)) (:background "gray10" :foreground "wheat3" :height 110))))
 '(diff-file-header ((((type x ns) (class color) (background dark)) (:background "grey60" :foreground "grey20" :weight bold))))
 '(diff-header ((((type x ns) (class color) (background dark)) (:background "grey24" :foreground "grey70"))))
 '(fringe ((t (:inherit default))))
 '(hl-line ((((type x ns) (class color) (background light)) (:background "azure2")) (((type x ns) (class color) (background dark)) (:background "gray23"))))
 '(isearch ((((type x ns) (class color) (background light)) (:background "plum2" :foreground "black"))))
 '(lazy-highlight ((((type x ns) (class color) (background dark)) (:background "RoyalBlue4" :foreground "LightSkyBlue"))))
 '(match ((((type x ns) (class color) (background dark)) (:background "orange2" :foreground "black"))))
 '(region ((((type x ns) (class color) (background light)) (:background "LightGoldenrod2" :foreground "black")) (((type x ns) (class color) (background dark)) (:background "wheat3" :foreground "gray10"))))
 '(show-paren-match ((((type x ns) (class color) (background dark)) (:background "green4" :foreground "white")))))

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
