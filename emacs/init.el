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

(define-key global-map (kbd "C-x C-c") nil)
(define-key global-map (kbd "C-x C-z") nil)
(define-key global-map (kbd "C-z") nil)

(define-key global-map (kbd "C-c B") #'ibuffer)
(define-key global-map (kbd "C-c H") #'hl-line-mode)
(define-key global-map (kbd "C-c J") #'join-line)
(define-key global-map (kbd "C-c P") #'font-lock-mode)
(define-key global-map (kbd "C-c t") #'toggle-truncate-lines)
(define-key global-map (kbd "C-c ;") #'comment-line)

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
;; entirely uniquely named.  I want the buffers to be named after the
;; command that is run in them.
(defun raghu/async-shell-command (cmd)
  "Run string CMD in buffer \"*Async: `CMD'*\".

Remove leading and trailing spaces from CMD first.  To preserve
any leading or trailing spaces, they must be surrounded by single
or double quotes."
  (interactive (list (read-shell-command "Async shell command? ")))
  ;; Remove leading/trailing spaces from CMD.  `\\\\[[:space:]]' is my
  ;; regular expression for escaped space.  I hope it is correct.
  (setq cmd (replace-regexp-in-string
	     "^\\(\\\\?[[:space:]]\\)+\\|\\(\\\\?[[:space:]]\\)+$"
	     ""
	     cmd))
  (unless (> (length cmd) 0) (user-error "%s" "Empty command"))
  (let ((dir default-directory) (buf-name nil))
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

(defun raghu/duplicate-and-comment (&optional lines beginning end)
  "Duplicate and comment lines.

If the region is active, work on the lines necessary and
sufficient to encapsulate the region; ignore all arguments.  If
region is not active, use arguments to determine on which lines
to work.

If region is not active, and if all three arguments are nil, work
on the current line only.

If region is not active, and if LINES is nil, work on the lines
necessary and sufficient to encapsulate the region defined by
BEGINNING and END, limited by `point-min' and `point-max'.  If
BEGINNING is nil, use `point-min' in its place, or if END is nil,
use `point-max' in its place.

If region is not active, and if LINES is non-nil, ignore
BEGINNING and END, and work on the current line and LINES
additional lines.  LINES>0 means LINES lines below the current
line, LINES<0 means -LINES lines above the current line, and
LINES=0 means current line only.

LINES can be specified via prefix argument when calling
interactively.  When no prefix argument is specified, LINES is
nil.

Return the number of lines copied.

Note: This function calls `comment-normalize-vars', which prompts
the user for comment syntax if comment syntax is undefined for
the buffer's major mode.  So, when using this function in Lisp,
ensure that comment syntax is fully defined.  That is, ensure
that `comment-normalize-vars' completes successfully without
prompting the user for anything."
  (interactive "*P")
  (comment-normalize-vars)
  (let ((try-using-region nil)
	(copied-lines nil)
	(num-copied-lines 0)
	(beginning-bol nil)
	(end-eol nil))
    (if (use-region-p)
	(setq try-using-region t
	      beginning (region-beginning)
	      end (region-end))
      (if (null lines)
	  (if (and (null beginning) (null end))
	      (setq lines 0
		    try-using-region nil)
	    (or beginning (setq beginning (point-min)))
	    (or end (setq end (point-max)))
	    (setq try-using-region t))
	(setq try-using-region nil)
	(when (listp lines)
	  (let ((z (car lines))) (when (integerp z) (setq lines z))))))
    (save-excursion
      (if try-using-region
	  (progn (unless (integerp beginning)
		   (signal 'wrong-type-argument (list #'integerp beginning)))
		 (unless (integerp end)
		   (signal 'wrong-type-argument (list #'integerp end)))
		 (when (> beginning end)
		   (let (x) (setq x beginning beginning end end x)))
		 (let ((pmin (point-min)) (pmax (point-max)))
		   (or (and (< beginning pmin) (setq beginning pmin))
		       (and (> beginning pmax) (setq beginning pmax)))
		   (or (and (< end pmin) (setq end pmin))
		       (and (> end pmax) (setq end pmax))))
		 (when (= beginning end) (error "%s" "Nothing to comment"))
		 (goto-char beginning) (beginning-of-line)
		 (setq beginning-bol (point))
		 (goto-char end) (end-of-line)
		 (setq end-eol (point)))
	;; try-using-region is nil here; so, use the lines argument to
	;; find beginning and end.
	(unless (integerp lines)
	  (signal 'wrong-type-argument (list #'integerp lines)))
	(let ((starting-point (point)))
	  (forward-line lines)
	  (if (> lines 0)
	      (progn (end-of-line)
		     (setq end (point))
		     (goto-char starting-point)
		     (beginning-of-line)
		     (setq beginning (point)))
	    (setq beginning (point))
	    (goto-char starting-point)
	    (end-of-line)
	    (setq end (point)))
	  (when (= beginning end) (error "%s" "Nothing to comment"))
	  (setq beginning-bol beginning end-eol end)))
      (setq copied-lines (buffer-substring beginning-bol end-eol)
	    num-copied-lines (count-lines beginning-bol end-eol))
      (goto-char beginning-bol)
      (open-line 1)
      (insert copied-lines)
      (comment-region beginning-bol end-eol))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (bolp) (= beginning (point))) (forward-line num-copied-lines))
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

;; From https://github.com/cjohansen/.emacs.d/blob/e669e7964be5484d78af11c67a3811e277114816/defuns/editing-defuns.el
(defun raghu/current-quotes-char ()
  "Return non-nil if point is inside a string, nil otherwise.

The return value is the character that will terminate the string,
or t if the string should be terminated by a generic string
delimiter."
  (nth 3 (syntax-ppss)))

(defalias 'raghu/point-is-in-string-p #'raghu/current-quotes-char)

(defun raghu/mark-string (&optional activate-mark-p)
  "Mark the string containing point.

If ACTIVATE-MARK-P is non-nil, activate mark too."
  (interactive "P")
  (if (raghu/point-is-in-string-p)
      (progn
	(while (raghu/point-is-in-string-p) (backward-char 1))
	(push-mark)
	(forward-char 1)
	(while (raghu/point-is-in-string-p) (forward-char 1))
	(when activate-mark-p (activate-mark)))
    (error "Point is not in a string")))
(define-key global-map (kbd "C-c \"") #'raghu/mark-string)

(defun raghu--enable-hl-line-mode-in-buffer ()
  "Highlight line containing point in current buffer."
  (hl-line-mode 1))
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
  (add-hook 'special-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'raghu--disable-line-wrap-in-buffer)
  (add-hook 'prog-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'prog-mode-hook #'raghu--show-trailing-whitespace-in-buffer))

(with-eval-after-load 'outline
  (add-hook 'outline-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'outline-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'cus-edit
  (add-hook 'Custom-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'tabulated-list
  (add-hook 'tabulated-list-mode-hook #'raghu--disable-line-wrap-in-buffer)
  (add-hook 'tabulated-list-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-mode-hook #'raghu--enable-hl-line-mode-in-buffer))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'raghu--enable-hl-line-mode-in-buffer)
  (add-hook 'dired-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'diff-mode
  (add-hook 'diff-mode-hook #'turn-on-font-lock)
  (add-hook 'diff-mode-hook #'raghu--disable-line-wrap-in-buffer))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(frame-background-mode (quote light))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
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
 '(default ((((type x)) (:height 100)) (((type ns)) (:height 110))))
 '(eshell-prompt ((t nil)))
 '(fringe ((t (:inherit default))))
 '(isearch ((((type x ns) (class color) (background light)) (:background "plum2" :foreground "black"))))
 '(region ((((type x ns) (class color) (background light)) (:background "LightGoldenrod2")))))

;;; init.el ends here
