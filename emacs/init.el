;;; init.el --- Emacs configuration file  -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs configuration file.
;; Author: Raghu V. Rao <raghu.v.rao@gmail.com>

;;; Code:

(require 'package)
(package-initialize)

;; Various programs (e.g. git, man) like to send their stdout through a
;; pagination program (e.g. more, less).  When those programs are
;; invoked at a command interpreter (a shell) in Emacs, pagination is
;; unnecessary.
;;
;; When working with a command interpreter through Emacs, use
;; `emacsclient' as the editor that other programs invoke (e.g. git,
;; svn), so the file to edit opens in an Emacs buffer.  It works because
;; I do server-start or, run Emacs in daemon mode.
(let ((pager "cat") (editor "emacsclient"))
  (setenv "EDITOR" editor)
  (setenv "VISUAL" editor)
  (setenv "PAGER" pager))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(define-key global-map (kbd "C-c F") #'customize-face)
(define-key global-map (kbd "C-c H") #'global-hl-line-mode)
(define-key global-map (kbd "C-c K") #'kill-whole-line)
(define-key global-map (kbd "C-c N") #'display-line-numbers-mode)
(define-key global-map (kbd "C-c f") #'forward-whitespace)

;; The menu bar helps me discover major-mode key-bindings.  The default
;; binding to access the menu---F10---is not easily available on some of
;; the keyboards that I must use.  So, add a more convenient binding.
(define-key global-map (kbd "C-c M") #'menu-bar-open)

(require 'windmove)
(windmove-default-keybindings 'control)

(require 'expand-region)
(define-key global-map (kbd "C-c e") #'er/expand-region)

;; Disable the scroll bars in the minibuffer window.
(set-window-scroll-bars (minibuffer-window) nil nil nil nil t)

(defun my/with-confirmation (fn &rest args)
  "With user confirmation, call function FN with arguments ARGS.

Obtain confirmation with `y-or-n-p' using the prompt \"Are you
sure?\".

Usage examples:

Call function foo (with arguments 2 3) with confirmation:
  (my/with-confirmation #'foo 2 3)
Arrange to get confirmation whenever function foo is called:
  (advice-add #'foo :around #'my/with-confirmation)"
  (when (y-or-n-p "Are you sure? ") (apply fn args)))

(defun my/text-scratch-buffer ()
  "Switch to the scratch `text-mode' buffer.

Create the buffer if it does not already exist."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch-text*"))
  (when (eq (buffer-local-value 'major-mode (current-buffer)) 'fundamental-mode)
    (text-mode)))
(define-key global-map (kbd "C-c T") #'my/text-scratch-buffer)

(defun my/scratch-buffer ()
  "Switch to the scratch buffer.

Create the buffer if it does not already exist."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (when (eq (buffer-local-value 'major-mode (current-buffer)) 'fundamental-mode)
    (lisp-interaction-mode)))
(define-key global-map (kbd "C-c S") #'my/scratch-buffer)

(defun my/join-region (beg end)
  "Join region into a single line.

BEG and END mark the beginning and the end of the region.  Whole
lines are joined.

Lines are joined using `join-line'.

Return number of lines joined."
  (unless (natnump beg) (signal 'wrong-type-argument (list #'natnump beg)))
  (unless (natnump end) (signal 'wrong-type-argument (list #'natnump end)))
  (let ((joined 0))
    (unless (= beg end)
      (when (< end beg) (setq end (prog1 beg (setq beg end))))
      (let ((start-line-start nil) (end-line-start nil) (end-line-end nil))
	(save-excursion
	  (goto-char beg)
	  (setq start-line-start (line-beginning-position))
	  (goto-char end)
	  (when (bolp) (forward-line -1))
	  (setq end-line-start (line-beginning-position))
	  (setq end-line-end (line-end-position)))
	(unless (= start-line-start end-line-start)
	  (goto-char end-line-end)
	  (while (/= (line-beginning-position) start-line-start)
	    (join-line)
	    (setq joined (1+ joined))))))
    joined))

(defun my/join-lines (lines)
  "Join multiple lines into a single line.

If LINES is 0, do not join any lines.  If LINES > 0, join the
current line and upto LINES lines above it.  If LINES < 0, join
the current line and upto -LINES lines below it.

Lines are joined using `join-line'.

Return number of lines joined."
  (unless (integerp lines)
    (signal 'wrong-type-argument (list #'integerp lines)))
  (let ((joined 0))
    (if (< lines 0)
	(progn (setq lines (- lines))
	       (while (and (< joined lines)
			   (progn (end-of-line) (not (eobp))))
		 (join-line t)
		 (setq joined (1+ joined))))
      (while (and (< joined lines)
		  (progn (beginning-of-line) (not (bobp))))
	(join-line)
	(setq joined (1+ joined))))
    joined))

(defun my/join-lines-or-region (&optional lines)
  "Join multiple lines.

If mark is active, ignore LINES, and join the lines in the region
using `my/join-region'.

If mark is not active, join LINES lines using `my/join-lines'.

This function is meant for interactive use.  When using
interactively, use \\[universal-argument] to provide an argument
for the LINES parameter."
  (interactive "*p")
  (if (use-region-p)
      (my/join-region (region-beginning) (region-end))
    (my/join-lines (prefix-numeric-value lines))))
(define-key global-map (kbd "C-c j") #'my/join-lines-or-region)

(defun my/join-whole-buffer (buf)
  "Join buffer BUF into one line.

If BUF is narrowed, join only its accessible portion.

If called interactively, read the buffer name from the
minibuffer.

`my/join-region' is used to do the joining."
  (interactive "bBuffer")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (my/join-region (point-min) (point-max))))
(define-key global-map (kbd "C-c J") #'my/join-whole-buffer)

(defun my/empty-buffer (buf &optional delp)
  "Remove the contents of the buffer BUF.

If BUF is narrowed, empty only its accessible portion.

If optional argument DELP is non-nil, delete the contents of BUF
without modifying the kill ring.  Otherwise, save the removed
contents in the kill ring.

In interactive use, read the buffer name from the minibuffer, and
get DELP from \\[universal-argument]."
  (interactive "bBuffer to empty\nP")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (if delp
	(delete-region (point-min) (point-max))
      (kill-region (point-min) (point-max)))))
(define-key global-map (kbd "C-c E") #'my/empty-buffer)

(defun my/backward-whitespace (arg)
  "Move point to start of previous sequence of whitespace characters.

Each such sequence may be a single newline, or a sequence of
consecutive space and/or tab characters.  With prefix argument
ARG, move backwards ARG times."
  (interactive "p")
  (forward-whitespace (if (< arg 0) arg (- arg))))
(define-key global-map (kbd "C-c b") #'my/backward-whitespace)

;; Useful for changing CRLF line terminators to LF line terminators.
(defun my/to-unix-utf-8-buffer (buf)
  "Convert file encoding system of buffer BUF to UNIX UTF-8.

If called interactively, read buffer name from minibuffer."
  (interactive "bBuffer")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (set-buffer-file-coding-system 'unix t)
    (set-buffer-file-coding-system 'utf-8 t)
    (message "To UNIX UTF-8: %S" (get-buffer buf))
    t))

(defun my/indent-buffer (buf)
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
(defun my/async-shell-command (cmd)
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
(define-key global-map (kbd "C-c d") #'my/async-shell-command)

(defun my/visit-emacs-configuration-file ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
(define-key global-map (kbd "C-c I") #'my/visit-emacs-configuration-file)

;; Scroll while keeping point on original text-line (so long as the
;; original text-line is in the window, of course).
;;
;; First, define a few scrolling-related functions.  These functions
;; will be eventually bound to keys.
(defun my--scroll-text-one-line-up ()
  "Scroll text up by one line."
  (interactive)
  (scroll-up 1))
(defun my--scroll-text-one-line-down ()
  "Scroll text down by one line."
  (interactive)
  (scroll-down 1))
(defun my--scroll-text-one-col-left ()
  "Scroll text left by one column."
  (interactive)
  (scroll-left 1))
(defun my--scroll-text-one-col-right ()
  "Scroll text right by one column."
  (interactive)
  (scroll-right 1))

;; Next, define a map variable, and in it, give key-bindings to the
;; above functions.
(defvar my/scroll-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'my--scroll-text-one-line-up)
    (define-key map "d" #'my--scroll-text-one-line-down)
    (define-key map "s" #'my--scroll-text-one-col-left)
    (define-key map "f" #'my--scroll-text-one-col-right)
    map)
  "Keymap for scrolling one line/column at a time.

While scrolling, point remains with the original text-line (not
screen-line) so long as the original text-line is within the
window.")

;; Finally, define a function and a key-binding to activate the above
;; keymap in a transient fashion.
(defun my/scroll ()
  "Scroll in the current buffer.

Activate the keymap `my/scroll-map', which makes scrolling a
little easier.  When a key that is not in the map is pressed, it
will deactivate the keymap so usual editing operations can be
resumed.  A mark is set at point's original starting position.

\\{my/scroll-map}"
  (interactive)
  (push-mark)
  (message "%s" "my/scroll-map ON")
  (set-transient-map my/scroll-map
		     (lambda ()
		       (when (assoc last-input-event my/scroll-map)
			 (message "%s" "my/scroll-map ON")))
		     (lambda ()
		       (message "%s" "my/scroll-map OFF"))))
(define-key global-map (kbd "C-c s") #'my/scroll)

(defun my/kill-backward-to-indentation (&optional arg)
  "Kill backward from point to first nonblank character on line.

When ARG is a non-zero integer, kill backward from point to the
indentation of the ARGth line above the current line.  When ARG
is 0, nil or t, kill backward from point to the indentation of
the current line.

ARG can be supplied through \\[universal-argument]."
  (interactive "*P")
  (cond ((booleanp arg) (setq arg 0))
	((consp arg) (let ((z (car arg))) (when (natnump z) (setq arg z)))))
  (unless (natnump arg)
    (signal 'wrong-type-argument (list (list #'natnump #'booleanp) arg)))
  (let ((starting-point (point)) (point-at-indent nil))
    (backward-to-indentation 0)
    (when (> arg 0)
      (let ((x (point))) (when (> x starting-point) (setq starting-point x)))
      (backward-to-indentation arg))
    (setq point-at-indent (point))
    (when (> starting-point point-at-indent)
      (kill-region point-at-indent starting-point))))
(define-key global-map (kbd "C-c k") #'my/kill-backward-to-indentation)

;; `comment-dwim' is too much magic, and `comment-line' includes, in
;; some situations, an extra line that I think ought not to be included.
;; I need something that just comments/uncomments lines or what I deem
;; as non-empty region, so write my own function that uses
;; `comment-lines' and `comment-or-uncomment-region' as appropriate.
(defun my/comment-or-uncomment-lines-or-region (&optional lines)
  "Comment or uncomment region or lines.

If region is active, ignore parameter LINES, and comment or
uncomment whole lines containing the region using
`comment-or-uncomment-region'.  Exclude leading/trailing empty
lines, and the leading/trailing newline, if any, in the region.

If region is not active, comment or uncomment LINES lines using
`comment-line'.

In interactive use, \\[universal-argument] can be used to provide
argument for LINES."
  (interactive "*P")
  (if (use-region-p)
      (let ((beg (region-beginning)) (end (region-end)))
	(save-excursion
	  (goto-char beg)
	  ;; Exclude leading newline (need not be due to an empty line),
	  ;; if any.
	  (when (eolp) (forward-line 1))
	  ;; Skip over leading empty lines, if any.
	  (while (and (eolp) (bolp) (< (point) end))
	    (forward-line 1))
	  (setq beg (line-beginning-position))
	  (when (= beg end) (user-error "Effectively empty region"))
	  (goto-char end)
	  ;; Exclude trailing newline (need not be due to an empty
	  ;; line), if any.
	  (when (bolp) (forward-line -1))
	  ;; Skip over trailing empty lines, if any.
	  (while (and (eolp) (bolp) (> (point) beg))
	    (forward-line -1))
	  (setq end (line-end-position))
	  (when (= beg end) (user-error "Effectively empty region"))
	  (comment-or-uncomment-region beg end)))
    (comment-line (prefix-numeric-value lines))))
(define-key global-map (kbd "C-c c") #'my/comment-or-uncomment-lines-or-region)

(defun my/duplicate-and-comment-region (beg end)
  "Duplicate and comment region.

BEG and END mark the region.  Duplicate and comment whole lines;
expand partial lines to whole lines.

This function does not check if comment-syntax is defined for the
buffer's major mode."
  (unless (natnump beg) (signal 'wrong-type-argument (list #'natnump beg)))
  (unless (natnump end) (signal 'wrong-type-argument (list #'natnump end)))
  (unless (= beg end)
    (when (> beg end) (let ((aux beg)) (setq beg end end aux)))
    ;; Create a marker and manage it myself here instead of using
    ;; `save-excursion' because `save-excursion' seems to be doing the
    ;; equivalent of creating a marker that advances on insert except
    ;; when the marker is at the beginning of the line.  I want the
    ;; marker to advance always.
    (let ((original-location (make-marker)))
      (set-marker-insertion-type original-location t) ; Advance on insert
      (set-marker original-location (point))
      (let ((copied-lines nil))
	(goto-char beg)
	(when (eolp) (forward-line 1))
	(setq beg (line-beginning-position))
	(goto-char end)
	(unless (= beg end)
	  (when (bolp) (forward-line -1))
	  (setq end (line-end-position))
	  (unless (= beg end)
	    (setq copied-lines (buffer-substring beg end))
	    (goto-char beg)
	    (open-line 1)
	    (insert copied-lines)
	    (comment-region beg end))))
      (goto-char original-location)
      (set-marker original-location nil))))

(defun my/duplicate-and-comment-lines (lines)
  "Duplicate and comment lines.

If LINES > 0, work on the current line and LINES-1 lines below
it.  If LINES < 0, work on the current line and -LINES-1 lines
above it.  If LINES = 0, do nothing.

This function does not check if comment-syntax is defined for the
buffer's major mode."
  (unless (integerp lines)
    (signal 'wrong-type-argument (list #'integerp lines)))
  (let ((copied-lines nil) (beg 0) (end 0))
    (cond ((> lines 0) (setq beg (line-beginning-position)
			     end (line-end-position lines)))
	  ((< lines 0) (setq end (line-end-position)
			     beg (line-beginning-position (+ lines 2)))))
    (unless (= beg end)
      ;; Create a marker and manage it myself here instead of using
      ;; `save-excursion' because `save-excursion' seems to be doing the
      ;; equivalent of creating a marker that advances on insert except
      ;; when the marker is at the beginning of the line.  I want the
      ;; marker to advance always.
      (let ((original-location (make-marker)))
	(set-marker-insertion-type original-location t) ; Advance on insert
	(set-marker original-location (point))
	(setq copied-lines (buffer-substring beg end))
	(goto-char beg)
	(open-line 1)
	(insert copied-lines)
	(comment-region beg end)
	(goto-char original-location)
	(set-marker original-location nil)))))

(defun my/duplicate-and-comment-lines-or-region (&optional lines)
  "Duplicate and comment lines.

If mark is active, duplicate and comment the region using
`my/duplicate-and-comment-region'.  Otherwise, duplicate and
comment LINES lines using `my/duplicate-and-comment-lines'.
Use \\[universal-argument] to provide an argument for the LINES
parameter.

This function is meant for interactive use only: it calls
`comment-normalize-vars', which prompts the user for comment
syntax if comment syntax is undefined for the buffer's major
mode."
  (interactive "*P")
  (comment-normalize-vars)
  (if (use-region-p)
      (my/duplicate-and-comment-region (region-beginning) (region-end))
    (my/duplicate-and-comment-lines (prefix-numeric-value lines))))
(define-key global-map (kbd "C-c C") #'my/duplicate-and-comment-lines-or-region)

(defun my/region-expand-whole-lines (beg end &optional extremities)
  "Expand region to cover whole lines, and activate mark.

BEG and END are the boundaries of the region to be expanded to
whole lines.

Optional prefix argument EXTREMITIES determines whether to
exclude indentation (at region beginning) and trailing
newline (at region end).

Supply EXTREMITIES with \\[universal-argument].

No \\[universal-argument]: include both trailing newline at
region end and indentation at region beginning

One \\[universal-argument]: exclude trailing newline at region
end, include indentation at region beginning

Two \\[universal-argument]: include trailing newline at region
end, exclude indentation at region beginning

Three or more \\[universal-argument]: exclude both trailing
newline at region end and indentation at region beginning"
  (interactive "r\np")
  (unless (natnump beg) (signal 'wrong-type-argument (list #'natnump beg)))
  (unless (natnump end) (signal 'wrong-type-argument (list #'natnump end)))
  (cond ((booleanp extremities) (setq extremities 1))
	((natnump extremities) t)
	(t (signal 'wrong-type-argument (list #'natnump extremities))))
  (when (< end beg) (setq beg (prog1 end (setq end beg))))
  (cond
   ;; No `universal-argument'
   ((< extremities 4)
    (if (= (point) end)
	(progn (unless (bolp) (forward-line 1))
	       (exchange-point-and-mark)
	       (unless (bolp) (goto-char (line-beginning-position))))
      (unless (bolp) (goto-char (line-beginning-position)))
      (exchange-point-and-mark)
      (unless (bolp) (forward-line 1))))
   ;; One `universal argument'
   ((< extremities 16)
    (if (= (point) end)
	(progn (when (bolp) (forward-line -1))
	       (unless (eolp) (goto-char (line-end-position)))
	       (exchange-point-and-mark)
	       (unless (bolp) (goto-char (line-beginning-position))))
      (unless (bolp) (goto-char (line-beginning-position)))
      (exchange-point-and-mark)
      (when (bolp) (forward-line -1))
      (unless (eolp) (goto-char (line-end-position)))))
   ;; Two `universal-argument'
   ((< extremities 64)
    (if (= (point) end)
	(progn (unless (bolp) (forward-line 1))
	       (exchange-point-and-mark)
	       (back-to-indentation))
      (back-to-indentation)
      (exchange-point-and-mark)
      (unless (bolp) (forward-line 1))))
   ;; Three or more `universal-argument'
   ((>= extremities 64)
    (if (= (point) end)
	(progn (when (bolp) (forward-line -1))
	       (unless (eolp) (goto-char (line-end-position)))
	       (exchange-point-and-mark)
	       (back-to-indentation))
      (back-to-indentation)
      (exchange-point-and-mark)
      (when (bolp) (forward-line -1))
      (unless (eolp) (goto-char (line-end-position))))))
  ;; In every case above, point and mark will have been exchanged, so
  ;; restore them now.
  (exchange-point-and-mark)
  (activate-mark))
(define-key global-map (kbd "C-c n") #'my/region-expand-whole-lines)

(defun my/new-line-above (arg)
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
(define-key global-map (kbd "C-c O") #'my/new-line-above)

(defun my/new-line-below (arg)
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
(define-key global-map (kbd "C-c o") #'my/new-line-below)

(defun my/insert-current-date-time (&optional arg)
  "At point, insert current date and time in system timezone.

Use \\[universal-argument] to supply optional prefix argument
ARG.

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
(define-key global-map (kbd "C-c t") #'my/insert-current-date-time)

(defun my/activate-mark (&optional no-tmm)
  "Activate the mark without moving point.

Activate the mark using `activate-mark'.  Send optional argument
NO-TMM to `activate-mark' without modification.

While keeping Transient Mark mode disabled, reactivating the
deactivated mark exchanges point and mark (see info
node `(emacs)Disabled Transient Mark').  Also, `activate-mark' is
not an interactive function.

Address the above through this function."
  (interactive "P")
  (if (mark)
      (activate-mark no-tmm)
    (user-error "%s" "No mark set in this buffer")))
(define-key global-map (kbd "C-c x") #'my/activate-mark)

(defun my--indent-without-tabs-in-buffer ()
  "In the current buffer, arrange not to use tabs for indentation."
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun my--backward-delete-char-untabify ()
  "When deleting a tab, arrange to turn the tab to many spaces to delete one space."
  (set (make-local-variable 'backward-delete-char-untabify-method) 'untabify))

(with-eval-after-load 'elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'my--backward-delete-char-untabify))

(with-eval-after-load 'js
  (add-hook 'js-mode-hook #'my--indent-without-tabs-in-buffer))

(defun my--customizations-prog-mode ()
  "My customizations for programming modes."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'show-trailing-whitespace) t))
(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'my--customizations-prog-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(defun my--customizations-python-mode ()
  "My customizations for python-mode."
  (hs-minor-mode -1))
(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'my--customizations-python-mode)
  (add-hook 'python-mode-hook #'outline-minor-mode))

(defun my--customizations-shell-mode ()
  "My customizations for shell-mode."
  (set (make-local-variable 'comint-process-echoes) t)
  (set (make-local-variable 'comint-buffer-maximum-size) 10240)
  (set (make-local-variable 'comint-scroll-to-bottom-on-input) 'this)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer 0 t))
(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook #'my--customizations-shell-mode))

(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook #'my--indent-without-tabs-in-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backward-delete-char-untabify-method nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(cursor-type '(bar . 2))
 '(fill-column 80)
 '(frame-background-mode 'light)
 '(frame-resize-pixelwise t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(help-window-select t)
 '(horizontal-scroll-bar-mode nil)
 '(hscroll-margin 2)
 '(hscroll-step 1)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ispell-dictionary nil)
 '(js-indent-level 2)
 '(line-move-visual nil)
 '(line-spacing 0.2)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(mouse-yank-at-point t)
 '(package-archive-priorities '(("gnu" . 70) ("nongnu" . 70) ("melpa" . 90)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(gnu-elpa-keyring-update lyrics markdown-mode expand-region yaml-mode))
 '(ring-bell-function 'ignore)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 5)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(window-resize-pixelwise t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-error ((((type x) (class color) (background light)) (:foreground "firebrick"))))
 '(compilation-info ((((type x) (class color) (background light)) (:foreground "DarkGreen"))))
 '(compilation-mode-line-fail ((((type x) (class color) (background light)) (:inherit compilation-error :weight bold))))
 '(compilation-warning ((((type x) (class color) (background light)) (:foreground "saddle brown"))))
 '(cursor ((((type x) (class color) (background light)) (:background "magenta3"))))
 '(eshell-prompt ((((type x) (class color) (background light)) (:foreground "DarkMagenta"))))
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((((type x tty) (class color) (min-colors 8) (background light)) (:foreground "MediumBlue"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((((class color) (min-colors 8) (background light)) (:foreground "DarkGreen"))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((((type x) (class color) (background light)) (:background "honeydew" :weight bold)) (t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((((type x) (class color) (background light)) (:background "lavender blush" :weight bold)) (t (:weight bold))))
 '(font-lock-string-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((((type x) (class color) (background light)) (:foreground "brown")) (((type tty) (class color) (min-colors 88) (background light)) (:foreground "brown"))))
 '(fringe ((((class color) (background light)) (:inherit shadow))))
 '(help-key-binding ((((type x) (class color) (background light)) (:background "grey96" :foreground "DarkBlue" :weight bold))))
 '(hl-line ((((type x tty) (class color) (background light)) (:extend t :background "linen"))))
 '(isearch ((((type x) (class color) (background light)) (:background "magenta3" :foreground "white"))))
 '(line-number ((((type x) (class color) (background light)) (:inherit (fixed-pitch shadow)))))
 '(line-number-current-line ((t (:inherit (highlight default)))))
 '(menu ((((type tty) (class color) (min-colors 88) (background light)) (:extend t :background "white" :foreground "black"))))
 '(mode-line ((((type x) (class color) (background light)) (:background "grey82" :foreground "black" :box (:line-width 1 :color "black")))))
 '(mode-line-highlight ((((type x) (class color) (background light)) (:background "PaleTurquoise"))))
 '(mode-line-inactive ((((type x) (class color) (background light)) (:inherit mode-line :background "gray92" :box (:line-width 1 :color "grey60")))))
 '(org-block ((t nil)))
 '(org-checkbox ((t nil)))
 '(org-date ((t nil)))
 '(org-document-info ((t nil)))
 '(org-document-info-keyword ((t nil)))
 '(org-document-title ((t nil)))
 '(org-meta-line ((t nil)))
 '(org-table ((t nil)))
 '(org-todo ((t nil)))
 '(org-verbatim ((t nil)))
 '(outline-4 ((t nil)))
 '(region ((((type x) (class color) (background light)) (:background "LightGoldenRod"))))
 '(scroll-bar ((((class color) (background light)) (:background "grey95" :foreground "grey65"))))
 '(sh-quoted-exec ((t nil)))
 '(show-paren-match ((((type x) (class color) (background light)) (:background "PaleTurquoise"))))
 '(trailing-whitespace ((((type x) (class color) (background light)) (:background "MistyRose2"))))
 '(tty-menu-disabled-face ((((type tty) (class color) (min-colors 88)) (:background "blue" :foreground "white"))))
 '(tty-menu-enabled-face ((((type tty) (class color) (min-colors 88)) (:background "blue" :foreground "brightwhite"))))
 '(tty-menu-selected-face ((((type tty) (class color) (min-colors 88)) (:background "magenta"))))
 '(widget-field ((((type tty) (class color) (min-colors 88) (background light)) (:background "white" :foreground "black")))))

(require 'server)
;; To check if *this* Emacs process started a server, use:
;;
;;   (and (boundp 'server-process) server-process)
;;
;; To check if *some* Emacs process started a server, use:
;;
;;   (server-running-p)
;;
;; See:
;;
;;   http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00720.html
;;   https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-26&id=8182d648cb18fb048495c761db7c21fbf3c2a624
(unless (server-running-p) (server-start))

;;; init.el ends here
