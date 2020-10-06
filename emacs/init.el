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
(setenv "PAGER" "cat")

;; When working with a command interpreter through Emacs, use
;; `emacsclient' as the editor that other programs invoke (e.g. git,
;; svn), so the file to edit opens in an Emacs buffer.  It works because
;; I do server-start or, run Emacs in daemon mode.
(let ((editor "emacsclient"))
  (setenv "EDITOR" editor)
  (setenv "VISUAL" editor))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Reply with y/SPC or n/DEL instead of `yes RET' or `no RET'.
(defalias 'yes-or-no-p #'y-or-n-p)

(define-key global-map (kbd "C-c H") #'hl-line-mode)
(define-key global-map (kbd "C-c K") #'kill-whole-line)
(define-key global-map (kbd "C-c S h") #'horizontal-scroll-bar-mode)
(define-key global-map (kbd "C-c S v") #'scroll-bar-mode)
(define-key global-map (kbd "C-c f") #'forward-whitespace)
(define-key global-map (kbd "C-c w") #'toggle-truncate-lines)

;; I keep menu-bar-mode enabled because it helps me discover major-mode
;; key-bindings.  The default binding to access it---F10---is not easily
;; available on some of the keyboards that I must use.  So, add a more
;; convenient binding.
(define-key global-map (kbd "C-c M") #'menu-bar-open)

(require 'windmove)
(windmove-default-keybindings 'control)

(defun raghu/with-confirmation (fn &rest args)
  "With user confirmation, call function FN with arguments ARGS.

Obtain confirmation with `y-or-n-p' using the prompt \"Are you
sure?\".

Usage examples:

Call function foo (with arguments 2 3) with confirmation:
  (raghu/with-confirmation #'foo 2 3)
Arrange to get confirmation whenever function foo is called:
  (advice-add #'foo :around #'raghu/with-confirmation)"
  (when (y-or-n-p "Are you sure? ") (apply fn args)))

(defun raghu/switch-to-text-scratchbuffer ()
  "Switch to the scratch `text-mode' buffer.

Create the buffer if it does not already exist."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch-text*"))
  (text-mode))
(define-key global-map (kbd "C-c r") #'raghu/switch-to-text-scratchbuffer)

(defun raghu/join-region (beg end)
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

(defun raghu/join-lines (lines)
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

(defun raghu/join-lines-or-region (&optional lines)
  "Join multiple lines.

If mark is active, ignore LINES, and join the lines in the region
using `raghu/join-region'.

If mark is not active, join LINES lines using `raghu/join-lines'.

This function is meant for interactive use.  When using
interactively, use \\[universal-argument] to provide an argument
for the LINES parameter."
  (interactive "*p")
  (if (use-region-p)
      (raghu/join-region (region-beginning) (region-end))
    (raghu/join-lines (prefix-numeric-value lines))))
(define-key global-map (kbd "C-c j") #'raghu/join-lines-or-region)

(defun raghu/join-whole-buffer (buf)
  "Join buffer BUF into one line.

If BUF is narrowed, join only its accessible portion.

If called interactively, read the buffer name from the
minibuffer.

`raghu/join-region' is used to do the joining."
  (interactive "bBuffer")
  (with-current-buffer buf
    (barf-if-buffer-read-only)
    (raghu/join-region (point-min) (point-max))))
(define-key global-map (kbd "C-c J") #'raghu/join-whole-buffer)

(defun raghu/empty-buffer (buf &optional delp)
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
(define-key global-map (kbd "C-c E") #'raghu/empty-buffer)

(defun raghu/backward-whitespace (arg)
  "Move point to start of previous sequence of whitespace characters.

Each such sequence may be a single newline, or a sequence of
consecutive space and/or tab characters.  With prefix argument
ARG, move backwards ARG times."
  (interactive "p")
  (forward-whitespace (if (< arg 0) arg (- arg))))
(define-key global-map (kbd "C-c b") #'raghu/backward-whitespace)

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
(define-key global-map (kbd "C-c d") #'raghu/async-shell-command)

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
(defun raghu--scroll-text-one-line-up ()
  "Scroll text up by one line."
  (interactive)
  (scroll-up 1))
(defun raghu--scroll-text-one-line-down ()
  "Scroll text down by one line."
  (interactive)
  (scroll-down 1))
(defun raghu--scroll-text-one-col-left ()
  "Scroll text left by one column."
  (interactive)
  (scroll-left 1))
(defun raghu--scroll-text-one-col-right ()
  "Scroll text right by one column."
  (interactive)
  (scroll-right 1))

;; Next, define a map variable, and in it, give key-bindings to the
;; above functions.
(defvar raghu/scroll-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'raghu--scroll-text-one-line-up)
    (define-key map "d" #'raghu--scroll-text-one-line-down)
    (define-key map "s" #'raghu--scroll-text-one-col-left)
    (define-key map "f" #'raghu--scroll-text-one-col-right)
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
(define-key global-map (kbd "C-c k") #'raghu/kill-backward-to-indentation)

;; `comment-dwim' is too much magic, and `comment-line' includes, in
;; some situations, an extra line that I think ought not to be included.
;; I need something that just comments/uncomments lines or what I deem
;; as non-empty region, so write my own function that uses
;; `comment-lines' and `comment-or-uncomment-region' as appropriate.
(defun raghu/comment-or-uncomment-lines-or-region (&optional lines)
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
(define-key global-map (kbd "C-c c") #'raghu/comment-or-uncomment-lines-or-region)

(defun raghu/duplicate-and-comment-region (beg end)
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

(defun raghu/duplicate-and-comment-lines (lines)
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

(defun raghu/duplicate-and-comment-lines-or-region (&optional lines)
  "Duplicate and comment lines.

If mark is active, duplicate and comment the region using
`raghu/duplicate-and-comment-region'.  Otherwise, duplicate and
comment LINES lines using `raghu/duplicate-and-comment-lines'.
Use \\[universal-argument] to provide an argument for the LINES
parameter.

This function is meant for interactive use only: it calls
`comment-normalize-vars', which prompts the user for comment
syntax if comment syntax is undefined for the buffer's major
mode."
  (interactive "*P")
  (comment-normalize-vars)
  (if (use-region-p)
      (raghu/duplicate-and-comment-region (region-beginning) (region-end))
    (raghu/duplicate-and-comment-lines (prefix-numeric-value lines))))
(define-key global-map (kbd "C-c C") #'raghu/duplicate-and-comment-lines-or-region)

(defun raghu/region-expand-whole-lines (beg end &optional extremities)
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
(define-key global-map (kbd "C-c n") #'raghu/region-expand-whole-lines)

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
(define-key global-map (kbd "C-c O") #'raghu/new-line-above)

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
(define-key global-map (kbd "C-c o") #'raghu/new-line-below)

(defun raghu/insert-current-date-time (&optional arg)
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
(define-key global-map (kbd "C-c t") #'raghu/insert-current-date-time)

(defun raghu/activate-mark (&optional no-tmm)
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
(define-key global-map (kbd "C-c x") #'raghu/activate-mark)

(defun raghu--do-word-wrap-in-buffer ()
  "Do word-wrapping in current buffer."
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'word-wrap) 1))

(defun raghu--indentation-cannot-insert-tabs-in-buffer ()
  "Indentation cannot use tabs in current buffer."
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun raghu--show-trailing-whitespace-in-buffer ()
  "Highlight trailing whitespace in the current buffer."
  (set (make-local-variable 'show-trailing-whitespace) t))

(with-eval-after-load 'help-mode
  (add-hook 'help-mode-hook #'raghu--do-word-wrap-in-buffer))

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'prog-mode-hook #'raghu--show-trailing-whitespace-in-buffer)
  (add-hook 'prog-mode-hook #'whitespace-mode))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook (lambda () (hs-minor-mode -1)))
  (add-hook 'python-mode-hook #'outline-minor-mode)
  (add-hook 'inferior-python-mode-hook #'turn-on-font-lock))

;; Limit the size of comint buffers (e.g. buffers from M-x shell).
;; Also, see `comint-buffer-maximum-size'.
(with-eval-after-load 'comint
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook #'raghu--do-word-wrap-in-buffer))

(with-eval-after-load 'js
  (add-hook 'js-mode-hook #'raghu--indentation-cannot-insert-tabs-in-buffer))

(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook #'raghu--indentation-cannot-insert-tabs-in-buffer))

(with-eval-after-load 'text-mode
  (add-hook 'text-mode-hook #'raghu--do-word-wrap-in-buffer))

(with-eval-after-load 'log-edit
  (add-hook 'log-edit-mode-hook #'turn-on-font-lock))

(with-eval-after-load 'conf-mode
  (add-hook 'conf-mode-hook #'raghu--disable-line-wrap-in-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10240)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(fill-column 72)
 '(frame-background-mode (quote dark))
 '(frame-resize-pixelwise t)
 '(help-window-select t)
 '(horizontal-scroll-bar-mode nil)
 '(hscroll-margin 2)
 '(hscroll-step 1)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(js-indent-level 2)
 '(line-move-visual nil)
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(mouse-yank-at-point t)
 '(package-archive-priorities (quote (("gnu" . 90) ("melpa" . 70))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(package-selected-packages (quote (yaml-mode)))
 '(ring-bell-function (quote ignore))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 5)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(transient-mark-mode nil)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(use-dialog-box nil)
 '(vc-follow-symlinks t)
 '(whitespace-display-mappings (quote ((tab-mark 9 [8594 9]))))
 '(whitespace-style (quote (face tabs tab-mark)))
 '(window-resize-pixelwise t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x ns) (class color) (background dark)) (:background "#000000" :foreground "#ccaa55"))))
 '(custom-button ((((type x ns) (class color) (background dark)) (:background "#aaaaaa" :foreground "#000000" :box (:line-width 1 :style released-button)))))
 '(custom-button-mouse ((((type x ns) (class color) (background dark)) (:inherit custom-button))))
 '(custom-button-pressed ((((type x ns) (class color) (background dark)) (:background "#555555" :foreground "#000000" :box (:line-width 1 :style pressed-button)))))
 '(diff-added ((((type x ns) (class color) (background dark)) (:foreground "#00aa00")) (((type tty) (class color) (min-colors 8) (background dark)) (:foreground "green"))))
 '(diff-context ((((type x ns) (class color grayscale) (background dark)) (:foreground "#aaaaaa")) (((type tty) (class color grayscale) (min-colors 8) (background dark)) (:foreground "white"))))
 '(diff-file-header ((((type x ns) (class color) (background dark)) (:foreground "#55ffff" :weight bold)) (((type tty) (class color) (min-colors 16) (background dark)) (:foreground "brightcyan" :weight bold)) (((type tty) (class color) (min-colors 8) (background dark)) (:foreground "cyan" :weight bold))))
 '(diff-header ((((type x ns) (class color) (background dark)) (:foreground "#00aaaa")) (((type tty) (class color) (min-colors 8) (background dark)) (:foreground "cyan"))))
 '(diff-refine-added ((((type x ns) (class color) (background dark)) (:foreground "#55ff55" :weight bold)) (((type tty) (class color) (min-colors 16) (background dark)) (:foreground "brightgreen" :weight bold)) (((type tty) (class color) (min-colors 8) (background dark)) (:background "green" :foreground "black"))))
 '(diff-refine-removed ((((type x ns) (class color) (background dark)) (:foreground "#ff5555" :weight bold)) (((type tty) (class color) (min-colors 16) (background dark)) (:foreground "brightred" :weight bold)) (((type tty) (class color) (min-colors 8) (background dark)) (:background "red" :foreground "white"))))
 '(diff-removed ((((type x ns) (class color) (background dark)) (:foreground "#aa0000")) (((type tty) (class color) (background dark)) (:foreground "red"))))
 '(error ((((type tty) (class color) (min-colors 16) (background dark)) (:background "red" :foreground "brightwhite")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "red" :foreground "white" :weight bold)) (((type x ns) (class color) (background dark)) (:background "#aa0000" :foreground "#ffffff"))))
 '(font-lock-builtin-face ((t (:underline t))))
 '(font-lock-comment-face ((((type x ns) (class color) (background dark)) (:foreground "#00aaaa")) (((type tty) (class color) (background dark)) (:foreground "cyan"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((((type x ns) (class color) (background dark)) (:foreground "#00aa00")) (((type tty) (class color) (background dark)) (:foreground "green"))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((((type tty) (class color) (min-colors 16) (background dark)) (:foreground "brightred")) (((type tty) (class color) (min-colors 8) (background dark)) (:foreground "red")) (((type x ns) (class color) (background dark)) (:foreground "#aa5500"))))
 '(fringe ((((type x ns) (class color) (background dark)) (:background "#000000" :foreground "#5555ff"))))
 '(header-line ((((type tty) (background dark)) (:weight bold))))
 '(highlight ((((type x ns) (class color) (background dark)) (:background "#005500" :foreground "#55ff55"))))
 '(hl-line ((((type x ns) (class color) (background dark)) (:background "#550055")) (((type tty) (class color) (min-colors 88) (background dark)) (:background "color-53")) (((type tty) (class color) (min-colors 16) (background dark)) (:background "brightblack")) (((type tty) (background dark)) (:underline t))))
 '(isearch ((((type x ns) (class color) (background dark)) (:background "#aa00aa" :foreground "#ffffff")) (((type tty) (class color) (min-colors 16) (background dark)) (:background "magenta" :foreground "brightwhite")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "magenta" :foreground "white" :weight bold))))
 '(lazy-highlight ((((type x ns) (class color) (background dark)) (:background "#00aaaa" :foreground "#000000")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "cyan" :foreground "black"))))
 '(match ((((type tty) (class color) (min-colors 8) (background dark)) (:background "yellow" :foreground "black"))))
 '(menu ((((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "black"))))
 '(mode-line ((((type x ns) (class color) (background dark)) (:background "#00aaaa" :foreground "#000000" :box (:line-width 1 :style released-button))) (((type tty) (class color) (min-colors 8) (background dark)) (:background "cyan" :foreground "black"))))
 '(mode-line-buffer-id ((((type tty) (background dark)) nil)))
 '(mode-line-highlight ((((type x ns) (class color) (background dark)) (:box (:line-width 1 :color "#555555" :style released-button)))))
 '(mode-line-inactive ((((type x ns) (class color) (background dark)) (:background "#555555" :foreground "#aaaaaa" :box (:line-width 1 :style released-button))) (((type tty) (class color) (min-colors 16) (background dark)) (:background "brightblack" :foreground "white")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "black"))))
 '(org-block ((t nil)))
 '(org-date ((t nil)))
 '(org-meta-line ((t nil)))
 '(org-table ((t nil)))
 '(org-verbatim ((t nil)))
 '(outline-4 ((t nil)))
 '(region ((((type x ns) (class color) (background dark)) (:background "#aa5500" :foreground "#000000")) (((type tty) (class color) (background dark)) (:background "yellow" :foreground "black"))))
 '(sh-quoted-exec ((t nil)))
 '(shadow ((((type x ns) (class color) (background dark)) (:foreground "#885500"))))
 '(show-paren-match ((((type x ns) (class color) (background dark)) (:background "#0055aa" :foreground "#ffffff" :weight bold)) (((type tty) (class color) (background dark)) (:background "blue" :foreground "brightwhite" :weight bold))))
 '(show-paren-mismatch ((((type x ns) (class color) (background dark)) (:background "#aa0000" :foreground "#ffffff" :weight bold)) (((type tty) (class color) (background dark)) (:background "red" :foreground "brightwhite" :weight bold))))
 '(tab-bar ((((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "black"))))
 '(tab-bar-tab ((((type tty) (class color) (min-colors 8) (background dark)) (:background "cyan" :foreground "black"))))
 '(tab-bar-tab-inactive ((((type tty) (class color) (min-colors 8) (background dark)) (:inherit tab-bar))))
 '(tab-line ((((type x ns) (class color) (background dark)) (:background "#aaaaaa" :foreground "#000000")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "black"))))
 '(tab-line-highlight ((((type x ns) (class color) (background dark)) (:background "#ffffff" :foreground "#000000"))))
 '(tab-line-tab ((((type x ns) (class color) (background dark)) (:background "#555555" :foreground "#aaaaaa" :box (:line-width 1 :color "#555555"))) (((type tty) (class color) (min-colors 16) (background dark)) (:background "brightblack" :foreground "white")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "yellow" :foreground "black"))))
 '(tab-line-tab-current ((((type x ns) (class color) (background dark)) (:background "#00aaaa" :foreground "#000000" :box (:line-width 1 :color "#005555"))) (((type tty) (class color) (min-colors 8) (background dark)) (:background "cyan" :foreground "black"))))
 '(tab-line-tab-inactive ((((type x ns) (class color) (background dark)) (:background "#aaaaaa" :foreground "#000000" :box (:line-width 1 :color "#555555"))) (((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "black"))))
 '(trailing-whitespace ((((type x ns) (class color) (background dark)) (:background "#aa0000")) (((type tty) (class color) (background dark)) (:background "red"))))
 '(tty-menu-disabled-face ((((type tty) (class color) (min-colors 16) (background dark)) (:background "white" :foreground "brightblack")) (((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "yellow"))))
 '(tty-menu-enabled-face ((((type tty) (class color) (min-colors 8) (background dark)) (:background "white" :foreground "black"))))
 '(tty-menu-selected-face ((((type tty) (class color) (min-colors 8) (background dark)) (:background "cyan"))))
 '(vertical-border ((((type tty) (class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(whitespace-tab ((((type x ns) (class color) (background dark)) (:foreground "#aa00aa")) (((type tty) (class color) (background dark)) (:foreground "magenta")) (((class mono) (background dark)) (:weight bold))))
 '(widget-field ((((type x ns) (class color) (background dark)) (:background "#555555" :foreground "#ffffff" :box (:line-width 1 :color "#555555" :style pressed-button))) (((type tty) (background dark)) (:inverse-video t))))
 '(widget-inactive ((((type tty) (class color) (background dark)) (:foreground "red")))))

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
