(require 'package)
(package-initialize)

;; Override yes-or-no-p's definition by making yes-or-no-p an alias
;; for y-or-n-p.  This way, when yes-or-no-p is called, y-or-n-p will
;; actually be called, and I can reply with y/SPC or n/DEL instead of
;; `yes RET' or `no RET'.
(defalias 'yes-or-no-p #'y-or-n-p)

(global-hl-line-mode 1)
(column-number-mode 1)
(setenv "PAGER" "cat")
(show-paren-mode 1)
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))

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
(define-key global-map (kbd "C-c p") #'font-lock-mode) ; Toggles font-lock-mode.

;; Make emacs-windows navigation easier.
(require 'windmove)
(define-key global-map (kbd "C-<up>") #'windmove-up)
(define-key global-map (kbd "C-<down>") #'windmove-down)
(define-key global-map (kbd "C-<left>") #'windmove-left)
(define-key global-map (kbd "C-<right>") #'windmove-right)

;; A few line-killing key-bindings.  Slightly easier than `C-a C-k
;; C-k' and/or `M-0 C-k'.
(define-key global-map (kbd "C-c k")
  (lambda ()
    "Kill backward from point to beginning of the line."
    (interactive)
    (kill-line 0)))
(define-key global-map (kbd "C-c K")
  (lambda ()
    "Kill the whole line."
    (interactive)
    (kill-whole-line)))

;; Make resizing windows a little easier.  The ESC <arrow> forms help
;; when running emacs in tmux.
(define-key global-map (kbd "M-<up>") #'enlarge-window)
(define-key global-map (kbd "ESC <up>") #'enlarge-window)
(define-key global-map (kbd "M-<down>") #'shrink-window)
(define-key global-map (kbd "ESC <down>") #'shrink-window)
(define-key global-map (kbd "M-<left>") #'shrink-window-horizontally)
(define-key global-map (kbd "ESC <left>") #'shrink-window-horizontally)
(define-key global-map (kbd "M-<right>") #'enlarge-window-horizontally)
(define-key global-map (kbd "ESC <right>") #'enlarge-window-horizontally)

(defun raghu/insert-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

As new lines are inserted, point's position will remain constant
relative to the line on which point was located originally."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (beginning-of-line 1)
      (open-line lines))
    ;; save-excursion does not work (works differently?) when column
    ;; is 0?
    (when (= (current-column) 0)
      (forward-line lines))))
(define-key global-map (kbd "C-c RET") #'raghu/insert-new-line-above)

(defun raghu/insert-and-go-to-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

The point is moved to the top-most line inserted."
  (interactive "p")
  (when (> lines 0)
    (beginning-of-line 1)
    (open-line lines)))
(define-key global-map (kbd "C-c O") #'raghu/insert-and-go-to-new-line-above)

(defun raghu/insert-and-go-to-new-line-below (&optional lines)
  "Insert LINES (default=1) new lines below current line.

Point moves to the newly-inserted line immediately below the line
on which point originally was."
  (interactive "p")
  (when (> lines 0)
    ;; Insert LINES-1 lines first without any post-insert formatting
    ;; (third arg to newline = nil => do not do any post-insert
    ;; formatting), while saving original point state
    ;; (save-excursion).
    (save-excursion
      (end-of-line 1)
      (newline (- lines 1) nil)))
  ;; That we did a save-excursion, point will still be at the original
  ;; position.  So, move to the end of that line and do the equivalent
  ;; of a RET (3rd arg to newline = non-nil => do post-insert
  ;; formatting).  This way, any indenting / formatting happens only
  ;; on the line immediately below the original line.
  (end-of-line 1)
  (newline 1 t))
(define-key global-map (kbd "C-c o") #'raghu/insert-and-go-to-new-line-below)

;; I accidentally do `C-x C-c' a lot (when I meant to do `C-x' or
;; `C-c'), so I will remove its binding.  To terminate emacs, I will
;; use `M-x sa-t' (which runs save-buffers-kill-terminal, bound to
;; `C-x C-c' by default) or `C-x #' or `C-x 5 0' or `M-x kill-emacs'
;; as appropriate.
(define-key global-map (kbd "C-x C-c") nil)

;; Use Ibuffer instead of list-buffers.
(define-key global-map (kbd "C-x C-b") 'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error t)
 '(frame-background-mode (quote light))
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
 '(scroll-conservatively most-positive-fixnum)
 '(sh-basic-offset 2)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(sh-indentation 2)
 '(show-paren-delay 0)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x) (class color) (background light)) (:background "white" :foreground "gray20" :height 90 :family "DejaVu Sans Mono"))))
 '(hl-line ((((type x) (class color) (background light)) (:background "DarkSeaGreen1"))))
 '(isearch ((((type x) (class color) (background light)) (:background "yellow"))))
 '(region ((((type x) (class color) (background light)) (:background "LightGoldenrod2")))))
