(require 'package)
(package-initialize)

(column-number-mode 1)
(setenv "PAGER" "cat")
(show-paren-mode 1)
(global-hl-line-mode 1)

;; Enable syntax highlighting only in some places.
(global-font-lock-mode -1)
(defun raghu--enable-font-lock-mode-in-buffer ()
  "Enable font-lock mode in the current buffer.

I add this function to various mode hooks."
  (interactive)
  (font-lock-mode 1))
(add-hook 'eshell-mode-hook #'raghu--enable-font-lock-mode-in-buffer)
(add-hook 'diff-mode-hook #'raghu--enable-font-lock-mode-in-buffer)
(add-hook 'dired-mode-hook #'raghu--enable-font-lock-mode-in-buffer)
(add-hook 'shell-mode-hook #'raghu--enable-font-lock-mode-in-buffer)
(add-hook 'special-mode-hook #'raghu--enable-font-lock-mode-in-buffer)
(add-hook 'inferior-python-mode-hook #'raghu--enable-font-lock-mode-in-buffer)

;; Create a keybinding to enable syntax highlighting when needed.
(define-key global-map (kbd "C-c p") #'font-lock-mode)

(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))

(require 'windmove)
(define-key global-map (kbd "C-<right>") #'windmove-right)
(define-key global-map (kbd "C-<left>") #'windmove-left)
(define-key global-map (kbd "C-<down>") #'windmove-down)
(define-key global-map (kbd "C-<up>") #'windmove-up)

(define-key global-map (kbd "M-C-<right>") #'enlarge-window-horizontally)
(define-key global-map (kbd "M-C-<left>") #'shrink-window-horizontally)
(define-key global-map (kbd "M-C-<down>") #'shrink-window)
(define-key global-map (kbd "M-C-<up>") #'enlarge-window)

;; Unbind `C-x C-c' so I do not accidentally kill emacs.  I will
;; instead use `M-x sa-t RET' instead.  It runs
;; save-buffers-kill-terminal, which is bound by default to `C-x C-c'.
(define-key global-map (kbd "C-x C-c") nil)

;; Use Ibuffer instead of list-buffer.
(define-key global-map (kbd "C-x C-b") #'ibuffer)

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
 )
