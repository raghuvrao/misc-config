(require 'package)
(package-initialize)

(column-number-mode 1)
(setenv "PAGER" "cat")
(show-paren-mode 1)
(global-hl-line-mode 1)

;; Enable syntax highlighting only in some places.
(global-font-lock-mode -1)
(add-hook 'diff-mode-hook #'font-lock-mode)
(add-hook 'special-mode-hook #'font-lock-mode)

;; Create a keybinding to enable syntax highlighting when needed.
(global-set-key (kbd "C-c r f") #'font-lock-mode)

(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))

(require 'windmove)
(windmove-default-keybindings)

(global-set-key (kbd "S-C-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "S-C-<down>") #'shrink-window)
(global-set-key (kbd "S-C-<up>") #'enlarge-window)

;; Unbind `C-x C-c' so I do not accidentally kill emacs.  I will
;; instead use `M-x sa-t RET' instead.  It runs
;; save-buffers-kill-terminal, which is bound by default to `C-x C-c'.
(define-key global-map (kbd "C-x C-c") nil)

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
 '(show-paren-delay 0)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x pm w32 ns)) (:height 90 :family "DejaVu Sans Mono"))))
 '(region ((((type x pm w32 ns) (background light)) (:background "LightGoldenRod2")))))
