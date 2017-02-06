(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(global-font-lock-mode -1)
(electric-pair-mode -1)
(menu-bar-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(setenv "PAGER" "cat")
(setq inhibit-startup-screen t
      make-backup-files nil
      frame-background-mode 'light
      transient-mark-mode t
      auto-save-default t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun my-after-make-frame-hook (&rest frame)
  (when (display-graphic-p)
    (setq frame-title-format (concat "%b - emacs@" (system-name)))
    (let ((f (if (car frame)
		 (car frame)
	       (selected-frame))))
      (progn
	(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
	(set-face-background 'default "#FFFFFF" f)
	(set-face-foreground 'default "#333333" f)
	(set-face-background 'isearch "#EECCEE" f)
	(set-face-foreground 'isearch nil f)
	(set-face-background 'region "#CCEEEE" f)
	(set-face-foreground 'region nil f)
	(set-face-background 'show-paren-match "#FFFF88" f)
	(set-face-foreground 'show-paren-match nil f)
	(global-hl-line-mode 1)
	(set-face-background 'hl-line "#EEEEEE" f)
	(set-face-foreground 'hl-line nil f)))))
(add-hook 'after-make-frame-functions 'my-after-make-frame-hook t)
(add-hook 'after-init-hook 'my-after-make-frame-hook t)

(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'previous-multiframe-window)
(global-set-key (kbd "M-<down>") 'next-multiframe-window)
(global-set-key (kbd "ESC <up>") 'previous-multiframe-window)
(global-set-key (kbd "ESC <down>") 'next-multiframe-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
