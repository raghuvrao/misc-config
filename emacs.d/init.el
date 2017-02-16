(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("melpa-stable" . "https://stable.melpa.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")))
  (setq package-archive-priorities
	'(("gnu" . 90)
	  ("melpa-stable" . 70)
	  ("melpa" . 50)))
  (setq package-enable-at-startup nil)
  (package-initialize))

(global-font-lock-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(setenv "PAGER" "cat")
(setq inhibit-startup-screen t
      make-backup-files nil
      frame-background-mode 'light
      transient-mark-mode t
      auto-save-default t
      scroll-step 1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun my-frame-misc-stuff-hook (&rest frame)
  (when (display-graphic-p)
    (setq frame-title-format (concat "%b - " (system-name)))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
    (let ((f (if (car frame)
		 (car frame)
	       (selected-frame))))
      (set-face-background 'isearch "#EECCEE" f)
      (set-face-foreground 'isearch nil f)
      (set-face-background 'region "#FFEEBB" f)
      (set-face-background 'show-paren-match "#BBDDFF" f))))
(add-hook 'after-make-frame-functions 'my-frame-misc-stuff-hook t)
(add-hook 'after-init-hook 'my-frame-misc-stuff-hook t)

(require 'windmove)
(windmove-default-keybindings)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

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
