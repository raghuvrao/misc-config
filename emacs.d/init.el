(require 'package)
(package-initialize)

(global-font-lock-mode -1)
(column-number-mode 1)
(setenv "PAGER" "cat")
(show-paren-mode 1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(defun raghu-frame-hook (&rest frame)
  "A few things to run each time a new frame is created."
  (when (display-graphic-p)
    (setq frame-title-format (concat "%b - " (system-name)))))
(add-hook 'after-make-frame-functions 'raghu-frame-hook t)
(add-hook 'after-init-hook 'raghu-frame-hook t)

(require 'windmove)
(windmove-default-keybindings)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(defun raghu-get-temporary-buffer ()
  "Switch to the buffer named \"*raghu-temporary*\".

If the buffer does not already exist, create it and then switch to it."
  (interactive)
  (switch-to-buffer (get-buffer-create "*raghu-temporary*")))
(global-set-key (kbd "C-c r t") 'raghu-get-temporary-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(scroll-conservatively 101)
 '(show-paren-delay 0)
 '(show-trailing-whitespace nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x)) (:height 100))))
 '(isearch ((((type x)) (:background "pink"))))
 '(region ((((type x)) (:background "moccasin"))))
 '(show-paren-match ((((type x)) (:background "pale turquoise" :underline t)))))
