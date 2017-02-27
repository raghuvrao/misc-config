(require 'package)
(package-initialize)

(column-number-mode 1)
(setenv "PAGER" "cat")
(show-paren-mode 1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(require 'windmove)
(windmove-default-keybindings)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(defun raghu-get-temporary-buffer ()
  "Switch to the buffer named \"*raghu-temporary*\".

If the buffer does not already exist, create it and then switch
to it.  I use this buffer as a hold area for text I do not plan
on saving."
  (interactive)
  (switch-to-buffer (get-buffer-create "*raghu-temporary*")))
(global-set-key (kbd "C-c r t") 'raghu-get-temporary-buffer)

;; Unbind `C-x C-c' so I do not accidentally kill emacs.  I will
;; instead use `M-x s a - t RET' instead.  It runs
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
 '(scroll-conservatively 101)
 '(show-paren-delay 0)
 '(show-trailing-whitespace nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type x)) (:height 100))))
 '(isearch ((((type x)) (:background "yellow2"))))
 '(region ((((type x)) (:background "LightGoldenRod2")))))
