(require 'package)
(package-initialize)

(global-font-lock-mode -1)
(column-number-mode 1)
(setenv "PAGER" "cat")
(show-paren-mode 1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(require 'auto-complete)
(define-key ac-mode-map (kbd "C-c TAB") 'auto-complete)

(with-eval-after-load 'go-mode
  (require 'go-guru)
  (require 'go-rename)
  (require 'go-errcheck)
  (require 'go-eldoc)
  (require 'go-autocomplete)
  (setq gofmt-command (or (executable-find "goimports")
			  (executable-find "gofmt")))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook '(lambda ()
			     (setq show-trailing-whitespace t)
			     (auto-complete-mode 1)
			     (if (not (string-match "^go" compile-command))
				 (set (make-local-variable 'compile-command)
				      "go build && go vet")))))

(defun my-frame-misc-stuff-hook (&rest frame)
  (when (display-graphic-p)
    (global-hl-line-mode 1)
    (setq frame-title-format (concat "%b - " (system-name)))))
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
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(ac-disable-inline t)
 '(frame-background-mode (quote light))
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-archive-priorities (quote (("gnu" . 90) ("melpa-stable" . 70) ("melpa" . 50))))
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (go-autocomplete go-mode go-rename go-guru go-errcheck go-eldoc auto-complete)))
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
 '(default ((((type x)) (:background "white" :foreground "black" :height 98 :family "DejaVu Sans Mono"))))
 '(hl-line ((((type x)) (:background "lemon chiffon"))))
 '(isearch ((((type x)) (:background "pink"))))
 '(region ((((type x)) (:background "moccasin"))))
 '(show-paren-match ((((type x)) (:background "pale turquoise" :underline t)))))
