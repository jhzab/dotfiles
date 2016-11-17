(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)
(setq ensime-startup-snapshot-notification nil)

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package flx-ido
  :demand
  :init
  (setq
   ido-enable-flex-matching t
   ;; C-d to open directories
   ;; C-f to revert to find-file
   ido-show-dot-for-dired nil
   ido-enable-dot-prefix t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :config (progn
	    (global-set-key "\C-s" 'swiper)
	    (global-set-key (kbd "C-c C-r") 'ivy-resume)))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package haskell-mode
  :ensure t)

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package company
  :diminish company-mode
  :commands company-mode
  :ensure t
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package flycheck :ensure t)
(use-package magit :ensure t)
(use-package flyspell :ensure t)
(use-package neotree :ensure t)
(use-package ace-window :ensure t)
(use-package smex :ensure t)
(use-package scala-mode :ensure t)
(use-package git-gutter :ensure t)
(use-package anzu :ensure t)
(use-package smex :ensure t)
(use-package smart-mode-line :ensure t)
(use-package projectile :ensure t)
(use-package ido-ubiquitous :ensure t)
(use-package rust-mode :ensure t)

(setq inhibit-startup-message t)

(setq sml/theme 'dark)
(sml/setup)

(global-set-key [f8] 'neotree-toggle)

; less GC overhead
(setq gc-cons-threshold 12000000)

(setq next-screen-context-lines 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (monokai-theme helm magit ensime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq
  x-select-enable-clipboard t
  auto-save-default nil
  make-backup-files nil
  column-number-mode t
  line-number-mode t
  whitespace-line-column 80
  load-prefer-newer t
  ibuffer-always-show-last-buffer t
  ibuffer-view-ibuffer t
  ;; hide empty filter groups
  ibuffer-show-empty-filter-groups nil
  ;; show human readable sizes in dired
  dired-listing-switches "-alh"
  scroll-step 1
  ;; Less jumpy arrow key scrolling
  scroll-conservatively 10000
  )

; text replace: https://github.com/syohex/emacs-anzu
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

; ace-window configuration
; https://github.com/abo-abo/ace-window
(global-set-key (kbd "M-p") 'ace-window)

; ido-ubiquitious
(ido-ubiquitous-mode 1)

;smex
(global-set-key (kbd "M-x") 'smex)

;flyspell

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))


(setq gnus-select-method
      '(nnimap "l3s"
               (nnimap-address "mail.l3s.uni-hannover.de")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date)
      gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

(setq gnus-use-cache t)
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)
;(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

(setq gnus-user-date-format-alist
          '(((gnus-seconds-today) . "Today, %H:%M")
            ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
            (604800 . "%A %H:%M") ;;that's one week
            ((gnus-seconds-month) . "%A %d")
            ((gnus-seconds-year) . "%B %d")
            (t . "%B %d '%y"))) ;;this one is used when no other does match

;(setq gnus-summary-line-format 
;  "%U%R: %&user-date; | %~(pad-right 25)~(max-right 25)a | %I%~(max-right 50)S \n")

(setq gnus-summary-line-format
      (concat "%U%R %~(pad-right 2)t%* %B%~(max-right 30)~(pad-right 30)n  "
              "%~(max-right 90)~(pad-right 90)s %-135=%&user-date;\n"))


(add-hook 'haskell-mode-hook 'intero-mode)

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ) )
