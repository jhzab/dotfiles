(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

(setq package-list '(ensime magit helm flyspell flycheck neotree avy ace-window scala-mode2 git-gutter monokai-theme anzu flx-ido swiper smart-mode-line ample-theme))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(helm-mode 1)

(setq sml/theme 'dark)
(sml/setup)

(global-set-key [f8] 'neotree-toggle)

; less GC overhead
(setq gc-cons-threshold 12000000)

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
  scroll-conservatively 1
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

; swiper is a replacement for isearch
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
