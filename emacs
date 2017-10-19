(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)
(setq ensime-startup-snapshot-notification nil)
(add-to-list 'load-path "~/src/mu/mu4e")

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize) ;; You might already have this line

;(use-package benchmark-init)
;(benchmark-init/activate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package flx-ido
  :ensure t
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
(use-package counsel
  :ensure t
  :bind ("M-x" . counsel-M-x)
  :bind ("C-c s" . counsel-ag)
  :bind ("C-c C-f" . counsel-find-file))
;; This is needed for last recently used in M-x and is automatically
;; used by for instance counsel-M-x
(use-package smex)

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package intero
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package haskell-mode
  :ensure t)

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package company
  :diminish company-mode
  :commands global-company-mode
  :ensure t
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-tooltip-align-annotations t
   company-minimum-prefix-length 4)
  ;; disables TAB in company-mode, freeing it for yasnippet
  ;(define-key company-active-map [tab] nil)
					;(define-key company-active-map (kbd "TAB") nil)
  :config
  (global-company-mode)
  )

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

(use-package flyspell :ensure t)
(use-package magit
  :ensure t
  :defer t
  :config
  (add-hook 'programming-mode-hook 'magit-mode)
  )
(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'programming-mode-hook 'flycheck-mode))
(use-package neotree :ensure t)
(use-package ace-window :ensure t)
(use-package scala-mode :ensure t)
(use-package git-gutter :ensure t)
(use-package anzu :ensure t)
(use-package smart-mode-line :ensure t)
(use-package projectile :ensure t)
(use-package ido-completing-read+ :ensure t)
(use-package rust-mode :ensure t)
(use-package racer :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode))
(use-package company-racer :ensure t
  :config
  (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-racer)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq inhibit-startup-message t)

(setq sml/theme 'dark)
(sml/setup)

(global-set-key [f8] 'neotree-toggle)

; less GC overhead
(setq gc-cons-threshold 12000000)

(setq next-screen-context-lines 4)

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
(global-set-key (kbd "M-o") 'ace-window)

; ido-ubiquitious
(ido-ubiquitous-mode 1)

(use-package company-jedi :ensure t)

(setq jedi:environment-root "jedi")  ; or any other name you like
(setq jedi:environment-virtualenv
     (list "virtualenv-3" "--system-site-packages"))
(setq jedi:complete-on-dot t) 

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;flyspell

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(add-hook 'haskell-mode-hook 'intero-mode)

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ) )

(setq user-mail-address "zab@l3s.de"
    user-full-name  "Jan-Hendrik Zab")
    
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("mail.l3s.uni-hannover.de" 587 nil nil))
      smtpmail-default-smtp-server "mail.l3s.uni-hannover.de"
      smtpmail-smtp-server "mail.l3s.uni-hannover.de"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "/home/gothos/Maildir"))
(setq mu4e-drafts-folder "/INBOX/.Drafts")
(setq mu4e-sent-folder   "/INBOX/.Sent")
(setq mu4e-trash-folder  "/INBOX/.Trash")
(setq mu4e-compose-signature-file "/home/gothos/.signature")
(setq mu4e-mu-binary "/usr/local/bin/mu")
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc uni"
      ;mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-in-new-frame t
      mu4e-sent-messages-behavior 'sent
      message-kill-buffer-on-exit t
      mu4e-view-show-addresses t
      mu4e-headers-skip-duplicates t
;      mu4e-use-fancy-chars t
      )
(setq mu4e-headers-fields
      '( (:date          .  18)
	 (:flags         .   4)
	 (:from          .  22)
	 (:subject       .  nil)))
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-show-images t)
(setq mu4e-view-show-addresses t)
(setq mu4e-attachment-dir "~/attachments")
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


(add-hook 'mu4e-compose-mode-hook
  (defun my-add-bcc ()
    "Add a Bcc: header."
    (save-excursion (message-add-header "Bcc: zab@l3s.de\n"))))

(setq gnus-select-method
      '(nnimap "L3S"
	       (nnimap-address "mail.l3s.uni-hannover.de")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(defun gnus-user-format-function-@ (header)
  "Display @ for message with attachment in summary line.
You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        indicator)
    (when (string-match "^multipart/mixed" ctype)
      (setq indicator "@"))
    (if indicator
        indicator
      " ")))

(defun sdl-gnus-summary-line-format-unicode nil
  (interactive)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}" "%10{│%}" "%1{%&user-date;%}" "%10{│%}"
         "%9{%u&@;%}" "%(%-20,20f %)" "%10{│%}" "%4k" "%10{│%}"
         "%10{%B%}" "%s\n"))
  (setq
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-false-root      "  "
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─>"
   gnus-sum-thread-tree-single-leaf     "└─>"
   gnus-sum-thread-tree-indent          "  ")
  )

(defun oxy-unicode-threads-heavy ()
  (interactive)
  (setq gnus-summary-line-format "%10{%4k│%}%0{%U%R%z%u&@;%}%10{│%}%*%-23,23f%10{║%} %10{%B%} %(%s%)\n"
        gnus-summary-dummy-line-format "    %8{│%}    %(%8{│%}                       %10{║%}%) %10{┏○%}  %S\n"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "┏● "
        gnus-sum-thread-tree-false-root "　○ "
        gnus-sum-thread-tree-single-indent "　● "
        gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
        gnus-sum-thread-tree-vertical "┃"
        gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(setq gnus-user-date-format-alist
      '((t . "%Y-%m-%d %T")))

(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(setq gnus-face-9 'font-lock-warning-face)
(setq gnus-face-10 'shadow)

(sdl-gnus-summary-line-format-unicode)

(setq gnus-summary-display-arrow t)
(setq gnus-article-sort-functions '(gnus-article-sort-by-number gnus-article-sort-by-score))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number gnus-thread-sort-by-most-recent-date))
(setq gnus-build-sparse-threads 'more)
(setq gnus-summary-display-while-building nil)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-fetch-old-headers 'some)

(setq gnus-summary-gather-subject-limit 'fuzzy)
(setq gnus-simplify-subject-functions '(gnus-simplify-subject-re
                                        gnus-simplify-subject-fuzzy
                                        gnus-simplify-whitespace))
(setq gnus-simplify-ignored-prefixes "\\(Re?\\|AW\\): *")
(setq gnus-list-identifiers "\\[go-nuts\\] *\\|\\[golang-dev\\] *")
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-references)

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "<f7>")   'fd-switch-dictionary)

(load-theme 'dracula)
