(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(package-initialize)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq pkgs '("anzu" "cl-lib" "color-theme" "ctable" "dash" "deferred" "el-get" "epc" "epl" "f" "flycheck" "flyspell" "fuzzy" "git-modes" "jedi" "magit" "pkg-info" "popup" "python-environment" "rainbow-delimiters" "rich-minority" "s" "smart-mode-line" "smex" "solarized-emacs" "writegood" "sbt-mode" "scala-mode2" "projectile" "pymacs" "direx" "yasnippet" "yasnippets" "rainbow-delimiters" "ensime" "company-mode" "ag" ""pandoc-mode" "reftex" "markdown-mode" "flx" "git-gutter" "project-explorer" "smartparens"))
(el-get 'sync pkgs)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
            ido-use-virtual-buffers t)

(require 'flx-ido)
(flx-ido-mode 1)

;(setq ido-use-faces nil)

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Type:
;;     M-x el-get-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
;; no backup files
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Always use "y" for "yes"
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)

(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

(add-hook 'prog-mode-hook 'git-gutter-mode)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" "6e25454c105f795282f543f738dc7c8fbe06cfe5852546a9d42908553b83460d" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "7a00b0710bb2e400d33a925f94b1cd8cfa2281f864ac9506b9046703e0045d66" "862d97751547ab8f90613b30e87a6f8772401018ce77427eb6d9a4e0fa4ca7ee" "c7471ce3bb42defac344b3ecfca74228731b5ab20f804fd1deb8e65dddeab26a" "bd115791a5ac6058164193164fd1245ac9dc97207783eae036f0bfc9ad9670e0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

(global-anzu-mode +1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/monokai-emacs/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai)

(require 'rainbow-delimiters)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

(require 'smart-mode-line)
(sml/setup)
(setq column-number-mode t)

;; Don't clutter the emacs screen
(tool-bar-mode -1)
;(menu-bar-mode -1)

;(require 'whitespace-mode)
;(add-hook 'prog-mode-hook 'whitespace-mode)
;(setq whitespace-style (quote (trailing-whitespace trailing)))
(setq whitespace-line-column 80)
(setq load-prefer-newer t)
(add-hook 'prog-mode-hook (lambda()
    (setq show-trailing-whitespace t)))

;(require 'mu4e)

(setq mu4e-maildir "/home/gothos/Maildir")
(setq mu4e-sent-folder   "/.Sent"
      mu4e-drafts-folder "/.Drafts"
      mu4e-trash-folder  "/.Trash")
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 600
      mu4e-headers-auto-update t
      user-mail-address "jan@jhz.name"
      user-full-name "Jan-Hendrik Zab"
      mu4e-html2text-command "/usr/bin/html2text"
      )

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(if (not server-mode)
    (server-start nil t))

(require 'scala-mode2)

(add-to-list 'load-path (concat user-emacs-directory "el-get/ensime"))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'yasnippet)
(yas-global-mode 1)

; conifgure how to split screen
;(setq split-height-threshold nil)
;(setq split-width-threshold 0)

(add-hook 'after-init-hook 'global-company-mode)

; projectile
(add-hook 'prog-mode-hook 'projectile-mode)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-x f") 'projectile-find-file)

; use ibuffer to switch buffers, much nicer!
(global-set-key (kbd "C-x b") 'ibuffer-other-window)
(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)

;; sort buffers by name
(setq-default ibuffer-default-sorting-mode 'alphabetic)

;; hide empty filter groups
(setq ibuffer-show-empty-filter-groups nil)

;; show human readable sizes in dired
(setq dired-listing-switches "-alh")

; pandoc stuff
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(setq reftex-default-bibliography '("/home/gothos/src/reftex.bib"))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;Flyspell considers that a word repeated twice is an error
(setq flyspell-doublon-as-error-flag nil)

(add-hook 'markdown-mode-hook
	  (lambda ()
	    ;; enable flyspell
	    (flyspell-mode 1)))

(setq scroll-step 1)
;(scroll-bar-mode -1)

(provide '.emacs)
;;; .emacs ends here

