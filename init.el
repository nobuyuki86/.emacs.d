;;; package --- Summary
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ime

(straight-use-package 'tr-ime)
(straight-use-package 'mozc)

(cond ((eq system-type 'windows-nt)
       (setq default-input-method "W32-IME")
       (tr-ime-standard-install)
       (w32-ime-initialize))

      ((eq system-type 'gnu/linux)
       (setq default-input-method "japanese-mozc")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #company

(straight-use-package 'company)
(straight-use-package 'company-box)

(setq company-idle-delay 0
      company-minimum-prefix-length 1)

(with-eval-after-load 'company
  (global-set-key [remap indent-for-tab-command] #'company-indent-or-complete-common)
  (global-set-key [remap c-indent-line-or-region] #'company-indent-or-complete-common)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") nil)
  (define-key company-active-map (kbd "C-p") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (add-to-list 'company-backends '(company-capf :separate company-yasnippet company-dabbrev)))

(add-hook 'company-mode-hook 'company-box-mode)

(global-company-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #selectrum

(straight-use-package 'selectrum)

(selectrum-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #prescient

(straight-use-package 'prescient)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'company-prescient)

(with-eval-after-load 'prescient
  (prescient-persist-mode +1))

(add-hook 'lsp-mode-hook (lambda ()
			   (setq-local company-prescient-sort-length-enable nil)))

(selectrum-prescient-mode +1)
(company-prescient-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #yasnippet

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

(yas-global-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #git

(straight-use-package 'magit)
(straight-use-package 'git-gutter)

(global-git-gutter-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #which-key

(straight-use-package 'which-key)

(which-key-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #projectile

(straight-use-package 'projectile)

(global-set-key (kbd "C-c p") 'projectile-command-map)

(projectile-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #flycheck

(straight-use-package 'flycheck)

(global-flycheck-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #undo-tree

(straight-use-package 'undo-tree)

(setq undo-tree-auto-save-history nil)

(global-undo-tree-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rg

(straight-use-package 'rg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #theme

(straight-use-package 'zerodark-theme)

(load-theme 'zerodark t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #modeline

(straight-use-package 'doom-modeline)
(straight-use-package 'nyan-mode)

(setq nyan-animate-nyancat t)

(doom-modeline-mode +1)
(nyan-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #restart-emacs

(straight-use-package 'restart-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #all-the-icons

(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rainbow-delimiters

(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #highlight-indent

(straight-use-package 'highlight-indent-guides)

(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-responsive 'top
      highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(straight-use-package 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #smartparens

(straight-use-package 'smartparens)

(with-eval-after-load 'smartparens
  (require 'smartparens-config)

  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(smartparens-global-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lsp-mode

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-java)
(straight-use-package 'lsp-pyright)

(setq lsp-keymap-prefix "M-l")

(add-hook 'html-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'java-mode-hook (lambda ()
			    (require 'lsp-java)
			    (lsp)))
(add-hook 'python-mode-hook (lambda ()
			      (require 'lsp-pyright)
			      (lsp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #java

(add-hook 'java-mode-hook (lambda ()
			    (setq-local tab-width 2
					c-basic-offset 2
					indent-tabs-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #web

(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))

(add-hook 'web-mode-hook (lambda ()
			   (setq-local tab-width 2)
			   (emmet-mode +1)))

(add-hook 'css-mode-hook (lambda ()
			   (setq-local tab-width 2)
			   (emmet-mode +1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rust

(straight-use-package 'rust-mode)
(straight-use-package 'cargo)

(add-hook 'rust-mode-hook (lambda ()
			    (setq-local tab-width 4
					indent-tabs-mode nil)
			    (cargo-minor-mode +1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #common-lisp

(straight-use-package 'slime)
(straight-use-package 'slime-company)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #sql

(straight-use-package 'sql-indent)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emacs

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      completion-ignore-case t
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      use-short-answers t)

(server-mode +1)
(savehist-mode +1)
(save-place-mode +1)
(recentf-mode +1)
(show-paren-mode +1)
(global-auto-revert-mode +1)
(global-hl-line-mode +1)
(which-function-mode +1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'init)
;;; init.el ends here

