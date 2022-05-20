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
;; #font

(cond ((eq system-type 'windows-nt)
       (add-to-list 'default-frame-alist '(font . "Consolas 10")))

      ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(font . "VLゴシック 9"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emacs

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      completion-ignore-case t
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      use-short-answers t)

(add-to-list 'completion-styles 'flex t)

(server-mode +1)
(savehist-mode +1)
(save-place-mode +1)
(recentf-mode +1)
(show-paren-mode +1)
(global-auto-revert-mode +1)
(global-hl-line-mode +1)
(which-function-mode +1)
(electric-pair-mode +1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(when (eq system-type 'windows-nt)
  (set-file-name-coding-system 'cp932)
  (set-keyboard-coding-system 'cp932)
  (set-terminal-coding-system 'cp932))

(set-charset-priority 'ascii
		      'japanese-jisx0208
		      'latin-jisx0201
		      'katakana-jisx0201
		      'iso-8859-1
		      'cp1252
		      'unicode)
(set-coding-system-priority 'utf-8
			    'euc-jp
			    'iso-2022-jp
			    'cp932)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #whitespace

(setq whitespace-style '(face
			 trailing))

(global-whitespace-mode +1)


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
(straight-use-package 'company-tabnine)

(with-eval-after-load 'company
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1
	company-dabbrev-ignore-case nil
	company-dabbrev-other-buffers nil
	company-dabbrev-downcase nil
	company-require-match 'never
	company-async-redisplay-delay 0.1
	company-box-doc-enable nil)
  
  (global-set-key [remap indent-for-tab-command] #'company-indent-or-complete-common)
  (global-set-key [remap c-indent-line-or-region] #'company-indent-or-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") nil)
  (define-key company-active-map (kbd "C-p") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (add-to-list 'company-backends '(company-capf :separate company-yasnippet company-tabnine)))

(add-hook 'company-mode-hook 'company-box-mode)

(global-company-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #selectrum

(straight-use-package 'selectrum)

(global-set-key (kbd "C-x C-z") #'selectrum-repeat)

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
;; #ctrlf

(straight-use-package 'ctrlf)

(setq ctrlf-default-search-style 'fuzzy)

(ctrlf-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #yasnippet

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

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
;; #rainbow-delimiters

(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #goto-last-change

(straight-use-package 'goto-last-change)

(global-set-key (kbd "M-g c") 'goto-last-change)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #unicode-fonts

(straight-use-package 'unicode-fonts)

(unicode-fonts-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #highlight-indent-guides

(straight-use-package 'highlight-indent-guides)

(setq highlight-indent-guides-method 'character
      highlight-indent-guides-character 124
      highlight-indent-guides-responsive 'top)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #theme

(straight-use-package 'zerodark-theme)

(load-theme 'zerodark t)

(zerodark-setup-modeline-format)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #modeline

(straight-use-package 'minions)

(minions-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #restart-emacs

(straight-use-package 'restart-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #all-the-icons

(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)

(when (display-graphic-p)
  (require 'all-the-icons))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(straight-use-package 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #format

(straight-use-package 'apheleia)

(add-hook 'rust-mode-hook 'apheleia-mode)
(add-hook 'python-mode-hook 'apheleia-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #anzu

(straight-use-package 'anzu)

(global-anzu-mode +1)

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #beacon

(straight-use-package 'beacon)

(setq beacon-color "yellow")

(beacon-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #imenu-list

(straight-use-package 'imenu-list)

(setq imenu-list-focus-after-activation t
      imenu-list-auto-resize t)

(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #gcmh

(straight-use-package 'gcmh)

(setq garbage-collection-messages t)

(gcmh-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #dashboard

(straight-use-package 'dashboard)

(setq dashboard-center-content t
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-set-navigator t
      dashboard-set-init-info t)

(dashboard-setup-startup-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #volatile-highlights

(straight-use-package 'volatile-highlights)

(volatile-highlights-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ace-window

(straight-use-package 'ace-window)

(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 4.0))

(global-set-key (kbd "M-o") 'ace-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #org

;; org-capture
(setq org-directory "~/.emacs.d/org/"
      org-default-notes-file (concat org-directory "/notes.org"))

(global-set-key (kbd "C-c c") 'org-capture)

;; org-agenda
(setq org-agenda-files '("~/.emacs.d/org/notes.org"))

(global-set-key (kbd "C-c a") 'org-agenda)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lsp-mode

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-java)
(straight-use-package 'lsp-pyright)

(setq lsp-keymap-prefix "M-l"
      lsp-eldoc-enable-hover nil
      lsp-enable-folding nil
      lsp-enable-symbol-highlighting nil
      lsp-headerline-breadcrumb-enable nil
      lsp-headerline-breadcrumb-enable-diagnostics nil
      lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))

(add-hook 'web-mode-hook #'lsp)
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
;; #python

(straight-use-package 'pyvenv)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #common-lisp

(straight-use-package 'slime)
(straight-use-package 'slime-company)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #sql

(straight-use-package 'sql-indent)
(straight-use-package 'sqlformat)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)

(provide 'init)
;;; init.el ends here

(put 'upcase-region 'disabled nil)
