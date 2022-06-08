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

(straight-use-package 'use-package)

(use-package straight
  :init
  (setq straight-use-package-by-default t))

(use-package bind-key)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emacs

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      completion-ignore-case t
      read-process-output-max (* 1024 1024)
      use-short-answers t)

(server-mode +1)
(savehist-mode +1)
(save-place-mode +1)
(recentf-mode +1)
(show-paren-mode +1)
(global-auto-revert-mode +1)
(global-hl-line-mode +1)
(global-display-line-numbers-mode +1)
(which-function-mode +1)
(electric-pair-mode +1)
(pixel-scroll-mode +1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #character-code

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-file-name-coding-system 'cp932)
  (set-keyboard-coding-system 'cp932)
  (set-terminal-coding-system 'cp932)
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
			      'cp932))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #font

(cond ((eq system-type 'windows-nt)
       (add-to-list 'default-frame-alist '(font . "ＭＳ ゴシック-10")))

      ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(font . "VLゴシック 9"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #whitespace

(setq whitespace-style '(face trailing))

(global-whitespace-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ime

(use-package tr-ime
  :if (eq system-type 'windows-nt)
  :init
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize))

(use-package mozc
  :if (eq system-type 'gnu/linux)
  :init
  (setq default-input-method "japanese-mozc"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #my-keymap

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(defvar my-quit-map (make-sparse-keymap)
  "My quit keymap.")

(defvar my-file-map (make-sparse-keymap)
  "My file keymap.")

(defvar my-buffer-map (make-sparse-keymap)
  "My buffer keymap.")

(defvar my-error-map (make-sparse-keymap)
  "My error keymap.")

(defvar my-org-map (make-sparse-keymap)
  "My error keymap.")

(define-key my-quit-map (kbd "q") 'save-buffers-kill-terminal)
(define-key my-file-map (kbd "f") 'find-file)
(define-key my-file-map (kbd "b") 'bookmark-jump)
(define-key my-buffer-map (kbd "b") 'switch-to-buffer)
(define-key my-buffer-map (kbd "p") 'project-switch-to-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #evil

(use-package evil
  :bind (:map my-buffer-map
	      ("d" . evil-delete-buffer))
  :init
  (setq evil-want-keybinding nil
	evil-symbol-word-search t)
  (evil-mode +1)

  :config
  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     ;; NOTE: This requires an evil version from 2018-03-20 or later
     (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
     state))

  (evil-define-key '(normal visual) my-intercept-mode-map
    (kbd "SPC SPC") 'execute-extended-command
    (kbd "SPC s") `("search" . ,search-map)
    (kbd "SPC g") `("goto" . ,goto-map)
    (kbd "SPC q") `("quit" . ,my-quit-map)
    (kbd "SPC f") `("file" . ,my-file-map)
    (kbd "SPC b") `("buffer" . ,my-buffer-map)
    (kbd "SPC e") `("error" . ,my-error-map)
    (kbd "SPC o") `("org" . ,my-org-map)
    (kbd "SPC 0") 'delete-window
    (kbd "SPC 1") 'delete-other-windows
    (kbd "SPC 2") 'split-window-below
    (kbd "SPC 3") 'split-window-right))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode +1))

(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode +1))

(use-package evil-matchit
  :after evil
  :init
  (global-evil-matchit-mode +1))

(use-package evil-org
  :after evil
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #company

(use-package company
  :bind (([remap indent-for-tab-command] . company-indent-or-complete-common)
	 ([remap c-indent-line-or-region] . company-indent-or-complete-common))
  :init
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-dabbrev-ignore-case nil
	company-dabbrev-other-buffers nil
	company-dabbrev-downcase nil
	company-require-match 'never
	company-async-redisplay-delay 0.1
	company-auto-complete nil
	company-box-doc-enable nil)

  (global-company-mode +1))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-tabnine
  :after company
  :init
  (add-to-list 'company-backends '(company-capf :separate company-yasnippet company-tabnine)))

(use-package company-dwim
  :straight (company-dwim :type git :host github :repo "zk-phi/company-dwim")
  :bind (:map company-active-map
	      ("TAB" . company-dwim)
	      ("<tab>" . company-dwim)
	      ("S-TAB" . company-dwim-select-previous)
	      ("<backtab>" . company-dwim-select-previous)
	      ("C-j" . company-complete-selection)
	      ("RET" . nil)
	      ("<return>" . nil))
  :config
  (setq company-frontends (remq 'company-preview-if-just-one-frontend company-frontends))
  (add-to-list 'company-frontends 'company-dwim-frontend))

(use-package company-anywhere
  :straight (company-anywhere :type git :host github :repo "zk-phi/company-anywhere"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #selectrum

(use-package selectrum
  :bind ("C-x C-z" . selectrum-repeat)
  :init
  (evil-define-key '(normal visual) my-intercept-mode-map
    (kbd "SPC z") 'selectrum-repeat)

  (selectrum-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #prescient

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1))

(use-package company-prescient
  :init
  (setq company-prescient-sort-length-enable nil)
  (company-prescient-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ctrlf

(use-package ctrlf
  :bind (:map search-map
	      ("s" . ctrlf-forward-default))
  :init
  (setq ctrlf-default-search-style 'fuzzy)
  (ctrlf-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #marginalia

(use-package marginalia
  :init
  (marginalia-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #consutl

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
	 ([remap switch-to-buffer] . consult-buffer) ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
	 ([remap bookmark-jump] . consult-bookmark) ;; orig. bookmark-jump
	 ([remap project-switch-to-buffer] . consult-project-buffer) ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop) ;; orig. yank-pop
	 ("<help> a" . consult-apropos) ;; orig. apropos-command
	 :map my-buffer-map
	 ("4" . consult-buffer-other-window)
	 ("5" . consult-buffer-other-frame)
	 ;; M-g bindings (goto-map)
	 :map goto-map
	 ("e" . consult-compile-error)
	 ("g" . consult-goto-line) ;; orig. goto-line
	 ("o" . consult-outline) ;; Alternative: consult-org-heading
	 ("m" . consult-mark)
	 ("k" . consult-global-mark)
	 ("i" . consult-imenu)
	 ("I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 :map search-map
	 ("d" . consult-find)
	 ("D" . consult-locate)
	 ("g" . consult-grep)
	 ("G" . consult-git-grep)
	 ("r" . consult-ripgrep)
	 ("l" . consult-line)
	 ("L" . consult-line-multi)
	 ("m" . consult-multi-occur)
	 ("k" . consult-keep-lines)
	 ("u" . consult-focus-lines)
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history) ;; orig. next-matching-history-element
	 ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emabrk

(use-package embark
  :bind (("C-." . embark-act) ;; pick some comfortable binding
	 ("C-;" . embark-dwim) ;; good alternative: M-.
	 ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #yasnippet

(use-package yasnippet
  :bind (:map yas-minor-mode-map
	      ("TAB" . nil)
	      ("<tab>" . nil))
  :init
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #git

(use-package magit)

(use-package git-gutter
  :init
  (global-git-gutter-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #which-key

(use-package which-key
  :init
  (which-key-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #projectile

(use-package projectile
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal my-intercept-mode-map
      (kbd "SPC p") `("projectile" . ,projectile-command-map)))

  (projectile-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #flycheck

(use-package flycheck
  :bind (:map my-error-map
	      ("l" . flycheck-list-errors)
	      ("n" . flycheck-next-error)
	      ("p" . flycheck-previous-error))
  :init
  (setq flycheck-idle-change-delay 4.0)
  (global-flycheck-mode +1))

(use-package consult-flycheck
  :after flycheck consult
  :bind ((:map goto-map
	       ("f" . consult-flycheck))
	 (:map my-error-map
	       ("e" . consult-flycheck))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #undo-tree

(use-package undo-tree
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-tree)
    (evil-define-key 'normal my-intercept-mode-map
      (kbd "SPC u") 'undo-tree-visualize))

  (global-undo-tree-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rg

(use-package rg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rainbow-delimiters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #highlight-indent-guides

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character 124
	highlight-indent-guides-responsive 'top))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #theme

(use-package zerodark-theme
  :init
  (load-theme 'zerodark t))

(defun disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #modeline

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :init
  (minions-mode +1))

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (nyan-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #restart-emacs

(use-package restart-emacs
  :bind (:map my-quit-map
	      ("r" . restart-emacs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #all-the-icons

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(use-package expand-region
  :bind ("C-=" . er/expand-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #format

(use-package apheleia
  :hook ((rust-mode . apheleia-mode)
	 (python-mode . apheleia-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #beacon

(use-package beacon
  :init
  (beacon-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #gcmh

(use-package gcmh
  :init
  (setq garbage-collection-messages t)
  (gcmh-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #dashboard

(use-package dashboard
  :init
  (setq dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-navigator t
	dashboard-set-init-info t)

  (dashboard-setup-startup-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #volatile-highlights

(use-package volatile-highlights
  :init
  (volatile-highlights-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #unicode-fonts

(use-package unicode-fonts
  :init
  (unicode-fonts-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #org

(setq org-tag-alist '(("@Sample1" . nil)
		      ("@Test" . nil))
      org-directory "~/org/"
      org-default-notes-file (concat org-directory "/notes.org")
      org-capture-templates '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
			       "* TODO %?\n  %i\n  %a")
			      ("j" "Journal" entry (file+datetree "~/org/journal.org")
			       "* %?\nEntered on %U\n  %i\n  %a"))
      org-agenda-files '("~/org/notes.org"
			 "~/org/journal.org"))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key my-org-map (kbd "c") 'org-capture)
(define-key my-org-map (kbd "a") 'org-agenda)
(define-key my-org-map (kbd "o") 'org-open-at-point)
(define-key my-org-map (kbd "l") 'org-link)

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-pomodoro
  :bind (:map my-org-map
	      ("p" . org-pomodoro)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #alert

(use-package alert
  :init
  (cond ((eq system-type 'windows-nt)
	 (setq alert-default-style 'toast))

	((eq system-type 'gnu/linux)
	 (setq alert-default-style 'libnotify))))

(use-package alert-toast
  :if (eq system-type 'windows-nt)
  :after alert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #valign

(use-package valign
  :hook ((org-mode . valign-mode)
	 (markdown-mode . valign-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #fussy

(use-package fussy
  :init
  (setq completion-styles '(fussy)
	completion-category-defaults nil
	compleiton-category-overrides nil
	fussy-filter-fn 'fussy-filter-fast
	fussy-fast-regex-fn 'fussy-pattern-flex-rx))

(use-package sublime-fuzzy
  :straight (sublime-fuzzy :repo "jcs-elpa/sublime-fuzzy" :fetcher github :files (:defaults "bin"))
  :config
  (setq fussy-score-fn 'fussy-sublime-fuzzy-score)
  (sublime-fuzzy-load-dyn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #diff-hl

(use-package diff-hl
  :init
  (global-diff-hl-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lsp-mode

(use-package lsp-mode
  :hook ((lsp-mode . (lambda ()
		       (with-eval-after-load 'evil
			 (evil-local-set-key 'normal (kbd "SPC m") `("lsp" . ,lsp-command-map)))))
	 (web-mode . lsp)
	 (css-mode . lsp)
	 (rust-mode . lsp)
	 (java-mode . lsp)
	 (python-mode . lsp))
  :init
  (setq lsp-keymap-prefix "M-l"
	lsp-eldoc-enable-hover nil
	lsp-enable-folding nil
	lsp-headerline-breadcrumb-enable nil
	lsp-headerline-breadcrumb-enable-diagnostics nil
	lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-java
  :after lsp-mode java-mode)

(use-package lsp-pyright
  :after lsp-mode python-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #java

(add-hook 'java-mode-hook (lambda ()
			    (setq-local tab-width 2
					c-basic-offset 2
					indent-tabs-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #web

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.jsp\\'" . web-mode))
  :hook (web-mode . (lambda ()
		      (setq-local tab-width 2))))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
	 (css-mode . emmet-mode)))

(use-package css-mode
  :hook (css-mode . (lambda ()
		      (setq-local tab-width 2))))

(use-package web-beautify)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rust

(use-package rust-mode
  :hook (rust-mode . (lambda ()
		       (setq-local tab-width 4
				   indent-tabs-mode nil))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #python

(use-package pyvenv)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #common-lisp

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package slime-company
  :after slime
  :init
  (slime-setup '(slime-fancy slime-company slime-banner)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #sql

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlformat)

(provide 'init)
;;; init.el ends here

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
