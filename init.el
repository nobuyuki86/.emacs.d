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
       (add-to-list 'default-frame-alist '(font . "ＭＳゴシック 12")))

      ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(font . "VLゴシック 9"))))


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
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-dabbrev-downcase nil
	company-require-match 'never)
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

(ctrlf-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #marginalia

(straight-use-package 'marginalia)

(marginalia-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #consult

(straight-use-package 'consult)

;; C-c bindings (mode-specific-map)
(global-set-key (kbd "C-c h") #'consult-history)
(global-set-key (kbd "C-c m") #'consult-mode-command)
(global-set-key (kbd "C-c k") #'consult-kmacro)

;; C-x bindings (ctl-x-map)
(global-set-key (kbd "C-x M-:") #'consult-complex-command)     ;; orig. repeat-complex-command
(global-set-key (kbd "C-x b") #'consult-buffer)                ;; orig. switch-to-buffer
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
(global-set-key (kbd "C-x r b") #'consult-bookmark)            ;; orig. bookmark-jump
(global-set-key (kbd "C-x p b") #'consult-project-buffer)      ;; orig. project-switch-to-buffer
;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") #'consult-register)
;; Other custom bindings
(global-set-key (kbd "M-y") #'consult-yank-pop)                ;; orig. yank-pop
(global-set-key (kbd "<help> a") #'consult-apropos)            ;; orig. apropos-command
;; M-g bindings (goto-map)
(global-set-key (kbd "M-g e") #'consult-compile-error)
;; (global-set-key (kbd "M-g f") #'consult-flymake)               ;; Alternative: consult-flycheck
(global-set-key (kbd "M-g g") #'consult-goto-line)             ;; orig. goto-line
(global-set-key (kbd "M-g M-g") #'consult-goto-line)           ;; orig. goto-line
(global-set-key (kbd "M-g o") #'consult-outline)               ;; Alternative: consult-org-heading
(global-set-key (kbd "M-g m") #'consult-mark)
(global-set-key (kbd "M-g k") #'consult-global-mark)
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-imenu-multi)
;; M-s bindings (search-map)
(global-set-key (kbd "M-s d") #'consult-find)
(global-set-key (kbd "M-s D") #'consult-locate)
(global-set-key (kbd "M-s g") #'consult-grep)
(global-set-key (kbd "M-s G") #'consult-git-grep)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s L") #'consult-line-multi)
(global-set-key (kbd "M-s m") #'consult-multi-occur)
(global-set-key (kbd "M-s k") #'consult-keep-lines)
(global-set-key (kbd "M-s u") #'consult-focus-lines)
;; Isearch integration
(global-set-key (kbd "M-s e") #'consult-isearch-history)
(define-key isearch-mode-map (kbd "M-e") #'consult-isearch-history)         ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch-history)       ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)                  ;; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L") #'consult-line-multi)            ;; needed by consult-line to detect isearch
;; Minibuffer history
(define-key minibuffer-local-map (kbd "M-s") #'consult-history)                 ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") #'consult-history)                ;; orig. previous-matching-history-element

(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(with-eval-after-load 'consult
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emabrk

(straight-use-package 'embark)

(global-set-key (kbd "C-.") #'embark-act)


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
(straight-use-package 'consult-flycheck)

(global-set-key (kbd "M-g f") #'consult-flycheck)

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

(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-responsive 'top
      highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)

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
;; #smartparens

(straight-use-package 'smartparens)

(with-eval-after-load 'smartparens
  (require 'smartparens-config)

  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET"))))

(smartparens-global-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(straight-use-package 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #format

(straight-use-package 'apheleia)
(straight-use-package 'sqlformat)

(add-hook 'rust-mode-hook 'apheleia-mode)
(add-hook 'python-mode-hook 'apheleia-mode)
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #orderless

(straight-use-package 'orderless)

(defun my/orderless-dispatch-flex-first (_pattern index _total)
  (and (eq index 0) 'orderless-flex))

(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(setq completion-styles '(basic orderless)
      completion-category-defaults nil
      completion-category-overrides nil
      orderless-style-dispatchers '(my/orderless-dispatch-flex-first))

(with-eval-after-load 'company
  (advice-add 'company-capf--candidates :around #'just-one-face))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #anzu

(straight-use-package 'anzu)

(global-anzu-mode +1)

(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)


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
;; #python

(straight-use-package 'pyvenv)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #common-lisp

(straight-use-package 'slime)
(straight-use-package 'slime-company)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #sql

(straight-use-package 'sql-indent)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #org

(straight-use-package 'org-modern)

(add-hook 'org-mode-hook 'org-modern-mode)

(provide 'init)
;;; init.el ends here

