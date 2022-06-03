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

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

(leaf leaf-keywords
  :require t
  :init
  (leaf-keywords-init))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emacs

(leaf emacs
  :init
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
  (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #character-code

(leaf character-code
  :init
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8))

(leaf character-code-for-windows
  :if (eq system-type 'windows-nt)
  :init
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

(leaf font-for-windows
  :if (eq system-type 'windows-nt)
  :init
  (add-to-list 'default-frame-alist '(font . "ＭＳ ゴシック-10")))

(leaf font-for-linux
  :if (eq system-type 'gnu/linux)
  :init
  (add-to-list 'default-frame-alist '(font . "VLゴシック 9")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #whitespace

(leaf whitespace
  :global-minor-mode global-whitespace-mode
  :init
  (setq whitespace-style '(face trailing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ime

(leaf tr-ime
  :if (eq system-type 'windows-nt)
  :straight t
  :require t
  :init
  (setq default-input-method "W32-IME")
  (tr-ime-standard-install)
  (w32-ime-initialize))

(leaf mozc
  :if (eq system-type 'gnu/linux)
  :straight t
  :require t
  :init
  (setq default-input-method "japanese-mozc"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #evil

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

(leaf evil
  :straight t
  :require t
  :global-minor-mode t
  :bind ((:my-quit-map
	  ("q" . save-buffers-kill-terminal))
	 (:my-file-map
	  ("f" . find-file)
	  ("b" . bookmark-jump))
	 (:my-buffer-map
	  ("b" . switch-to-buffer)
	  ("d" . evil-delete-buffer)
	  ("p" . project-switch-to-buffer)))
  :init
  (setq evil-want-keybinding nil
	evil-symbol-word-search  t)

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
    (kbd "SPC 0") 'delete-window
    (kbd "SPC 1") 'delete-other-windows
    (kbd "SPC 2") 'split-window-below
    (kbd "SPC 3") 'split-window-right))

(leaf evil-collection
  :straight t
  :require t
  :after evil
  :init
  (evil-collection-init))

(leaf evil-commentary
  :straight t
  :require t
  :after evil
  :global-minor-mode t)

(leaf evil-surround
  :straight t
  :require t
  :after evil
  :global-minor-mode global-evil-surround-mode)

(leaf evil-matchit
  :straight t
  :require t
  :after evil
  :global-minor-mode global-evil-matchit-mode)

(leaf evil-org
  :straight t
  :require t
  :after evil
  :hook (org-mode-hook . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #company

(leaf company
  :straight t
  :require t
  :bind (([remap indent-for-tab-command] . company-indent-or-complete-common)
	 ([remap c-indent-line-or-region] . company-indent-or-complete-common)
	 (:company-active-map
	  ("<tab>" . company-complete-selection)
	  ("TAB" . company-complete-selection)
	  ("RET" . nil)
	  ("<return>" . nil)))
  :global-minor-mode global-company-mode
  :init
  (setq company-idle-delay 0
	company-minimum-prefix-length 1
	company-dabbrev-ignore-case nil
	company-dabbrev-other-buffers nil
	company-dabbrev-downcase nil
	company-require-match 'never
	company-async-redisplay-delay 0.1
	company-auto-complete nil
	company-box-doc-enable nil))

(leaf company-box
  :straight t
  :require t
  :after company
  :hook (company-mode-hook . company-box-mode))

(leaf company-tabnine
  :straight t
  :require t
  :after company
  :init
  (add-to-list 'company-backends '(company-capf :separate company-yasnippet company-tabnine)))

(leaf company-dwim
  :straight '(company-dwim :type git :host github :repo "zk-phi/company-dwim")
  :require t
  :bind (:company-active-map
	 ("<tab>" . company-dwim)
	 ("TAB" . company-dwim))
  :init
  (setq company-frontends (delq 'company-preview-if-just-one-frontend company-frontends))
  (add-to-list 'company-frontends 'company-dwim-frontend))

(leaf company-anywhere
  :straight '(company-anywhere :type git :host github :repo "zk-phi/company-anywhere")
  :require t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #selectrum

(leaf selectrum
  :straight t
  :require t
  :bind ("C-x C-z" . selectrum-repeat)
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #prescient

(leaf prescient
  :straight t
  :require t
  :global-minor-mode prescient-persist-mode)

(leaf selectrum-prescient
  :straight t
  :require t
  :global-minor-mode t)

(leaf company-prescient
  :straight t
  :require t
  :global-minor-mode t
  :init
  (setq company-prescient-sort-length-enable nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ctrlf

(leaf ctrlf
  :straight t
  :require t
  :bind (:search-map
	 ("s" . ctrlf-forward-default))
  :global-minor-mode t
  :init
  (setq ctrlf-default-search-style 'fuzzy))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #marginalia

(leaf marginalia
  :straight t
  :require t
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #consutl

(leaf consult
  :straight t
  :require t
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
	 (:my-buffer-map
	  ("4" . consult-buffer-other-window)
	  ("5" . consult-buffer-other-frame))
	 ;; M-g bindings (goto-map)
	 (:goto-map
	  ("e" . consult-compile-error)
	  ("g" . consult-goto-line) ;; orig. goto-line
	  ("o" . consult-outline) ;; Alternative: consult-org-heading
	  ("m" . consult-mark)
	  ("k" . consult-global-mark)
	  ("i" . consult-imenu)
	  ("I" . consult-imenu-multi))
	 ;; M-s bindings (search-map)
	 (:search-map
	  ("d" . consult-find)
	  ("D" . consult-locate)
	  ("g" . consult-grep)
	  ("G" . consult-git-grep)
	  ("r" . consult-ripgrep)
	  ("l" . consult-line)
	  ("L" . consult-line-multi)
	  ("m" . consult-multi-occur)
	  ("k" . consult-keep-lines)
	  ("u" . consult-focus-lines))
	 ;; Minibuffer history
	 (:minibuffer-local-map
	  ("M-s" . consult-history) ;; orig. next-matching-history-element
	  ("M-r" . consult-history))) ;; orig. previous-matching-history-element
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format
	xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref
	consult-narrow-key "<")

  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize consult-theme consult-ripgrep consult-git-grep consult-grep
		     consult-bookmark consult-recent-file consult-xref
		     consult--source-bookmark consult--source-recent-file
		     consult--source-project-recent-file
		     :preview-key (kbd "M-.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emabrk

(leaf embark
  :straight t
  :require t
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

(leaf embark-consult
  :straight t
  :require t
  :after embark consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #yasnippet

(leaf yasnippet
  :straight t
  :require t
  :bind (:yas-minor-mode-map
	 ("TAB" . nil)
	 ("<tab>" . nil))
  :global-minor-mode yas-global-mode)

(leaf yasnippet-snippets
  :straight t
  :require t
  :after yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #git

(leaf magit
  :straight t
  :require t)

(leaf git-gutter
  :straight t
  :require t
  :global-minor-mode global-git-gutter-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #which-key

(leaf which-key
  :straight t
  :require t
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #projectile

(leaf projectile
  :straight t
  :require t
  :bind (:projectile-mode-map
	 ("C-c p" . projectile-command-map))
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #flycheck

(leaf flycheck
  :straight t
  :require t
  :global-minor-mode global-flycheck-mode
  :init
  (setq flycheck-idle-change-delay 4.0))

(leaf consult-flycheck
  :straight t
  :require t
  :after consult flycheck
  :bind (:goto-map
	 ("f" . consult-flycheck)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #undo-tree

(leaf undo-tree
  :straight t
  :require t
  :global-minor-mode global-undo-tree-mode
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-tree)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rg

(leaf rg
  :straight t
  :require t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rainbow-delimiters

(leaf rainbow-delimiters
  :straight t
  :require t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #highlight-indent-guides

(leaf highlight-indent-guides
  :straight t
  :require t
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character 124
	highlight-indent-guides-responsive 'top))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #theme

(leaf zerodark-theme
  :straight t
  :require t
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

(leaf moody
  :straight t
  :require t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(leaf minions
  :straight t
  :require t
  :global-minor-mode t)

(leaf nyan-mode
  :straight t
  :require t
  :global-minor-mode t
  :init
  (setq nyan-animate-nyancat t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #restart-emacs

(leaf restart-emacs
  :straight t
  :require t
  :bind (:my-quit-map
	 ("r" . restart-emacs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #all-the-icons

(leaf all-the-icons
  :if (display-graphic-p)
  :straight t
  :require t)

(leaf all-the-icons-dired
  :straight t
  :require t
  :after all-the-icons
  :hook (dired-mode-hook . all-the-icons-dired-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(leaf expand-region
  :straight t
  :require t
  :bind ("C-=" . er/expand-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #format

(leaf apheleia
  :straight t
  :require t
  :hook ((rust-mode-hook . apheleia-mode)
	 (python-mode-hook . apheleia-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #beacon

(leaf beacon
  :straight t
  :require t
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #gcmh

(leaf gcmh
  :straight t
  :require t
  :global-minor-mode t
  :init
  (setq garbage-collection-messages t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #dashboard

(leaf dashboard
  :straight t
  :require t
  :init
  (setq dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-navigator t
	dashboard-set-init-info t)

  (dashboard-setup-startup-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #volatile-highlights

(leaf volatile-highlights
  :straight t
  :require t
  :global-minor-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #unicode-fonts

(leaf unicode-fonts
  :straight t
  :require t
  :init
  (unicode-fonts-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #org

(leaf org
  :init
  (setq org-tab-alist '(("@Sample1" . nil)
			("@Test" . nil))))

(leaf org-capture
  :bind ("C-c c" . org-capture)
  :init
  (setq org-directory "~/org/"
	org-default-notes-file (concat org-directory "/notes.org")
	org-capture-templates '(("t" "Todo" entry (file+headline "~/org/notes.org" "Tasks")
				 "* TODO %?\n  %i\n  %a")
				("j" "Journal" entry (file+datetree "~/org/journal.org")
				 "* %?\nEntered on %U\n  %i\n  %a"))))

(leaf org-agenda
  :bind ("C-c a" . org-agenda)
  :init
  (setq org-agenda-files '("~/org/notes.org"
			   "~/org/journal.org")))

(leaf org-modern
  :straight t
  :require t
  :hook (org-mode-hook . org-modern-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #alert

(leaf alert
  :straight t
  :require t
  :init
  (cond ((eq system-type 'windows-nt)
	 (setq alert-default-style 'toast))

	((eq system-type 'gnu/linux)
	 (setq alert-default-style 'libnotify))))

(leaf alert-toast
  :if (eq system-type 'windows-nt)
  :straight t
  :require t
  :after alert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #org-pomodoro

(leaf org-pomodoro
  :straight t
  :require t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #valign

(leaf valign
  :straight t
  :require t
  :hook ((org-mode-hook . valign-mode)
	 (markdown-mode-hook . valign-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #fussy

(leaf fussy
  :straight '(fussy :type git :host github :repo "jojojames/fussy")
  :require t
  :init
  (setq completion-styles '(fussy)
	completion-category-defaults nil
	compleiton-category-overrides nil))

(leaf liquidmetal
  :straight t
  :require t
  :init
  (setq fussy-score-fn 'fussy-liquidmetal-score))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lsp-mode

(leaf lsp-mode
  :straight t
  :require t
  :hook ((lsp-mode-hook . (lambda ()
			    (with-eval-after-load 'evil
			      (evil-local-set-key 'normal (kbd "SPC m") `("lsp" . ,lsp-command-map)))))
	 (web-mode-hook . lsp)
	 (css-mode-hook . lsp)
	 (rust-mode-hook . lsp)
	 (java-mode-hook . lsp)
	 (python-mode-hook . lsp))
  :init
  (setq lsp-keymap-prefix "M-l"
	lsp-eldoc-enable-hover nil
	lsp-enable-folding nil
	lsp-headerline-breadcrumb-enable nil
	lsp-headerline-breadcrumb-enable-diagnostics nil
	lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

(leaf lsp-ui
  :straight t
  :require t
  :after lsp-mode)

(leaf lsp-java
  :straight t
  :require t
  :after lsp-mode java-mode)

(leaf lsp-pyright
  :straight t
  :require t
  :after lsp-mode python-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #java

(leaf java
  :hook (java-mode-hook . (lambda ()
			    (setq-local tab-width 2
					c-basic-offset 2
					indent-tabs-mode t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #web

(leaf web-mode
  :straight t
  :require t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.jsp\\'" . web-mode))
  :hook (web-mode-hook . (lambda ()
			   (setq-local tab-width 2))))

(leaf emmet-mode
  :straight t
  :require t
  :hook ((web-mode-hook . emmet-mode)
	 (css-mode-hook . emmet-mode)))

(leaf css-mode
  :hook (css-mode-hook . (lambda ()
			   (setq-local tab-width 2))))

(leaf web-beautify
  :straight t
  :require t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rust

(leaf rust-mode
  :straight t
  :require t
  :hook (rust-mode-hook . (lambda ()
			    (setq-local tab-width 4
					indent-tabs-mode nil))))

(leaf cargo
  :straight t
  :require t
  :hook (rust-mode-hook . cargo-minor-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #python

(leaf pyvenv
  :straight t
  :require t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #common-lisp

(leaf slime
  :straight t
  :require t
  :init
  (setq inferior-lisp-program "sbcl"))

(leaf slime-company
  :straight t
  :require t
  :after slime
  :init
  (slime-setup '(slime-fancy slime-company slime-banner)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #sql

(leaf sql-indent
  :straight t
  :require t
  :hook (sql-mode-hook . sqlind-minor-mode))

(leaf sqlformat
  :straight t
  :require t)

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
