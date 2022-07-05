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
;; #emacs

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      completion-ignore-case t
      read-process-output-max (* 1024 1024)
      use-short-answers t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(setq pgtk-wait-for-event-timeout 0.001)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

(server-mode +1)
(savehist-mode +1)
(save-place-mode +1)
(recentf-mode +1)
(show-paren-mode +1)
(electric-pair-mode +1)
(global-auto-revert-mode +1)
;; (global-hl-line-mode +1)
(global-display-line-numbers-mode +1)
(which-function-mode +1)
(pixel-scroll-mode +1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-ui

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode +1)

;; GUIs are inconsistent across systems and themes (and will rarely match our
;; active Emacs theme). They impose inconsistent shortcut key paradigms too.
;; It's best to avoid them altogether and have Emacs handle the prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips nil))

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


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

(straight-use-package 'fontaine)

(setq fontaine-presets
      '((regular
         :default-family "VLゴシック"
         :default-height 90
         :fixed-pitch-family "VLゴシック"
         :variable-pitch-family "VLPゴシック"
         :italic-family "VLゴシック"
         :line-spacing 1)
        (large
         :default-family "VLゴシック"
         :default-height 150
         :variable-pitch-family "VLPゴシック"
         :line-spacing 1)))

;; Recover last preset or fall back to desired style from
;; `fontaine-presets'.
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

;; (cond ((eq system-type 'windows-nt)
;;        (add-to-list 'default-frame-alist '(font . "ＭＳ ゴシック-10")))

;;       ((eq system-type 'gnu/linux)
;;        (add-to-list 'default-frame-alist '(font . "VLゴシック 9"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #whitespace

(setq whitespace-style '(face trailing))

(global-whitespace-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #modeline

(straight-use-package 'moody)
(straight-use-package 'minions)
(straight-use-package 'nyan-mode)

(setq x-underline-at-descent-line t
      nyan-animate-nyancat t
      nyan-bar-length 24)

(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(moody-replace-eldoc-minibuffer-message-function)
(minions-mode +1)
(nyan-mode +1)


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

(defvar my-toggle-map (make-sparse-keymap)
  "My toggle keymap.")

(defvar my-org-map (make-sparse-keymap)
  "My error keymap.")

(define-key my-quit-map (kbd "q") 'save-buffers-kill-terminal)
(define-key my-file-map (kbd "f") 'find-file)
(define-key my-file-map (kbd "b") 'bookmark-jump)
(define-key my-buffer-map (kbd "b") 'switch-to-buffer)
(define-key my-buffer-map (kbd "p") 'project-switch-to-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #evil

(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-commentary)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-matchit)
(straight-use-package 'evil-org)

(setq evil-want-keybinding nil
      evil-symbol-word-search t)

(define-key my-buffer-map (kbd "d") #'evil-delete-buffer)

(with-eval-after-load 'evil
  (evil-collection-init)
  (evil-commentary-mode +1)
  (global-evil-surround-mode +1)
  (global-evil-matchit-mode +1)

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
    (kbd "SPC t") `("toggle" . ,my-toggle-map)
    (kbd "SPC o") `("org" . ,my-org-map)
    (kbd "SPC 0") 'delete-window
    (kbd "SPC 1") 'delete-other-windows
    (kbd "SPC 2") 'split-window-below
    (kbd "SPC 3") 'split-window-right
    (kbd "SPC 4") 'switch-to-buffer-other-window
    (kbd "SPC 5") 'switch-to-buffer-other-frame
    (kbd "SPC w") 'evil-window-next))

(with-eval-after-load 'evil-org
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

(add-hook 'org-mode-hook 'evil-org-mode)

(evil-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #company

(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'company-tabnine)
(straight-use-package '(company-dwim :type git :host github :repo "zk-phi/company-dwim"))
(straight-use-package '(company-anywhere :type git :host github :repo "zk-phi/company-anywhere"))

(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-dabbrev-other-buffers nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-require-match 'never
      company-auto-complete nil)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)

  (add-to-list 'company-backends '(:separate company-capf company-yasnippet company-tabnine))

  (require 'company-box)
  (delq 'company-preview-if-just-one-frontend company-frontends)
  (delq 'company-echo-metadata-frontend company-frontends)
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

  (require 'company-dwim)
  (define-key company-active-map (kbd "TAB") #'company-dwim)
  (define-key company-active-map (kbd "<tab>") #'company-dwim)
  (define-key company-active-map (kbd "S-TAB") #'company-dwim-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-dwim-select-previous)
  (define-key company-active-map (kbd "C-j") #'company-complete-selection)
  (add-to-list 'company-frontends 'company-dwim-frontend t)

  (require 'company-anywhere))

(global-set-key [remap indent-for-tab-command] #'company-indent-or-complete-common)
(global-set-key [remap c-indent-line-or-region] #'company-indent-or-complete-common)

(add-hook 'company-mode-hook 'company-box-mode)

(global-company-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #selectrum

(straight-use-package 'selectrum)

(global-set-key (kbd "C-x C-z") #'selectrum-repeat)

(with-eval-after-load 'selectrum
  (with-eval-after-load 'evil
    (evil-define-key '(normal visual) my-intercept-mode-map
      (kbd "SPC z") 'selectrum-repeat)))

(selectrum-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #prescient

(straight-use-package 'prescient)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'company-prescient)

(setq company-prescient-sort-length-enable nil)

(with-eval-after-load 'prescient
  (prescient-persist-mode +1))

(selectrum-prescient-mode +1)

(company-prescient-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ctrlf

(straight-use-package 'ctrlf)

(setq ctrlf-default-search-style 'fuzzy)

(define-key search-map (kbd "s") #'ctrlf-forward-default)

(ctrlf-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #marginalia

(straight-use-package 'marginalia)

(marginalia-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #consutl

(straight-use-package 'consult)

(require 'consult)

;; C-c bindings (mode-specific-map)
(global-set-key (kbd "C-c h") #'consult-history)
(global-set-key (kbd "C-c m") #'consult-mode-command)
(global-set-key (kbd "C-c k") #'consult-kmacro)
;; C-x bindings (ctl-x-map)
(global-set-key (kbd "C-x M-:") #'consult-complex-command) ;; orig. repeat-complex-command
(global-set-key [remap switch-to-buffer] #'consult-buffer) ;; orig. switch-to-buffer
(global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
(global-set-key [remap bookmark-jump] #'consult-bookmark) ;; orig. bookmark-jump
(global-set-key [remap project-switch-to-buffer] #'consult-project-buffer) ;; orig. project-switch-to-buffer
;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") #'consult-register)
;; Other custom bindings
(global-set-key (kbd "M-y") #'consult-yank-pop) ;; orig. yank-pop
(global-set-key (kbd "<help> a") #'consult-apropos) ;; orig. apropos-command
(define-key my-buffer-map (kbd "4") #'consult-buffer-other-window)
(define-key my-buffer-map (kbd "5") #'consult-buffer-other-frame)
;; M-g bindings (goto-map)
(define-key goto-map (kbd "e") #'consult-compile-error)
(define-key goto-map (kbd "g") #'consult-goto-line) ;; orig. goto-line
(define-key goto-map (kbd "o") #'consult-outline) ;; Alternative: consult-org-heading
(define-key goto-map (kbd "m") #'consult-mark)
(define-key goto-map (kbd "k") #'consult-global-mark)
(define-key goto-map (kbd "i") #'consult-imenu)
(define-key goto-map (kbd "I") #'consult-imenu-multi)
;; M-s bindings (search-map)
(define-key search-map (kbd "d") #'consult-find)
(define-key search-map (kbd "D") #'consult-locate)
(define-key search-map (kbd "g") #'consult-grep)
(define-key search-map (kbd "G") #'consult-git-grep)
(define-key search-map (kbd "r") #'consult-ripgrep)
(define-key search-map (kbd "l") #'consult-line)
(define-key search-map (kbd "L") #'consult-line-multi)
(define-key search-map (kbd "m") #'consult-multi-occur)
(define-key search-map (kbd "k") #'consult-keep-lines)
(define-key search-map (kbd "u") #'consult-focus-lines)
;; Minibuffer history
(define-key minibuffer-local-map (kbd "M-s") #'consult-history) ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") #'consult-history) ;; orig. previous-matching-history-element

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode 'consult-preview-at-point-mode)

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
(with-eval-after-load 'consult
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
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
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emabrk

(straight-use-package 'embark)
(straight-use-package 'embark-consult)

(global-set-key (kbd "C-.") #'embark-act) ;; pick some comfortable binding
(global-set-key (kbd "C-;") #'embark-dwim) ;; good alternative: M-.
(global-set-key (kbd "C-h B") #'embark-bindings) ;; alternative for `describe-bindings'

(setq prefix-help-command #'embark-prefix-help-command)

(with-eval-after-load 'embark
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #yasnippet

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)

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

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

(with-eval-after-load 'evil
  (evil-define-key 'normal my-intercept-mode-map
    (kbd "SPC p") `("projectile" . projectile-command-map)))

(projectile-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #flycheck

(straight-use-package 'flycheck)
(straight-use-package 'consult-flycheck)
(straight-use-package 'flycheck-posframe)

(define-key my-error-map (kbd "l") #'flycheck-list-errors)
(define-key my-error-map (kbd "n") #'flycheck-next-error)
(define-key my-error-map (kbd "p") #'flycheck-previous-error)
(define-key my-error-map (kbd "e") #'consult-flycheck)

(with-eval-after-load 'flycheck
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)
  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)

  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(with-eval-after-load 'company
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))

(global-flycheck-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #undo-tree

(straight-use-package 'undo-tree)

(setq undo-tree-visualizer-diff t
      undo-tree-enable-undo-in-region t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))

(with-eval-after-load 'evil
  (evil-set-undo-system 'undo-tree)
  (evil-define-key 'normal my-intercept-mode-map
    (kbd "SPC u") 'undo-tree-visualize))

(global-undo-tree-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rg

(straight-use-package 'rg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #rainbow-delimiters

(straight-use-package 'rainbow-delimiters)

(define-key my-toggle-map (kbd "r") #'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #highlight-indent-guides

(straight-use-package 'highlight-indent-guides)

(setq highlight-indent-guides-method 'character
      highlight-indent-guides-character 124
      highlight-indent-guides-responsive 'top)

(define-key my-toggle-map (kbd "h") 'highlight-indent-guides-mode)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #theme

(defun disable-all-themes ()
  "Disable all active themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(straight-use-package 'doom-themes)
(straight-use-package 'base16-theme)

(setq base16-sakura-theme-colors
      '(:base00 "#FEEEED" ;; 桜色
                :base01 "#E8D3D1" ;; 灰桜色
                :base02 "#D8C6BC" ;; 桜鼠
                :base03 "#9fa0a0" ;; 薄墨色
                :base04 "#006543" ;; 柚葉色
                :base05 "#2f2725" ;; 墨色
                :base06 "#405C36" ;; 老緑
                :base07 "#433D3C" ;; 檳榔子染
                :base08 "#E9546B" ;; 梅重
                :base09 "#E9546B" ;; 韓紅
                :base0A "#0086AD" ;; 花色
                :base0B "#7BAA17" ;; 柳緑
                :base0C "#22825D" ;; 木賊色
                :base0D "#5E3862" ;; 杜若
                :base0E "#6967AB" ;; 竜胆色
                :base0F "#6A1435" ;; 紫檀色
                ))

(load-theme 'base16-sakura t)

(with-eval-after-load 'flycheck-posframe
  (set-face-background 'flycheck-posframe-face "#eddbd8"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #restart-emacs

(straight-use-package 'restart-emacs)

(define-key my-quit-map (kbd "r") #'restart-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #all-the-icons

(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)

(when (display-graphic-p)
  (require 'all-the-icons)

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(straight-use-package 'expand-region)

(global-set-key (kbd "C-=") #'er/expand-region)

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) my-intercept-mode-map
    (kbd "SPC v") 'er/expand-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #format

(straight-use-package 'apheleia)

(add-hook 'rust-mode-hook 'apheleia-mode)
(add-hook 'python-mode-hook 'apheleia-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #pulsar

(straight-use-package 'pulsar)

(pulsar-global-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lin

(straight-use-package 'lin)

(require 'lin)

(setq lin-face 'lin-blue) ; check doc string for alternative styles

(setq lin-mode-hooks
      '(bongo-mode-hook
        dired-mode-hook
        elfeed-search-mode-hook
        git-rebase-mode-hook
        grep-mode-hook
        ibuffer-mode-hook
        ilist-mode-hook
        ledger-report-mode-hook
        log-view-mode-hook
        magit-log-mode-hook
        mu4e-headers-mode
        notmuch-search-mode-hook
        notmuch-tree-mode-hook
        occur-mode-hook
        org-agenda-mode-hook
        proced-mode-hook
        tabulated-list-mode-hook))

(lin-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #gcmh

(straight-use-package 'gcmh)

(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 128 1024 1024))

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
;; #org

(straight-use-package 'org-pomodoro)

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

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(define-key my-org-map (kbd "c") #'org-capture)
(define-key my-org-map (kbd "a") #'org-agenda)
(define-key my-org-map (kbd "o") #'org-open-at-point)
(define-key my-org-map (kbd "l") #'org-link)
(define-key my-org-map (kbd "p") #'org-pomodoro)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #alert

(straight-use-package 'alert)
(straight-use-package 'alert-toast)

(cond ((eq system-type 'gnu/linux)
       (setq alert-default-style 'libnotify))

      ((eq system-type 'windows-nt)
       (setq alert-default-style 'toast)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #valign

(straight-use-package 'valign)

(add-hook 'org-mode-hook 'valign-mode)
(add-hook 'markdown-mode-hook 'valign-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #fussy

(straight-use-package '(fussy :type git :host github :repo "jojojames/fussy"))
(straight-use-package '(fuz-bin :repo "jcs-elpa/fuz-bin" :fetcher github :files (:defaults "bin")))

(require 'fussy)

(setq completion-styles '(fussy)
      completion-category-defaults nil
      compleiton-category-overrides nil
      fussy-filter-fn #'fussy-filter-fast
      fussy-score-fn #'fussy-fuz-bin-score)

(fuz-bin-load-dyn)

(defmacro fussy--measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     (let ((result ,@body))
       (message "%.06f" (float-time (time-since time)))
       result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #page-break-lines

(straight-use-package 'page-break-lines)

(add-hook 'prog-mode-hook 'page-break-lines-mode)
(add-hook 'text-mode-hook 'page-break-lines-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #dtrt-indent

(straight-use-package 'dtrt-indent)

(dtrt-indent-global-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #copilot

(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))

(setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node")

(add-hook 'prog-mode-hook 'copilot-mode)

;; accept completion from copilot and fallback to company
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(global-set-key (kbd "C-TAB") #'my-tab)
(global-set-key (kbd "C-<tab>") #'my-tab)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-TAB") #'my-tab)
  (define-key company-active-map (kbd "C-<tab>") #'my-tab)
  (define-key company-mode-map (kbd "C-TAB") #'my-tab)
  (define-key company-mode-map (kbd "C-<tab>") #'my-tab))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lsp-mode

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-java)
(straight-use-package 'lsp-pyright)

(setq lsp-keymap-prefix "M-l"
      lsp-eldoc-enable-hover nil
      lsp-enable-folding nil
      lsp-headerline-breadcrumb-enable nil
      lsp-headerline-breadcrumb-enable-diagnostics nil
      lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m"))

(add-hook 'lsp-mode-hook (lambda ()
                           (with-eval-after-load 'evil
                             (evil-local-set-key 'normal (kbd "SPC m") `("lsp" . ,lsp-command-map)))))

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
;; #highlight-quotes

(straight-use-package 'highlight-quoted)

(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #java

(add-hook 'java-mode-hook (lambda ()
                            (setq-local tab-width 2
                                        c-basic-offset 2
                                        indent-tabs-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #web

(straight-use-package 'web-mode)
(straight-use-package 'emmet-mode)
(straight-use-package 'web-beautify)

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

(setq inferior-lisp-program "sbcl")

(with-eval-after-load 'slime
  (slime-setup '(slime-fancy slime-company slime-banner)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #sql

(straight-use-package 'sql-indent)
(straight-use-package 'sqlformat)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)

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
