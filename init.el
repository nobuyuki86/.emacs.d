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
      use-short-answers t
      recentf-max-saved-items 200)

(server-mode +1)
(savehist-mode +1)
(save-place-mode +1)
(recentf-mode +1)
(show-paren-mode +1)
(electric-pair-mode +1)
(global-auto-revert-mode +1)
(global-display-line-numbers-mode +1)
(which-function-mode +1)
(pixel-scroll-mode +1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #character-code

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #font

(straight-use-package 'fontaine)

(cond ((eq system-type 'gnu/linux)
       (setq fontaine-presets
             '((regular
                :default-family "VLゴシック"
                :default-height 100
                :fixed-pitch-family "VLゴシック"
                :variable-pitch-family "VLPゴシック"
                :italic-family "VLゴシック"
                :line-spacing 1)
               (large
                :default-family "VLゴシック"
                :default-height 150
                :variable-pitch-family "VLPゴシック"
                :line-spacing 1))))

      ((eq system-type 'windows-nt)
       (setq fontaine-presets
             '((regular
                :default-family "BIZ UDゴシック"
                :default-height 100
                :fixed-pitch-family "BIZ UDゴシック"
                :variable-pitch-family "BIZ UDPゴシック"
                :italic-family "BIZ UDゴシック"
                :line-spacing 1)
               (large
                :default-family "BIZ UDゴシック"
                :default-height 150
                :variable-pitch-family "BIZ UDPゴシック"
                :line-spacing 1)))))

;; Recover last preset or fall back to desired style from
;; `fontaine-presets'.
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook 'fontaine-store-latest-preset)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #whitespace

(setq whitespace-style '(face trailing))

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
;; #modeline

(straight-use-package 'doom-modeline)
(straight-use-package 'nyan-mode)

(setq nyan-animate-nyancat t
      nyan-bar-length 24)

(doom-modeline-mode +1)
(nyan-mode +1)


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
(straight-use-package 'evil-lion)

(setq evil-want-keybinding nil
      evil-symbol-word-search t
      evil-kill-on-visual-paste nil)

(define-key my-buffer-map (kbd "d") 'kill-this-buffer)

(with-eval-after-load 'evil
  (evil-collection-init)
  (evil-commentary-mode +1)
  (global-evil-surround-mode +1)
  (global-evil-matchit-mode +1)
  (evil-lion-mode +1)

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
    (kbd "SPC 5") `("C-x 5" . ,ctl-x-5-map)
    (kbd "SPC 0") 'delete-window
    (kbd "SPC 1") 'delete-other-windows
    (kbd "SPC 2") 'split-window-below
    (kbd "SPC 3") 'split-window-right
    (kbd "SPC 4") 'switch-to-buffer-other-window
    (kbd "SPC 5") 'ctl-x-5-prefix
    (kbd "SPC w") 'evil-window-next
    (kbd "SPC W") 'other-frame))

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
(straight-use-package '(company-dwim :type git :host github :repo "nobuyuki86/company-dwim"))
(straight-use-package '(company-anywhere :type git :host github :repo "zk-phi/company-anywhere"))

(setq company-minimum-prefix-length 1
      company-dabbrev-ignore-case nil)

(with-eval-after-load 'company
  (require 'company-dwim)
  (require 'company-anywhere)

  (add-to-list 'company-backends '(:separate company-capf company-yasnippet company-tabnine company-dabbrev))
  (add-to-list 'company-frontends 'company-dwim-frontend t)
  (delq 'company-preview-if-just-one-frontend company-frontends)

  (setq company-selection-default nil)
  (define-key company-active-map (kbd "RET") 'company-dwim-complete-or-newline)
  (define-key company-active-map (kbd "<return>") 'company-dwim-complete-or-newline)
  (define-key company-active-map (kbd "TAB") 'company-dwim-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-dwim-select-next)
  (define-key company-active-map (kbd "S-TAB") 'company-dwim-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-dwim-select-previous))

(global-set-key [remap indent-for-tab-command] 'company-indent-or-complete-common)
(global-set-key [remap c-indent-line-or-region] 'company-indent-or-complete-common)
(add-hook 'company-mode-hook 'company-box-mode)

(global-company-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #vertico

(straight-use-package '(vertico :files ("*.el" "extensions/*.el")))

(global-set-key (kbd "C-x C-z") 'vertico-repeat)

(with-eval-after-load 'evil
  (evil-define-key '(normal visual) my-intercept-mode-map
    (kbd "SPC z") 'vertico-repeat))

(vertico-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #prescient

(straight-use-package 'prescient)
(straight-use-package 'company-prescient)

(setq company-prescient-sort-length-enable nil)

(with-eval-after-load 'prescient
  (prescient-persist-mode +1)
  (with-eval-after-load 'vertico
    (setq vertico-sort-function #'prescient-sort)
    (advice-add #'vertico-insert :after
                (lambda ()
                  (prescient-remember (vertico--candidate))))))

(company-prescient-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #ctrlf

(straight-use-package 'ctrlf)

(setq ctrlf-default-search-style 'fuzzy)

(define-key search-map (kbd "s") 'ctrlf-forward-default)

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
(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "C-c m") 'consult-mode-command)
(global-set-key (kbd "C-c k") 'consult-kmacro)
;; C-x bindings (ctl-x-map)
(global-set-key (kbd "C-x M-:") 'consult-complex-command) ;; orig. repeat-complex-command
(global-set-key [remap switch-to-buffer] 'consult-buffer) ;; orig. switch-to-buffer
(global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
(global-set-key [remap bookmark-jump] 'consult-bookmark) ;; orig. bookmark-jump
(global-set-key [remap project-switch-to-buffer] 'consult-project-buffer) ;; orig. project-switch-to-buffer
;; Custom M-# bindings for fast register access
(global-set-key (kbd "M-#") 'consult-register-load)
(global-set-key (kbd "M-'") 'consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") 'consult-register)
;; Other custom bindings
(global-set-key (kbd "M-y") 'consult-yank-pop) ;; orig. yank-pop
(global-set-key (kbd "<help> a") 'consult-apropos) ;; orig. apropos-command
(define-key my-buffer-map (kbd "4") 'consult-buffer-other-window)
(define-key my-buffer-map (kbd "5") 'consult-buffer-other-frame)
;; M-g bindings (goto-map)
(define-key goto-map (kbd "e") 'consult-compile-error)
(define-key goto-map (kbd "g") 'consult-goto-line) ;; orig. goto-line
(define-key goto-map (kbd "o") 'consult-outline) ;; Alternative: consult-org-heading
(define-key goto-map (kbd "m") 'consult-mark)
(define-key goto-map (kbd "k") 'consult-global-mark)
(define-key goto-map (kbd "i") 'consult-imenu)
(define-key goto-map (kbd "I") 'consult-imenu-multi)
;; M-s bindings (search-map)
(define-key search-map (kbd "d") 'consult-find)
(define-key search-map (kbd "D") 'consult-locate)
(define-key search-map (kbd "g") 'consult-grep)
(define-key search-map (kbd "G") 'consult-git-grep)
(define-key search-map (kbd "r") 'consult-ripgrep)
(define-key search-map (kbd "l") 'consult-line)
(define-key search-map (kbd "L") 'consult-line-multi)
(define-key search-map (kbd "m") 'consult-multi-occur)
(define-key search-map (kbd "k") 'consult-keep-lines)
(define-key search-map (kbd "u") 'consult-focus-lines)
;; Minibuffer history
(define-key minibuffer-local-map (kbd "M-s") 'consult-history) ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") 'consult-history) ;; orig. previous-matching-history-element

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode 'consult-preview-at-point-mode)

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function 'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add 'register-preview :override 'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function 'consult-xref
      xref-show-definitions-function 'consult-xref)

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
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") 'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function 'consult--default-project--function)
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

(global-set-key (kbd "C-.") 'embark-act) ;; pick some comfortable binding
(global-set-key (kbd "C-;") 'embark-dwim) ;; good alternative: M-.
(global-set-key (kbd "C-h B") 'embark-bindings) ;; alternative for `describe-bindings'

(setq prefix-help-command 'embark-prefix-help-command)

(with-eval-after-load 'embark
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (with-eval-after-load 'consult
    (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)))


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

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(with-eval-after-load 'evil
  (evil-define-key 'normal my-intercept-mode-map
    (kbd "SPC p") `("projectile" . projectile-command-map)))

(projectile-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #flycheck

(straight-use-package 'flycheck)
(straight-use-package 'consult-flycheck)
(straight-use-package 'flycheck-posframe)

(define-key my-error-map (kbd "l") 'flycheck-list-errors)
(define-key my-error-map (kbd "n") 'flycheck-next-error)
(define-key my-error-map (kbd "p") 'flycheck-previous-error)
(define-key my-error-map (kbd "e") 'consult-flycheck)

(add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)

(with-eval-after-load 'company
  (add-hook 'flycheck-posframe-inhibit-functions 'company--active-p))

(global-flycheck-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #undo-tree

(straight-use-package 'undo-tree)

(setq undo-tree-auto-save-history nil)

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

(define-key my-toggle-map (kbd "r") 'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #highlight-indent-guides

(straight-use-package 'highlight-indent-guides)

(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-responsive 'top)

(define-key my-toggle-map (kbd "i") 'highlight-indent-guides-mode)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #theme

(straight-use-package 'modus-themes)

;;; The Modus themes

;; These are built into Emacs 28 or higher, though I use the package for
;; my development purposes (I need to make sure it always builds cleanly
;; and works properly).

;; Read their manual with Emacs' Info reader, or visit:
;; <https://protesilaos.com/emacs/modus-themes>.
(require 'modus-themes)

;; Add all your customizations prior to loading the themes
;;
;; NOTE: these are not my preferences!  I am always testing various
;; configurations.  Though I still like what I have here.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts nil
      modus-themes-subtle-line-numbers t
      modus-themes-intense-mouseovers nil
      modus-themes-deuteranopia nil
      modus-themes-tabs-accented nil
      modus-themes-variable-pitch-ui nil
      modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

      modus-themes-fringes nil ; {nil,'subtle,'intense}

      ;; Options for `modus-themes-lang-checkers' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `straight-underline', `text-also', `background',
      ;; `intense' OR `faint'.
      modus-themes-lang-checkers nil

      ;; Options for `modus-themes-mode-line' are either nil, or a list
      ;; that can combine any of `3d' OR `moody', `borderless',
      ;; `accented', a natural number for extra padding (or a cons cell
      ;; of padding and NATNUM), and a floating point for the height of
      ;; the text relative to the base font size (or a cons cell of
      ;; height and FLOAT)
      modus-themes-mode-line '(accented)

      ;; Options for `modus-themes-markup' are either nil, or a list
      ;; that can combine any of `bold', `italic', `background',
      ;; `intense'.
      modus-themes-markup nil

      ;; Options for `modus-themes-syntax' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
      modus-themes-syntax nil

      ;; Options for `modus-themes-hl-line' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `accented', `underline', `intense'
      modus-themes-hl-line nil

      ;; Options for `modus-themes-paren-match' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `bold', `intense', `underline'
      modus-themes-paren-match '(bold)

      ;; Options for `modus-themes-links' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
      ;; `bold', `italic', `background'
      modus-themes-links '(neutral-underline)

      ;; Options for `modus-themes-box-buttons' are either nil (the
      ;; default), or a list that can combine any of `flat',
      ;; `accented', `faint', `variable-pitch', `underline',
      ;; `all-buttons', the symbol of any font weight as listed in
      ;; `modus-themes-weights', and a floating point number
      ;; (e.g. 0.9) for the height of the button's text.
      modus-themes-box-buttons nil

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `background', `bold', `gray', `intense', `italic'
      modus-themes-prompts nil

      ;; The `modus-themes-completions' is an alist that reads three
      ;; keys: `matches', `selection', `popup'.  Each accepts a nil
      ;; value (or empty list) or a list of properties that can include
      ;; any of the following (for WEIGHT read further below):
      ;;
      ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
      ;; `selection' - `accented', `intense', `underline', `italic', `text-also', WEIGHT
      ;; `popup' - same as `selected'
      ;; `t' - applies to any key not explicitly referenced (check docs)
      ;;
      ;; WEIGHT is a symbol such as `semibold', `light', or anything
      ;; covered in `modus-themes-weights'.  Bold is used in the absence
      ;; of an explicit WEIGHT.
      modus-themes-completions
      '((matches . (semibold))
        (selection . (extrabold accented))
        (popup . (extrabold accented)))

      modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

      ;; Options for `modus-themes-region' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `no-extend', `bg-only', `accented'
      modus-themes-region '(accented no-extend)

      ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
      modus-themes-diffs nil

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      modus-themes-org-agenda ; this is an alist: read the manual or its doc string
      '((header-block . (variable-pitch light 1.6))
        (header-date . (underline-today grayscale workaholic 1.2))
        (event . (accented italic varied))
        (scheduled . rainbow)
        (habit . simplified))

      ;; The `modus-themes-headings' is an alist with lots of possible
      ;; combinations, include per-heading-level tweaks: read the
      ;; manual or its doc string
      modus-themes-headings
      '((0 . (variable-pitch light (height 2.2)))
        (1 . (rainbow variable-pitch light (height 1.6)))
        (2 . (rainbow variable-pitch light (height 1.4)))
        (3 . (rainbow variable-pitch regular (height 1.3)))
        (4 . (rainbow regular (height 1.2)))
        (5 . (rainbow (height 1.1)))
        (t . (variable-pitch extrabold))))

;; Load the theme files before enabling a theme (else you get an error).
(modus-themes-load-themes)

;; Optionally set the `modus-themes-toggle' to a key binding:
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;;;; Modus themes "Summertime"

;; Read the relevant blog post:
;; <https://protesilaos.com/codelog/2022-07-26-modus-themes-color-override-demo/>

;; Thanks to user "Summer Emacs" for (i) suggesting the name
;; "summertime", (ii) testing variants of this in her setup, and (iii)
;; sending me feedback on possible tweaks and refinements.  All errors
;; are my own.  (This information is shared with permission.)
(define-minor-mode modus-themes-summertime
  "Refashion the Modus themes by overriding their colors.

This is a complete technology demonstration to show how to
manually override the colors of the Modus themes.  I have taken
good care of those overrides to make them work as a fully fledged
color scheme that is compatible with all user options of the
Modus themes.

These overrides are usable by those who (i) like something more
fancy than the comparatively austere looks of the Modus themes,
and (ii) can cope with a lower contrast ratio.

The overrides are set up as a minor mode, so that the user can
activate the effect on demand.  Those who want to load the
overrides at all times can either add them directly to their
configuration or enable `modus-themes-summertime' BEFORE loading
either of the Modus themes (if the overrides are evaluated after
the theme, the theme must be reloaded).

Remember that all changes to theme-related variables require a
reload of the theme to take effect (the Modus themes have lots of
user options, apart from those overrides).

The `modus-themes-summertime' IS NOT an official extension to the
Modus themes and DOES NOT comply with its lofty accessibility
standards.  It is included in the official manual as guidance for
those who want to make use of the color overriding facility we
provide."
  :init-value nil
  :global t
  (if modus-themes-summertime
      (setq modus-themes-operandi-color-overrides
            '((bg-main . "#fff0f2")
              (bg-dim . "#fbe6ef")
              (bg-alt . "#f5dae6")
              (bg-hl-line . "#fad8e3")
              (bg-active . "#efcadf")
              (bg-inactive . "#f3ddef")
              (bg-active-accent . "#ffbbef")
              (bg-region . "#dfc5d1")
              (bg-region-accent . "#efbfef")
              (bg-region-accent-subtle . "#ffd6ef")
              (bg-header . "#edd3e0")
              (bg-tab-active . "#ffeff2")
              (bg-tab-inactive . "#f8d3ef")
              (bg-tab-inactive-accent . "#ffd9f5")
              (bg-tab-inactive-alt . "#e5c0d5")
              (bg-tab-inactive-alt-accent . "#f3cce0")
              (fg-main . "#543f78")
              (fg-dim . "#5f476f")
              (fg-alt . "#7f6f99")
              (fg-unfocused . "#8f6f9f")
              (fg-active . "#563068")
              (fg-inactive . "#8a5698")
              (fg-docstring . "#5f5fa7")
              (fg-comment-yellow . "#a9534f")
              (fg-escape-char-construct . "#8b207f")
              (fg-escape-char-backslash . "#a06d00")
              (bg-special-cold . "#d3e0f4")
              (bg-special-faint-cold . "#e0efff")
              (bg-special-mild . "#c4ede0")
              (bg-special-faint-mild . "#e0f0ea")
              (bg-special-warm . "#efd0c4")
              (bg-special-faint-warm . "#ffe4da")
              (bg-special-calm . "#f0d3ea")
              (bg-special-faint-calm . "#fadff9")
              (fg-special-cold . "#405fb8")
              (fg-special-mild . "#407f74")
              (fg-special-warm . "#9d6f4f")
              (fg-special-calm . "#af509f")
              (bg-completion . "#ffc5e5")
              (bg-completion-subtle . "#f7cfef")
              (red . "#ed2f44")
              (red-alt . "#e0403d")
              (red-alt-other . "#e04059")
              (red-faint . "#ed4f44")
              (red-alt-faint . "#e0603d")
              (red-alt-other-faint . "#e06059")
              (green . "#217a3c")
              (green-alt . "#417a1c")
              (green-alt-other . "#006f3c")
              (green-faint . "#318a4c")
              (green-alt-faint . "#518a2c")
              (green-alt-other-faint . "#20885c")
              (yellow . "#b06202")
              (yellow-alt . "#a95642")
              (yellow-alt-other . "#a06f42")
              (yellow-faint . "#b07232")
              (yellow-alt-faint . "#a96642")
              (yellow-alt-other-faint . "#a08042")
              (blue . "#275ccf")
              (blue-alt . "#475cc0")
              (blue-alt-other . "#3340ef")
              (blue-faint . "#476ce0")
              (blue-alt-faint . "#575ccf")
              (blue-alt-other-faint . "#3f60d7")
              (magenta . "#bf317f")
              (magenta-alt . "#d033c0")
              (magenta-alt-other . "#844fe4")
              (magenta-faint . "#bf517f")
              (magenta-alt-faint . "#d053c0")
              (magenta-alt-other-faint . "#846fe4")
              (cyan . "#007a9f")
              (cyan-alt . "#3f709f")
              (cyan-alt-other . "#107f7f")
              (cyan-faint . "#108aaf")
              (cyan-alt-faint . "#3f80af")
              (cyan-alt-other-faint . "#3088af")
              (red-active . "#cd2f44")
              (green-active . "#116a6c")
              (yellow-active . "#993602")
              (blue-active . "#475ccf")
              (magenta-active . "#7f2ccf")
              (cyan-active . "#007a8f")
              (red-nuanced-bg . "#ffdbd0")
              (red-nuanced-fg . "#ed6f74")
              (green-nuanced-bg . "#dcf0dd")
              (green-nuanced-fg . "#3f9a4c")
              (yellow-nuanced-bg . "#fff3aa")
              (yellow-nuanced-fg . "#b47232")
              (blue-nuanced-bg . "#e3e3ff")
              (blue-nuanced-fg . "#201f6f")
              (magenta-nuanced-bg . "#fdd0ff")
              (magenta-nuanced-fg . "#c0527f")
              (cyan-nuanced-bg . "#dbefff")
              (cyan-nuanced-fg . "#0f3f60")
              (bg-diff-heading . "#b7cfe0")
              (fg-diff-heading . "#041645")
              (bg-diff-added . "#d6f0d6")
              (fg-diff-added . "#004520")
              (bg-diff-changed . "#fcefcf")
              (fg-diff-changed . "#524200")
              (bg-diff-removed . "#ffe0ef")
              (fg-diff-removed . "#891626")
              (bg-diff-refine-added . "#84cfa4")
              (fg-diff-refine-added . "#002a00")
              (bg-diff-refine-changed . "#cccf8f")
              (fg-diff-refine-changed . "#302010")
              (bg-diff-refine-removed . "#da92b0")
              (fg-diff-refine-removed . "#500010")
              (bg-diff-focus-added . "#a6e5c6")
              (fg-diff-focus-added . "#002c00")
              (bg-diff-focus-changed . "#ecdfbf")
              (fg-diff-focus-changed . "#392900")
              (bg-diff-focus-removed . "#efbbcf")
              (fg-diff-focus-removed . "#5a0010"))
            modus-themes-vivendi-color-overrides
            '((bg-main . "#25152a")
              (bg-dim . "#2a1930")
              (bg-alt . "#382443")
              (bg-hl-line . "#332650")
              (bg-active . "#463358")
              (bg-inactive . "#2d1f3a")
              (bg-active-accent . "#50308f")
              (bg-region . "#5d4a67")
              (bg-region-accent . "#60509f")
              (bg-region-accent-subtle . "#3f285f")
              (bg-header . "#3a2543")
              (bg-tab-active . "#26162f")
              (bg-tab-inactive . "#362647")
              (bg-tab-inactive-accent . "#36265a")
              (bg-tab-inactive-alt . "#3e2f5a")
              (bg-tab-inactive-alt-accent . "#3e2f6f")
              (fg-main . "#debfe0")
              (fg-dim . "#d0b0da")
              (fg-alt . "#ae85af")
              (fg-unfocused . "#8e7f9f")
              (fg-active . "#cfbfef")
              (fg-inactive . "#b0a0c0")
              (fg-docstring . "#c8d9f7")
              (fg-comment-yellow . "#cf9a70")
              (fg-escape-char-construct . "#ff75aa")
              (fg-escape-char-backslash . "#dbab40")
              (bg-special-cold . "#2a3f58")
              (bg-special-faint-cold . "#1e283f")
              (bg-special-mild . "#0f3f31")
              (bg-special-faint-mild . "#0f281f")
              (bg-special-warm . "#44331f")
              (bg-special-faint-warm . "#372213")
              (bg-special-calm . "#4a314f")
              (bg-special-faint-calm . "#3a223f")
              (fg-special-cold . "#c0b0ff")
              (fg-special-mild . "#bfe0cf")
              (fg-special-warm . "#edc0a6")
              (fg-special-calm . "#ff9fdf")
              (bg-completion . "#502d70")
              (bg-completion-subtle . "#451d65")
              (red . "#ff5f6f")
              (red-alt . "#ff8f6d")
              (red-alt-other . "#ff6f9d")
              (red-faint . "#ffa0a0")
              (red-alt-faint . "#f5aa80")
              (red-alt-other-faint . "#ff9fbf")
              (green . "#51ca5c")
              (green-alt . "#71ca3c")
              (green-alt-other . "#51ca9c")
              (green-faint . "#78bf78")
              (green-alt-faint . "#99b56f")
              (green-alt-other-faint . "#88bf99")
              (yellow . "#f0b262")
              (yellow-alt . "#f0e242")
              (yellow-alt-other . "#d0a272")
              (yellow-faint . "#d2b580")
              (yellow-alt-faint . "#cabf77")
              (yellow-alt-other-faint . "#d0ba95")
              (blue . "#778cff")
              (blue-alt . "#8f90ff")
              (blue-alt-other . "#8380ff")
              (blue-faint . "#82b0ec")
              (blue-alt-faint . "#a0acef")
              (blue-alt-other-faint . "#80b2f0")
              (magenta . "#ff70cf")
              (magenta-alt . "#ff77f0")
              (magenta-alt-other . "#ca7fff")
              (magenta-faint . "#e0b2d6")
              (magenta-alt-faint . "#ef9fe4")
              (magenta-alt-other-faint . "#cfa6ff")
              (cyan . "#30cacf")
              (cyan-alt . "#60caff")
              (cyan-alt-other . "#40b79f")
              (cyan-faint . "#90c4ed")
              (cyan-alt-faint . "#a0bfdf")
              (cyan-alt-other-faint . "#a4d0bb")
              (red-active . "#ff6059")
              (green-active . "#64dc64")
              (yellow-active . "#ffac80")
              (blue-active . "#4fafff")
              (magenta-active . "#cf88ff")
              (cyan-active . "#50d3d0")
              (red-nuanced-bg . "#440a1f")
              (red-nuanced-fg . "#ffcccc")
              (green-nuanced-bg . "#002904")
              (green-nuanced-fg . "#b8e2b8")
              (yellow-nuanced-bg . "#422000")
              (yellow-nuanced-fg . "#dfdfb0")
              (blue-nuanced-bg . "#1f1f5f")
              (blue-nuanced-fg . "#bfd9ff")
              (magenta-nuanced-bg . "#431641")
              (magenta-nuanced-fg . "#e5cfef")
              (cyan-nuanced-bg . "#042f49")
              (cyan-nuanced-fg . "#a8e5e5")
              (bg-diff-heading . "#304466")
              (fg-diff-heading . "#dae7ff")
              (bg-diff-added . "#0a383a")
              (fg-diff-added . "#94ba94")
              (bg-diff-changed . "#2a2000")
              (fg-diff-changed . "#b0ba9f")
              (bg-diff-removed . "#50163f")
              (fg-diff-removed . "#c6adaa")
              (bg-diff-refine-added . "#006a46")
              (fg-diff-refine-added . "#e0f6e0")
              (bg-diff-refine-changed . "#585800")
              (fg-diff-refine-changed . "#ffffcc")
              (bg-diff-refine-removed . "#952838")
              (fg-diff-refine-removed . "#ffd9eb")
              (bg-diff-focus-added . "#1d4c3f")
              (fg-diff-focus-added . "#b4dfb4")
              (bg-diff-focus-changed . "#424200")
              (fg-diff-focus-changed . "#d0daaf")
              (bg-diff-focus-removed . "#6f0f39")
              (fg-diff-focus-removed . "#eebdba")))
    (setq modus-themes-operandi-color-overrides nil
          modus-themes-vivendi-color-overrides nil)))

(modus-themes-summertime)
(modus-themes-load-vivendi)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #restart-emacs

(straight-use-package 'restart-emacs)

(define-key my-quit-map (kbd "r") 'restart-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #all-the-icons

(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)

(when (display-graphic-p)
  (require 'all-the-icons))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #expand-region

(straight-use-package 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)

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
;; #gcmh

(straight-use-package 'gcmh)

(setq gcmh-auto-idle-delay-factor 'auto
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
                         "~/org/journal.org")
      org-fontify-quote-and-verse-blocks t
      org-startup-folded 'content)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key my-org-map (kbd "c") 'org-capture)
(define-key my-org-map (kbd "a") 'org-agenda)
(define-key my-org-map (kbd "o") 'org-open-at-point)
(define-key my-org-map (kbd "l") 'org-link)
(define-key my-org-map (kbd "p") 'org-pomodoro)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #alert

(straight-use-package 'alert)
(straight-use-package 'alert-toast)

(cond ((eq system-type 'gnu/linux)
       (setq alert-default-style 'libnotify))

      ((eq system-type 'windows-nt)
       (require 'alert-toast)
       (setq alert-default-style 'toast)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #valign

(straight-use-package 'valign)

(add-hook 'org-mode-hook 'valign-mode)
(add-hook 'markdown-mode-hook 'valign-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #fussy

(straight-use-package '(fussy :type git :host github :repo "jojojames/fussy"))
(straight-use-package '(fzf-native :repo "dangduc/fzf-native" :host github :files (:defaults "bin")))

(setq completion-styles '(fussy)
      completion-category-defaults nil
      compleiton-category-overrides nil
      fussy-filter-fn 'fussy-filter-default
      fussy-score-fn 'fussy-fzf-native-score)

(fzf-native-load-dyn)

(with-eval-after-load 'company
  (defun j-company-capf (f &rest args)
    "Manage `completion-styles'."
    (let ((fussy-max-candidate-limit 5000)
          (fussy-default-regex-fn 'fussy-pattern-first-letter)
          (fussy-prefer-prefix nil))
      (apply f args)))

  (advice-add 'company-capf :around 'j-company-capf))


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
;; #lin

(straight-use-package 'lin)

(setq lin-face 'lin-red)

(lin-global-mode +1)


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

(global-set-key (kbd "C-RET") 'my-tab)
(global-set-key (kbd "C-<return>") 'my-tab)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-RET") 'my-tab)
  (define-key company-active-map (kbd "C-<return>") 'my-tab)
  (define-key company-mode-map (kbd "C-RET") 'my-tab)
  (define-key company-mode-map (kbd "C-<return>") 'my-tab))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #tree-sitter

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package '(ts-fold :type git :host github :repo "jcs-elpa/ts-fold"))

(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'ts-fold)

(global-tree-sitter-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #denote

(straight-use-package 'denote)

(with-eval-after-load 'org-capture
  (require 'denote-org-capture)
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #emoji

(straight-use-package 'emojify)

(global-emojify-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #mmm

(straight-use-package 'mmm-mode)

(require 'mmm-mode)
(require 'mmm-auto)

(setq mmm-global-mode 'maybe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #treemacs

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-magit)
(straight-use-package 'treemacs-icons-dired)
(straight-use-package 'treemacs-persp)
(straight-use-package 'treemacs-perspective)
(straight-use-package 'treemacs-tab-bar)
(straight-use-package 'treemacs-all-the-icons)

(defvar my-treemacs-map (make-sparse-keymap)
  "My file keymap.")

(define-key my-treemacs-map (kbd "1") 'treemacs-select-window)
(define-key my-treemacs-map (kbd "t") 'treemacs)
(define-key my-treemacs-map (kbd "d") 'treemacs-select-directory)
(define-key my-treemacs-map (kbd "B") 'treemacs-bookmark)
(define-key my-treemacs-map (kbd "f") 'treemacs-find-file)
(define-key my-treemacs-map (kbd "F") 'treemacs-find-tag)
(define-key my-treemacs-map (kbd "c") 'treemacs-display-current-project-exclusively)

(define-key my-file-map (kbd "t") `("treemacs" . ,my-treemacs-map))

(with-eval-after-load 'treemacs
  (treemacs-follow-mode +1)
  (treemacs-filewatch-mode +1)
  (treemacs-fringe-indicator-mode 'always)

  (with-eval-after-load 'evil
    (require 'treemacs-evil))

  (with-eval-after-load 'projectile
    (require 'treemacs-projectile))

  (with-eval-after-load 'magit
    (require 'treemacs-magit))

  (with-eval-after-load 'persp
    (require 'persp-mode)
    (treemacs-set-scope-type 'Perspectives))

  (require 'treemacs-tab-bar)
  (treemacs-set-scope-type 'Tabs))

(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lsp-mode

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-java)
(straight-use-package 'lsp-pyright)

(setq lsp-keymap-prefix "M-l")

(add-hook 'lsp-mode-hook (lambda ()
                           (with-eval-after-load 'evil
                             (evil-local-set-key 'normal (kbd "SPC m") `("lsp" . ,lsp-command-map)))))

(add-hook 'html-mode-hook 'lsp)
(add-hook 'css-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'java-mode-hook (lambda ()
                            (require 'lsp-java)
                            (lsp)))
(add-hook 'python-mode-hook (lambda ()
                              (require 'lsp-pyright)
                              (lsp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #elisp

(straight-use-package 'highlight-defined)
(straight-use-package 'highlight-quoted)

(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #java

(add-hook 'java-mode-hook (lambda ()
                            (setq-local tab-width 2
                                        c-basic-offset 2
                                        indent-tabs-mode t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #web

(straight-use-package 'emmet-mode)
(straight-use-package 'web-beautify)

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))

(add-hook 'html-mode-hook (lambda ()
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #xml

(add-hook 'nxml-mode-hook (lambda ()
                            (setq tab-width 4)))


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
