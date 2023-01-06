;;; package --- Summary
;;; Commentary:
;;; Code:

(setq straight-repository-branch "develop")

(when (and (executable-find "watchexec")
	   (executable-find "python"))
  (setq straight-check-for-modifications '(watch-files)))

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

(straight-use-package 'org)

(eval-when-compile
  (require 'org)
  (org-babel-load-file (expand-file-name "early-init.org" user-emacs-directory))
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
  (byte-compile-file (expand-file-name "early-init.el" user-emacs-directory))
  (byte-compile-file (expand-file-name "config.el" user-emacs-directory)))

(defun byte-compile-init-file ()
  (interactive)
  (byte-compile-file (expand-file-name "init.el" user-emacs-directory)))

(add-to-list 'load-path (expand-file-name user-emacs-directory))
(require 'config)

;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)

;; Occasionally prune the build directory as well. For similar reasons
;; as above, we need to do this after local configuration.
(unless (bound-and-true-p radian--currently-profiling-p)
  (when (= 0 (random 100))
    (straight-prune-build-directory)))

(provide 'init)
;;; init.el ends here

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((emacs) (org-element-cache) (initialization) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
