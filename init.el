;;; package --- Summary
;;; Commentary:
;;; Code:

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

(provide 'init)
;;; init.el ends here

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((initialization) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
