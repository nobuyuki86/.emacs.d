;;; package --- Summary
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

(push '(fullscreen . maximized) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
