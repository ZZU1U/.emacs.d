;;; Startup stuff
;; Silence compiler warnings
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
    (setq native-comp-deferred-compilation nil))
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(load-file "~/.emacs.d/elisp/emacs.el")
(load-file "~/.emacs.d/meow.el")
;;(load-file "~/.emacs.d/xah-fly-keys.el")
(load-file "~/.emacs.d/elisp/cosmetics.el")
(load-file "~/.emacs.d/elisp/tools.el")
(load-file "~/.emacs.d/elisp/langs.el")
