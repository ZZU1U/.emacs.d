 ;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
    (setq native-comp-deferred-compilation nil))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Straight.el
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

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)


;; Some emacs-tune shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("45631691477ddee3df12013e718689dafa607771e7fd37ebc6c6eb9529a8ede5"
     "fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0"
     default))
 '(ein:output-area-inlined-images t)
 '(ns-command-modifier 'control)
 '(ns-control-modifier 'meta))

;; PATH issues
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH")))

;; (global-display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

(setq make-backup-files nil) ; stop creating ~ files

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; (global-set-key "\C-xk" 'kill-current-buffer)

;; Scroll fixed
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 10) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; C-c + C-v fixed
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; If I forget the binds
(use-package which-key
    :config
    (which-key-mode))

;; Recent files
(recentf-mode)
(setq recentf-auto-cleanup 'never)
(global-set-key "\C-x\C-r" 'recentf-open-files) ;; this actually overrides some shity bind idk

;; Needed for `:after char-fold' to work
(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

;; Cyrillic -> English kbinds
(use-package reverse-im
  :demand t ; always load it
  :after char-fold ; but only after `char-fold' is loaded
  :bind
  ("M-T" . reverse-im-translate-word) ; fix a word in wrong layout
  :custom
  ;; cache generated keymaps
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  ;; use lax matching
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  ;; translate these methods
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t)) ; turn the mode on

;; Little theming
(add-to-list 'default-frame-alist '(background-color . "honeydew"))
(set-frame-font "Fantasque Sans Mono 16" nil t) ;; IBM Plex Mono, DejaVu Sans Mono

(use-package standard-themes
  :config
  (setq standard-themes-mixed-fonts 1)
  (setq standard-light-palette-overrides
	'((bg-main "honeydew")
	  ;; (bg-dim "#c8ffc8")
	  )))

(use-package heaven-and-hell
  :ensure t
  :config
  (setq heaven-and-hell-theme-type 'light) 
  (setq heaven-and-hell-themes
        '((light . standard-light) 
          (dark . standard-dark))) 

  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  :init
  (doom-modeline-mode 1))
;; (use-package telephone-line
;;   :config
;;   (telephone-line-mode 1))
;; (use-package smart-mode-line
;;   :config
;;   (sml/setup)
;;   (setq sml/theme 'light))
(use-package minions
  :config
  (minions-mode))
;; (load-file "~/.emacs.d/mode-line.el")

;; Modal keybindings
(load-file "~/.emacs.d/meow.el")
;; (use-package xah-fly-keys
;;   :ensure nil
;;   :load-path "~/.emacs.d/xah-fly-keys.el"
;;   :config
;;   (xah-fly-keys-set-layout "russian")
;;   (xah-fly-keys-set-layout "qwerty")
;;   (xah-fly-keys 1)
;;)

;; Dirvish
;; (use-package dirvish
;;   :config
;;   (dirvish-override-dired-mode)) 

;; Treemacs
(use-package treemacs)

;; Projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
             )

;; Treemacs
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Magit
(use-package magit)

(use-package treemacs-magit
  :after (treemacs magit))

;; Workspaces (somehow)
(use-package perspective
  ;; :bind
  ;; ("C-x C-b" . persp-ibuffer)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-'"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

;; LSP
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (c-mode . lsp)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; Helm
(use-package helm
  :config
  (helm-mode))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; (require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap list-buffers] #'helm-buffers-list)
(define-key global-map [remap recentf-open-files] #'helm-recentf)

(define-key global-map (kbd "C-c C-d") #'deft)

;; Parents
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(electric-pair-mode 1)

;; Typst language support
(use-package typst-ts-mode
  :after (lsp)
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode"
                    :files (:defaults "*.el"))
  :custom
  (typst-ts-mode-watch-options "--open")
  :config
  ;; make sure to install typst-lsp from
  ;; https://github.com/nvarner/typst-lsp/releases
  ;; or use tinymist
  (add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "typst-lsp")
    :major-modes '(typst-ts-mode)
    :server-id 'typst-lsp))
  )
(keymap-set typst-ts-mode-map "C-c C-t" #'typst-ts-tmenu)

(use-package outline-indent-mode ;; Better tabs
  :straight '(:type git :host sourcehut :repo "meow_king/outline-indent-mode"))

(add-hook 'typst-ts-mode-hook 'outline-indent-mode)

(use-package websocket) ;; for preview
(use-package typst-preview
  :straight '(:type git :host github :repo "havarddj/typst-preview.el")
  :config
  ;;(setq typst-preview-executable "tinymist preview")
  (setq typst-preview-browser "safari"))

;; Nix support
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

;; PDF
(use-package pdf-tools
  :config
  (setq auto-revert-interval 0.5)
  (auto-revert-set-timer))

;; Better AsciiDoc
(use-package adoc-mode)

;; Better Mars
(use-package ein)

;; cfg
(editorconfig-mode 1)

;; Complete my stupid code
(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode)
  (setq corfu-auto t
      corfu-quit-no-match 'separator))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Cgek on fliy
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; My home library
(use-package deft  
  :init
  (setq deft-use-filename-as-title t)
  (setq deft-extensions '("adoc" "typ"))
  (setq deft-recursive t)
  (setq deft-directory "~/Typst"))

;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adoc-title-0-face ((t (:inherit adoc-title-face :height 1.6))))
 '(adoc-title-1-face ((t (:inherit adoc-title-face :height 1.4))))
 '(adoc-title-2-face ((t (:inherit adoc-title-face :height 1.3))))
 '(adoc-title-3-face ((t (:inherit adoc-title-face :height 1.2))))
 '(adoc-title-4-face ((t (:inherit adoc-title-face :height 1.1)))))

