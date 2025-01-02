;; if I forget the binds (no way)
(use-package which-key :demand t
    :config
    (which-key-mode))

;; Treemacs
(use-package treemacs
  :bind
  (("C-c C-t C-t" . treemacs))
  :config
  (setq treemacs-no-png-images t))

;; Projectile
(use-package projectile :demand t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

;; Treemacs
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Magit
(use-package magit)

(use-package treemacs-magit
  :after (treemacs magit))

;; timers
(use-package tmr :demand t
  :bind (("C-c C-a C-t" . tmr)))

;; basically workspaces
(use-package perspective :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-c C-c"))  ; pick your own prefix key here
  :config
  (persp-mode))

;; player
(use-package mpc
  :bind (("C-c C-a C-l" . mpc)))

;; LSP
(use-package lsp-bridge :demand t
  :after (yasnippet markdown-mode)
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :config
  (global-lsp-bridge-mode))

;;; Helm (but better?)
(use-package embark-consult)

(use-package marginalia :demand t
  :config
  (marginalia-mode))

(use-package embark :demand t
  :bind 
  (("C-." . embark-act)) ;; alternative for `describe-bindings'
  :hook
  (eldoc-documentation-functions . embark-eldoc-first-target)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult :after (perspective)
  :demand t
  :init
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :config
  (define-key global-map [remap list-buffers] #'consult-buffer)
  :bind
  (("C-c C-t C-o" . consult-outline)
  ("C-c C-t C-i" . consult-imenu)))

(use-package orderless :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico :demand t
  :config
  (vertico-mode))

;; Complete my stupid code
(use-package corfu :demand t
  :hook ((prog-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

;; Cgek on fliy // you mean "Check on fly"?
(use-package flycheck :demand t
  :custom
  (flycheck-enabled-checkers '(python-mypy))
  :hook
  (after-init-hook global-flycheck-mode))

(use-package jinx :demand t
  :custom
  (jinx-languages "en_US ru_RU")
  :hook (emacs-startup . global-jinx-mode))

;; Snippets
;; (use-package yasnippet :demand t
;;   :custom
;;   (yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   :config
;;   (yas-global-mode 1)

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(use-package yasnippet-snippets)

(load-file "~/.emacs.d/elisp/denotes.el")
