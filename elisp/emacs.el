;;; Using garbage magic hack.
(use-package gcmh
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; PATH issues
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH")))

;; osx clipboard sync
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; recent files
(use-package recentf
  :custom
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode))

;;; Enable cyrillic keybindings
;; Needed for `:after char-fold' to work
(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

;; Oh yeah
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

;; yes
(use-package savehist
  :init
  (savehist-mode))

;;; Emacs?!
(use-package emacs
  :hook (dired-mode . dired-omit-mode)
  :custom (dired-omit-files (rx (seq bol ".")))
  :init
  (setq backup-by-copying t     ; don't clobber symlinks
	backup-directory-alist '(("." . "~/.saves/")) ; don't litter my fs tree
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t      ; use versioned backups

	custom-file "~/.emacs.d/custom.el"
	
	tab-always-indent 'complete
	enable-recursive-minibuffers t
	text-mode-ispell-word-completion nil
	read-extended-command-predicate #'command-completion-default-include-p

	ring-bell-function 'ignore
	inhibit-startup-message t
	confirm-kill-emacs 'y-or-n-p
	sentence-end-double-space nil
	visible-bell t
	line-move-visual t

	trash-directory "~/.Trash"
	delete-by-moving-to-trash t

	inhibit-compacting-font-caches t)
  (load custom-file)
  (delete-selection-mode)

  (setopt use-short-answers t)

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (eval-after-load "dired" '(progn
			      (define-key dired-mode-map (kbd "|") #'dired-omit-mode) ))
  :bind ("C-c C-r" . restart-emacs)
)
