;; line numbers
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

;; scroll fixed
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(setq auto-window-vscroll nil)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 10) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; heh
(setq frame-resize-pixelwise t)
(setq ns-pop-up-frames nil)
(setq window-resize-pixelwise nil)
(setq truncate-lines t)

;; yes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(fringe-mode 5)
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

;;; Little theming
;; themes
(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-vivendi :no-confirm)
  :bind
  ("<f6>" . modus-themes-toggle))

;; i also like Fantasque Sans Mono
(set-face-attribute 'default nil :family "Iosevka Comfy" :height 150)
;; (set-face-attribute 'variable-pitch nil :family "Jonova" :height 1.2)

(use-package transpose-frame
  :bind ("C-x 7" . transpose-frame))

;; cursor thing
(use-package beacon
  :config
  (beacon-mode))

;; dashboard (obviously)
(use-package enlight
  :after (denote)
  :init
  (setq enlight-quotes '(
		       "I like JPEGs, uh\nFor the resolution,\nThe color,\nThe size of them"
		       "You think you know me"
		       "Today, right here, right now\nI'll love again"
		       "Is there anything that's worth more\nThan peace and love on the planet earth?"
		       "If I could begin to be\nHalf of what you think of me\nI could do about anything\nI could even learn how to love like you"
		       "It's over, isn't it?\nIsn't it?\nIsn't it over?"))
  :config
  (setopt initial-buffer-choice #'enlight)
  :custom
  (enlight-content
   (concat
    (elt enlight-quotes (random (length enlight-quotes)))
    "\n\n"
    (enlight-menu
     '(("Org Mode"
	("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("Files&Folders"
	("Downloads" (dired "~/Downloads") "d")
	("Library" (dired denote-directory) "l")
	("Journal" denote-journal-extras-new-or-existing-entry "w")
	("Config" (dired "~/.emacs.d") "c"))
       ("Navigation"
	("Projects" project-switch-project "p")
	("Notes" denote-open-or-create "n")))))))

; hide mods in modeline
(use-package minions
  :config
  (minions-mode))

;; Parents
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paren
  :config
  (setq show-paren-delay 0.1
	show-paren-highlight-openparen t
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t)
  (show-paren-mode))

(electric-pair-mode)
;; woow
(use-package focus)

