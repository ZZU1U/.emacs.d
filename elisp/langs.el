;; Typst language support
(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode"
                    :files (:defaults "*.el"))
  :bind
  ("C-c C-l C-t" . typst-ts-tmenu)
  :custom
  (typst-ts-markup-header-same-height nil)
  (typst-ts-mode-fontification-precise 'max)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-watch-options "--open"))

(use-package outline-indent-mode ;; Better tabs
  :demand t
  :straight '(:type git :host sourcehut :repo "meow_king/outline-indent-mode")
  :config
  (add-hook 'typst-ts-mode-hook #'outline-indent-mode))

;; Nix
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

;; Better AsciiDoc
(use-package adoc-mode)

;; Better Mars (jupyter)
(use-package ein)

;; cfg
(editorconfig-mode 1)
