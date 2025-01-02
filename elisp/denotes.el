;;; My home library
(use-package denote :demand t
  :custom
  (denote-directory (expand-file-name "~/Typst/"))
  (denote-save-buffers nil)
  (denote-known-keywords '("deprecated"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title subdirectory file-type keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-date-format nil) ; read doc string
  (denote-dired-directories
	(list denote-directory))
  :hook
  ;; (dired-mode-hook denote-dired-mode)
  (context-menu-functions denote-context-menu)
  :config
  (setq denote-file-types
   '((asciidoc :extension ".adoc")
     (typst :extension ".typ")
     (org
      :extension ".org"
      :front-matter denote-org-front-matter
      :title-key-regexp "^#\\+title\\s-*:"
      :title-value-function denote-format-string-for-org-front-matter
      :title-value-reverse-function denote-trim-whitespace
      :keywords-key-regexp "^#\\+filetags\\s-*:"
      :keywords-value-function denote-format-keywords-for-org-front-matter
      :keywords-value-reverse-function denote-extract-keywords-from-front-matter
      :signature-key-regexp "^#\\+signature\\s-*:"
      :signature-value-function denote-format-string-for-org-front-matter
      :signature-value-reverse-function denote-trim-whitespace
      :identifier-key-regexp "^#\\+identifier\\s-*:"
      :identifier-value-function denote-format-string-for-org-front-matter
      :identifier-value-reverse-function denote-trim-whitespace
      :date-key-regexp "^#\\+date\\s-*:"
      :date-value-function denote-date-org-timestamp
      :date-value-reverse-function denote-extract-date-from-front-matter
      :link denote-org-link-format
      :link-in-context-regexp denote-org-link-in-context-regexp)
     (markdown-yaml
      :extension ".md"
      :front-matter denote-yaml-front-matter
      :title-key-regexp "^title\\s-*:"
      :title-value-function denote-format-string-for-md-front-matter
      :title-value-reverse-function denote-trim-whitespace-then-quotes
      :keywords-key-regexp "^tags\\s-*:"
      :keywords-value-function denote-format-keywords-for-md-front-matter
      :keywords-value-reverse-function denote-extract-keywords-from-front-matter
      :signature-key-regexp "^signature\\s-*:"
      :signature-value-function denote-format-string-for-md-front-matter
      :signature-value-reverse-function denote-trim-whitespace-then-quotes
      :identifier-key-regexp "^identifier\\s-*:"
      :identifier-value-function denote-format-string-for-md-front-matter
      :identifier-value-reverse-function denote-trim-whitespace-then-quotes
      :date-key-regexp "^date\\s-*:"
      :date-value-function denote-date-rfc3339
      :date-value-reverse-function denote-extract-date-from-front-matter
      :link denote-md-ink-format
      :link-in-context-regexp denote-md-link-in-context-regexp)
     (markdown-toml
      :extension ".md"
      :front-matter denote-toml-front-matter
      :title-key-regexp "^title\\s-*="
      :title-value-function denote-format-string-for-md-front-matter
      :title-value-reverse-function denote-trim-whitespace-then-quotes
      :keywords-key-regexp "^tags\\s-*="
      :keywords-value-function denote-format-keywords-for-md-front-matter
      :keywords-value-reverse-function denote-extract-keywords-from-front-matter
      :signature-key-regexp "^signature\\s-*="
      :signature-value-function denote-format-string-for-md-front-matter
      :signature-value-reverse-function denote-trim-whitespace-then-quotes
      :identifier-key-regexp "^identifier\\s-*="
      :identifier-value-function denote-format-string-for-md-front-matter
      :identifier-value-reverse-function denote-trim-whitespace-then-quotes
      :date-key-regexp "^date\\s-*="
      :date-value-function denote-date-rfc3339
      :date-value-reverse-function denote-extract-date-from-front-matter
      :link denote-md-link-format
      :link-in-context-regexp denote-md-link-in-context-regexp)
     (text
      :extension ".txt"
      :front-matter denote-text-front-matter
      :title-key-regexp "^title\\s-*:"
      :title-value-function denote-format-string-for-org-front-matter
      :title-value-reverse-function denote-trim-whitespace
      :keywords-key-regexp "^tags\\s-*:"
      :keywords-value-function denote-format-keywords-for-text-front-matter
      :keywords-value-reverse-function denote-extract-keywords-from-front-matter
      :signature-key-regexp "^signature\\s-*:"
      :signature-value-function denote-format-string-for-org-front-matter
      :signature-value-reverse-function denote-trim-whitespace
      :identifier-key-regexp "^identifier\\s-*:"
      :identifier-value-function denote-format-string-for-org-front-matter
      :identifier-value-reverse-function denote-trim-whitespace
      :date-key-regexp "^date\\s-*:"
      :date-value-function denote-date-iso-8601
      :date-value-reverse-function denote-extract-date-from-front-matter
      :link denote-org-link-format
      :link-in-context-regexp denote-org-link-in-context-regexp)
     ))
  (denote-rename-buffer-mode 1)

  (let ((map global-map))
    (define-key map (kbd "C-c C-n n") #'denote-open-or-create)
    (define-key map (kbd "C-c C-n j") #'denote-journal-extras-new-or-existing-entry)
    (define-key map (kbd "C-c C-n r") #'denote-rename-file))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)))


(use-package consult-denote
  :bind
  (("C-c C-n g" . consult-denote-grep)
   ("C-c C-n f" . consult-denote-find))
  :config
  (consult-denote-mode))

(use-package denote-menu)
