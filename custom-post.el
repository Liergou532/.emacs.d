;;; custom-post.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; This my personal configurations file.
;;; Code:
;; This is my personal configurations.

(use-package org-download
  :ensure t
  :after org
  :bind (:map org-mode-map
         ("<f2>" . org-download-screenshot)
         ("<f3>" . org-download-clipboard))
  :config
  ;; Directory where downloaded images are stored
  (setq org-download-image-dir "~/note/")
  ;; Method of inserting images (attach/directory)
  (setq org-download-method 'attach)
  ;; Heading level to create automatically for images
  (setq org-download-heading-lvl nil)
  ;; Image link format (default "#+ATTR_ORG: :width 300\n[[file:%s]]\n")
  (setq org-download-image-org-width 400)
  ;; Timestamp as filename to avoid name collisions
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  ;; display images
  (setq org-download-display-inline-images 'posframe)
  ;; Automatically annotate images with timestamps or captions
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq org-download-image-attr-list
        '("#+ATTR_HTML: :width 80% :align center"))
  ;; Enable drag-and-drop images directly from browser or filesystem
  (setq org-download-screenshot-method "flameshot gui --raw > %s")
  (when (eq system-type 'windows-nt)
    (setq org-download-screenshot-method "convert clipboard: %s"))
  (org-download-enable))


;; support mermaid for org babel
;; This need "mermaid-cil" mmdc
(org-babel-do-load-languages
'org-babel-load-languages
'((mermaid . t)
  (scheme . t)))
;; org mode settings
(setq org-startup-folded 'overview)
;;(setq org-startup-with-inline-images t)

;;; custom-post.el ends here
