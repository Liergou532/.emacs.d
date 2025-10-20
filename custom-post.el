;;; custom-post.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; This my personal configurations file.
;;; Code:
;; This is my personal configurations.

(require 'init-meow)
(require 'init-beancount)
(require 'init-denote)

(use-package org-download
  :ensure t
  :after org
  :bind (:map org-mode-map
         ("c-<f2>" . org-download-screenshot)
         ("<f3>" . org-download-clipboard))
  :config
  ;; Directory where downloaded images are stored
  (setq org-download-image-dir "~/note/")
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
  ;; org-download save all pngs to a directory
  ;; with the same name of buffer
  ;;(setq org-download-method 'my/org-download-method)
  (defun my-org-download-method (link)
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          (dirname (concat (file-name-sans-extension (buffer-name)) "-img")))
      (make-directory dirname)
      (expand-file-name filename dirname)))
                                        ;(setq org-download-method 'attach)
  (setq org-download-method 'my-org-download-method)
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

;; ---- org代码块相关的设置
(setq org-src-fontify-natively 1);代码块语法高亮
(setq org-src-tab-acts-natively 1);开启代码块语法缩进
(setq org-edit-src-content-indentation 0);代码块初始缩进范围

(use-package org
  :after tex
  :config
  (setq org-highlight-latex-and-related '(native latex entities))
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq my/latex-preview-scale 2.0)
  (setq org-format-latex-options
        `(:foreground default :backgroud default :scale ,my/latex-preview-scale :html-foreground "Black" :html-backgroud "Transparent" :html-scale ,my/latex-preview-scale :matchers ("begin" "$1" "$" "$$" "\\" "\\[")))
  (add-hook 'org-mode-hook #'org-cdlatex-mode))

;;; custom-post.el ends here
