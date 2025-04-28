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

;; ---- org代码块相关的设置
(setq org-src-fontify-natively 1);代码块语法高亮
(setq org-src-tab-acts-natively 1);开启代码块语法缩进
(setq org-edit-src-content-indentation 0);代码块初始缩进范围

(defun meow-setup ()
  ;;(setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-motion-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :defer nil
  :config
  (meow-setup)
  (meow-global-mode 1))

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
(setq org-download-method 'my-org-download-method)

;;; custom-post.el ends here
