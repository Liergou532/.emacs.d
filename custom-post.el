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

;; 0. 定义路径变量
;;    使用 defvar 来声明变量，如果变量已定义则不会改变其值。
;;    您可以根据您的实际目录结构修改这些路径。
(defvar my-para-base-path "~/para/" "Base path for PARA directories.")
(defvar my-finance-area-path (expand-file-name "finance/" my-para-base-path)
  "Path to the Finance area within PARA.")
(defvar my-math-area-path (expand-file-name "math/" my-para-base-path)
  "Path to the Linux Tweaks area within PARA.")

;; ───── Denote 配置：官方范例 × PARA 数字花园 ─────
(use-package denote
  :ensure t
  :demand t                               ; 启动即用
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  ;; 全局键位（挑自己顺手的）
  (("C-c n n" . denote)                  ; 新笔记
   ("C-c n s" . my/denote-new-seed)      ; 新 seed
   ("C-c n l" . denote-link)
   ("C-c n r" . denote-rename-file)
   ("C-c n N" . my/denote-in-this-dir)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ;; Dired 专用
   :map dired-mode-map
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords))
  :config
  ;; —— 路径 & 文件基本规则 ————————————————
                                        ;(setq denote-directory
                                        ;     (expand-file-name "2area/" "~/myfile/note/"))
  (setq denote-directory my-para-base-path)
  (setq denote-file-type 'org)           ; 统一 .org
  (setq denote-date-format nil)          ; 20250508T235959
  ;; —— 关键词 & 提示 ————————————————
  (setq denote-known-keywords
        '("seed" "sprout" "evergreen"
          "linux" "photography" "finance"))
  (setq denote-infer-keywords t
        denote-sort-keywords t
        denote-prompts '(subdirectory title keywords))
  ;; 让 Dired 视图也识别所有子目录（方便着色、批量改名）
  (setq denote-dired-directories (list denote-directory))
  ;; —— 其他官方建议保持默认即可 ————————————
  (setq denote-date-prompt-use-org-read-date t)
  (denote-rename-buffer-mode 1)
  ;; —— 小助手：新建 🌱 种子 ——————————————
  (defun my/denote-new-seed (title area)
    "Create a new Denote note tagged as seed in selected AREA."
    (interactive
     (list (read-string "Seed title: ")
           (completing-read
            "Select area: "
            (seq-filter
             (lambda (d)
               (and (not (member d '("." "..")))
                    (file-directory-p (expand-file-name d my-para-base-path))))
             (directory-files my-para-base-path)))))
    (let ((denote-directory (expand-file-name (concat area "/") my-para-base-path)))
      (denote title '("seed"))))
  ;; 写个 helper，在当前 Dired 子目录快速新建笔记
  (defun my/denote-in-this-dir ()
    "在当前 Dired 目录中创建 Denote 笔记。"
    (interactive)
    (let ((denote-directory default-directory)
          (denote-prompts '(title keywords)))
      (call-interactively #'denote))))
;; ───────────────────────────────────────────────

;; ──────────────────────────────────────────────────────
;; consult-notes × Denote × PARA 例子 (路径变量化)
;; 依赖：consult、denote 已安装并先于此加载
;; ──────────────────────────────────────────────────────
(use-package consult-notes
  :ensure t                          ; 已推到 MELPA，可直接 :ensure t
  :after (consult denote)            ; consult / denote 均已加载
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind (("C-c n F" . consult-notes) ; F = find
         ("C-c n S" . consult-notes-search-in-all-notes)
         ("C-c n A" . my/create-para-subarea)) ; S = search
  :config
  ;; 1. 定义「文件夹‑源」
  ;;    语法:  ("显示名称" 快捷键  "绝对路径" . 文件匹配通配符)
  ;;    快捷键 = 单字符，用来在 minibuffer 里过滤；可省略写 nil
  (setq consult-notes-file-dir-sources
        (my/consult-notes-areas-sources))

  ;; 2. 开启 Denote 集成：让 `consult-notes` 直接用 Denote 的文件索引
  (consult-notes-denote-mode 1)

  ;(setq consult-notes-denote-files-function (function denote-directory-text-only-files))

  ;; Helper: create a new PARA sub-area and refresh sources
  (defun my/create-para-subarea (name)
    "Create a new sub-area under `my-para-base-path' and refresh consult-notes sources."
    (interactive "sSub-area name: ")
    (let ((new-dir (expand-file-name (concat name "/") my-para-base-path)))
      (unless (file-exists-p new-dir)
        (make-directory new-dir t)
        (message "Created PARA sub-area: %s" new-dir)
        ;; Refresh the consult-notes sources
        (setq consult-notes-file-dir-sources
              (my/consult-notes-areas-sources)))))
  )
;; ──────────────────────────────────────────────────────


;;; Automatically scan AREA subdirectories for consult-notes sources
(require 'seq)
(defun my/consult-notes-areas-sources ()
  "Scan `my-para-base-path' for subdirectories and build `consult-notes' sources."
  (let* ((entries (directory-files my-para-base-path))
         (dirs (seq-filter
                (lambda (d)
                  (and (not (member d '("." "..")))
                       (file-directory-p (expand-file-name d my-para-base-path))))
                entries)))
    (mapcar (lambda (name)
              (let ((path (expand-file-name (concat name "/") my-para-base-path)))
                ;; Assign first character of name as key if letter, else no key
                (list name
                      (let ((c (downcase (string-to-char name))))
                        (if (and (>= c ?a) (<= c ?z)) c nil))
                      path)))
            dirs)))

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
