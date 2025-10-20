;; init-denote.el --- Initialize denote configurations.	-*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; denote configuration
;;

;;; Code:
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

(defun my/denote-colleagues-get-file (name)
  "Find file in variable `denote-directory' for NAME colleague.
If there are more than one files, prompt with completion for one among
them.

NAME is one among `my-denote-colleagues'."
  (if-let* ((files (denote-directory-files name))
            (length-of-files (length files)))
      (cond
       ((= length-of-files 1)
        (car files))
       ((> length-of-files 1)
        (completing-read "Select a file: " files nil :require-match)))
    (user-error "No files for colleague with name `%s'" name)))

(use-package denote-markdown
  :ensure t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-markdown-convert-links-to-file-paths
              denote-markdown-convert-links-to-denote-type
              denote-markdown-convert-links-to-obsidian-type
              denote-markdown-convert-obsidian-links-to-denote-type ))

(provide 'init-denote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
