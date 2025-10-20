;; init-meow.el --- Initialize meow configurations.	-*- lexical-binding: t -*-

(defun meow-setup ()
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
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
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

   ;; '("t" . meow-right)
   ;; '("j" . meow-join)
   ;; '("p" . meow-prev)
   ;; '("n" . meow-next)
   ;; '("h" . meow-left)
   ;; '("d" . meow-delete)
   ;; '("D" . meow-backward-delete)
   ;; '("," . meow-inner-of-thing)
   ;; '("." . meow-bounds-of-thing)
   ;; '("N" . meow-next-expand)
   ;; '("T" . meow-right-expand)

   ;; personal setup
   '("t" . meow-prev)
   '("T" . my-meow-prev)
   '("h" . meow-next)
   '("H" . my-meow-next)
   '("d" . meow-left)
   '("D" . meow-join)
   '("n" . meow-right)
   '("N" . meow-line)
   '("-" . meow-delete)
   '("_" . meow-backward-delete)
   '("." . meow-inner-of-thing)
   '("," . meow-bounds-of-thing)
   '("<escape>" . ignore)))

(defun my-meow-prev-num (arg)
  "Move to the previous line.

By default moves 5 lines up.
Will cancel all other selection, except char selection.

Use with universal argument to move to the first line of buffer.
Use with numeric argument to move specified number of lines."
  ;;(interactive "P")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (cond
   ((meow--with-universal-argument-p arg)
    (goto-char (point-min)))
   (t
    (setq this-command #'previous-line)
    (let ((lines (or (and (numberp arg) arg)
                     5)))
      (forward-line (- lines))))))

(defun my-meow-prev (arg)
  (interactive "P")
  (my-meow-prev-num 5)
  )
(defun my-meow-next (arg)
  (interactive "P")
  (my-meow-prev-num -5))


(use-package meow
  :ensure t
  :defer nil
  :config
  (meow-setup)
  (meow-global-mode 1))

(provide 'init-meow)
;;; init-meow.el ends here
