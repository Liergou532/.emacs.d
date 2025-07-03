;; init-beancount.el --- Initialize beancount configurations.	-*- lexical-binding: t -*-

(use-package beancount
  :ensure t
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.bean\\'" . beancount-mode)
  :init
  :config
  ;;(add-hook 'beancount-mode-hook #'outline-minor-mode)
  ;;(setq beancount-use-outline t)
  :hook
  (beancount-mode-hook . (lambda () (setq-local electric-indent-chars nil)))
  )

(provide 'init-beancount)

;;; init-beancount.el ends here
