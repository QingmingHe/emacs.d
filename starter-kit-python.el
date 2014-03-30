
(starter-kit-install-if-needed 'python-mode 'ipython)

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(when (require 'cython-mode nil 'no-error)
  (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
  (add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode)))
