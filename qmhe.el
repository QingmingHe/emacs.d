;; activate python configuration of starter-kit
(starter-kit-load "python")

;; activate org configuration of starter-kit
(starter-kit-load "org")

;; activate eshell configuration of starter-kit
;; This cause error
;; (starter-kit-load "eshell")

;; activate lisp configuration of starter-kit
(starter-kit-load "lisp")

;; activate misc-recommended configuration of starter-kit
(starter-kit-load "misc-recommended")

;; dash
(add-to-list 'load-path "~/.emacs.d/elpa/dash")

;; smartpatens
(defun my-smartparens-config ()
  (require 'smartparens-config)
  (smartparens-mode 1)
  (show-smartparens-global-mode t)
  )
  
;; aspell
(custom-set-variables
    '(ispell-dictionary "british")
    '(ispell-program-name "aspell"))
  
;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-gnome2)

;; evil mode
(require 'evil)
(evil-mode 1)

;; rst mode
(add-hook 'rst-mode-hook 'flyspell-mode)
(add-hook 'rst-mode-hook 'column-number-mode)
(add-hook 'rst-mode-hook (lambda () (my-smartparens-config)))

;; window numbering
(require 'window-numbering)
(window-numbering-mode 1)

;; auctex
(setq Tex-auto-save t)
(setq Tex-parse-self t)
(setq-default Tex-master nil)
(add-hook 'Latex-mode-hook 'visual-line-mode)
(add-hook 'Latex-mode-hook 'flyspell-mode)
(add-hook 'Latex-mode-hook 'Latex-math-mode)
(add-hook 'Latex-mode-hook 'true-on-reftex)
(add-hook 'Latex-mode-hook (lambda () (my-smartparens-config)))
(setq reftex-plug-into-AUCTeX t)

;; python
(add-hook 'python-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'python-mode-hook 'flyspell-prog-mode)

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

;; Set 78 column rule. 78 not 80 for that two spaces are spared to fill
;; continuation symbol
(setq default-fill-column 78)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; w3m
(require 'w3m-load)
(setq w3m-command "/usr/bin/w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

;; multi-eshell
(require 'multi-eshell)