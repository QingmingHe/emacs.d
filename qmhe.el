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

;; activate Yasnippet configuration of starter-kit
(starter-kit-load "yasnippet")

;-------------------------------------------------------------------------------
;; smartpatens
;-------------------------------------------------------------------------------
(defun my-smartparens-config ()
  (require 'smartparens-config)
  (smartparens-mode 1)
  (show-smartparens-global-mode t)
  )

;-------------------------------------------------------------------------------
;; EMACS layout
;-------------------------------------------------------------------------------
(scroll-bar-mode 0)
(column-number-mode 1)
  
;-------------------------------------------------------------------------------
;; aspell
;-------------------------------------------------------------------------------
(custom-set-variables
    '(ispell-dictionary "british")
    '(ispell-program-name "aspell"))
  
;-------------------------------------------------------------------------------
;; color theme
;-------------------------------------------------------------------------------
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-blue)

;-------------------------------------------------------------------------------
;; undo-tree
;-------------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode)

;-------------------------------------------------------------------------------
;; evil mode
;-------------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)
(require 'goto-chg)

;-------------------------------------------------------------------------------
;; rst mode.
;-------------------------------------------------------------------------------
(add-hook 'rst-mode-hook (lambda ()
                           (flyspell-mode)
                           (my-smartparens-config)
                           (linum-mode)
                           (outline-minor-mode)
                           (setq outline-regexp "\.\. _\\(CHAPTER\\|SECTION\\|SUBSECTION\\)")
                           (hide-body)
                           ))

;-------------------------------------------------------------------------------
;; window numbering
;-------------------------------------------------------------------------------
(require 'window-numbering)
(window-numbering-mode 1)

;-------------------------------------------------------------------------------
;; auctex
;-------------------------------------------------------------------------------
(setq Tex-auto-save t)
(setq Tex-parse-self t)
(setq-default Tex-master nil)
(add-hook 'Latex-mode-hook 'visual-line-mode)
(add-hook 'Latex-mode-hook 'Latex-math-mode)
(add-hook 'Latex-mode-hook 'true-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; Use evince to view PDF files. However, inverse search is not supported.
;; Note that if Evince is opened in preview mode, M-SPACE, X to maximize and
;; M-<F4> to close
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("Evince" "evince --preview --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))
;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook 
    (lambda()
        (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --shell-escape --synctex=1 %(mode)%'  %t" TeX-run-TeX nil t))
        (setq TeX-command-default "XeLaTeX")
        (setq TeX-save-query nil)
        (setq TeX-show-compilation t)
        (linum-mode)
        (company-mode)
        (my-smartparens-config)
        (yas-minor-mode)
        (flyspell-mode)
        ))

;-------------------------------------------------------------------------------
;; python
;-------------------------------------------------------------------------------
(add-hook 'python-mode-hook (lambda ()
                              (my-smartparens-config)
                              (flyspell-prog-mode)
                              (linum-mode)
                              (outline-minor-mode)
                              (setq outline-regexp " *\\(def \\|class \\|if __name__\\)")
                              (hide-body)
                              ))
;; use jedi for completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;-------------------------------------------------------------------------------
;; emacs lisp
;-------------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook (lambda () (my-smartparens-config)))
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;-------------------------------------------------------------------------------
;; Set 78 column rule. 78 not 80 for that two spaces are spared to fill
;; continuation symbol
;-------------------------------------------------------------------------------
(setq default-fill-column 78)

;-------------------------------------------------------------------------------
;; smex
;-------------------------------------------------------------------------------
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;-------------------------------------------------------------------------------
;; w3m
;-------------------------------------------------------------------------------
(require 'w3m-load)
(setq w3m-command "/usr/bin/w3m")
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)
(setq w3m-home-page "https://www.google.com.hk")

;-------------------------------------------------------------------------------
;; company mode
;-------------------------------------------------------------------------------
(require 'company)
(setq company-idle-delay 0.2)

;-------------------------------------------------------------------------------
;; multi-eshell and multi-term
;-------------------------------------------------------------------------------
(require 'multi-eshell)
(require 'multi-term)
(setq multi-term-program "/usr/bin/bash")

;-------------------------------------------------------------------------------
;; archives for package install
;-------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;-------------------------------------------------------------------------------
;; Fortran
;-------------------------------------------------------------------------------
(require 'f90-interface-browser)
(add-hook 'f90-mode-hook '(lambda () 
                               (company-mode)
                               (my-smartparens-config)
                               (flyspell-prog-mode)
                               (linum-mode)
                               (outline-minor-mode)
                               (setq outline-regexp " *\\(function \\|subroutine \\|type ::\\|module \\|interface \\|program \\)")
                               (hide-body)
                          ))

;-------------------------------------------------------------------------------
;; Dictionary
;-------------------------------------------------------------------------------
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)

;-------------------------------------------------------------------------------
; org mode for gtd
;-------------------------------------------------------------------------------
(global-set-key "\C-cc" 'remember)
;; GTD templates
(org-remember-insinuate) 
(setq org-directory "~/docs/gtd/")
(setq org-remember-templates '(
("Task" ?t "** TODO %? \n   SCHEDULED: %T \n   %i" "~/docs/gtd/inbox.org" "Tasks")
("Daily" ?d "** %? \n   SCHEDULED: %T \n   %i" "~/docs/gtd/inbox.org" "Dailies") 
("Calendar" ?l "** %? \n   %T" "~/docs/gtd/inbox.org" "Calendar") 
("Project" ?p "** %? \n   SCHEDULED: %T \n   %i" "~/docs/gtd/inbox.org" "Projects"))) 
;; specify org agenda files
(setq org-agenda-files 
      (list "~/docs/gtd/inbox.org"
            "~/docs/gtd/projects.org"
            ))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
; Refile targets include this file and any file contributing to the agenda 
(setq org-refile-files
      (list "~/docs/gtd/inbox.org"
            "~/docs/gtd/projects.org"
            "~/docs/gtd/finished.org"
            "~/docs/gtd/canceled.org")
      )
(setq org-refile-targets (quote (
                                 (nil :maxlevel . 3)
                                 (org-refile-files :maxlevel . 3)
                                ; (org-agenda-files :maxlevel . 3)
                                 )))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)
;; find inbox file
(defun inbox() (interactive) (find-file "~/docs/gtd/inbox.org")) 
;; org mode hook
(add-hook 'org-mode-hook '(lambda () 
                               (my-smartparens-config)
                               (linum-mode)
                          ))
;; export org-mode into PDF
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-src-content-indentation 0
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      ;org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-export-odt-preferred-output-format "doc"
      org-tags-column 80
      ;org-startup-indented t
      )
;; Show agenda at startup
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)
