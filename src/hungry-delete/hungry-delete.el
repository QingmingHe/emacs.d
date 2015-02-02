;;; hungry-delete.el --- Delete preceding white spaces

;; Copyright (c) 2015 Qingming He <906459647@qq.com>

;; Author: Qingming He
;; Version: 0.1
;; Package-Requires: ()

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Installation
;; Configure your load-path and call require
;; (require 'hungry-delete)

;; Define key map for several major modes
;; (mapc
;;  (lambda (map)
;;    (define-key map [backspace] 'hungry-backspace))
;;  `(,prog-mode-map
;;    ,org-mode-map
;;    ,text-mode-map))
;; It's risky to define global-map!

;;; Code:

(defun hungry-backspace-python (arg)
  (interactive "*P")
  (let (n)
    (save-excursion
      (setq n (- (skip-chars-backward " \t"))))
    (cond ((= 0 n)
           (delete-backward-char 1))
          ((looking-back "^ +")
           (if (nth 3 (syntax-ppss))
               (delete-backward-char n)
             (if (= 0 (mod n python-indent))
                 (delete-backward-char python-indent)
               (delete-backward-char (mod n python-indent)))))
          (t (delete-backward-char n)))))

(defun hungry-backspace-default (arg)
  (interactive "*P")
  (let (n)
    (save-excursion
      (setq n (- (skip-chars-backward " \t"))))
    (cond ((= 0 n) (delete-backward-char 1))
          (t (delete-backward-char n)))))

(defun hungry-backspace (arg)
  (interactive "*P")
  (cond (buffer-read-only (scroll-down-command))
        ((and (featurep 'evil)
              (evil-normal-state-p))
         (backward-char 1))
        ((eq major-mode 'python-mode)
         (hungry-backspace-python arg))
        (t (hungry-backspace-default arg))))

(provide 'hungry-delete)

;;; hungry-delete.el ends here
