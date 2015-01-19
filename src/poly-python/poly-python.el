(require 'polymode)

;; 1. Define hostmode object
(defcustom pm-host/python
  (pm-bchunkmode "python"
                 :mode 'python-mode)
  "Python host chunkmode"
  :group 'hostmodes
  :type 'object)

;; 2. Define innermode object
(defcustom  pm-inner/rst
  (pm-hbtchunkmode "rst"
                   :head-reg "^[ \t]*\"\"\"{rst}$"
                   :tail-reg "^[ \t]*{rst}\"\"\"\"$"
                   :head-mode 'host
                   :tail-mode 'host
                   :font-lock-narrow t)
  "Markdown typical chunk."
  :group 'innermodes
  :type 'object)

;; 3. Define polymode object
(defcustom pm-poly/python+rst
  (pm-polymode-one "rst"
                   :hostmode 'pm-host/python
                   :innermode 'pm-inner/rst)
  "Python typical configuration"
  :group 'polymodes
  :type 'object)

;; 4. Define polymode function
(define-polymode poly-python-mode pm-poly/python+rst)

(provide 'poly-python)
