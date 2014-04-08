-----
Ctags
-----

Use etags instead of ctags to generate TAGS, otherwise the TAGS file
will not be recognized by EMACS. For that Vim key binding is used
through evil mode, to get access to EMACS short cut, type **C-z** to
come back to EMACS first. EMACS short cut and command for tags are
listed below:

+-----------+--------------------------+--------------+
|key-binding|command                   |definition    |
+-----------+--------------------------+--------------+
|M-.        |M-x find-tag              |find a tag    |
|           |                          |              |
+-----------+--------------------------+--------------+
|C-u M.     |None or have not idea     |go to next    | 
|           |                          |match         |
+-----------+--------------------------+--------------+
|           |M-x find-tag-other-window |just what the |
|           |                          |command mean  |
+-----------+--------------------------+--------------+
|M-*        |M-x pop-tag-mark          |jump back     |
+-----------+--------------------------+--------------+
|           |M-x tags-search           |regexp-search |
|           |                          |though tag    |
|           |                          |file          |
+-----------+--------------------------+--------------+
|           |M-x list-tags             |list all tags |
|           |                          |defined in a  |
|           |                          |source file   |
+-----------+--------------------------+--------------+

----------
multi-term
----------

Eshell .vs. shell .vs. term .vs. multi-eshell .vs. multi-term. Why multi-term?

* IPthon can be used in multi-term and support completion;
* Create terminal easily;
* No more.

To kill process, *C-C* does not work and you have to kill it by command
*kill*. First find what you want to kill by **ps** and **grep**::

    ps | grep some-process

Then kill it::

    kill some-process-id

--------
Ropemacs
--------

Ropemacs is based on rope, ropemode and Pymacs. At first usage, emacs will 
inform to enter "rope project root folder", just enter ".ropeproject".

---------
w3m-emacs
---------

* **S**: search with default engine
* **a**: add to bookmark
* **d**: download url at point
* **B**: backward to history page
* **N**: inverse to **B**
* **C-c C-p**: w3m-previous-buffer
* **C-c C-n**: w3m-next-buffer

------
auctex
------

Latex Chinese Support for non Windows OS
----------------------------------------

* Install texlive with full scheme;
* Dowload fonts files or copy from /c/Windows/Fonts::

    cp /c/Windows/Fonts/{sim,SIM}* ~/.fonts

* **fc-cache** to update fonts;
* **fc-list :lang=zh** to view Chinese fonts;
* Write a Latex file for testing::

    \documentclass{ctexart}
    \begin{document}
    Some Chinese Words
    \end{document}
    
* Compile with **xelatex**;
* Error may occur that fonts not found. Open /usr/local/texlive/2013/texmf-dist/tex/latex/ctex/fontset/ctex-xecjk-winfonts.def
  and modify fonts name according to names viewed by **fc-list :lang=zh**
* Compile again and view *.pdf in EMACS.

------
python
------

IPython
-------

IPython could be used in cygwin EMACS shell if IPython is installed from
source. Never try to use Windows native IPython terminal in cygwin EMACS shell.

matplotlib
----------

You can never use the matplotlib 1.3 in cygwin and cygwin emacs. However,
there are ways to **dodge**. If you have Windows native Python and Matplotlib 
installed (say, through Pythonxy), write a python script *plot.py*::

    import numpy as np
    import matplotlib.pyplot as plt

    x = np.linspace(-np.pi, np.pi)
    y = np.sin(x)
    plt.plot(x, y)
    plt.show()
    
Then open a shell window in EMACS and key in::

    /path/to/windows/native/python plot.py

And you can enjoy the figure yourself. However, the author has no idea how to
use Matplotlib in EMACS Python terminal interactively.

------
Kmacro
------

Basic usage
-----------

**C-x (**: begin to define a keyboard macro;
**C-x )**: end the definition;
**C-u 10 C-x e**: execute last kmacro for 10 times.
