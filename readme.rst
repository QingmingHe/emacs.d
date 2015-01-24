.. _SECTION-install:

-------
Install
-------

Download zip file and extract to ~/.emacs.d or use Git:

.. code-block:: sh

    git clone https://github.com/QingmingHe/emacs.d
    mv Emacs.d .emacs.d

and then change directory to ~/.emacs.d.

.. note::

    * You may need use proxy such as `goagent`_ to clone/pull/push
      code from/to github.
    * To use git with goagent, add to **~/.gitconfig**::

        [http]
              sslVerify = false
              proxy = http://127.0.0.1:8087
              
    * If encounter **RPC failed ...**, add to **~/.gitconfig**::

        [http]
              postBuffer = 524288000
              
.. _SECTION-dependencies:              

---------------------------------
Third Party Packages Dependencies
---------------------------------

* `Aspell`_ to check spell.

  * Needed by flyspell-mode;
  * Add **lang en** to **/etc/aspell.conf** to set default language to
    be English for Chinese OS.
    
* `Ctags`_ and gtags for code navigation.
* `w3m`_ text-based web browser.
* `Evince`_ for pdf preview.
* Fonts: Consolas for English and Microsoft Yahei for Chinese
* `LaTeX`_ packages collections such as `TEX live`_  
* `Python`_, `IPython`_, flake8, jedi, numpy, h5py
* Tool chain for coding: cmake, make, automake, autoconf, gcc, clang,
  libclang, llvm, gdb
* Git for version control
* zip, unzip, unrar, xz
* wget, curl
* ssh
* tmux
* fontconfig, ghostscript
* antiword transforms *.doc
* xpdf for DocViewMode
* **perl** for updating tags file
* May be vim
* pkg-config
* sdcv command line dictionary
  
.. _SECTION-bugs-and-problems:

----------------
Bugs or Problems
----------------

flyspell
--------

Add flyspell mode to other mode hook will cause error: "Autoloading
failed to define function turn-on-flyspell". You have to activate
flyspell mode manually.

w3m
---

Sometimes error of *apply: Setting current directory: permission denied,
~/.w3m/* occurs and you should remove ~/.w3m.

Emacs-GTK
---------

May a bug of GTK or Emacs.

.. _dash: https://github.com/magnars/dash.el
.. _flyspell: http://www-sop.inria.fr/members/Manuel.Serrano/flyspell/flyspell.html
.. _evil: https://gitorious.org/evil/pages/Home
.. _goagent: https://code.google.con/p/goagent/
.. _Aspell: https://aspell.net/
.. _Ctags: http://ctags.sourceforge.net/
.. _w3m: http://w3m.sourceforge.net/
.. _ropemacs: https://pypi.python.org/pypi/ropemacs
.. _rope: http://rope.sourceforge.net/
.. _ropemode: https://pypi.python.org/pypi/ropemode
.. _Pymacs: https://github.com/pinard/Pymacs
.. _jedi: http://tkf.github.io/emacs-jedi/latest/#pyinstall
.. _Evince: https://wiki.gnome.org/Apps/Evince  
.. _python: https://www.python.org/
.. _IPython: http://ipython.org/
.. _LaTeX: http://www.latex-project.org/
.. _TEX live: https://www.tug.org/texlive/
