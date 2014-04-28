.. _SECTION-install:

-------
Install
-------

Download zip file and extract to ~/.emacs.d or use Git:

.. code-block:: sh

    git clone https://github.com/QingmingHe/emacs.d

and then move to ~/.emacs.d.

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
    
* `Ctags`_ for code navigation.
* `w3m`_ text-based web browser.
* `Pymacs`_, `rope`_, `ropemacs`_, `ropemode`_. The author has no idea the
  relationship between them.
* Never use packages listed above and use `jedi`_ instead, which is much
  faster for python completion. To install **jedi**, follow the instructions
  given in **jedi** 's home page.
* `Evince`_ for pdf preview.
* Fonts: Consolas for English and Microsoft Yahei for Chinese
  
.. _SECTION-bugs-and-problems:

----------------
Bugs or Problems
----------------

el-get
------

el-get can not install several packages like `dash`_, `flyspell`_,
`evil`_ and so on. To fix it, you have to download them manually and
add the path to emacs load path.

flyspell
--------

Add flyspell mode to other mode hook will cause error: "Autoloading
failed to define function turn-on-flyspell". You have to activate
flyspell mode manually.

eshell
------

Starter-kit-eshell does not work.

w3m
---

Sometimes error of *apply: Setting current directory: permission denied,
~/.w3m/* occurs and you should remove ~/.w3m.

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
