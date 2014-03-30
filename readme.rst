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


---------------------------------
Third Party Packages Dependencies
---------------------------------

* `Aspell`_ to check spell.

  * Needed by flyspell-mode;
  * Add **lang en** to **/etc/aspell.conf** to set default language to
    be English for Chinese OS.
    
* `Ctags`_ for code navigation.

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

.. _dash: https://github.com/magnars/dash.el
.. _flyspell: http://www-sop.inria.fr/members/Manuel.Serrano/flyspell/flyspell.html
.. _evil: https://gitorious.org/evil/pages/Home
.. _goagent: https://code.google.con/p/goagent/
.. _Aspell: https://aspell.net/
.. _Ctags: http://ctags.sourceforge.net/
