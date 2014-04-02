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


