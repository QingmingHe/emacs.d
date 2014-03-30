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
