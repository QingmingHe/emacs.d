;; Keyboard Macro Editor.  Press C-c C-c to finish; press C-x k RET to cancel.
;; Original keys: / b r o a d r RET 2*j 0 w r 6 j o 2 9 3 . SPC 6 2*0 . SPC 9 2*0 . SPC 1 2 2*0 . SPC 1 5 2*0 . SPC 1 8 2*0 . <escape> j 2*d k 2*y / ^ \ ( u n r e s r \ | p u 2*r \ ) RET 2*j 0 w r 6 p j 2*d k 2*y / t h e r m r RET 2*j 0 3*w r 6 p j 2*d k 2*y / g r o u p r RET 2*j 0 5*W r 6 j p j 2*d / t e m p e r RET V / ^ 0 SPC * / RET y 6*P 3*w c $ 2 9 3 SPC K <escape> / t e m p e r RET w c $ 6 2*0 SPC K <escape> n w c $ 9 2*0 SPC K <escape> n w c $ 1 2 2*0 SPC K <escape> n w c $ 1 5 2*0 SPC K <escape> n w c $ 1 8 2*0 SPC K <escape> n V / w i m s r RET 2*k d / w i m s r RET 5*j 0 3*W r 6 SPC s s

Command: last-kbd-macro
Key: none

Macro:

g g
/			;; isearch-forward-regexp
b			;; evil-backward-word-begin
r			;; evil-replace
o			;; evil-open-below
a			;; evil-append
d			;; evil-delete
r			;; evil-replace
RET			;; evil-ret
2*j			;; evil-next-visual-line-dwim
0			;; move-beginning-of-line-dwim
w			;; evil-forward-word-begin
r			;; evil-replace
6			;; digit-argument
j			;; evil-next-visual-line-dwim
o			;; evil-open-below
2			;; digit-argument
9			;; digit-argument
3			;; digit-argument
.			;; evil-repeat
SPC 6
2*0			;; move-beginning-of-line-dwim
.			;; evil-repeat
SPC 9
2*0			;; move-beginning-of-line-dwim
.			;; evil-repeat
SPC 1
2			;; digit-argument
2*0			;; move-beginning-of-line-dwim
.			;; evil-repeat
SPC 1
5			;; digit-argument
2*0			;; move-beginning-of-line-dwim
.			;; evil-repeat
SPC 1
8			;; digit-argument
2*0			;; move-beginning-of-line-dwim
.			;; evil-repeat
<escape>		;; keyboard-quit
j			;; evil-next-visual-line-dwim
2*d			;; evil-delete
k			;; evil-previous-visual-line-dwim
2*y			;; evil-yank
/			;; isearch-forward-regexp
^			;; evil-first-non-blank
\			;; evil-execute-in-emacs-state
(			;; evil-backward-sentence-begin
 u			;; undo
 n			;; evil-search-next
 r			;; evil-replace
 e			;; evil-forward-word-end
 s			;; evil-substitute
 r			;; evil-replace
 \			;; evil-execute-in-emacs-state
 |			;; evil-goto-column
 p			;; evil-paste-after
 u			;; undo
 2*r			;; evil-replace
 \			;; evil-execute-in-emacs-state
 )			;; evil-forward-sentence-begin
RET			;; evil-ret
2*j			;; evil-next-visual-line-dwim
0			;; move-beginning-of-line-dwim
w			;; evil-forward-word-begin
r			;; evil-replace
6			;; digit-argument
p			;; evil-paste-after
j			;; evil-next-visual-line-dwim
2*d			;; evil-delete
k			;; evil-previous-visual-line-dwim
2*y			;; evil-yank
/			;; isearch-forward-regexp
t			;; evil-find-char-to
h			;; evil-backward-char
e			;; evil-forward-word-end
r			;; evil-replace
m			;; evil-set-marker
r			;; evil-replace
RET			;; evil-ret
2*j			;; evil-next-visual-line-dwim
0			;; move-beginning-of-line-dwim
3*w			;; evil-forward-word-begin
r			;; evil-replace
6			;; digit-argument
p			;; evil-paste-after
j			;; evil-next-visual-line-dwim
2*d			;; evil-delete
k			;; evil-previous-visual-line-dwim
2*y			;; evil-yank
/			;; isearch-forward-regexp
g r
o			;; evil-open-below
u			;; undo
p			;; evil-paste-after
r			;; evil-replace
RET			;; evil-ret
2*j			;; evil-next-visual-line-dwim
0			;; move-beginning-of-line-dwim
5*W			;; evil-forward-WORD-begin
r			;; evil-replace
6			;; digit-argument
j			;; evil-next-visual-line-dwim
p			;; evil-paste-after
j			;; evil-next-visual-line-dwim
2*d			;; evil-delete
/			;; isearch-forward-regexp
t			;; evil-find-char-to
e			;; evil-forward-word-end
m			;; evil-set-marker
p			;; evil-paste-after
e			;; evil-forward-word-end
r			;; evil-replace
RET			;; evil-ret
V			;; evil-visual-line
/			;; isearch-forward-regexp
^			;; evil-first-non-blank
0			;; move-beginning-of-line-dwim
SPC *
/			;; isearch-forward-regexp
RET			;; evil-ret
y			;; evil-yank
6*P			;; evil-paste-before
3*w			;; evil-forward-word-begin
c			;; evil-change
$			;; end-of-visual-line
2			;; digit-argument
9			;; digit-argument
3			;; digit-argument
SPC K
<escape>		;; keyboard-quit
/			;; isearch-forward-regexp
t			;; evil-find-char-to
e			;; evil-forward-word-end
m			;; evil-set-marker
p			;; evil-paste-after
e			;; evil-forward-word-end
r			;; evil-replace
RET			;; evil-ret
w			;; evil-forward-word-begin
c			;; evil-change
$			;; end-of-visual-line
6			;; digit-argument
2*0			;; move-beginning-of-line-dwim
SPC K
<escape>		;; keyboard-quit
n			;; evil-search-next
w			;; evil-forward-word-begin
c			;; evil-change
$			;; end-of-visual-line
9			;; digit-argument
2*0			;; move-beginning-of-line-dwim
SPC K
<escape>		;; keyboard-quit
n			;; evil-search-next
w			;; evil-forward-word-begin
c			;; evil-change
$			;; end-of-visual-line
1			;; digit-argument
2			;; digit-argument
2*0			;; move-beginning-of-line-dwim
SPC K
<escape>		;; keyboard-quit
n			;; evil-search-next
w			;; evil-forward-word-begin
c			;; evil-change
$			;; end-of-visual-line
1			;; digit-argument
5			;; digit-argument
2*0			;; move-beginning-of-line-dwim
SPC K
<escape>		;; keyboard-quit
n			;; evil-search-next
w			;; evil-forward-word-begin
c			;; evil-change
$			;; end-of-visual-line
1			;; digit-argument
8			;; digit-argument
2*0			;; move-beginning-of-line-dwim
SPC K
<escape>		;; keyboard-quit
n			;; evil-search-next
V			;; evil-visual-line
/			;; isearch-forward-regexp
w			;; evil-forward-word-begin
i			;; evil-insert
m			;; evil-set-marker
s			;; evil-substitute
r			;; evil-replace
RET			;; evil-ret
2*k			;; evil-previous-visual-line-dwim
d			;; evil-delete
SPC s s
