;;; change-wimsn-temp.el --- Used to change temperature of njoy input  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Qingming He

;; Author: Qingming He <906459647@qq.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Change njoy input temperature to 293, 600, 900, 1200, 1500 and 1800.
;; To use it, `edit-last-kbd-macro' and replace the text with codes below. Then
;; key in C-c C-c.

;;; Code:

;; Keyboard Macro Editor.  Press C-c C-c to finish; press C-x k RET to cancel.
;; Original keys: 2*g / b r o a d r RET 2*j 0 w r 6 j o 2 9 3 . SPC 6 2*0 . SPC 9 2*0 . SPC 1 2 2*0 . SPC 1 5 2*0 . SPC 1 8 2*0 . ESC j 2*d k 2*y 2*g / ^ \ ( u n r e s r \ | p u 2*r \ ) RET 2*j 0 w r 6 p j 2*d k 2*y 2*g / t h e r m r RET 2*j 0 3*w r 6 p j 2*d k 2*y 2*g / g r o u p r RET 2*j 0 5*W r 6 j p j 2*d k 2*y 2*C-f 5*k 2*g / g r o u p r RET / t e m p e r a t u r e RET 2*z V / 0 DEL ^ 0 SPC * / RET y / w i m s r RET 2*k p k p k p k p k p k p k V 2*g / T e m p e r a t u r e RET d 2*g / RET t e m p e r a t u r e RET w c $ 2 9 3 . K ESC / RET t e m p e r a t u r e RET w c $ 6 2*0 . K ESC / RET t e m p e r a t u r e RET w c $ 9 2*0 . K ESC / RET t e m p e r a t u r e RET w c $ 1 2 2*0 . K ESC / RET t e m p e r a t u r e RET w c $ 1 5 2*0 . K ESC / RET t e m p e r a t u r e RET w c $ 1 8 2*0 . K ESC SPC 2*s

Command: last-kbd-macro
Key: none

Macro:

2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
b			;; diredp-byte-compile-this-file
r			;; diredp-rename-this-file
o			;; dired-find-file-other-window
a			;; dired-find-alternate-file
d			;; dired-flag-file-deletion
r			;; diredp-rename-this-file
RET
2*j
0			;; digit-argument
w			;; dired-copy-filename-as-kill
r			;; diredp-rename-this-file
6			;; digit-argument
j
o			;; dired-find-file-other-window
2			;; digit-argument
9			;; digit-argument
3			;; digit-argument
.			;; dired-clean-directory
SPC			;; diredp-next-line
6			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
SPC			;; diredp-next-line
9			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
SPC			;; diredp-next-line
1			;; digit-argument
2			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
SPC			;; diredp-next-line
1			;; digit-argument
5			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
SPC			;; diredp-next-line
1			;; digit-argument
8			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
ESC
j
2*d			;; dired-flag-file-deletion
k			;; dired-do-kill-lines
2*y			;; diredp-relsymlink-this-file
2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
^
\			;; self-insert-command
(			;; dired-hide-details-mode
 u			;; dired-unmark
 n			;; dired-move-to-next-or-first
 r			;; diredp-rename-this-file
 e			;; dired-find-file
 s			;; dired-sort-toggle-or-edit
 r			;; diredp-rename-this-file
 \			;; self-insert-command
 |			;; self-insert-command
 p			;; dired-move-to-previous-or-last
 u			;; dired-unmark
 2*r			;; diredp-rename-this-file
 \			;; self-insert-command
 )			;; self-insert-command
RET
2*j
0			;; digit-argument
w			;; dired-copy-filename-as-kill
r			;; diredp-rename-this-file
6			;; digit-argument
p			;; dired-move-to-previous-or-last
j
2*d			;; dired-flag-file-deletion
k			;; dired-do-kill-lines
2*y			;; diredp-relsymlink-this-file
2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
t			;; dired-toggle-marks
h			;; describe-mode
e			;; dired-find-file
r			;; diredp-rename-this-file
m			;; dired-mark
r			;; diredp-rename-this-file
RET
2*j
0			;; digit-argument
3*w			;; dired-copy-filename-as-kill
r			;; diredp-rename-this-file
6			;; digit-argument
p			;; dired-move-to-previous-or-last
j
2*d			;; dired-flag-file-deletion
k			;; dired-do-kill-lines
2*y			;; diredp-relsymlink-this-file
2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
g			;; revert-buffer
r			;; diredp-rename-this-file
o			;; dired-find-file-other-window
u			;; dired-unmark
p			;; dired-move-to-previous-or-last
r			;; diredp-rename-this-file
RET
2*j
0			;; digit-argument
5*W			;; woman-dired-find-file
r			;; diredp-rename-this-file
6			;; digit-argument
j
p			;; dired-move-to-previous-or-last
j
2*d			;; dired-flag-file-deletion
k			;; dired-do-kill-lines
2*y			;; diredp-relsymlink-this-file
2*C-f			;; scroll-up-command
5*k			;; dired-do-kill-lines
2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
g			;; revert-buffer
r			;; diredp-rename-this-file
o			;; dired-find-file-other-window
u			;; dired-unmark
p			;; dired-move-to-previous-or-last
r			;; diredp-rename-this-file
RET
/			;; dired-open-w32-prog-at-point
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
2*z			;; diredp-compress-this-file
V			;; dired-do-run-mail
/			;; dired-open-w32-prog-at-point
0			;; digit-argument
DEL			;; dired-unmark-backward
^
0			;; digit-argument
SPC			;; diredp-next-line
* /			;; dired-mark-directories
RET
y			;; diredp-relsymlink-this-file
/			;; dired-open-w32-prog-at-point
w			;; dired-copy-filename-as-kill
i			;; dired-maybe-insert-subdir
m			;; dired-mark
s			;; dired-sort-toggle-or-edit
r			;; diredp-rename-this-file
RET
2*k			;; dired-do-kill-lines
p			;; dired-move-to-previous-or-last
k			;; dired-do-kill-lines
p			;; dired-move-to-previous-or-last
k			;; dired-do-kill-lines
p			;; dired-move-to-previous-or-last
k			;; dired-do-kill-lines
p			;; dired-move-to-previous-or-last
k			;; dired-do-kill-lines
p			;; dired-move-to-previous-or-last
k			;; dired-do-kill-lines
p			;; dired-move-to-previous-or-last
k			;; dired-do-kill-lines
V			;; dired-do-run-mail
2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
T			;; dired-tar-tar-untar
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
d			;; dired-flag-file-deletion
2*g			;; revert-buffer
/			;; dired-open-w32-prog-at-point
RET
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
w			;; dired-copy-filename-as-kill
c			;; self-insert-command
$			;; diredp-hide-subdir-nomove
2			;; digit-argument
9			;; digit-argument
3			;; digit-argument
.			;; dired-clean-directory
K			;; self-insert-command
ESC
/			;; dired-open-w32-prog-at-point
RET
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
w			;; dired-copy-filename-as-kill
c			;; self-insert-command
$			;; diredp-hide-subdir-nomove
6			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
K			;; self-insert-command
ESC
/			;; dired-open-w32-prog-at-point
RET
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
w			;; dired-copy-filename-as-kill
c			;; self-insert-command
$			;; diredp-hide-subdir-nomove
9			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
K			;; self-insert-command
ESC
/			;; dired-open-w32-prog-at-point
RET
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
w			;; dired-copy-filename-as-kill
c			;; self-insert-command
$			;; diredp-hide-subdir-nomove
1			;; digit-argument
2			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
K			;; self-insert-command
ESC
/			;; dired-open-w32-prog-at-point
RET
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
w			;; dired-copy-filename-as-kill
c			;; self-insert-command
$			;; diredp-hide-subdir-nomove
1			;; digit-argument
5			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
K			;; self-insert-command
ESC
/			;; dired-open-w32-prog-at-point
RET
t			;; dired-toggle-marks
e			;; dired-find-file
m			;; dired-mark
p			;; dired-move-to-previous-or-last
e			;; dired-find-file
r			;; diredp-rename-this-file
a			;; dired-find-alternate-file
t			;; dired-toggle-marks
u			;; dired-unmark
r			;; diredp-rename-this-file
e			;; dired-find-file
RET
w			;; dired-copy-filename-as-kill
c			;; self-insert-command
$			;; diredp-hide-subdir-nomove
1			;; digit-argument
8			;; digit-argument
2*0			;; digit-argument
.			;; dired-clean-directory
K			;; self-insert-command
ESC
SPC			;; diredp-next-line
2*s			;; dired-sort-toggle-or-edit

;;; change-wimsn-temp.el ends here
