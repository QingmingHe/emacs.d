; Emacs
; #IfWinActive ahk_class Emacs
; +Capslock::Capslock
; Capslock::Control
; #IfWinActive

; Firefox
; #IfWinActive ahk_class MozillaWindowClass
; +Capslock::Capslock
; Capslock::Control
; #IfWinActive

SetTitleMatchMode, 2
#IfWinNotActive, Oracle VM VirtualBox
Control::Capslock
Capslock::Control
#IfWinNotActive
