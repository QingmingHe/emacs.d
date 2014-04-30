(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute
 'default nil :font "Consolas")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Microsoft Yahei" :size 14)))

(scroll-bar-mode 0)
(column-number-mode 1)

(setq split-height-threshold nil)
(setq split-width-threshold 160)

(setq default-fill-column 78)
