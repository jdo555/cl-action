(defpackage #:cl-action/wvk
  (:use #:common-lisp)
  (:export #:+md-xbutton1
           #:+md-xbutton2
           #:+me-move+
           #:+me-leftdown+
           #:+me-leftup+
           #:+me-rightdown+
           #:+me-rightup+
           #:+me-middledown+
           #:+me-middleup+
           #:+me-xdown+
           #:+me-xup+
           #:+me-wheel+
           #:+me-hwheel+
           #:+me-move+
           #:+me-virtualdesk+
           #:+me-absolute+
           #:*me-press-to-release*
           #:+ka-extendedkey+
           #:+ka-keyup+
           #:+ka-unicode+
           #:+ka-scancode+
           #:+vk-lbutton+
           #:+vk-rbutton+
           #:+vk-cancel+
           #:+vk-mbutton+
           #:+vk-xbutton1+
           #:+vk-xbutton2+
           #:+vk-back+
           #:+vk-tab+
           #:+vk-clear+
           #:+vk-return+
           #:+vk-shift+
           #:+vk-control+
           #:+vk-menu+
           #:+vk-pause+
           #:+vk-capital+
           #:+vk-escape+
           #:+vk-convert+
           #:+vk-nonconvert+
           #:+vk-accept+
           #:+vk-modechange+
           #:+vk-space+
           #:+vk-prior+
           #:+vk-next+
           #:+vk-end+
           #:+vk-home+
           #:+vk-left+
           #:+vk-up+
           #:+vk-right+
           #:+vk-down+
           #:+vk-select+
           #:+vk-print+
           #:+vk-execute+
           #:+vk-snapshot+
           #:+vk-insert+
           #:+vk-delete+
           #:+vk-help+
           #:+vk-0+
           #:+vk-1+
           #:+vk-2+
           #:+vk-3+
           #:+vk-4+
           #:+vk-5+
           #:+vk-6+
           #:+vk-7+
           #:+vk-8+
           #:+vk-9+
           #:+vk-a+
           #:+vk-b+
           #:+vk-c+
           #:+vk-d+
           #:+vk-e+
           #:+vk-f+
           #:+vk-g+
           #:+vk-h+
           #:+vk-i+
           #:+vk-j+
           #:+vk-k+
           #:+vk-l+
           #:+vk-m+
           #:+vk-n+
           #:+vk-o+
           #:+vk-p+
           #:+vk-q+
           #:+vk-r+
           #:+vk-s+
           #:+vk-t+
           #:+vk-u+
           #:+vk-v+
           #:+vk-w+
           #:+vk-x+
           #:+vk-y+
           #:+vk-z+
           #:+vk-lwin+
           #:+vk-rwin+
           #:+vk-apps+
           #:+vk-sleep+
           #:+vk-numpad0+
           #:+vk-numpad1+
           #:+vk-numpad2+
           #:+vk-numpad3+
           #:+vk-numpad4+
           #:+vk-numpad5+
           #:+vk-numpad6+
           #:+vk-numpad7+
           #:+vk-numpad8+
           #:+vk-numpad9+
           #:+vk-multiply+
           #:+vk-add+
           #:+vk-separator+
           #:+vk-subtract+
           #:+vk-decimal+
           #:+vk-divide+
           #:+vk-f1+
           #:+vk-f2+
           #:+vk-f3+
           #:+vk-f4+
           #:+vk-f5+
           #:+vk-f6+
           #:+vk-f7+
           #:+vk-f8+
           #:+vk-f9+
           #:+vk-f10+
           #:+vk-f11+
           #:+vk-f12+
           #:+vk-f13+
           #:+vk-f14+
           #:+vk-f15+
           #:+vk-f16+
           #:+vk-f17+
           #:+vk-f18+
           #:+vk-f19+
           #:+vk-f20+
           #:+vk-f21+
           #:+vk-f22+
           #:+vk-f23+
           #:+vk-f24+
           #:+vk-numlock+
           #:+vk-scroll+
           #:+vk-lshift+
           #:+vk-rshift+
           #:+vk-lcontrol+
           #:+vk-rcontrol+
           #:+vk-lmenu+
           #:+vk-rmenu+
           #:+vk-browser-back+
           #:+vk-browser-forward+
           #:+vk-browser-refresh+
           #:+vk-browser-stop+
           #:+vk-browser-search+
           #:+vk-browser-favorites+
           #:+vk-browser-home+
           #:+vk-volume-mute+
           #:+vk-volume-down+
           #:+vk-volume-up+
           #:+vk-media-next-track+
           #:+vk-media-prev-track+
           #:+vk-media-stop+
           #:+vk-media-play-pause+
           #:+vk-launch-mail+
           #:+vk-launch-media-select+
           #:+vk-launch-app1+
           #:+vk-launch-app2+
           #:+vk-oem-1+
           #:+vk-oem-plus+
           #:+vk-oem-comma+
           #:+vk-oem-minus+
           #:+vk-oem-period+
           #:+vk-oem-2+
           #:+vk-oem-3+
           #:+vk-oem-4+
           #:+vk-oem-5+
           #:+vk-oem-6+
           #:+vk-oem-7+
           #:+vk-oem-102+
           #:+vk-packet+
           #:*base-character-ht*
           #:*shiftable-character-ht*))
(in-package #:cl-action/wvk)

(defparameter +md-xbutton1 "Set if the first X button is pressed or released.")
(defparameter +md-xbutton2 "Set if the second X button is pressed or released.")

(defparameter +me-move+ 1 "Movement occurred.")
(defparameter +me-leftdown+ 2 "The left button was pressed.")
(defparameter +me-leftup+ 4 "The left button was released.")
(defparameter +me-rightdown+ 8 "The right button was pressed.")
(defparameter +me-rightup+ 16 "The right button was released.")
(defparameter +me-middledown+ 32 "The middle button was pressed.")
(defparameter +me-middleup+ 64 "The middle button was released.")
(defparameter +me-xdown+ 128 "An X button was pressed.")
(defparameter +me-xup+ 256 "An X button was released.")
(defparameter +me-wheel+ 2048 "The wheel was moved, if the mouse has a wheel. The amount of movement is specified in mouseData.")
(defparameter +me-hwheel+ 4096 "The wheel was moved horizontally, if the mouse has a wheel. The amount of movement is specified in mouseData. Windows XP/2000: This value is not supported.")
(defparameter +me-move-no-coalesce+ 8192 "The WM_MOUSEMOVE messages will not be coalesced. The default behavior is to coalesce WM_MOUSEMOVE messages. Windows XP/2000: This value is not supported.")
(defparameter +me-virtualdesk+ 16384 "Maps coordinates to the entire desktop. Must be used with MOUSEEVENTF_ABSOLUTE.")
(defparameter +me-absolute+ 32768 "The dx and dy members contain normalized absolute coordinates. If the flag is not set, dxand dy contain relative data (the change in position since the last reported position). This flag can be set, or not set, regardless of what kind of mouse or other pointing device, if any, is connected to the system. For further information about relative mouse motion, see the following Remarks section.")

(defparameter *me-press-to-release* (make-hash-table))
(setf (gethash +me-leftdown+ *me-press-to-release*) +me-leftup+)
(setf (gethash +me-rightdown+ *me-press-to-release*) +me-rightup+)
(setf (gethash +me-middledown+ *me-press-to-release*) +me-middleup+)
;;(setf (gethash +me-xdown+ *me-press-to-release*) +me-xup+)

(defparameter +ka-extendedkey+ 1 "If specified, the scan code was preceded by a prefix byte that has the value 0xE0 (224).")
(defparameter +ka-keyup+ 2 "If specified, the key is being released. If not specified, the key is being pressed.")
(defparameter +ka-unicode+ 4 "If specified, the system synthesizes a VK_PACKET keystroke. The wVk parameter must be zero. This flag can only be combined with the KEYEVENTF_KEYUP flag. For more information, see the Remarks section.")
(defparameter +ka-scancode+ 8 "If specified, wScan identifies the key and wVk is ignored.")

(defparameter +vk-lbutton+ 1 "Left mouse button")
(defparameter +vk-rbutton+ 2 "Right mouse button")
(defparameter +vk-cancel+ 3 "Control-break processing")
(defparameter +vk-mbutton+ 4 "Middle mouse button (three-button mouse)")
(defparameter +vk-xbutton1+ 5 "X1 mouse button")
(defparameter +vk-xbutton2+ 6 "X2 mouse button")
(defparameter +vk-back+ 8 "BACKSPACE key")
(defparameter +vk-tab+ 9 "TAB key")
(defparameter +vk-clear+ 12 "CLEAR key")
(defparameter +vk-return+ 13 "ENTER key")
(defparameter +vk-shift+ 16 "SHIFT key")
(defparameter +vk-control+ 17 "CTRL key")
(defparameter +vk-menu+ 18 "ALT key")
(defparameter +vk-pause+ 19 "PAUSE key")
(defparameter +vk-capital+ 20 "CAPS LOCK key")
(defparameter +vk-escape+ 27 "ESC key")
(defparameter +vk-convert+ 28 "IME convert")
(defparameter +vk-nonconvert+ 29 "IME nonconvert")
(defparameter +vk-accept+ 30 "IME accept")
(defparameter +vk-modechange+ 31 "IME mode change request")
(defparameter +vk-space+ 32 "SPACEBAR")
(defparameter +vk-prior+ 33 "PAGE UP key")
(defparameter +vk-next+ 34 "PAGE DOWN key")
(defparameter +vk-end+ 35 "END key")
(defparameter +vk-home+ 36 "HOME key")
(defparameter +vk-left+ 37 "LEFT ARROW key")
(defparameter +vk-up+ 38 "UP ARROW key")
(defparameter +vk-right+ 39 "RIGHT ARROW key")
(defparameter +vk-down+ 40 "DOWN ARROW key")
(defparameter +vk-select+ 41 "SELECT key")
(defparameter +vk-print+ 42 "PRINT key")
(defparameter +vk-execute+ 43 "EXECUTE key")
(defparameter +vk-snapshot+ 44 "PRINT SCREEN key")
(defparameter +vk-insert+ 45 "INS key")
(defparameter +vk-delete+ 46 "DEL key")
(defparameter +vk-help+ 47 "HELP key")
(defparameter +vk-0+ 48 "0 key")
(defparameter +vk-1+ 49 "1 key")
(defparameter +vk-2+ 50 "2 key")
(defparameter +vk-3+ 51 "3 key")
(defparameter +vk-4+ 52 "4 key")
(defparameter +vk-5+ 53 "5 key")
(defparameter +vk-6+ 54 "6 key")
(defparameter +vk-7+ 55 "7 key")
(defparameter +vk-8+ 56 "8 key")
(defparameter +vk-9+ 57 "9 key")
(defparameter +vk-a+ 65 "A key")
(defparameter +vk-b+ 66 "B key")
(defparameter +vk-c+ 67 "C key")
(defparameter +vk-d+ 68 "D key")
(defparameter +vk-e+ 69 "E key")
(defparameter +vk-f+ 70 "F key")
(defparameter +vk-g+ 71 "G key")
(defparameter +vk-h+ 72 "H key")
(defparameter +vk-i+ 73 "I key")
(defparameter +vk-j+ 74 "J key")
(defparameter +vk-k+ 75 "K key")
(defparameter +vk-l+ 76 "L key")
(defparameter +vk-m+ 77 "M key")
(defparameter +vk-n+ 78 "N key")
(defparameter +vk-o+ 79 "O key")
(defparameter +vk-p+ 80 "P key")
(defparameter +vk-q+ 81 "Q key")
(defparameter +vk-r+ 82 "R key")
(defparameter +vk-s+ 83 "S key")
(defparameter +vk-t+ 84 "T key")
(defparameter +vk-u+ 85 "U key")
(defparameter +vk-v+ 86 "V key")
(defparameter +vk-w+ 87 "W key")
(defparameter +vk-x+ 88 "X key")
(defparameter +vk-y+ 89 "Y key")
(defparameter +vk-z+ 90 "Z key")
(defparameter +vk-lwin+ 91 "Left Windows key (Natural keyboard)")
(defparameter +vk-rwin+ 92 "Right Windows key (Natural keyboard)")
(defparameter +vk-apps+ 93 "Applications key (Natural keyboard)")
(defparameter +vk-sleep+ 95 "Computer Sleep key")
(defparameter +vk-numpad0+ 96 "Numeric keypad 0 key")
(defparameter +vk-numpad1+ 97 "Numeric keypad 1 key")
(defparameter +vk-numpad2+ 98 "Numeric keypad 2 key")
(defparameter +vk-numpad3+ 99 "Numeric keypad 3 key")
(defparameter +vk-numpad4+ 100 "Numeric keypad 4 key")
(defparameter +vk-numpad5+ 101 "Numeric keypad 5 key")
(defparameter +vk-numpad6+ 102 "Numeric keypad 6 key")
(defparameter +vk-numpad7+ 103 "Numeric keypad 7 key")
(defparameter +vk-numpad8+ 104 "Numeric keypad 8 key")
(defparameter +vk-numpad9+ 105 "Numeric keypad 9 key")
(defparameter +vk-multiply+ 106 "Multiply key")
(defparameter +vk-add+ 107 "Add key")
(defparameter +vk-separator+ 108 "Separator key")
(defparameter +vk-subtract+ 109 "Subtract key")
(defparameter +vk-decimal+ 110 "Decimal key")
(defparameter +vk-divide+ 111 "Divide key")
(defparameter +vk-f1+ 112 "F1 key")
(defparameter +vk-f2+ 113 "F2 key")
(defparameter +vk-f3+ 114 "F3 key")
(defparameter +vk-f4+ 115 "F4 key")
(defparameter +vk-f5+ 116 "F5 key")
(defparameter +vk-f6+ 117 "F6 key")
(defparameter +vk-f7+ 118 "F7 key")
(defparameter +vk-f8+ 119 "F8 key")
(defparameter +vk-f9+ 120 "F9 key")
(defparameter +vk-f10+ 121 "F10 key")
(defparameter +vk-f11+ 122 "F11 key")
(defparameter +vk-f12+ 123 "F12 key")
(defparameter +vk-f13+ 124 "F13 key")
(defparameter +vk-f14+ 125 "F14 key")
(defparameter +vk-f15+ 126 "F15 key")
(defparameter +vk-f16+ 127 "F16 key")
(defparameter +vk-f17+ 128 "F17 key")
(defparameter +vk-f18+ 129 "F18 key")
(defparameter +vk-f19+ 130 "F19 key")
(defparameter +vk-f20+ 131 "F20 key")
(defparameter +vk-f21+ 132 "F21 key")
(defparameter +vk-f22+ 133 "F22 key")
(defparameter +vk-f23+ 134 "F23 key")
(defparameter +vk-f24+ 135 "F24 key")
(defparameter +vk-numlock+ 144 "NUM LOCK key")
(defparameter +vk-scroll+ 145 "SCROLL LOCK key")
(defparameter +vk-lshift+ 160 "Left SHIFT key")
(defparameter +vk-rshift+ 161 "Right SHIFT key")
(defparameter +vk-lcontrol+ 162 "Left CONTROL key")
(defparameter +vk-rcontrol+ 163 "Right CONTROL key")
(defparameter +vk-lmenu+ 164 "Left MENU key")
(defparameter +vk-rmenu+ 165 "Right MENU key")
(defparameter +vk-browser-back+ 166 "Browser Back key")
(defparameter +vk-browser-forward+ 167 "Browser Forward key")
(defparameter +vk-browser-refresh+ 168 "Browser Refresh key")
(defparameter +vk-browser-stop+ 169 "Browser Stop key")
(defparameter +vk-browser-search+ 170 "Browser Search key")
(defparameter +vk-browser-favorites+ 171 "Browser Favorites key")
(defparameter +vk-browser-home+ 172 "Browser Start and Home key")
(defparameter +vk-volume-mute+ 173 "Volume Mute key")
(defparameter +vk-volume-down+ 174 "Volume Down key")
(defparameter +vk-volume-up+ 175 "Volume Up key")
(defparameter +vk-media-next-track+ 176 "Next Track key")
(defparameter +vk-media-prev-track+ 177 "Previous Track key")
(defparameter +vk-media-stop+ 178 "Stop Media key")
(defparameter +vk-media-play-pause+ 179 "Play/Pause Media key")
(defparameter +vk-launch-mail+ 180 "Start Mail key")
(defparameter +vk-launch-media-select+ 181 "Select Media key")
(defparameter +vk-launch-app1+ 182 "Start Application 1 key")
(defparameter +vk-launch-app2+ 183 "Start Application 2 key")
(defparameter +vk-oem-1+ 186 "For the US standard keyboard, the ';:' key")
(defparameter +vk-oem-plus+ 187 "For any country/region, the '+' key")
(defparameter +vk-oem-comma+ 188 "For any country/region, the ',' key")
(defparameter +vk-oem-minus+ 189 "For any country/region, the '-' key")
(defparameter +vk-oem-period+ 190 "For any country/region, the '.' key")
(defparameter +vk-oem-2+ 191 "For the US standard keyboard, the '/?' key")
(defparameter +vk-oem-3+ 192 "For the US standard keyboard, the '`~' key")
(defparameter +vk-oem-4+ 219 "For the US standard keyboard, the '[{' key")
(defparameter +vk-oem-5+ 220 "For the US standard keyboard, the '\|' key")
(defparameter +vk-oem-6+ 221 "For the US standard keyboard, the ']}' key")
(defparameter +vk-oem-7+ 222 "For the US standard keyboard, the 'single-quote/double-quote' key")
(defparameter +vk-oem-102+ 226 "Either the angle bracket key or the backslash key on the RT 102-key keyboard")
(defparameter +vk-packet+ 231 "Used to pass Unicode characters as if they were keystrokes")

(defparameter *base-character-ht* (make-hash-table))
(setf (gethash #\a *base-character-ht*) +vk-a+)
(setf (gethash #\b *base-character-ht*) +vk-b+)
(setf (gethash #\c *base-character-ht*) +vk-c+)
(setf (gethash #\d *base-character-ht*) +vk-d+)
(setf (gethash #\e *base-character-ht*) +vk-e+)
(setf (gethash #\f *base-character-ht*) +vk-f+)
(setf (gethash #\g *base-character-ht*) +vk-g+)
(setf (gethash #\h *base-character-ht*) +vk-h+)
(setf (gethash #\i *base-character-ht*) +vk-i+)
(setf (gethash #\j *base-character-ht*) +vk-j+)
(setf (gethash #\k *base-character-ht*) +vk-k+)
(setf (gethash #\l *base-character-ht*) +vk-l+)
(setf (gethash #\m *base-character-ht*) +vk-m+)
(setf (gethash #\n *base-character-ht*) +vk-n+)
(setf (gethash #\o *base-character-ht*) +vk-o+)
(setf (gethash #\p *base-character-ht*) +vk-p+)
(setf (gethash #\q *base-character-ht*) +vk-q+)
(setf (gethash #\r *base-character-ht*) +vk-r+)
(setf (gethash #\s *base-character-ht*) +vk-s+)
(setf (gethash #\t *base-character-ht*) +vk-t+)
(setf (gethash #\u *base-character-ht*) +vk-u+)
(setf (gethash #\v *base-character-ht*) +vk-v+)
(setf (gethash #\w *base-character-ht*) +vk-w+)
(setf (gethash #\x *base-character-ht*) +vk-x+)
(setf (gethash #\y *base-character-ht*) +vk-y+)
(setf (gethash #\z *base-character-ht*) +vk-z+)
(setf (gethash #\` *base-character-ht*) +vk-oem-3+)
(setf (gethash #\1 *base-character-ht*) +vk-1+)
(setf (gethash #\2 *base-character-ht*) +vk-2+)
(setf (gethash #\3 *base-character-ht*) +vk-3+)
(setf (gethash #\4 *base-character-ht*) +vk-4+)
(setf (gethash #\5 *base-character-ht*) +vk-5+)
(setf (gethash #\6 *base-character-ht*) +vk-6+)
(setf (gethash #\7 *base-character-ht*) +vk-7+)
(setf (gethash #\8 *base-character-ht*) +vk-8+)
(setf (gethash #\9 *base-character-ht*) +vk-9+)
(setf (gethash #\0 *base-character-ht*) +vk-0+)
(setf (gethash #\- *base-character-ht*) +vk-oem-minus+)
(setf (gethash #\= *base-character-ht*) +vk-oem-plus+)
(setf (gethash #\[ *base-character-ht*) +vk-oem-4+)
(setf (gethash #\] *base-character-ht*) +vk-oem-6+)
(setf (gethash #\\ *base-character-ht*) +vk-oem-5+)
(setf (gethash #\; *base-character-ht*) +vk-oem-1+)
(setf (gethash #\' *base-character-ht*) +vk-oem-7+)
(setf (gethash #\, *base-character-ht*) +vk-oem-comma+)
(setf (gethash #\. *base-character-ht*) +vk-oem-period+)
(setf (gethash #\/ *base-character-ht*) +vk-oem-2+)
(setf (gethash #\space *base-character-ht*) +vk-space+)
(setf (gethash #\Newline *base-character-ht*) +vk-return+)

(defparameter *shiftable-character-ht* (make-hash-table))
(setf (gethash #\A *shiftable-character-ht*) +vk-a+)
(setf (gethash #\B *shiftable-character-ht*) +vk-b+)
(setf (gethash #\C *shiftable-character-ht*) +vk-c+)
(setf (gethash #\D *shiftable-character-ht*) +vk-d+)
(setf (gethash #\E *shiftable-character-ht*) +vk-e+)
(setf (gethash #\F *shiftable-character-ht*) +vk-f+)
(setf (gethash #\G *shiftable-character-ht*) +vk-g+)
(setf (gethash #\H *shiftable-character-ht*) +vk-h+)
(setf (gethash #\I *shiftable-character-ht*) +vk-i+)
(setf (gethash #\J *shiftable-character-ht*) +vk-j+)
(setf (gethash #\K *shiftable-character-ht*) +vk-k+)
(setf (gethash #\L *shiftable-character-ht*) +vk-l+)
(setf (gethash #\M *shiftable-character-ht*) +vk-m+)
(setf (gethash #\N *shiftable-character-ht*) +vk-n+)
(setf (gethash #\O *shiftable-character-ht*) +vk-o+)
(setf (gethash #\P *shiftable-character-ht*) +vk-p+)
(setf (gethash #\Q *shiftable-character-ht*) +vk-q+)
(setf (gethash #\R *shiftable-character-ht*) +vk-r+)
(setf (gethash #\S *shiftable-character-ht*) +vk-s+)
(setf (gethash #\T *shiftable-character-ht*) +vk-t+)
(setf (gethash #\U *shiftable-character-ht*) +vk-u+)
(setf (gethash #\V *shiftable-character-ht*) +vk-v+)
(setf (gethash #\W *shiftable-character-ht*) +vk-w+)
(setf (gethash #\X *shiftable-character-ht*) +vk-x+)
(setf (gethash #\Y *shiftable-character-ht*) +vk-y+)
(setf (gethash #\Z *shiftable-character-ht*) +vk-z+)
(setf (gethash #\~ *shiftable-character-ht*) +vk-oem-3+)
(setf (gethash #\! *shiftable-character-ht*) +vk-1+)
(setf (gethash #\@ *shiftable-character-ht*) +vk-2+)
(setf (gethash #\# *shiftable-character-ht*) +vk-3+)
(setf (gethash #\$ *shiftable-character-ht*) +vk-4+)
(setf (gethash #\% *shiftable-character-ht*) +vk-5+)
(setf (gethash #\^ *shiftable-character-ht*) +vk-6+)
(setf (gethash #\& *shiftable-character-ht*) +vk-7+)
(setf (gethash #\* *shiftable-character-ht*) +vk-8+)
(setf (gethash #\( *shiftable-character-ht*) +vk-9+)
(setf (gethash #\) *shiftable-character-ht*) +vk-0+)
(setf (gethash #\_ *shiftable-character-ht*) +vk-oem-minus+)
(setf (gethash #\+ *shiftable-character-ht*) +vk-oem-plus+)
(setf (gethash #\{ *shiftable-character-ht*) +vk-oem-4+)
(setf (gethash #\} *shiftable-character-ht*) +vk-oem-6+)
(setf (gethash #\| *shiftable-character-ht*) +vk-oem-5+)
(setf (gethash #\: *shiftable-character-ht*) +vk-oem-1+)
(setf (gethash #\" *shiftable-character-ht*) +vk-oem-7+)
(setf (gethash #\< *shiftable-character-ht*) +vk-oem-comma+)
(setf (gethash #\> *shiftable-character-ht*) +vk-oem-period+)
(setf (gethash #\? *shiftable-character-ht*) +vk-oem-2+)
