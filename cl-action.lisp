(defpackage #:cl-action
  (:use #:common-lisp
        #:cl-action/wvk
        #:cffi)
  (:export ;; constants
           #:+screen-width+
           #:+screen-height+
           #:+ms-coordinate+
           #:+abs-x+
           #:+abs-y+
           ;; base functions
           #:get-key-state
           #:set-cursor-pos
           #:get-cursor-pos
           #:mouse-action
           #:keybd-action
           ;; time-functions and delay macros
           #:get-internal-time-in-seconds
           #:time-limit-reached-p
           #:delay-when-set
           #:with-delay-before
           #:with-delay-after
           #:get-random-delay
           #:do-until-time-expires
           ;; listener-like macros and related functions
           #:get-vk-toggle-state
           #:do-once-after-toggle
           #:do-until-toggle
           #:do-after-toggle-until-repeat-toggle
           #:do-once-after-toggle-until-end
           #:do-after-toggle-until-repeat-toggle-until-end
           #:do-until-time-expires-or-toggle-occurs
           ;; specific mouse functions
           #:left-click
           #:right-click
           #:middle-click
           #:x-click
           #:mouse-wheel
           #:rotate-mouse-wheel
           #:scroller
           #:move-mouse-to
           #:move-mouse
           #:move-mouse-gradually
           #:move-mouse-to-gradually
           #:with-mouse-button-held
           #:click-and-drag
           #:click-and-drag-to
           #:get-new-position-on-axis
           #:clicker
           #:coverage-test
           ;; specific keyboard functions
           #:hold-key
           #:release-key
           #:type-unicode
           #:with-key-held
           #:tap-key
           #:repeat-key
           #:type-vk-list
           #:get-vk-list-from-string
           #:type-text
           #:repeat-typing-task
           ;; more complex functions and macros
           #:use-key-bindings
           #:intermittent-function
           #:with-intermittent-functions
           #:run-intermittent-functions))
(in-package #:cl-action)

;;; readying foreign libraries for use...

#+win32
(progn
  (cffi:define-foreign-library user32
    (:windows "user32.dll"))
  (cffi:use-foreign-library user32))

;;; preparing to create constants...

#+win32
(cffi:defcfun ("GetSystemMetrics" %get-system-metrics) (:int)
  "Information on this foreign function can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getsystemmetrics"
  (nIndex :int))

;;; general screen constants

(defparameter +screen-width+ (%get-system-metrics 0)) ;; collecting SM_CXSCREEN
(defparameter +screen-height+ (%get-system-metrics 1)) ;; collecting SM_CYSCREEN
(defparameter +ms-coordinate+ 65536.0)
(defparameter +abs-x+ (/ +ms-coordinate+ +screen-width+))
(defparameter +abs-y+ (/ +ms-coordinate+ +screen-height+))

;;; general functions

#+win32
(cffi:defcfun ("GetKeyState" %get-key-state) (:short)
  "Information on this foreign function can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getkeystate"
  (nVirtKey :int))

(defun get-key-state (vk)
  "Returns a single numeric value that represents the key state. 0 means untoggled; 1 means toggled; -127 means held and toggled; -128 means held and untoggled. Note that all keys can be toggled, not only those with a toggle-light indicator."
  (check-type vk integer)
  #+win32 (%get-key-state vk)
  #-win32 (error "Not supported on this platform."))

#+win32
(cffi:defcfun ("SetCursorPos" %set-cursor-pos) (:boolean :int)
  "Information on this foreign function can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setcursorpos"
  (x :int)
  (y :int))

(defun set-cursor-pos (x y)
  "Places the mouse cursor (directly and without relative movement) at the absolute x-y position on the screen."
  (check-type x integer)
  (check-type y integer)
  #+win32 (%set-cursor-pos x y)
  #-win32 (error "Not supported on this platform."))

#+win32
(cffi:defcstruct point
  "This represents the windef.h POINT structure; info can be found here: https://learn.microsoft.com/en-us/windows/win32/api/windef/ns-windef-point"
  (x :long)
  (y :long))

#+win32
(cffi:defcfun ("GetCursorPos" %get-cursor-pos) :boolean
  "Information on this foreign function can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getcursorpos"
  (lpPoint (:pointer)))

(defun get-cursor-pos ()
  "Returns the current x and y coordinates of the of mouse cursor as a CONS."
  #+win32
  (cffi:with-foreign-object (ptr '(:struct point))
    ;; setting the structure PTR, passing it by reference
    (%get-cursor-pos ptr)
    ;; extracting and returning values from foreign structure
    (cffi:with-foreign-slots ((x y) ptr (:struct point))
      (cons x y)))
  #-win32 (error "Not supported on this platform."))

#+win32
(cffi:defcstruct mouse-input
  "This represents the winuser.h MOUSEINPUT structure; info can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-mouseinput"
  (dx :long)
  (dy :long)
  (mouseData :long) ;; ! note that this is changed from uint32 because mouse data must be capable of being negative
  (dwFlags :uint32)
  (time :uint32)
  (dwExtraInfo (:pointer :ulong)))

#+win32
(cffi:defcstruct keybd-input
  "This represents the winuser.h KEYBDINPUT structure; info can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-keybdinput"
  (wVk :uint16)
  (wScan :uint16)
  (dwFlags :uint32)
  (time :uint32)
  (dwExtraInfo (:pointer :ulong)))

#+win32
(cffi:defcstruct hardware-input
  "This represents the winuser.h HARDWAREINPUT structure; info can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-hardwareinput"
  (uMsg :uint32)
  (wParamL :uint16)
  (wParamH :uint16))

#+win32
(cffi:defcunion struct-union
  "This represents the UNION that is required by the below INPUT structure."
  (mi (:struct mouse-input))
  (ki (:struct keybd-input))
  (hi (:struct hardware-input)))

#+win32
(cffi:defcstruct input
  "This represents the winuser.h INPUT structure; info can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/ns-winuser-input"
  (type :uint32)
  (su (:union struct-union)))

#+win32
(defparameter +input-struct-size+ (cffi:foreign-type-size '(:struct input)))

#+win32
(cffi:defcfun ("SendInput" %send-input) (:uint)
  "Information on this foreign function can be found here: https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-sendinput"
  (cInputs :unsigned-int) ;; count of INPUTs (meaning the length of pInputs)
  (pInputs (:pointer (:struct input))) ;; pointer to array of INPUTs
  (cbSize :int)) ;; size of a single INPUT

(defun simplified-send-input (ptr-input)
  "This function handles the actual scheduling of any mouse or keyboard action within the Windows OS. Note that the original \"SendInput\" function expects to receive an array of tasks to schedule, but this function is a simplified implementation of it that only allows scheduling one task at a time. This function is not meant to be used manually, and is used exclusively within the functions MOUSE-ACTION and KEYBD-ACTION after significant data preparation. PTR-INPUT is expected to be a C-structure of type INPUT."
  (%send-input 1 ptr-input +input-struct-size+))

(defun mouse-action (mouse-code &key (change-in-x 0) (change-in-y 0) (mouse-data 0))
  "Schedules one mouse action at a time, effectively causing the mouse to be used as indicated by the arguments. MOUSE-CODE is an integer (that in binary is a set of bitflags) that indicates all mouse-events to be applied; use any of the mouse-event constants (+me-<event>+) additively here. CHANGE-IN-X represents the intended change in the x-axis for the mouse cursor, and CHANGE-IN-Y represents the same for the y-axis; this change in the axis may be a relative change or a new absolute value, based on the passed MOUSE-CODE value. MOUSE-DATA is only relevant when specific MOUSE-CODE flags are set. For more details about valid arguments visit the Microsoft page \"MOUSEINPUT structure (winuser.h)\" online."
  #+win32
  (cffi:with-foreign-object (ptr-mouse-input '(:struct mouse-input))
    (setf (cffi:foreign-slot-value ptr-mouse-input '(:struct mouse-input) 'dx) change-in-x
          (cffi:foreign-slot-value ptr-mouse-input '(:struct mouse-input) 'dy) change-in-y
          (cffi:foreign-slot-value ptr-mouse-input '(:struct mouse-input) 'mouseData) mouse-data
          (cffi:foreign-slot-value ptr-mouse-input '(:struct mouse-input) 'dwFlags) mouse-code
          (cffi:foreign-slot-value ptr-mouse-input '(:struct mouse-input) 'time) 0
          (cffi:foreign-slot-value ptr-mouse-input '(:struct mouse-input) 'dwExtraInfo) (cffi:null-pointer))
    (cffi:with-foreign-object (ptr-struct-union '(:union struct-union))
      (setf (cffi:foreign-slot-value ptr-struct-union '(:union struct-union) 'mi) ptr-mouse-input)
      (cffi:with-foreign-object (ptr-input '(:struct input))
        (setf (cffi:foreign-slot-value ptr-input '(:struct input) 'type) 0 ;; 0 indicates mouse input
              (cffi:foreign-slot-value ptr-input '(:struct input) 'su) ptr-struct-union)
        (simplified-send-input ptr-input))))
  #-win32 (error "Not supported on this platform."))

(defun keybd-action (key-code &key (key-action 0) (unicode 0))
  "Schedules one keyboard action at a time, effectively causing the typing of the key as indicated by the arguments. KEY-CODE is an integer that matches one of the virtual key constants (+vk-<key-name>+). KEY-ACTION is an integer (that in binary is a set of bitflags) that indicates a more specific keyboard action; use any of the keyboard-action constants (+ka-<action>+). UNICODE is the decimal representation of the Unicode to be typed; to use this parameter KEY-ACTION must be +KA-UNICODE+ and KEY-CODE must be 0. For more details about valid arguments visit the Microsoft page \"KEYBDINPUT structure (winuser.h)\" online."
  #+win32
  (cffi:with-foreign-object (ptr-keybd-input '(:struct keybd-input))
    (setf (cffi:foreign-slot-value ptr-keybd-input '(:struct keybd-input) 'wVk) key-code
          (cffi:foreign-slot-value ptr-keybd-input '(:struct keybd-input) 'wScan) unicode
          (cffi:foreign-slot-value ptr-keybd-input '(:struct keybd-input) 'dwFlags) key-action
          (cffi:foreign-slot-value ptr-keybd-input '(:struct keybd-input) 'time) 0
          (cffi:foreign-slot-value ptr-keybd-input '(:struct keybd-input) 'dwExtraInfo) (cffi:null-pointer))
    (cffi:with-foreign-object (ptr-struct-union '(:union struct-union))
      (setf (cffi:foreign-slot-value ptr-struct-union '(:union struct-union) 'ki) ptr-keybd-input)
      (cffi:with-foreign-object (ptr-input '(:struct input))
        (setf (cffi:foreign-slot-value ptr-input '(:struct input) 'type) 1 ;; 1 indicates keyboard input
              (cffi:foreign-slot-value ptr-input '(:struct input) 'su) ptr-struct-union)
        (simplified-send-input ptr-input))))
  #-win32 (error "Not supported on this platform."))

;;; timing-related functions and macros

(defun get-internal-time-in-seconds ()
  "Returns the internal time of the Lisp system, representing the time in seconds."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun time-limit-reached-p (start-time time-limit)
  "Returns T when the second-based time limit has been reached; otherwise returns NIL. START-TIME must have been collected earlier using the function GET-INTERNAL-TIME-IN-SECONDS to ensure that this function works as expected. TIME-LIMIT must be a number of seconds."
  (>= (- (get-internal-time-in-seconds) start-time) time-limit))

(defun delay-when-set (delay)
  "Executes a delay (or sleep timer) of DELAY seconds when DELAY is set appropriately. DELAY is expected be an INTEGER or FLOAT of 0 or greater value or else NIL."
  (when (and delay (> delay 0))
    (sleep delay)))

(defmacro with-delay-before (seconds &body body)
  "Delays for SECONDS seconds, and then executes BODY."
  (let ((sec-gs (gensym)))
    `(let ((,sec-gs ,seconds))
       (sleep ,sec-gs)
       ,@body)))

(defmacro with-delay-after (seconds &body body)
  "Executes the BODY and then delays for SECONDS seconds, unless an error occurs within BODY."
  (let ((sec-gs (gensym)))
    `(let ((,sec-gs ,seconds))
       (handler-case (progn ,@body)
         (t (c) ;; catching all errors here...
           (error "Error in body of WITH-DELAY-AFTER:~%    ~a~%Skipping delay and terminating execution!" c)))
       (sleep ,sec-gs))))

(defun get-random-delay (shortest longest)
  "Returns a FLOAT value between the values SHORTEST and LONGEST; note that both of these values are generally to be interpreted as seconds."
  (when (< shortest 0)
    (error "SHORTEST cannot be negative!"))
  (when (> shortest longest)
    (error "SHORTEST time cannot be greater than LONGEST time!"))
  (when (= shortest longest)
    (error "SHORTEST and LONGEST cannot have the same value!"))
  (let ((dif (- longest shortest 0.0)))
    (+ shortest (random dif))))

(defmacro do-until-time-expires (seconds &body body)
  "Repeatedly executes BODY until the time expires, or SECONDS seconds has passed; note that the final iteration is always completed and the BODY will not be interrupted by the timer. Note that this macro is vulnerable to race-condition-like scenarios, and should be used with this in mind (for example, avoid using PRINT within an otherwise very fast BODY)."
  (let ((sec-gs (gensym))
        (start-time-gs (gensym)))
    `(let ((,sec-gs ,seconds)
           (,start-time-gs (get-internal-time-in-seconds)))
       (do ()
           ((time-limit-reached-p ,start-time-gs ,sec-gs))
         ,@body))))

;;; listener-like macros and related data...

(defparameter *key-toggled-p* (make-hash-table))
(progn ;; note that the values used here relate to GET-KEY-STATE; see that function for more details
  (setf (gethash -127 *key-toggled-p*) t) ;; held and toggled
  (setf (gethash 1 *key-toggled-p*) t) ;; toggled
  (setf (gethash -128 *key-toggled-p*) nil) ;; held and untoggled
  (setf (gethash 0 *key-toggled-p*) nil)) ;; untoggled

(defun get-vk-toggle-state (vk)
  "Gets the current toggle-state for the passed virtual-key (VK), returning T when toggled or else NIL for untoggled. Note that this function, by means of the hash-table *KEY-TOGGLED-P*, abstracts away the \"held\" or \"unheld\" parts of a toggle-state so that, instead of 4 states, there are only 2 to be considered."
  (gethash (get-key-state vk) *key-toggled-p*))

(defmacro do-once-after-toggle (vk &body body)
  "Executes BODY a single time after the key related to the passed virtual-key (VK) changes its toggle-state. VK must be one of the +VK-*+ constants, although no checks are done to ensure this."
  (let ((vk-gs (gensym))
        (original-state-gs (gensym))
        (current-state-gs (gensym)))
    `(let ((,vk-gs ,vk))
       (let ((,original-state-gs (get-vk-toggle-state ,vk-gs)))
         (do ((,current-state-gs (get-vk-toggle-state ,vk-gs) (get-vk-toggle-state ,vk-gs)))
             ((not (eq ,current-state-gs ,original-state-gs))))
         ,@body))))

(defmacro do-until-toggle (vk &body body)
  "Repeats BODY until the key related to the passed virtual-key (VK) changes its toggle-state. VK must be one of the +VK-*+ constants, although no checks are done to ensure this. Note that this macro is vulnerable to race-condition-like scenarios, and should be used with this in mind (for example, avoid using PRINT within an otherwise very fast BODY)."
  (let ((vk-gs (gensym))
        (original-state-gs (gensym))
        (current-state-gs (gensym)))
    `(let ((,vk-gs ,vk))
       (let ((,original-state-gs (get-vk-toggle-state ,vk-gs)))
         (do ((,current-state-gs (get-vk-toggle-state ,vk-gs) (get-vk-toggle-state ,vk-gs)))
             ((not (eq ,current-state-gs ,original-state-gs)))
           ,@body)))))

(defmacro do-after-toggle-until-repeat-toggle (vk &body body)
  "Repeats BODY, but only after the key related to the passed virtual-key (VK) changes it toggle-state; execution stops once the same virtual-key's toggle-state changes once more. VK must be one of the +VK-*+ constants, although no checks are done to ensure this. Note that this macro is vulnerable to race-condition-like scenarios, and should be used with this in mind (for example, avoid using PRINT within an otherwise very fast BODY)."
  (let ((vk-gs (gensym)))
    `(let ((,vk-gs ,vk))
       (do-once-after-toggle ,vk-gs
         (do-until-toggle ,vk-gs
           ,@body)))))

(defmacro do-once-after-toggle-until-end (start-vk end-vk &body body)
  "This macro causes execution to be in a waiting state, waiting for the toggling of either START-VK or END-VK. When START-VK is toggled, then the BODY will immediately execute once; it can then be toggled again. But when END-VK is toggled, then the macro will end, but only when toggled from within waiting state. Virtual-key values must be distinct and both must be one of the +VK-*+ constants, although no checks are done to ensure they are valid virtual-key values."
  (let ((start-gs (gensym))
        (end-gs (gensym))
        (start-initial-state-gs (gensym))
        (end-initial-state-gs (gensym)))
    `(let ((,start-gs ,start-vk)
           (,end-gs ,end-vk))
       (when (= ,start-gs ,end-gs)
         (error "START-VK and END-VK must be distinct virtual-key values!"))
       (let ((,start-initial-state-gs (get-vk-toggle-state ,start-gs))
             (,end-initial-state-gs (get-vk-toggle-state ,end-gs)))
         (do ()
             ((not (eq ,end-initial-state-gs (get-vk-toggle-state ,end-gs))))
           (when (not (eq ,start-initial-state-gs (get-vk-toggle-state ,start-gs)))
             ,@body
             (setq ,start-initial-state-gs (get-vk-toggle-state ,start-gs))
             ;; resetting saved END-GS state so as to allow for its potential use during running of BODY
             (setq ,end-initial-state-gs (get-vk-toggle-state ,end-gs))))))))

(defmacro do-after-toggle-until-repeat-toggle-until-end (start-and-stop-vk end-vk &body body)
  "This macro causes execution to be in a waiting state, waiting for the toggling of either START-AND-STOP-VK or END-VK. When START-AND-STOP-VK is toggled, then the BODY will repeatedly execute until the same virtual-key is toggled again, at which point the BODY code will complete its final repetition if one was already started; START-AND-STOP-VK may then be toggled again to repeat this process. But when END-VK is toggled, then the macro will end; note that the toggling of END-VK will only have an effect during the waiting state. Virtual-key values must be distinct and both must be one of the +VK-*+ constants, although no checks are done to ensure they are valid virtual-key values. Note that this macro is vulnerable to race-condition-like scenarios, and should be used with this in mind (for example, avoid using PRINT within an otherwise very fast BODY)."
  (let ((stst-gs (gensym)) ;; STST is short for STart-and-STop
        (end-gs (gensym))
        (stst-initial-state-gs (gensym))
        (end-initial-state-gs (gensym)))
    `(let ((,stst-gs ,start-and-stop-vk)
           (,end-gs ,end-vk))
       (when (= ,stst-gs ,end-gs)
         (error "START-AND-STOP-VK and END-VK must be distinct virtual-key values!"))
       (let ((,stst-initial-state-gs (get-vk-toggle-state ,stst-gs))
             (,end-initial-state-gs (get-vk-toggle-state ,end-gs)))
         (do ()
             ((not (eq ,end-initial-state-gs (get-vk-toggle-state ,end-gs))))
           (when (not (eq ,stst-initial-state-gs (get-vk-toggle-state ,stst-gs)))
             (do-until-toggle ,stst-gs
               ,@body)
             ;; resetting saved END-GS state so as to allow for its potential use during running of BODY
             (setq ,end-initial-state-gs (get-vk-toggle-state ,end-gs))))))))

(defmacro do-until-time-expires-or-toggle-occurs (seconds vk &body body)
  "Repeatedly executes BODY until the time expires -- SECONDS seconds has passed -- or the passed virtual-key (VK) is toggled; note that the final iteration is always completed and will not be interrupted by timer or virtual-key."
  (let ((sec-gs (gensym))
        (start-time-gs (gensym))
        (vk-gs (gensym))
        (original-state-gs (gensym))
        (current-state-gs (gensym)))
    `(let ((,sec-gs ,seconds)
           (,vk-gs ,vk)
           (,start-time-gs (get-internal-time-in-seconds)))
       (do ((,original-state-gs (get-vk-toggle-state ,vk-gs))
            (,current-state-gs (get-vk-toggle-state ,vk-gs) (get-vk-toggle-state ,vk-gs)))
           ((or (time-limit-reached-p ,start-time-gs ,sec-gs)
                (not (eq ,current-state-gs ,original-state-gs))))
         ,@body))))

;;; specific mouse functions

(defun left-click (&key (delay-between-press-and-release 0) (delay-after-scheduling 0))
  "Simulates the pressing and the subsequent release of the left mouse-button. DELAY-BETWEEN-PRESS-AND-RELEASE is the delay, in seconds, to wait between the action of pressing and subsequently releasing the left mouse-button; this delay is only meant to be used in application environments that cannot, for whatever reason, register extremely fast clicking. DELAY-AFTER-SCHEDULING is the delay, in seconds, to wait after the completion of the full scheduling of the left-click; this is to be used primarily to regulate the speed of repeating clicks, but also to prevent potential race conditions between program pacing and action scheduling."
  (mouse-action +me-leftdown+)
  (delay-when-set delay-between-press-and-release)
  (mouse-action +me-leftup+)
  (delay-when-set delay-after-scheduling))

(defun right-click (&key (delay-between-press-and-release 0) (delay-after-scheduling 0))
  "Simulates the pressing and the subsequent release of the right mouse-button. DELAY-BETWEEN-PRESS-AND-RELEASE is the delay, in seconds, to wait between the action of pressing and subsequently releasing the right mouse-button; this delay is only meant to be used in application environments that cannot, for whatever reason, register extremely fast clicking. DELAY-AFTER-SCHEDULING is the delay, in seconds, to wait after the completion of the full scheduling of the right-click; this is to be used primarily to regulate the speed of repeating clicks, but also to prevent potential race conditions between program pacing and action scheduling."
  (mouse-action +me-rightdown+)
  (delay-when-set delay-between-press-and-release)
  (mouse-action +me-rightup+)
  (delay-when-set delay-after-scheduling))

(defun middle-click (&key (delay-between-press-and-release 0) (delay-after-scheduling 0))
  "Simulates the pressing and the subsequent release of the center mouse-button. DELAY-BETWEEN-PRESS-AND-RELEASE is the delay, in seconds, to wait between the action of pressing and subsequently releasing the middle mouse-button; this delay is only meant to be used in application environments that cannot, for whatever reason, register extremely fast clicking. DELAY-AFTER-SCHEDULING is the delay, in seconds, to wait after the completion of the full scheduling of the middle-click; this is to be used primarily to regulate the speed of repeating clicks, but also to prevent potential race conditions between program pacing and action scheduling."
  (mouse-action +me-middledown+)
  (delay-when-set delay-between-press-and-release)
  (mouse-action +me-middleup+)
  (delay-when-set delay-after-scheduling))

;; !!! unfinished !!!
(defun x-click (xbutton &key (delay-between-press-and-release 0) (delay-after-scheduling 0))
  "This function is incomplete and untested!!!!!"
  (mouse-action +me-xdown+ :mouse-data xbutton)
  (delay-when-set delay-between-press-and-release)
  (mouse-action +me-xup+ :mouse-data xbutton)
  (delay-when-set delay-after-scheduling))

(defun mouse-wheel (movement)
  "Simulates a movement of the mouse's wheel, allowing for fine-tuned fractional movement of the wheel. MOVEMENT is expected to be an integer, either positive or negative; positive numbers cause upward scrolling; negative numbers cause downward scrolling. A full wheel movement is defined as 120 (WHEEL_DELTA); keep in mind that some application environments treat fractional wheel movements as full movements. Note that the simulated movement of the mouse wheel is done all at once with no delay."
  (mouse-action +me-wheel+ :mouse-data movement))

(defun rotate-mouse-wheel (movements &key (downward t))
  "Simulates the scrolling of the mouse-wheel, rotating the wheel a number of rotary MOVEMENTS in a DOWNWARD direction, by default, or else upward. MOVEMENTS must be a positive integer, and it refers to a standardized amount of rotation of the wheel, often related directly to the clicking-sound of the physical mouse's wheel during its rotation, and in terms of this library relates directly to the Windows-defined-constant WHEEL_DELTA (120). DOWNWARD is a boolean that, when T, causes downward scrolling; when NIL, causes upward scrolling. Note that the simulated movement of the mouse wheel is done all at once with no delay."
  (when (or (not (integerp movements)) (<= movements 0))
    (error "MOVEMENTS must be a positive integer!"))
  (let ((wheel-delta 120)) ;; ! this is a Windows-defined-constant
    (mouse-wheel (* movements wheel-delta (if downward -1 1)))))

(defun scroller (scroll-count &key (downward t) (delay-after-each 0.2) (delay-at-end t))
  "Causes the mouse wheel to scroll SCROLL-COUNT times, either downwards or upwards, with a delay in seconds of DELAY-AFTER-EACH after each wheel movement. SCROLL-COUNT should be a positive integer, and it refers to a standardized amount of rotation of the wheel, often related directly to the clicking-sound of the physical mouse's wheel during its rotation, and in terms of this library relates directly to the Windows-defined-constant WHEEL_DELTA (120). DOWNWARD is a boolean that, when T, causes downward scrolling; when NIL, causes upward scrolling. DELAY-AFTER-EACH is the delay in seconds between each scroll action; note that this delay, if too short, may prevent the completion of the expected scrolling amount within certain application environments. When DELAY-AT-END is T, then the delay will also take place after the final wheel movement; otherwise, there will be no delay at very end."
  (when (or (not (integerp scroll-count)) (< scroll-count 0)) ;; ! allowing 0 here, since DOTIMES below prevents issues
    (error "SCROLL-COUNT should be a positive integer!"))
  (when (or (not (numberp delay-after-each)) (< delay-after-each 0))
    (error "DELAY-AFTER-EACH should be a positive integer or float, or else 0."))
  (let ((delaying-p (> delay-after-each 0))
        (last-iter-value (1- scroll-count)))
    (dotimes (x scroll-count)
      (rotate-mouse-wheel 1 :downward downward)
      (when delaying-p
        (if (< x last-iter-value)
            ;; THEN, delay...
            (sleep delay-after-each)
            ;; ELSE, X is necessarily equal to DELAY-AFTER-EACH, meaning this is the last iteration
            (when delay-at-end
              (sleep delay-after-each)))))))

(defun move-mouse-to (x y &key (delay-after-scheduling 0))
  "Moves the mouse-cursor to an absolute X-Y coordinate on the screen. X and Y are expected to be numerical; note that no checks are done to ensure the appropriateness of X and Y; using unrealistically large X and Y values may have unusual effects. DELAY-AFTER-SCHEDULING is the delay, in seconds, to wait after the completion of the scheduling of mouse movements; this is to be used to prevent potential race-conditions due to the fact that mouse actions are scheduled and then subsequently handled separately (by the OS), apart from the Lisp code. Also note that this function moves the cursor extremely fast; to get more realistic mouse movements use the function MOVE-MOUSE-TO-GRADUALLY."
  (let ((pos-x (ceiling (* x +abs-x+)))
        (pos-y (ceiling (* y +abs-y+))))
    ;; ! note that the task scheduled by MOUSE-ACTION may not actually have completed before execution moves on, because the OS handles the action
    (mouse-action (+ +me-move+ +me-absolute+ +me-virtualdesk+) :change-in-x pos-x :change-in-y pos-y))
  (delay-when-set delay-after-scheduling))

(defun move-mouse (x y &key (delay-after-scheduling 0))
  "Moves the mouse-cursor relative to its current position, adjusting current x-position by X and current y-position by Y; thus, X and Y should be positive or negative numbers; note that no checks are done to ensure the appropriateness of X and Y; using unrealistically large values may have unusual effects. DELAY-AFTER-SCHEDULING is the delay, in seconds, to wait after the completion of the scheduling of mouse movements; this is to be used to prevent potential race-conditions due to the fact that mouse actions are scheduled and then subsequently handled separately (by the OS), apart from the Lisp code. Also note that this function moves the cursor extremely fast; to get more realistic mouse movements use the function MOVE-MOUSE-GRADUALLY."
  (when (or (/= x 0) (/= y 0))
    (let ((pos (get-cursor-pos)))
      (move-mouse-to (+ x (car pos)) (+ y (cdr pos)) :delay-after-scheduling delay-after-scheduling))))

(defun move-mouse-gradually (x y pixels-per-delay delay)
  "Moves the mouse cursor X and Y pixels relative to its current position on the screen by moving incrementally PIXELS-PER-DELAY pixels at a time, such that there is a delay of DELAY seconds between the scheduling of each mouse movement; to clarify, each delay the cursor will move PIXELS-PER-DELAY pixels on the x-axis and y-axis together, then if movement has been completed for only one axis so far then movement will continue along the remaining axis afterwards; thus when X or Y is sufficiently larger than the other, the total mouse movement will not be linear, but will instead appear to follow two distinct linear patterns. The primary usage of this function is to allow for more realistic mouse movements, in terms of speed mostly, and to enable things like clicking and dragging in more restrictive application environments. Note that this function will NOT work as expected if there is any manual mouse input during its execution."
  (unless (integerp x)
    (error "X must be an integer!"))
  (unless (integerp y)
    (error "Y must be an integer!"))
  (when (or (not (integerp pixels-per-delay)) (<= pixels-per-delay 0))
    (error "PIXELS-PER-DELAY must be and integer greater than 0!"))
  (when (or (not (numberp delay)) (< delay 0.001))
    (error "DELAY must be a number and cannot be less than 0.001!"))
  (let* ((total-change-in-x x) ;; avoiding the use of short variable names...
         (total-change-in-y y)
         (x-sign-value (signum total-change-in-x)) ;; -1 for negative; 0 for zero; 1 for positive
         (y-sign-value (signum total-change-in-y))
         (x-at-or-beyond (if (= -1 x-sign-value) #'<= #'>=)) ;; function to determine ending
         (y-at-or-beyond (if (= -1 y-sign-value) #'<= #'>=))
         (x-beyond (if (= -1 x-sign-value) #'< #'>)) ;; function to prevent overstepping at end
         (y-beyond (if (= -1 y-sign-value) #'< #'>))
         (change-in-x (* pixels-per-delay x-sign-value)) ;; number of pixels to step on axis
         (change-in-y (* pixels-per-delay y-sign-value)))
    (do ((x-pixels-moved 0 (+ x-pixels-moved change-in-x)) ;; pixels moved on axis since start
         (y-pixels-moved 0 (+ y-pixels-moved change-in-y)))
        ((and (funcall x-at-or-beyond x-pixels-moved total-change-in-x) (funcall y-at-or-beyond y-pixels-moved total-change-in-y)))
      (if (= total-change-in-x x-pixels-moved)
          ;; THEN, since x movement is complete, stop changing x
          (setq change-in-x 0)
          ;; ELSE, make sure that last x-step does not overstep
          (when (funcall x-beyond (+ change-in-x x-pixels-moved) total-change-in-x)
            (setq change-in-x (- change-in-x (- (+ change-in-x x-pixels-moved) total-change-in-x)))))
      (if (= total-change-in-y y-pixels-moved)
          ;; THEN, since y movement is complete, stop changing y
          (setq change-in-y 0)
          ;; ELSE, make sure that last y-step does not overstep
          (when (funcall y-beyond (+ change-in-y y-pixels-moved) total-change-in-y)
            (setq change-in-y (- change-in-y (- (+ change-in-y y-pixels-moved) total-change-in-y)))))
      (move-mouse change-in-x change-in-y :delay-after-scheduling delay))))

(defun move-mouse-to-gradually (x y pixels-per-delay delay)
  "Moves the mouse cursor to an absolute X-Y coordinate on the screen by moving incrementally PIXELS-PER-DELAY pixels at a time, such that there is a delay of DELAY seconds between the scheduling of each mouse movement. Note that this function makes direct use of the function MOVE-MOUSE-GRADUALLY; see that function for more details. The primary usage of this function is to allow for more realistic mouse movements, in terms of speed mostly, and to enable things like clicking and dragging in more restrictive application environments. Note that this function will NOT work as expected if there is any manual mouse input during its execution."
  (when (or (not (integerp x)) (< x 0))
    (error "X must be a non-negative integer."))
  (when (or (not (integerp y)) (< y 0))
    (error "Y must be a non-negative integer."))
  (let* ((cur-pos (get-cursor-pos))
         (total-change-in-x (- x (car cur-pos))) ;; total change to be made on this axis to reach passed value
         (total-change-in-y (- y (cdr cur-pos))))
    (move-mouse-gradually total-change-in-x total-change-in-y pixels-per-delay delay)))

(defmacro with-mouse-button-held (mouse-event &body body)
  "Holds the passed MOUSE-EVENT action (simulating a button-hold) while executing the passed BODY, and then releases the action afterwards. Note that if any error occurs within BODY, the held mouse button will be released. This macro is particularly useful for clicking-and-dragging. MOUSE-EVENT must be one the few +ME-*+ constants as available within the *ME-PRESS-TO-RELEASE* hash-table."
  (let ((me1 (gensym))
        (me2 (gensym)))
    `(let ((,me1 ,mouse-event))
       (let ((,me2 (gethash ,me1 ,*me-press-to-release*)))
         (unless ,me2
           (error "ERROR: There is no associated release-event for this mouse event (~a).~%No mouse event was initiated. Terminating execution!" ,me1))
         (mouse-action ,me1)
         ;; executing body code and releasing key (by ME2) if there was any error in BODY
         (handler-case (progn ,@body)
           (t (c) ;; catching all errors here...
             (mouse-action ,me2)
             (error "Error in body of WITH-MOUSE-BUTTON-HELD:~%    ~a~%Held button was released. Terminating execution!" c)))
         ;; releasing key (again,by ME2) when there was NOT an error in BODY
         (mouse-action ,me2)))))

(defun click-and-drag (x y &key (pixels-per-delay 2) (delay 0.01) (final-hold-delay 0) (held-vk +me-leftdown+))
  "This function causes the left mouse button (with default args) to be pressed and held for the duration of a mouse movement, moving the mouse cursor X and Y pixels relative to its current position on the screen by moving incrementally PIXELS-PER-DELAY pixels at a time, such that there is a delay of DELAY seconds between the scheduling of each mouse movement. FINAL-HOLD-DELAY is the delay, in seconds, to wait after the entire click-and-drag action has completed, before the key is finally released. HELD-VK is the held key, by default the left-mouse-button; it is expected to be one of the +ME-*+ constants."
  (when (or (not (numberp final-hold-delay)) (< final-hold-delay 0))
    (error "FINAL-HOLD-DELAY must be a integer or float of value 0 or greater."))
  (with-mouse-button-held held-vk
    (move-mouse-gradually x y pixels-per-delay delay)
    (delay-when-set final-hold-delay)))

(defun click-and-drag-to (x y &key (pixels-per-delay 2) (delay 0.01) (final-hold-delay 0) (held-vk +me-leftdown+))
  "This function causes the left mouse button (with default args) to be pressed and held while the cursor is moved to the absolute X-Y position on screen, by moving the cursor incrementally PIXELS-PER-DELAY pixels at a time, such that there is a delay of DELAY seconds between the scheduling of each mouse movement. FINAL-HOLD-DELAY is the delay, in seconds, to wait after the entire click-and-drag action has completed, before the key is finally released. HELD-VK is the held key, by default the left-mouse-button; it is expected to be one of the +ME-*+ constants."
  (when (or (not (numberp final-hold-delay)) (< final-hold-delay 0))
    (error "FINAL-HOLD-DELAY must be a integer or float of value 0 or greater."))
  (with-mouse-button-held held-vk
    (move-mouse-to-gradually x y pixels-per-delay delay)
    (delay-when-set final-hold-delay)))

(defun get-new-position-on-axis (axis-pos axis-min axis-max max-mouse-movement &key (randomize-movement t))
  "Returns a integer that indicates a new pixel position on the x or y axis that is necessarily within the bounds of AXIS-MIN and AXIS-MAX according to the current position on the axis, as AXIS-POS. This function is used to help regulate automated movements of the mouse for the CLICKER function, ensuring that they stay within specified boundaries. All standard arguments must be positive integers (to include 0 as well); note that there are basically no checks done to verify their correctness. AXIS-MIN is lowest axis value, and AXIS-MAX is highest axis value; both should be positive and together they determine the axis bounds. MAX-MOUSE-MOVEMENT indicates how many pixels to move at max, adjusted so as to stay within bounds. When RANDOMIZE-MOVEMENT is T, the default, MAX-MOUSE-MOVEMENT acts as the upper inclusive bound for a randomized integer; otherwise the adjustment will simply be MAX-MOUSE-MOVEMENT."
  (when (> axis-min axis-max)
    (error "AXIS-MIN must be less than or equal to AXIS-MAX!"))
  (cond
    ((< axis-pos axis-min) ;; when out of bounds by being below minimum
     axis-min)
    ((> axis-pos axis-max) ;; when out of bounds by being above maximum
     axis-max)
    ((= max-mouse-movement 0) ;; when movement disabled, and current position within bounds...
     axis-pos)
    (t ;; otherwise (meaning current-position is within bounds, and movement is enabled)...
     (let ((adjustment (if randomize-movement (random (1+ max-mouse-movement)) max-mouse-movement)))
       (if (= adjustment 0)
           ;; THEN, simply return current position since it has been verified to be within bounds
           axis-pos
           ;; ELSE, determine (and then return) the new adjusted position
           (progn
             ;; randomizing the sign of ADJUSTMENT
             (setq adjustment (if (zerop (random 2)) adjustment (- adjustment)))
             ;; ensuring that the movement caused by ADJUSTMENT stays within bounds...
             (let ((new-pos (+ axis-pos adjustment))
                   (adjustment-is-positive (if (= (signum adjustment) 1) t nil)))
               (cond
                 ((and adjustment-is-positive (> new-pos axis-max)) ;; when NEW-POS is above max
                  axis-max)
                 ((and (not adjustment-is-positive) (< new-pos axis-min)) ;; when NEW-POS is below min
                  axis-min)
                 (t ;; else, when NEW-POS is within the appropriate range
                  new-pos)))))))))

(defun clicker (&key (enforce-boundary t) (target-x nil) (target-y nil) (x-radius 5) (y-radius 5) (max-mouse-movement 3) (set-cursor-on-target t) (shortest-click-delay 0.001) (longest-click-delay nil) (click-count nil) (click-time nil) (terminating-vk nil) (emergency-stop-vk +vk-escape+) (testing nil))
  "This function will cause repeated clicking to occur at, and then possibly around, TARGET-X and TARGET-Y, or else the cursor's current position, returning the count of clicks at end.
When ENFORCE-BOUNDARY is T, the mouse will stay within a specified boundary throughout clicking process, thereby requiring that X-RADIUS, Y-RADIUS, and MAX-MOUSE-MOVEMENT be set appropriately; when ENFORCE-BOUNDARY is NIL, however, then all clicking will follow manual mouse movements and the aforementioned arguments will be completely ignored.
TARGET-X and TARGET-Y are to indicate, as integers, the center of the clicking area, but when either is NIL, then its value will be derived from the cursor's current position.
X-RADIUS and Y-RADIUS determine (by pixel count) the clicking boundaries left and right and above and below, respectively, of TARGET-X/TARGET-Y coordinate.
MAX-MOUSE-MOVEMENT is the maximum number of pixels moved (via simple randomization) before every click; this movement is always restricted by the clicking boundary. 
When SET-CURSOR-ON-TARGET is T, the cursor will be set exactly to the TARGET-X/TARGET-Y coordinate on screen -- as long as the coordinate is not fully derived -- before the clicking process begins; when argument is NIL, nothing will happen (although mouse-movement may still occur if a boundary is set, since the cursor may need to be pulled into the boundary before the first click).
SHORTEST-CLICK-DELAY is the amount of time, in seconds, that must expire between clicks; note that SHORTEST-CLICK-DELAY should generally be at least 0.001 (and not 0) because some race conditions are possible without this delay.
LONGEST-CLICK-DELAY is NIL by default, implying that there is no variation in click-delay; but when it is a number, then the actual click-delay will be a random value within (inclusively) a range made from shortest and longest.
CLICK-COUNT is the number of clicks the clicker will complete before stopping.
CLICK-TIME is the length of time, again in seconds, that the clicker will continue to click before stopping.
TERMINATING-VK is the virtual-key to be pressed to manually stop the clicker; it is expected to be one of the +VK-*+ constants.
Note that at least one of CLICK-COUNT, CLICK-TIME, or TERMINATING-VK must be set for the clicker to do anything, and when some or all are set, the clicker will stop as soon as the first of all conditions is met.
EMERGENCY-STOP-VK is expected to be one of the +VK-*+ constants; pressing the virtual-key related to EMERGENCY-STOP-VK, will cause an error to be signalled from within the main loop body, so as to allow for convenient full-stop termination.
Set TESTING to T if you want to run the clicker without having any actual clicks occur.
Note that when ENFORCE-BOUNDARY is T, concurrent manual movement of the mouse is not expected to be happening while this function operates, although the boundary restrictions will keep returning the mouse to within the set boundaries thereby preventing or minimizing some issues.
Finally, keep in mind that delays are important to prevent race conditions; for example, if no delays are set when the terminal condition for the clicker is a timer, then clicks may continue well after the timer has expired, due to how fast the clicks were scheduled, with more clicks having been scheduled before previously scheduled clicks had even taken place. In other words, the CLICKER does not stop and wait for scheduled actions to conclude, but only concerns itself with the scheduling, thereby allowing for the piling up of actions; hence the need for timing delays."
  (let ((target-is-derived nil))
    (when (and target-x (not (integerp target-x)))
      (error "TARGET-X, if set, must be an integer."))
    (when (and target-y (not (integerp target-y)))
      (error "TARGET-Y, if set, must be an integer."))
    (when (or (not target-x) (not target-y))
      ;; indicating that both TARGET-X and TARGET-Y will be derived from the cursor's current position, when neither is set
      (when (and (not target-x) (not target-y))
        (setq target-is-derived t))
      ;; deriving one or both of TARGET-X and TARGET-Y from the cursor's current position when either has not been set through the calling of this function
      (let ((current-pos (get-cursor-pos)))
        (when (not target-x)
          (setq target-x (car current-pos)))
        (when (not target-y)
          (setq target-y (cdr current-pos)))))
    (when enforce-boundary
      (when (or (not (integerp x-radius)) (< x-radius 0))
        (error "X-RADIUS must be a positive integer or 0!"))
      (when (or (not (integerp y-radius)) (< y-radius 0))
        (error "Y-RADIUS must be a positive integer or 0!"))
      (when (or (not (integerp max-mouse-movement)) (< max-mouse-movement 0))
        (error "MAX-MOUSE-MOVEMENT must be a positive integer or 0!")))
    (when (or (not (numberp shortest-click-delay)) (< shortest-click-delay 0))
      (error "SHORTEST-CLICK-DELAY must be a non-negative integer or float value."))
    (when (and longest-click-delay (or (not (numberp longest-click-delay)) (< longest-click-delay 0)))
      ;; ! note that the function GET-RANDOM-DELAY (used later) will check whether LONGEST-CLICK-DELAY is actually greater than SHORTEST-CLICK-DELAY
      (error "LONGEST-CLICK-DELAY, if set, must be a non-negative integer or float value."))
    (when (and click-count (or (not (integerp click-count)) (<= click-count 0)))
      (error "CLICK-COUNT, if set, must be a positive integer."))
    (when (and click-time (or (not (numberp click-time)) (<= click-time 0)))
      (error "CLICK-TIME, if set, must be a positive integer or float value."))
    (let ((assess-vk (if terminating-vk t nil))
          (original-vk-state nil)
          (current-vk-state nil)
          (delay-clicks (and shortest-click-delay (> shortest-click-delay 0)))
          (emergency-stop (if emergency-stop-vk t nil))
          (original-emergency-key-status nil)
          (current-emergency-key-status nil))
      (when assess-vk
        (when (not (integerp terminating-vk)) ;; !!! vk-validity is not checked
          (error "TERMINATING-VK, if set, must be an integer that represents a valid virtual key."))
        (setq original-vk-state (get-vk-toggle-state terminating-vk))
        (setq current-vk-state original-vk-state))
      (when emergency-stop
        (when (not (integerp emergency-stop-vk)) ;; !!! vk-validity is not checked
          (error "EMERGENCY-STOP-VK, if set, must be an integer that represents a valid virtual key."))
        (setq original-emergency-key-status (get-vk-toggle-state emergency-stop-vk))
        (setq current-emergency-key-status original-emergency-key-status)
        (when (and assess-vk (= terminating-vk emergency-stop-vk))
          (error "TERMINATING-VK and EMERGENCY-STOP-VK cannot be the same virtual key!")))
      ;; checking that the clicker will know when to stop clicking
      (when (and (not click-count) (not click-time) (not terminating-vk))
        (error "At least one of CLICK-COUNT, CLICK-TIME, or TERMINATING-VK must be set to use the CLICKER function, as it needs to know when to stop clicking."))
      (let ((cur-click 0) ;; acts as a count of clicks so far
            (start-time (if click-time (get-internal-time-in-seconds) nil))
            (max-x (if enforce-boundary (+ target-x x-radius) nil))
            (min-x (if enforce-boundary (- target-x x-radius) nil))
            (max-y (if enforce-boundary (+ target-y y-radius) nil))
            (min-y (if enforce-boundary (- target-y y-radius) nil)))
        ;; when a boundary is enforced, adjusting maximum and minimum values for it if they are set too high or low
        (when enforce-boundary
          (when (>= max-x +screen-width+)
            (setq max-x (1- +screen-width+)))
          (when (< min-x 0)
            (setq min-x 0))
          (when (>= max-y +screen-height+)
            (setq max-y (1- +screen-height+)))
          (when (< min-y 0)
            (setq min-y 0)))
        ;; setting cursor on target, when appropriate
        (when (and set-cursor-on-target (not target-is-derived))
          (set-cursor-pos target-x target-y)) ;; ! notice that SET-CURSOR-POS is used here, hence delays are less important
        ;; clicking according to the passed arguments...
        (do ((iter 0 (1+ iter))
             (pos nil) ;; is set inside loop body
             (final-x-pos nil) ;; ditto...
             (final-y-pos nil)) ;; ditto...
            ((or (and click-count (>= cur-click click-count)) ;; end when click quota is met...
                 (and start-time (time-limit-reached-p start-time click-time)) ;; or end when time limit is reached...
                 (and assess-vk (not (eq current-vk-state original-vk-state))))) ;; or end when terminating key was pressed
          (when enforce-boundary
            ;; getting current cursor position
            (setq pos (get-cursor-pos))
            ;; getting new x and y positions (that are necessarily within the set boundary)
            (setq final-x-pos (get-new-position-on-axis (car pos) min-x max-x max-mouse-movement))
            (setq final-y-pos (get-new-position-on-axis (cdr pos) min-y max-y max-mouse-movement))
            ;; moving the cursor to new position
            (move-mouse-to final-x-pos final-y-pos))
          ;; handling clicking and count
          (unless testing
            (left-click))
          (incf cur-click)
          ;; handling delay
          ;; ! notice that the delay occurs after the movement AND click, to (try to) help prevent issues caused by interference via manual mouse movement
          ;; ... also, this way, a click occurs immediately at start instead of after an initial delay
          (when delay-clicks
            (if longest-click-delay
                (sleep (get-random-delay shortest-click-delay longest-click-delay))
                (sleep shortest-click-delay)))
          ;; checking and updating the current state of terminating key (if initially set)
          (when assess-vk
            (setq current-vk-state (get-vk-toggle-state terminating-vk)))
          ;; looking for emergency key press and triggering error if toggle has occurred
          (when emergency-stop
            (setq current-emergency-key-status (get-vk-toggle-state emergency-stop-vk))
            (when (not (eq current-emergency-key-status original-emergency-key-status))
              (error "Emergency-stop key was pressed!"))))
        ;; returning the count of scheduled clicks
        cur-click))))

(defun coverage-test ()
  "This is a simple test about monitor-display coverage and the importance of delays. If there is any output it is because the cursor is not at the expected position when a check occurs, implying either user interference, or that the moved-to position is not a valid position on the display (this should never be the case though), or the sleep-delay is too short such that the mouse could not reach the expected position (via a scheduled mouse-movement) before the check occurred."
  (dotimes (y 3) ;; (y +screen-height+) ;; running the full test is way too long
    (dotimes (x +screen-width+)
      (move-mouse-to x y)
      ;; ! note that removing the SLEEP function results in a lot of output
      (sleep 0.001) ;; ! on Windows the delay cannot be faster than 0.001 seconds
      (let ((pos (get-cursor-pos)))
        (when (or (>= (abs (- (car pos) x)) 1) (>= (abs (- (cdr pos) y)) 1))
          (format t "~a~%x:~a, y: ~a~%~%" pos x y))))))

;;; specific keyboard functions

(defun hold-key (vk)
  "Simulates the pressing (without release) of a keyboard key. Note that this does not cause the key to be signalled more than once; see REPEAT-KEY for that functionality and an explanation. Every call of HOLD-KEY should be followed eventually by a RELEASE-KEY call. VK must be one of the +VK-*+ constants, although no checks are done to ensure this."
  (keybd-action vk))

(defun release-key (vk)
  "Simulates the releasing of a keyboard key. Note that no check is done to ensure that the key was actually already pressed. VK must be one of the +VK-*+ constants, although no checks are done to ensure this."
  (keybd-action vk :key-action +ka-keyup+))

(defun type-unicode (unicode-value &key (delay 0))
  "Types out the Unicode character that relates to the passed Unicode value. UNICODE-VALUE is expected to be a decimal value, a hex literal, or a string representing the Unicode; note that this function does not accept Lisp Unicode literals. DELAY is the delay, in seconds, to wait after the typing of the Unicode character."
  (when (stringp unicode-value)
    (setq unicode-value (parse-integer unicode-value :radix 16)))
  (keybd-action 0 :key-action +ka-unicode+ :unicode unicode-value)
  (delay-when-set delay))

(defmacro with-key-held (vk &body body)
  "Causes the passed virtual-key (VK) to be held while executing the passed BODY, and then releases the key afterwards. If any error occurs during the execution of BODY, the key will still be released before termination. This macro is particularly useful for holding SHIFT while typing, or doing things like CTRL commands. VK should be one of the +VK-*+ constants, although no checks are done to ensure this."
  (let ((vk-gs (gensym)))
    `(let ((,vk-gs ,vk))
       (hold-key ,vk-gs)
       ;; executing body code and releasing the key if there was any error
       (handler-case (progn ,@body)
         (t (c) ;; catching all errors here...
           (release-key ,vk-gs)
           (error "Error in body of WITH-KEY-HELD:~%    ~a~%Held key (~a) was released. Terminating execution!" c ,vk-gs)))
       (release-key ,vk-gs))))

(defun tap-key (vk &key (delay-between-press-and-release 0) (delay-after-scheduling 0))
  "Simulates pressing and subsequently releasing the passed virtual-key (VK). VK should be one of the +VK-*+ constants, although no checks are done to ensure this. DELAY-BETWEEN-PRESS-AND-RELEASE is the delay, in seconds, to wait between the actions of pressing and subsequently releasing VK; this delay is only meant to be used in application environments that cannot, for whatever reason, register an extremely fast press-and-release. DELAY-AFTER-SCHEDULING is the delay, in seconds, to wait after the scheduling of the two parts of the key-tap; this is to be used primarily to simulate a typing speed, but also to prevent potential race conditions between program pacing and action scheduling."
  (hold-key vk)
  (delay-when-set delay-between-press-and-release)
  (release-key vk)
  (delay-when-set delay-after-scheduling))

(defun repeat-key (vk duration &key (delay-before-repeat .8) (repeat-delay 0.025))
  "Simulates the typematic effect of physical keyboards (the repeated sending of a character-based key's signal when it is held) on the passed virtual-key (VK). Note that the function HOLD-KEY does not cause this effect, as it is a keyboard functionality. VK should be one of the +VK-*+ constants, although no checks are done to ensure this. DURATION determines the total length, in seconds, that the key is held. DELAY-BEFORE-REPEAT determines the length, in seconds, before the repeating signals begin. REPEAT-DELAY is the delay, in seconds, between each repeating signal after repeating has begun."
  (when (or (not (numberp duration)) (<= duration 0))
    (error "DURATION must be a positive integer or float."))
  (let ((start-time (get-internal-time-in-seconds)))
    ;; simulating the initial pressing of key, which will trigger the typing of a single character
    (tap-key vk :delay-after-scheduling 0)
    (if (< duration delay-before-repeat)
        ;; THEN, still simulate the holding of the key, although no further "typing" will occur
        (sleep duration)
        ;; ELSE, simulate typematic effect because the DURATION is sufficiently long
        (progn
          ;; simulating the delay before repetition
          (sleep delay-before-repeat)
          ;; simulating the repetition
          (do ()
              ((time-limit-reached-p start-time duration))
            (tap-key vk :delay-after-scheduling repeat-delay))))))

(defun type-vk-list (vk-list &key (delay-between-keys .025) (unicode nil))
  "Types all the virtual-keys in the list VK-LIST according to the rules of a VK-LIST (which are indirectly explained subsequently). This is a recursive function that differentiates primarily between two conditions: whether the considered element of the list is a number or another list. When the element is a list, then the first element of the list is one of two things: either a virtual-key to be held (implying that later elements in the same list must be virtual keys OR other lists of virtual keys), or the symbol :UNICODE (thus implying that all subsequent values in the list are Unicode [represented by a hex literal, hex string, or integer]). Thus, when any element is not a list or the first element of a list, it must be (as implied by the rules above), either a virtual-key value or a Unicode value. Note that this recursion allows for arbitrary layers of key-holding; for example: (TYPE-VK-LIST (LIST (LIST +VK-SHIFT+ (LIST +VK-CONTROL+ (LIST +VK-MENU+ +VK-5+))))) activates the \"find and replace\" functionality in EMACS, by typing SHIFT + CTRL + ALT + 5. Also note that \"holding\" keys is really only useful in this context when the held keys are functional keys like SHIFT or CTRL. (Note also that this function does not check whether held keys are functional.) Lastly, here is another example that uses Unicode: (type-vk-list (list (list 16 83) 65 89 32 (list :UNICODE #x4F60 #x597D))) yields the typed text \"Say 你好\". Note that the VK-LIST is not checked for correctness, thus care must be taken to construct a VK-LIST that meets the requirements of the function, or else an error may be triggered mid-execution. DELAY-BETWEEN-KEYS is the delay, in seconds, between the typing of each individual key or character. The argument UNICODE should be left NIL, as it is only to be used internally by a recursion."
  (dolist (x vk-list)
    (if (not (listp x))
        ;; THEN, because it is not a list, check whether it is Unicode
        (if unicode
            (type-unicode x :delay delay-between-keys)
            (tap-key x :delay-after-scheduling delay-between-keys))
        ;; ELSE, because it is a list, handle it accordingly
        (let ((first-element (car x)))
          (if (and (symbolp first-element) (eq first-element :unicode))
              ;; THEN, because the symbol :UNICODE was found, all subsequent characters within this LIST should be Unicode
              (type-vk-list (cdr x) :delay-between-keys delay-between-keys :unicode t) ;; ! notice the CDR
              ;; ELSE, the first character in this LIST should be held before pressing the later ones
              (with-key-held first-element
                (type-vk-list (cdr x) :delay-between-keys delay-between-keys))))))) ;; ! notice the CDR

(defun get-vk-list-from-string (str)
  "Creates and returns a VK-LIST that is derived from the passed string (STR), by changing each character of STR into the related virtual-key value, or if no related virtual-key is found the character is treated as Unicode, while filling out the list appropriately. This is mostly a convenience function for when you need a VK-LIST but don't want to look up each virtual-key name or value, let alone type it up manually. To see more details about the structure of VK-LISTs, please see the function TYPE-VK-LIST. Note that this function does not optimize the internal lists; thus every Unicode character, for example, will be part of a separate LIST that starts with the SYMBOL :UNICODE; and every shifted character, to give another example, will be in its own LIST that starts with the integer that represents the shift key. Further note that all virtual-key characters and Unicode characters will be presented as integers."
  (let ((str-len (length str))
        (vk-list nil))
    ;; moving through string backwards pushing each transformed character onto VK-LIST
    (do ((i (1- str-len) (1- i)))
        ((< i 0))
      (let ((c (char str i)))
        (cond
          ((gethash c *base-character-ht*)
           (let ((vk (gethash c *base-character-ht*)))
             (push vk vk-list)))
          ((gethash c *shiftable-character-ht*)
           (let ((vk (gethash c *shiftable-character-ht*)))
             (push (list +vk-shift+ vk) vk-list)))
          (t
           (push (list :unicode (char-code c)) vk-list)))))
    vk-list))

(defun type-text (text &key (delay 0.025))
  "Causes the typing of TEXT in its entirety with a delay of DELAY seconds after each simulated key-press. TEXT must be a string. Keep in mind that this function is only to be used to type displayable characters. Note that this function makes direct use of GET-VK-LIST-FROM-STRING and TYPE-VK-LIST; see those functions for more information."
  (let ((vk-list (get-vk-list-from-string text)))
    (type-vk-list vk-list :delay-between-keys delay)))

(defun repeat-typing-task (vk-list repeat-count &key (closing-vk-list nil) (delay .025))
  "Types VK-LIST a total of REPEAT-COUNT times, ending each repetition except the last with CLOSING-VK-LIST, if provided. See the function TYPE-VK-LIST for an explanation of the requirements of a VK-LIST. REPEAT-COUNT should be a positive integer. DELAY is the delay, in seconds, between the typing of each individual key. This function is useful in automating repetitive typing tasks; for example, it could be used to add two spaces to the front of 5 lines in a text file: (REPEAT-TYPING-TASK (LIST +VK-SPACE+ +VK-SPACE+) 5 :CLOSING-VK-LIST (LIST +VK-DOWN+ +VK-HOME+))"
  (dotimes (x repeat-count)
    (type-vk-list vk-list :delay-between-keys delay)
    (when (and closing-vk-list (< x (1- repeat-count))) ;; on all iterations except last (when CLOSING-VK-LIST is set)...
      (type-vk-list closing-vk-list :delay-between-keys delay))))

;;; other more complex functionality...

(defun use-key-bindings (end-vk list-of-vk-to-function-conses)
  "This function causes execution to be in a waiting state, waiting for the toggling of END-VK or any virtual-key in LIST-OF-VK-TO-FUNCTION-CONSES. When END-VK is toggled, then the function will end; note that the toggling of END-VK will only have an effect during the waiting state. Otherwise, when any virtual-key in LIST-OF-VK-TO-FUNCTION-CONSES is toggled, then the associated function for that virtual-key will execute immediately and only once; execution then returns to the waiting state. LIST-OF-VK-TO-FUNCTION-CONSES must be a list of any number of CONSes in this format --> (LIST (CONS <virtual-key-integer-value> #'<function-name-or-lambda-expression>) ...). Note that the ordering of LIST-OF-VK-TO-FUNCTION-CONSES matters; for example, if there were concurrent key-presses then only the earliest related function in the list would be executed; also note that no other functions would be \"scheduled\" by concurrent or later presses because all virtual-key states are reset before returning to the waiting state, thereby allowing for toggling of keys during functions. Finally, all virtual-key values must be distinct and must be one of the +VK-*+ constants, although no checks are done to ensure that they are valid virtual-key values."
  (let ((ht (make-hash-table)) ;; this hash-table holds all data from LIST-OF-VK-TO-FUNCTION-CONSES in hash-table format with VK as key and a property list as value
                               ;; ... the p-list has the following format --> '(:state <VK-toggle-state-at-start> :function #'<the-related-function-name>)
        (vk-list nil)) ;; this list will hold all virtual-keys from LIST-OF-VK-TO-FUNCTION-CONSES in the same order as passed; this is used for convenient iteration
    ;; setting up the initial data and checking that passed data is valid...
    (dolist (cns list-of-vk-to-function-conses)
      (unless (consp cns) ;; ! note that this check does not guarantee that this element is exclusively a CONS, as it could still be a standard LIST
        (error "Each element in LIST-OF-VK-TO-FUNCTION-CONSES must be a CONS!"))
      (let ((vk (car cns))
            (function (cdr cns)))
        (unless (integerp vk) ;; !!! could be more specific later to check for valid vk integers...
          (error "The CAR value of each CONS in LIST-OF-VK-TO-FUNCTION-CONSES must be an integer that relates to a virtual-key!"))
        (unless (functionp function)
          (error "The CDR value of each CONS in LIST-OF-VK-TO-FUNCTION-CONSES must be a function or lambda expression!"))
        (when (= end-vk vk)
          (error "The CAR value of any CONS in LIST-OF-VK-TO-FUNCTION-CONSES cannot be the same as END-VK!"))
        (if (gethash vk ht)
            (error "No virtual key may be assigned more than once!~%The key that relates to the integer ~d was assigned twice!" vk)
            (progn
              (setf (gethash vk ht) (list :state (get-vk-toggle-state vk) :function function))
              (push vk vk-list)))))
    ;; reversing VK-LIST to ensure that the original ordering holds
    (setq vk-list (reverse vk-list))
    ;; constantly checking for key presses... and stopping if END-VK is pressed...
    (do ((end-vk-initial-state (get-vk-toggle-state end-vk))) ;; ! note that this is reset within loop as well
        ((not (eq end-vk-initial-state (get-vk-toggle-state end-vk))))
      ;; checking the state of each virtual-key in VK-LIST...
      (dolist (current-vk vk-list)
        ;; when the "current" virtual-key is pressed
        (when (not (eq (getf (gethash current-vk ht) :state) (get-vk-toggle-state current-vk)))
          ;; execute the related function
          (funcall (getf (gethash current-vk ht) :function))
          ;; then reset state for all virtual-keys in VK-LIST (to prevent "scheduling" of functions)
          (dolist (current-vk vk-list)
            (setf (getf (gethash current-vk ht) :state) (get-vk-toggle-state current-vk)))
          ;; also reset the END-VK state (so that termination cannot be "scheduled" either)
          (setq end-vk-initial-state (get-vk-toggle-state end-vk))
          ;; return to "waiting" state (waiting for another key press)
          (return))))))

(defclass intermittent-function ()
  ((start-time
    :reader start-time
    :initform nil
    :documentation "This is a number that will be set automatically by the related macro WITH-INTERMITTENT-FUNCTIONS (and also within RUN-AND-UPDATE-INTERMITTENT-FUNCTION under the right conditions); it is set with GET-INTERNAL-TIME-IN-SECONDS.")
   (min-seconds-between
    :reader min-seconds-between
    :initarg :min-seconds-between
    :documentation "This is the number of seconds that should, at minimum, expire before the next execution of FUNCTION-OR-LAMBDA-EXPRESSION. Note that this is also the time that must expire before the first execution when the first execution is not affected by other slots, such as TRIGGER-IMMEDIATE-EXECUTION.")
   (first-min-seconds-between
    :reader first-min-seconds-between
    :initarg :first-min-seconds-between
    :initform nil
    :documentation "This is like MIN-SECONDS-BETWEEN but only affects the timing of the very first execution and then is afterwards irrelevant. This slot is useful when the first execution should come sooner or later than usual but only once at start. When NIL, it will have no effect; when set, this slot must be set to a number greater than 0; note that this argument cannot be set, and must remain NIL, when TRIGGER-IMMEDIATE-EXECUTION is T or EXECUTE-FIRST-WITHIN-MACRO is T. Also, note that when scheduled timing is used (meaning, POST-EXECUTION-OR-SCHEDULED-TIMING was set to NIL), then this slot, when properly set, causes the first time-period to be of FIRST-MIN-SECONDS-BETWEEN seconds in length, with no execution possibly occurring within that first period; use this combination of slots to adjust the actual starting time of a scheduled action.")
   (function-or-lambda-expression
    :reader function-or-lambda-expression
    :initarg :function-or-lambda-expression
    :documentation "This must be a function name or lambda-expression passed like the following: #'function-name or #'(lambda ()), so that it can be used with FUNCALL.")
   (trigger-immediate-execution
    :reader trigger-immediate-execution
    :initarg :trigger-immediate-execution
    :initform nil
    :documentation "When T, causes the related function to be ready to execute as soon as the macro starts; when NIL, MIN-SECONDS-BETWEEN must expire before first execution. Note that this argument cannot be T when FIRST-MIN-SECONDS-BETWEEN is set to a number. Note that this argument does not cause immediate execution from within the macro; use EXECUTE-FIRST-WITHIN-MACRO for that. Lastly, this argument cannot be T when EXECUTE-FIRST-WITHIN-MACRO is also T.")
   (execute-first-within-macro
    :reader execute-first-within-macro
    :initarg :execute-first-within-macro
    :initform nil
    :documentation "When T causes the related FUNCTION-OR-LAMBDA-EXPRESSION to be executed from within the macro WITH-INTERMITTENT-FUNCTIONS. Note that this argument cannot be T when FIRST-MIN-SECONDS-BETWEEN is set to a number. Also note that this argument cannot be T when TRIGGER-IMMEDIATE-EXECUTION is also T. Lastly, note that for any function that is executed within the macro, its initial LAST-EXECUTION-TIME is set within the macro after the execution of all macro-executed functions, regardless of the status of POST-EXECUTION-OR-SCHEDULED-TIMING.")
   (post-execution-or-scheduled-timing
    :reader post-execution-or-scheduled-timing
    :initarg :post-execution-or-scheduled-timing
    :initform t
    :documentation "When T, then the timing of every subsequent execution of the related function will be based on the time of the previous completion, such that if there is any delay to the starting of the function's execution then the next (and every later) execution will be delayed by this amount of time (plus its own execution time); for example, if a function that executes every 5 seconds were delayed by 2 seconds, meaning the first execution took place at 7 seconds, the next execution would occur at 12 seconds at earliest. But when NIL, then the timing of every subsequent execution of the related function will be tied to absolute time periods that are determined from the moment the macro WITH-INTERMITTENT-FUNCTIONS is first called; for example, if a function of this type executed at 10 MIN-SECONDS-BETWEEN (or, rather, once within each 10-second period), then if the macro WITH-INTERMITTENT-FUNCTIONS set the START-TIME to something corresponding to xx:xx:03, then the first period would be xx:xx:03 to xx:xx:12.99, and the second would be xx:xx:13 - xx:xx:22.99, etc.; note that an execution will only trigger during the first period when activated by TRIGGER-IMMEDIATE-EXECUTION or the like; if FIRST-MIN-SECONDS-BETWEEN is set, then the first period will be that many seconds long and execution will not be possible within it, but later periods will be based, as usual, on MIN-SECONDS-BETWEEN with a start time that coincides with the ending of the first period; finally, keep in mind that delays caused by other long-executing functions might cause the missing of any number of time-periods.")
   (execution-count
    :accessor execution-count
    :initform 0
    :documentation "This is total number of executions of the related function so far.")
   (execution-count-limit
    :reader execution-count-limit
    :initarg :execution-count-limit
    :initform nil
    :documentation "This value represents the maximum number of executions that this function is allowed to complete; when NIL, there is no limit; when non-NIL, it must be an integer greater than 0.")
   (last-execution-time
    :accessor last-execution-time
    :initform nil
    :documentation "This is the last time, according to GET-INTERNAL-TIME-IN-SECONDS, that the most recent execution occurred.")))

(defmethod initialize-instance :after ((ifo intermittent-function) &key)
  (when (and (not (floatp (min-seconds-between ifo))) (not (integerp (min-seconds-between ifo))))
    (error "MIN-SECONDS-BETWEEN must be an integer or a float."))
  (when (<= (min-seconds-between ifo) 0)
    (error "MIN-SECONDS-BETWEEN cannot be 0 or less."))
  (when (first-min-seconds-between ifo)
    (when (and (not (floatp (first-min-seconds-between ifo))) (not (integerp (first-min-seconds-between ifo))))
      (error "FIRST-MIN-SECONDS-BETWEEN, when set, must be an integer or a float."))
    (when (<= (first-min-seconds-between ifo) 0)
      (error "FIRST-MIN-SECONDS-BETWEEN must be greater than 0.")))
  (unless (functionp (function-or-lambda-expression ifo))
    (error "FUNCTION-OR-LAMBDA-EXPRESSION must be a function or lambda expression!"))
  (when (first-min-seconds-between ifo) ;; when FIRST-MIN-SECONDS-BETWEEN is non-NIL (and is necessarily a number greater than 0, because of previous checks)
    (when (trigger-immediate-execution ifo)
      (error "FIRST-MIN-SECONDS-BETWEEN cannot be set to a number when TRIGGER-IMMEDIATE-EXECUTION is T.~%Only one of these may be non-NIL at a time, or both may be NIL."))
    (when (execute-first-within-macro ifo)
      (error "FIRST-MIN-SECONDS-BETWEEN cannot be set to a number when EXECUTE-FIRST-WITHIN-MACRO is T.~%Only one of these may be non-NIL at a time, or both may be NIL.")))
  (when (and (trigger-immediate-execution ifo) (execute-first-within-macro ifo))
    (error "Only one of TRIGGER-IMMEDIATE-EXECUTION or EXECUTE-FIRST-WITHIN-MACRO may be T!"))
  (when (execution-count-limit ifo)
    (unless (integerp (execution-count-limit ifo))
      (error "EXECUTION-COUNT-LIMIT must be NIL or an integer!"))
    (when (<= (execution-count-limit ifo) 0)
      ;; ! note that if EXECUTION-COUNT-LIMIT were allowed to be zero, then EXECUTE-FIRST-WITHIN-MACRO could be used to allow one execution of a function whose EXECUTION-COUNT-LIMIT was 0; an execution limit of zero makes little sense anyway...
      (error "EXECUTION-COUNT-LIMIT must be greater than 0!"))))

(defmethod run-and-update-intermittent-function ((ifo intermittent-function) &key (save-time t))
  "Causes the passed \"intermittent function\" to execute and update its appropriate slots. When SAVE-TIME is T, the last execution time will be saved according to IFO's timing mechanism; when NIL, no time will be saved; note that SAVE-TIME should always be T except for when the \"intermittent function\" is executed from within the main macro via EXECUTE-FIRST-WITHIN-MACRO."
  ;; when using scheduled timing, then set the last execution time to the first second of the most recent time-period of length MIN-SECONDS-BETWEEN ...
  ;; ... so that as soon as any new period starts, MIN-SECONDS-BETWEEN will have already passed (meaning this function will again be due for an execution)
  (when (and save-time (not (post-execution-or-scheduled-timing ifo)))
    (let ((cur-time (get-internal-time-in-seconds)))
      (when (and (zerop (execution-count ifo)) (first-min-seconds-between ifo)) ;; if on first execution and delaying scheduling (by FIRST-MIN-SECONDS-BETWEEN)
        ;; adjusting START-TIME to the beginning of the second period
        (let ((beginning-of-second-period (+ (start-time ifo) (first-min-seconds-between ifo))))
          (setf (slot-value ifo 'start-time) beginning-of-second-period)))
      ;; setting LAST-EXECUTION-TIME to the beginning of the most recent time-period
      (setf (last-execution-time ifo) (- cur-time (mod (- cur-time (start-time ifo)) (min-seconds-between ifo))))))
  ;; executing the function and keeping track of execution count...
  (funcall (function-or-lambda-expression ifo))
  (incf (execution-count ifo))
  ;; when using post-execution timing, then set the last execution time according to the time after execution
  (when (and save-time (post-execution-or-scheduled-timing ifo))
    (setf (last-execution-time ifo) (get-internal-time-in-seconds))))

(defmethod run-and-update-intermittent-function-if-time-expired ((ifo intermittent-function) min-time)
  "Causes the passed \"intermittent function\" to execute so long as its time requirement is met; note that this method is only to be used once all other IFO requirements have been met. MIN-TIME is expected to be an integer representing a count of seconds, and should generally be the slot MIN-SECONDS-BETWEEN, or else an alternative such as FIRST-MIN-SECONDS-BETWEEN."
  (when (time-limit-reached-p (last-execution-time ifo) min-time)
    (run-and-update-intermittent-function ifo)))

(defmethod run-intermittent-function-if-appropriate ((ifo intermittent-function))
  "Causes the passed \"intermittent function\" to execute so long as all of its internal requirements are met."
  (cond
    ;; when "intermittent function" has already been executed once (meaning a countdown has begun, and timing needs to be assessed)
    ((> (execution-count ifo) 0)
     (when (or (not (execution-count-limit ifo)) (< (execution-count ifo) (execution-count-limit ifo)))
       (run-and-update-intermittent-function-if-time-expired ifo (min-seconds-between ifo))))
    ;; when "intermittent function" has not yet been executed (meaning timing MIGHT not need to be assessed)
    ((= (execution-count ifo) 0)
     (cond ;; ! note that EXECUTION-COUNT-LIMIT need not be assessed here, because it is prohibited from being 0
       ((trigger-immediate-execution ifo)
        (run-and-update-intermittent-function ifo))
       ((first-min-seconds-between ifo)
        (run-and-update-intermittent-function-if-time-expired ifo (first-min-seconds-between ifo)))
       (t
        (run-and-update-intermittent-function-if-time-expired ifo (min-seconds-between ifo)))))
    (t ;; ! just in case...
     (error "EXECUTION-COUNT is (somehow) negative!"))))

(defmacro with-intermittent-functions ((var list-of-intermittent-function-objects) &body body)
  "Prepares a variable that is to be used by a specialized function (RUN-INTERMITTENT-FUNCTIONS), which is to be called within BODY from within a looping construct; the created variable contains all necessary data to allow for the intermittent execution of arbitrary functions. LIST-OF-INTERMITTENT-FUNCTION-OBJECTS is a list of INTERMITTENT-FUNCTION objects, such that each element is created with (MAKE-INSTANCE 'INTERMITTENT-FUNCTION ...). Note that the ordering of this passed list does not matter, as this macro will create a new list that is ordered by shortness of time."
  (let ((original-list-gs (gensym))
        (main-list-gs (gensym))
        (early-execute-gs (gensym))
        (obj-gs (gensym))
        (start-time-gs (gensym)))
    `(let ((,original-list-gs ,list-of-intermittent-function-objects)
           (,main-list-gs nil)
           (,early-execute-gs nil))
       ;; checking that passed data is valid, while also preparing the main-list that VAR will be set to
       (dolist (,obj-gs ,original-list-gs)
         (unless (typep ,obj-gs 'intermittent-function)
           (error "Each element in LIST-OF-INTERMITTENT-FUNCTION-OBJECTS must be an instance of class INTERMITTENT-FUNCTION!"))
         (when (execute-first-within-macro ,obj-gs)
           (setq ,early-execute-gs t)) ;; setting a flag, to indicate that some object has an early execution set
         (push ,obj-gs ,main-list-gs))
       ;; sorting the main-list by seconds...
       (setq ,main-list-gs (sort ,main-list-gs #'< :key #'min-seconds-between))
       ;; executing functions now that have EXECUTE-FIRST-WITHIN-MACRO set to T (if any)
       (when ,early-execute-gs
         (dolist (,obj-gs ,main-list-gs)
           (when (execute-first-within-macro ,obj-gs)
             (run-and-update-intermittent-function ,obj-gs :save-time nil))))
       ;; updating the start-time for all objects
       (let ((,start-time-gs (get-internal-time-in-seconds)))
         (dolist (,obj-gs ,main-list-gs)
           (setf (last-execution-time ,obj-gs) ,start-time-gs) ;; ! note that this will overwrite (intentionally) an already saved time for any macro-executed function
           (setf (slot-value ,obj-gs 'start-time) ,start-time-gs)))
       ;; setting VAR to the main-list and proceeding to BODY...
       (let ((,var ,main-list-gs))
         ,@body))))

(defun run-intermittent-functions (intermittent-functions-var)
  "Causes the execution of any \"intermittent functions\" whose time requirements have been met. INTERMITTENT-FUNCTIONS-VAR must be a variable created by the macro WITH-INTERMITTENT-FUNCTIONS. Note that this function is meant to be used within a loop (as many times as necessary) wherever any intermittent-functions may be expected to run. Note that an attempt will be made to execute every intermittent-function in the exact order as presented by the passed variable, thus, as many functions as the LENGTH of INTERMITTENT-FUNCTIONS-VAR may be executed from a single call of this function."
  (dolist (obj intermittent-functions-var)
    (run-intermittent-function-if-appropriate obj)))
