# cl-action

This is a library of Common-Lisp functions, macros, etc. to be used in automating mouse and keyboard functionality, allowing the user to programmatically manipulate the mouse and keyboard through Common-Lisp code. It also includes macros for key-binding and an assortment of other macros for regulating timing and duration of actions.

## Purpose

I started creating this library a few years back when implementing the automated clicking of my **[nonogram-solver](https://github.com/jdo555/nonogram-solver)**, upon finding that nothing of the sort existed within the Common-Lisp programming language (for Windows, at least). At its current stage of development, this library can be used for a large variety of tasks. For example, it could be used to set up key-bindings that activate simple or complex desktop actions; it could be used as needed to make tedious and repetitive edits to a text document; it could be used to create a fully automated script for playing browser games, particularly clicker-games. Of course, a myriad of other potential use-cases exist.

## Windows only

Please note that this library currently only works with the Windows operating system (although I had orignally intended to make it work in at least Linux as well, but never got around to doing so).

In manipulating the mouse and keyboard, this library directly uses the **user32** DLL file, and in turn uses a variety of Windows C++ functions, via the Common-Lisp library [CFFI](https://github.com/cffi/cffi). Note that I have included a link to Microsoft's online documentation for each such Windows-based function and structure (in the documentation strings in the code), to aid in understanding how this library is set up.

Note that all available Windows virtual-keys, events, etc. exist as parameters exported from the file **[windows-virtual-keys.lisp](./windows-virtual-keys/windows-virtual-keys.lisp)**. See that file for more information.

## Functionality

For a simple overview of all available functionality I recommend looking at the export list at the top of the file [cl-action.lisp](./cl-action.lisp), although I shall provide a summarized list below. Note that the general user is encouraged to ignore the more difficult functions like MOUSE-ACTION and KEYBD-ACTION, using instead any of the simpler functions that make specific use of either of these; for example, instead of executing following to activate a left-click:
```common-lisp
(progn
  (mouse-action +me-leftdown+)
  (mouse-action +me-leftup+))
```
simply execute this (with or without optional keyed arguments):
```common-lisp
(left-click)
```

As for basic functionality with the mouse, there are functions for handling the following:

1. left-clicking
2. right-clicking
3. middle-clicking
4. scrolling (3 different functions)
5. relative mouse movement (2 functions: immediate or gradual)
6. absolute mouse movement (2 functions: immediate or gradual)
7. exact cursor placement without movement (via SET-CURSOR-POS)
8. getting current cursor position (via GET-CURSOR-POS)
9. mouse-button hold and release
10. click-and-drag (2 functions: relative and absolute movement)
11. repetitive clicking with rate-control and zone-regulation (via CLICKER)

As for basic functionality with the keyboard, there are functions for handling the following:

1. checking a key's state
2. simulating any key-press
3. typing a single character via Unicode value
4. holding and releasing a key
5. simulating a key being held (typematic effect)
6. typing arbitrary keys or text, including unicode (2 functions)
7. typing repeating patterns of arbitrary key-strokes

Timing functions and macros let you do the following:

1. check the current REPL execution time
2. check that a certain number of seconds has elapsed
3. delay execution by count of seconds (fractional or whole number)
4. delay an arbitrary action by x seconds
5. delay x seconds after an arbitrary action
6. do an arbitrary action until x seconds have passed

Listener-like macros let you do the following:

1. do an arbitrary action only once after a specified key-press
2. do an arbitrary action continuously until a specified key-press
3. after a specified key-press, do an arbitrary action continuously until pressed again
4. do an arbitrary action with each press of specified key, until another specified key is pressed
5. do option 3, but from within a waiting state, with a separate key to end that waiting state
6. do an arbitrary action until x seconds expire or a specified key is press

## Basic setup

I recommend running this in library in [Portacle](https://portacle.github.io/). To get started, run the following code in a common-lisp REPL, making sure to provide the appropriate path for the asd file:
```common-lisp
(asdf:load-asd #p"provide/path/to/cl-action.asd")
(asdf:load-system "cl-action")
(in-package :cl-action)
```

## Examples

To get the cursor's current position on the screen, enter:
```common-lisp
(get-cursor-pos)
```

This might yield ```(808 . 353)```; you could move the cursor to that exact spot again (if moved away), by entering:
```common-lisp
(move-mouse-to 808 353)
```

If you wanted to exactly set the cursor (without movement) to the same position anytime the A-key was pressed, enter:
```common-lisp
(do-once-after-toggle-until-end +vk-a+ +vk-space+
  (set-cursor-pos 808 353))
```
Then, to end the macro, press the space-key (so that pressing the A-key will no longer trigger this action).

If you wanted to test a simulated clicking speed online (at, let's say, [this site](https://cpstest.org/10-seconds.php)), then you might do so with the following code, making sure to minimize Portacle and ensure that the browser is open and correctly adjusted, before pressing the A-key (which will cause the clicking to begin):
```common-lisp
(do-once-after-toggle +vk-a+
  ;; make sure that TARGET-X and TARGET-Y are correct for your display
  (clicker :target-x 625 :target-y 589 :x-radius 50 :y-radius 20 :longest-click-delay 0.05 :click-time 10))
```
This way the clicker will click at a rate of between 0.001 and 0.05 clicks per second for 10 seconds, gradually moving the cursor between each click anywhere from 0 to 3 pixels while staying within a set boundary.

Then if you wanted to do the same without a boundary limiting the cursor's movement (while using the default minimum click speed), with clicking to start at the press of the A-key and all clicking to stop at the press of the Z-key, enter:
```common-lisp
(do-once-after-toggle +vk-a+
  (clicker :enforce-boundary nil :terminating-vk +vk-z+))
```

In the following example, tapping the A-key causes the cursor to move briskly to the "minimize all" button on the bottom right of the screen (but only when your screen is the same size as mine) where a single left-click then occurs; here is the code:
```common-lisp
(do-once-after-toggle +vk-a+
  (move-mouse-to-gradually 1439 899 20 0.001) ;; notice that the X and Y are only relevant to my screen-size
  (left-click))
```
Notice that manual mouse movements during the execution of MOVE-MOUSE-TO-GRADUALLY can prevent the cursor from arriving at the correct destination.

In the following example, tapping the A-key causes virtual keys to be used to trigger the "restore down" functionality of a window, via the pressing of RIGHT-WINDOW-KEY + DOWN-KEY:
```common-lisp
(do-once-after-toggle +vk-a+
  (type-vk-list (list (list +vk-rwin+ +vk-down+))))
```

In the following final example, tapping the A-key causes the text "hello" to be typed into the active window, with a backspace clearing away the pressed "a" beforehand:
```common-lisp
(do-once-after-toggle +vk-a+
  (tap-key +vk-back+)
  (type-text "hello"))
```

## More complex functionality

### Key-bindings

I created a specialized function (USE-KEY-BINDINGS) for enabling a more complex key-binding system. With this function you can create a waiting state in the Lisp REPL where it is doing nothing but waiting for one of a few assigned keys, where each key ties to a function or arbitrary action, with one final distinct key to be used to end the waiting state.

Here is a very simple example:

```common-lisp
;; coming soon
```

### Intermittent Functions

I created a specialized class for handling the timing of arbitrary functions/actions within a single macro. There are two timings: scheduled, where every execution of an arbitrary action will occur at once most within any regular time-period of the specified length; and post-execution, where every execution of an arbitrary action will occur, at soonest, after the expiration of a specified amount of time since the end of its very last execution. In the context of video games, the former is useful for automating actions that reset at very exacting regular intervals (for example, daily rewards that reset at 8:00, 16:00, and 00:00 midnight, every 8 hours); and the latter is to be used where cooldowns are involved (for example, an ability that cannot be used again until 75 seconds pass).

Here is a very simple example (that does not fit into the gaming context):

```common-lisp
;; coming soon
```

## Testing and Issues

I have thoroughly tested this library, although there are no standardized tests written up for it. Though there may not technically be any bugs present, there are certainly some issues to be aware of.

Most issues for this library relate to timing and delays. Firstly, it is important to understand that this library does not make use of threading, locking, or anything like promises in any direct way. Secondly, keep in mind that every automated action, such as a mouse movement or a click, is a "scheduled" action; the flow of the Common-Lisp code used to schedule such actions then, does not wait until their completion before proceeding onward; instead, those scheduled actions are passed off and handled separately by the Windows OS; thus, actions may be scheduled faster than they can be completed if delays are too short.

The solution to this is always providing enough of a delay (via the function SLEEP or DELAY-WHEN-SET) for any action that can be scheduled with great speed, either in direct sequence with another function call, or (in particular) from within a loop. For example, if using the clicker with 0 delay between clicks, the clicks can be scheduled much faster than they can be completed, due to the sheer speed of the DO-loop that handles the click-scheduling; thus, upon ending the loop, clicks will continue even afterwards (potentially for quite a while); caution is important on this point, since unceasing fast clicks could be quite dangerous to one's pc with the cursor in certain places. Nonetheless, I have still allowed passing a 0 delay to CLICKER, since that is the best way to get the highest number of clicks... but again, be careful.

As another example, if MOVE-MOUSE-TO is used and immediately followed by a call to GET-CURSOR-POS without any delays, then as long as the movement distance is great enough, the value returned by GET-CURSOR-POS will not match the actual terminal position of the movement, since the call to GET-CURSOR-POS was completed before the mouse-movement, implying that certain Windows-based functions are scheduled, perhaps, into different queues. All this is to say, again, that delays are important between the scheduling of actions; it is strongly advised to always have a delay of at least 0.001 where a delay can be set.

Another issue is that, in some application environments, the defualt use of LEFT-CLICK, for example, does not work because the delay between the actual press and release is too fast when no delay is set, such that the left-click does not even register. In those cases simply set the value of the keyed argument **delay-between-press-and-release** to something greater than 0. Similar issues could of course affect RIGHT-CLICK and MIDDLE-CLICK, and might even sometimes affect TAP-KEY; the same keyed argument as used above can be used to address this if necessary.

Another issue is that the minimal SLEEP time within Windows is not particuarly fast (I thought that it was 0.016 seconds -- although I'm not certain of this value). And that acts as a cap on every action that uses a delay. Keep in mind that manually setting a very fast delay of something like 0.00001 seconds will do nothing to override this cap in the delay-speed.

Also, note that I have not restricted X and Y values for the functions MOVE-MOUSE and MOVE-MOUSE-TO to allow for possible irregularities as brought on by multi-monitor setups (as I'm not sure how that would affect things), among other reasons. Keep in mind, though, that passing reasonable X and Y values for such functions is important although it is not necessarily checked.

On the topic of mouse movement, as suggested in one of the above examples, when executing cursor-moving functions, the user is NOT expected to be using the mouse manually at the same time; this is especially important for the "gradual" cursor-moving functions.

Lastly, note that such issues as these are either alluded to or even elaborated upon within the code's documentation strings where relevant.