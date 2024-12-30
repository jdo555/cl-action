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

## Examples

...

## More complex functionality

...