
hmenu
=====

`hmenu` is intended to be a drop-in replacement for, and
successor to, the suckless dmenu tool. The core focus is
to make it better at handling streams of data, such as
menu alternatives coming from a slow internet connection.

Much work still needs to be done, though.

Rough todo:

 1. Refactor `TextRendering` so it's not a mess. A bunch
    of arguments could probably be abstracted away in the
    `RenderInfo` data type.
 2. Take care of input from the user!
 3. Read alternatives lazily from stdin.
 4. Search among the alternatives based on what the user
    writes.
 5. Let the user select alternatives with arrow keys and
    such.
 6. Print the alternative to stdout when the user has
    pressed return.
 7. Fix convenience stuff to mimick dmenu better. This
    includes pasting, emacs-like shortcuts for deleting
    and navigation and such.


For Collaborators
-----------------

Module structure:

 *  `Main.hs` is the entry point of the application. This
    is where X gets initialised, a window is created and
    there's an event receiving loop.
 *  `MenuConf.hs` is a module that contains configuration
    information for the application (and also the `AppEvent`
    datatype).
 *  `Graphics.hs` contains the higher-level API for opening
    the menu window, updating its contents and so on.
 *  `TextRendering.hs` contains helper functions for... well,
    rendering text, such as figuring out line heights,
    offsets and what have you.
 *  `X.hs` contains basic wrappers around Xlib procedures.


For users
---------

To build, make sure you have a somewhat recent GHC and
cabal-install &gt;= 1.18 (for sandboxes). Then just

    git clone git@github.com:kqr/hmenu.git
    cd hmenu
    cabal sandbox init
    cabal install --only-dependencies
    cabal build

and then you should have your executable as
`dist/build/hmenu/hmenu`!

Be aware that since `lens` is currently a dependency, installing
might take some time. I should probably think about switching
to a more lightweight lens library at some point.



