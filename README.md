
hmenu
=====

`hmenu` is intended to be a drop-in replacement for, and
successor to, the suckless dmenu tool. The core focus is
to make it better at handling streams of data, such as
menu alternatives coming from a slow internet connection.

Much work still needs to be done, though.


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
    the menu window, printing strings to it and so on.
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

Be aware that since `lens` is currently a dependency, building
might take some time. I should probably think about switching
to a more lightweight lens library at some point.



