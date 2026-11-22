# The archive must not be added to itself (#7)

`synch` excluded the archive by comparing its name as a string, so an archive
given as `./arc.gulp`, with an absolute path or with `..` components was added
to itself. Paths are now compared after expansion.

`test_selfarchive.lpr` creates archives with several path forms, including one
inside a synchronized subdirectory, and checks that none of them contains
itself and that a second synch adds nothing.
