# Synch of a subset must not mark other files as deleted (#6)

When `synch` was given some files, every archived item outside them was marked
as deleted. Only items within the include patterns, and not excluded, can now
be marked as deleted. A directory pattern also covers its archived content when
the directory no longer exists.

`test_syncinclude.lpr` checks each update with `--only=VERSION`: a synch of an
unchanged file creates no update, a deleted file given explicitly is marked as
deleted alone, a deleted directory is marked as deleted with all its content,
an excluded deleted file is kept and a full synch then records its deletion.
