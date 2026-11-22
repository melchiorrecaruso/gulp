# Restore of a subset must not delete other files (#3)

When `restore` was given some files or patterns, every other external item was
deleted, because the deletion loop also removed items outside the requested
patterns.

`test_restoreinclude.lpr` restores a single file and a single directory and
checks that items outside them are left untouched, while items inside a
restored directory that are not in the archive are deleted. A full restore must
still delete the items that are not in the archive.
