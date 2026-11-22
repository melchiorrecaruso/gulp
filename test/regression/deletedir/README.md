# Deleting non-empty directories (#5)

`deletedir` only removed empty directories, because the scanner was given the
directory itself instead of its content. When an archived file had to replace a
non-empty directory, the directory was not deleted, the file was not restored
and the metadata of the file (e.g. mode 644) were applied to the directory.

`test_deletedir.lpr` restores a file over a non-empty directory with nested
subdirectories, hidden files and a symbolic link to an external directory (Unix),
which must not be followed. On Unix it also checks that a directory that cannot
be deleted keeps its own permissions. The permission check is skipped as root.
