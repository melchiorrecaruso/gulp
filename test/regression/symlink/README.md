# Restoring symbolic links (#1, #16)

The modification time of a symbolic link could not be restored, and every
restored link produced two warnings ("Unable to set date" and "Unable to set
mode"), because the time was set through the link and the mode of a link is
not used on Linux.

`test_symlink.lpr` restores a valid and a dangling link after waiting more than
one second, so that a link created at restore time would have a different
time, and checks that the times of the links are restored and no warning is
printed. Unix only.
