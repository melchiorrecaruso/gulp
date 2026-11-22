# Unreadable files during synch (#4)

A file that could not be opened during `synch` was stored without data and with
a null checksum, so any later `restore` failed with a checksum error; the file
was never retried. Unreadable files are now skipped with a warning and retried
at the next synch; if the data of a file cannot be read while the update is
written, the whole update is removed from the archive.

`test_unreadable.lpr` synchronizes a directory with an unreadable file, restores
the archive, makes the file readable again and checks that it is added by the
next synch. Unix only; skipped when running as root.
