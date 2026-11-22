# Restore must not delete the archive (#2)

`restore` deletes the external items that do not exist in the archive. The
archive is never stored in itself, so restoring in the directory that contains
the archive deleted the archive being read.

`test_restorearchive.lpr` restores an archive placed in the restored directory,
given as `arc.gulp` and as `./arc.gulp`, and an archive placed in a directory
that is not archived (`bk/arc.gulp`). The archive, its directory and the
excluded files must survive; other items not in the archive must be deleted.
