# Restore without partial files (#18)

When the data of a file failed the checksum during `restore`, the existing file
had already been deleted and a partial, corrupted file was left in its place.
Operations also did not release streams and lists on errors.

Files are now restored to a temporary file beside the target, which replaces
the existing item only when the checksum matches. `test_atomicrestore.lpr`
corrupts the data of an archive and checks that the restore fails, that the
existing file keeps its content and that no temporary file is left. It then
checks that the intact archive is restored correctly.
