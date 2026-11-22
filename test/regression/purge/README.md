# Purge replaces the archive atomically (#8)

`purge` wrote the purged copy to a temporary file, deleted the archive and then
renamed the temporary file. For an archive given without a directory the
temporary file was created in the system temporary directory: when it is on
another file system the rename failed after the archive had been deleted.

The temporary file is now created beside the archive and renamed over it, and
it is removed if the purge fails. `test_purge.lpr` purges an archive given
without a directory while `TMPDIR` points to a missing directory (so the
temporary file must be created beside the archive), checks that only the latest
versions remain and no temporary file is left. On Unix it checks that a purge
that cannot create the temporary file keeps the archive unchanged (skipped as
root).
