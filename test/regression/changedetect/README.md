# Detecting changed items (#17)

`synch` and `restore` compared only the modification time, so a change of type
(file, directory, link), size or permissions with the same time was not
detected. The README documented size and permissions as change criteria.

`test_changedetect.lpr` keeps the modification time unchanged and checks that a
permission change, a size change and a file replaced by a directory are
recorded by `synch`, that `restore` turns a file back into the archived
directory, and that repeated synch and restore operations find no change.
Unix only (permissions).
