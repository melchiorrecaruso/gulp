# Trailing slash in paths

A trailing path delimiter turned a directory into the pattern `dir/*`, which
matches the content but not the directory: `-i dir/` did not store the
directory itself, `-e dir/` stored it empty, and `gulp -s arc dir/` stored the
directory with the name `dir/`.

`test_trailingslash.lpr` checks that `dir`, `dir/` and `dir//` give the same
result with `-i`, `-e`, plain file arguments, `list` and `restore`.
