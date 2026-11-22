# Command line parsing (#9, #10, #11, #12, #13, #14)

Several command line problems are checked by `test_commandline.lpr`:

- `-h` was rejected as an invalid option (#9);
- option arguments contained in the option list, such as `a` or `in`, were
  mistaken for options, because options were found with a substring search (#10);
- only the first `-e`/`-i` option was used (#11);
- `--synch archive` was accepted and then failed with an empty archive name;
  long options need the `--option=value` form (#12);
- a missing argument was reported as "does not allow an argument", and options
  without argument accepted `=value` (#13);
- errors returned exit code 255 instead of the documented 1 (#14).
