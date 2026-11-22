# gulpdiff regression tests

The tests, helpers and runner are entirely Pascal. Run from the repository root:

```sh
instantfpc ./.github/workflows/regression.pas --only=gulpdiff/
```

The tests are also included in a full regression run. No Python is required.
The runner builds `gulpdiff.pas` with debug symbols, heap tracing, and range,
overflow and I/O checks. Both tests select `gulpdiff` in the third field of
`tests.txt`; they use the existing process/file helpers in `RegressionUtils`.
Build and run logs are saved under `.regression-build/` as for other tests.

- `test_gulpdiff.lpr`: six core cases covering identical and empty directories,
  quoted DATA output, nested/missing files, invalid arguments and file/directory
  mismatches.
- `test_gulpdiff_unix.lpr`: four Unix cases covering dangling symbolic links,
  differing link targets, unreadable files without false DATA differences, and
  permissions. The runner skips this program on non-Unix systems. The unreadable
  file case is explicitly skipped when running as root.
- `gulpdifftestutils.pas`: common setup and process checks, including exit codes,
  reported memory leaks and runtime errors.

Symbolic links are compared by their stored target and metadata, not by hashing
the referenced file. Invalid arguments and file read errors return exit code 2.
For compatibility, successful comparisons still return 0 even if differences
are found.
