# Regression programs

These programs verify gulp behavior and terminate without interactive input.
Exit code **0** means success; any nonzero exit code means failure. A test must
catch expected errors itself and must return a nonzero status if a check fails.
Each directory contains a `README.md` describing the problem being checked.

Run the tests from the repository root (`instantfpc` needs the leading `./`):

```sh
instantfpc ./.github/workflows/regression.pas
```

The Pascal runner first compiles the command line programs (`src/gulp.pas` and
`gulpdiff.pas`), then compiles each test registered in `tests.txt` with plain FPC,
with `src` and `test/regression/common` in the unit path. The gulpdiff build uses
debug symbols, heap tracing, and range, overflow and I/O checks. No Python is
required. `tests.txt` registers each source and its execution mode:

- `core`: runs on every system.
- `unix`: runs on Unix only (symbolic links, permissions, `lstat`). The runner
  reports it as skipped on other systems.

An optional third manifest field selects the executable (`gulp` by default,
`gulpdiff` for the diff tests). Each test receives the absolute path of its
selected executable as its first argument and runs with an empty results
directory as cwd, where it creates its files. The helpers in
`common/regressionutils.pas` run that executable (`RunGulp`,
`GulpOk`), read its listing (`ListedNames`) and handle files and directories.
Tests that check permissions skip that part when running as root.

The runner saves `build.log`, `run.log` and generated files under
`.regression-build/<mode>/<test-name>/`, ignored by Git. Compilation errors,
launch failures, nonzero statuses and timeouts fail the run. Tests default to a
120-second timeout. The summary reports passed, failed and skipped tests. An
empty selection or malformed manifest is an error.

The `gulpdiff` tests are written entirely in Pascal and included in the same
runner. To run only those tests:

```sh
instantfpc ./.github/workflows/regression.pas --only=gulpdiff/
```

See `gulpdiff/README.md` for their coverage.

Useful local options (all paths can be absolute):

```sh
instantfpc ./.github/workflows/regression.pas --only=purge/
```

Other options: `--root=...`, `--manifest=...`, `--build-dir=...`, `--fpc=...`,
`--timeout=seconds`. Adding a test also requires adding it to `tests.txt`.
