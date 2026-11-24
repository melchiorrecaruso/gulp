# Empty file preview

Checks the library operation used by the GUI preview. Creates an archive with
an empty file and verifies that viewing it produces zero bytes, both with the
default snapshot selection and with `OnlyVersion = 1`.

This guards against `TStream.CopyFrom(Source, 0)`, which copies the entire
archive rather than zero bytes.

Run from the repository root:

```sh
instantfpc ./.github/workflows/regression.pas --only=emptyview/
```
