# GULP: A simple journaling archiver

**GULP** is a free, open-source incremental archiver for Windows and Linux. Each synchronization appends changes to the archive without overwriting earlier versions. Use `--until=VERSION` to list or restore a snapshot from a specific version. Purging keeps only the latest snapshot and discards the version history.

## Build

Install Lazarus (including `lazbuild`) and Free Pascal. To build the command-line archiver from the current sources:

```sh
git clone https://github.com/melchiorrecaruso/gulp.git
cd gulp
lazbuild gulp.lpi
```

The executable is `build/bin/gulp` on Linux and `build/bin/gulp.exe` on Windows. On Linux, you can install it system-wide with:

```sh
sudo install -m 755 build/bin/gulp /usr/local/bin/gulp
```

On Windows, run `build\bin\gulp.exe` or add its directory to `PATH`.

To build the graphical interface (`gulp-ux`), install LCL and LazControls, then run `lazbuild gulpux.lpi`. To build the directory comparison tool (`gulp-diff`), run `lazbuild gulpdiff.lpi`. Both executables are written to `build/bin` and have an `.exe` extension on Windows.

## Usage

Choose one main operation per command. Each archive operation requires a path to the archive. With a short option, pass the path as a separate argument (`-s archive.gulp`). With a long option, put it after `=` (`--synch=archive.gulp`). The same syntax applies to other options that take a value. Do not combine short options into a single argument.

```text
gulp (-s ARCHIVE | --synch=ARCHIVE)   [OPTIONS] [FILES ...]
gulp (-r ARCHIVE | --restore=ARCHIVE) [OPTIONS] [FILES ...]
gulp (-l ARCHIVE | --list=ARCHIVE)    [OPTIONS] [FILES ...]
gulp (-p ARCHIVE | --purge=ARCHIVE)   [OPTIONS]
gulp (-c ARCHIVE | --check=ARCHIVE)   [OPTIONS]
gulp (-f ARCHIVE | --fix=ARCHIVE)     [OPTIONS]
gulp -h | --help
gulp --version
```

`--version` prints the program version (currently `0.5`). The exit status is `0` on success and `1` on error. Use GULP to back up user files, not an entire operating system.

## Operations

### Synchronize an archive (`-s`, `--synch`)

Append changes to an existing archive or create a new one. To select paths, pass them as positional arguments (`FILES`) or use `--include`. If you do neither, GULP scans the current directory. Selecting a directory includes everything under it. The last component of a path may contain `*` (any string) or `?` (any single character). Quote wildcard patterns to prevent the shell from expanding them before GULP sees them.

To detect changes, GULP compares the entry type and the modification time, truncated to whole seconds in UTC. It also compares the size of regular files. On Linux, it compares permissions and symbolic link targets; on Windows, it compares file attributes. It does not compare file contents. GULP records a deletion only if an entry is missing from disk, falls within the selected paths, and is not excluded.

The archive stores paths, file contents, and modification times. On Windows it also stores file attributes; on Linux it stores permissions and numeric user and group IDs. It does not store user or group names or last access times. On Linux, GULP preserves symbolic links without following them and treats hard links as separate files. GULP does not archive devices, named pipes, sockets, or the archive itself. If it finds an unreadable file, it warns you, skips the file, and can try again the next time you synchronize.

> **Note:** If an update fails, GULP rolls it back. If an interruption leaves an incomplete update in the archive, use `--fix` to repair it.

**Example:** Create `archive.gulp` from the `foo` and `bar` directories.

```sh
gulp -s archive.gulp foo bar
```

### Restore files and directories (`-r`, `--restore`)

By default, GULP restores the selected paths from the latest snapshot. Use `--until` to restore a different version. If you specify neither paths nor include patterns, GULP restores the entire snapshot. Before replacing a regular file, GULP verifies the restored data. It applies the saved modification time and, on Windows, the saved file attributes. On Linux, it attempts to restore permissions and ownership from the saved user and group IDs.

> **Note:** By default, GULP can delete files and directories within the selected paths if they are absent from the snapshot, even if they were never archived. Use `--nodelete` to keep those entries. This option does not stop GULP from replacing changed files. To avoid affecting unrelated files, restore into an empty directory.

**Example:** Restore all files from `archive.gulp` into the current directory without removing files that are absent from the archive.

```sh
gulp -r archive.gulp --nodelete
```

### Purge an archive (`-p`, `--purge`)

Keep only the latest snapshot and discard older versions and deletion records. GULP writes the new archive in the same directory as the original and replaces the original only after the new copy is complete. If the snapshot is not empty, it becomes version 1.

### List archive contents (`-l`, `--list`)

By default, list the latest snapshot. Use `--until` to list a snapshot from a specific version or `--only` to list the changes made in that version. Pass paths or use include and exclude patterns to narrow the results. Each entry shows its version, change marker, and name. Additions also show permissions, attributes, the last modification time (in the local time zone), and file size where applicable.

> **Note:**
>
> - Permissions appear in octal, as in `chmod(1)`. Attributes appear as letters: `R` (read-only), `H` (hidden), `S` (system), `V` (volume), `D` (directory), `A` (archive), and `L` (symbolic link).
> - `Last version` counts all complete updates in the archive, even if filters hide some of them.

**Example:** List all files in `archive.gulp`.

```sh
gulp -l archive.gulp
```

### Check archive integrity (`-c`, `--check`)

Check the archive structure and verify the SHA-1 checksums of the entry headers and stored file data.

### Fix a damaged archive (`-f`, `--fix`)

Truncate the archive after its last complete update. Use `--check` to verify the stored file data.

## Options

### Exclude paths by pattern (`-e`, `--exclude=PATTERN`)

Exclude paths that match `PATTERN`. Repeat this option to add more patterns. Excluding a directory also excludes everything under it. A trailing slash makes no difference: `dir` and `dir/` mean the same thing.

**Example:** List all files in `archive.gulp` but exclude `.pas` files.

```sh
gulp -l archive.gulp --exclude='*.pas'
```

### Exclude attributes (`--excludeattr=ATTR`)

Exclude items that have any of the specified attributes (`R`, `H`, `S`, `V`, `D`, `A`, `L`).

**Example:** List all files in `archive.gulp` but exclude directories and hidden files.

```sh
gulp -l archive.gulp --excludeattr=DH
```

### Exclude modes (`--excludemode=MODE`)

Exclude items that have any of the permission bits specified by the octal mask `MODE`.

**Example:** List the contents of `archive.gulp`, excluding entries with any execute permission bit set.

```sh
gulp -l archive.gulp --excludemode=111
```

### Allow absolute paths (`--forcepath`)

Allow absolute source paths when synchronizing. Use relative paths if you plan to restore the archive to a different location.

### Include paths by pattern (`-i`, `--include=PATTERN`)

Include paths that match `PATTERN` in addition to any paths you pass as positional arguments. Repeat this option to add more patterns. Including a directory also includes everything under it. As with `--exclude`, a trailing slash makes no difference.

**Example:** List `*.pas` files in `archive.gulp`.

```sh
gulp -l archive.gulp --include='*.pas'
```

### Skip deletions (`--nodelete`)

During synchronization, do not record deletions for entries that are missing from disk. During restoration, keep existing entries that are absent from the selected snapshot.

### List changes in one version (`--only=VERSION`)

With `--list`, show only the changes made in the specified version, including deletions. If the requested version is above a positive `--until` limit, the listing shows no changes. Set `--only=0` to turn off this filter.

**Example:** Show files added or deleted in version 20.

```sh
gulp -l archive.gulp --only=20
```

### Select a snapshot (`-u`, `--until=VERSION`)

List or restore the snapshot at the specified version. Versions start at 1. Use `--until=last` for the latest snapshot, which is the default. With `--list`, a value of `0` shows the full change history unless the `--only` filter is active. With `--restore`, a value of `0` selects the latest snapshot.

**Examples:**

- Restore version 120 of the archive into the current directory:

  ```sh
  gulp -r archivename.gulp -u 120
  ```

- Show the archive snapshot as of version 20:

  ```sh
  gulp -l archive.gulp --until=20
  ```

### Verbose output (`--verbose`)

Show per-item progress while synchronizing, restoring, checking, fixing, or purging an archive.

## Regression tests

With Free Pascal installed, run the following command from the root of the repository:

```sh
instantfpc ./.github/workflows/regression.pas
```

See [the regression test guide](test/regression/README.md) for test selection and platform requirements.

## Archive format (GULP 0.5)

A GULP archive is a sequence of timestamped updates called slices. Each slice contains the entry headers, followed by the data for the regular files stored in that update. The order of the slices determines the version numbers; the headers do not store them.

### Archive layout

```text
- Slice 1
- Slice 2
- ...
- Slice N
```

### Slice layout

```text
- Marker          (20 bytes)
- Header      1
- Marker          (20 bytes)
- Header      2
- ...
- Marker          (20 bytes)
- Header      N
- Stored data 1
- Stored data 2
- ...
- Stored data N
```

### Header layout

```text
- Name         rawbytestring
- Flags        set             ( 4 bytes)
- mTime (UTC)  double          ( 8 bytes)
- sTime (UTC)  double          ( 8 bytes)
- Attributes   set             ( 4 bytes)
- Mode         set             ( 4 bytes)
- Size         int64           ( 8 bytes)
- Link Name    rawbytestring
- User ID      longint         ( 4 bytes)
- Group ID     longint         ( 4 bytes)
- User Name    rawbytestring
- Group Name   rawbytestring
- Comment      rawbytestring
- Offset1      int64           ( 8 bytes)
- Offset2      int64           ( 8 bytes)
- Checksum1    array of bytes  (20 bytes)
- Checksum2    array of bytes  (20 bytes)
```

> **Note:**
>
> - A `rawbytestring` begins with its length (a 4-byte `longint`), followed by the string bytes.
> - `mTime` (modification time) and `sTime` (update time) are UTC `TDateTime` values, measured in days since 1899-12-30. GULP truncates newly written timestamps to whole seconds.
> - `Offset1` gives the position of the first byte of stored data, relative to the start of the archive. `Offset2` gives the position immediately after the last byte.
> - `Checksum1` is the SHA-1 digest of the header fields from `Name` through `Offset2`, excluding string length prefixes. `Checksum2` is the SHA-1 digest of the stored file data.
> - The `Flags` field uses `0x01` for an addition, `0x02` for a deletion, and `0x04` to mark the last header in a slice. GULP records a changed entry as a deletion followed by an addition.

## Credits

Copyright (c) 2014–2026 Melchiorre Caruso (melchiorrecaruso (at) gmail (dot) com). GULP is licensed under the [GNU General Public License, version 2](LICENSE), or (at your option) any later version.
