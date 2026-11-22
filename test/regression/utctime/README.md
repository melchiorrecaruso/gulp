# Timestamps stored in real UTC (#19)

Modification times were converted between local time and UTC with the current
offset instead of the offset in effect at the date of the file. Times in the
other daylight saving period were stored one hour off, and the computed time of
an unchanged file changed when the system switched between standard and
daylight saving time, so that synch considered it changed.

`test_utctime.lpr` compares `gettimeutc` and `settimeutc` with timestamps
reported by the OS (`GetFileTime` on Windows, `lstat` on Unix). It checks
winter and summer dates on files and directories, and on symbolic links on
Unix. The program syncs and restores the items through the command line
and checks that timestamps are truncated to whole UTC seconds. On Windows
it also checks that an odd second is preserved and that a change within
the same second does not create a new version.
