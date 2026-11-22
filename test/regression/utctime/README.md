# Timestamps stored in real UTC (#19)

Modification times were converted between local time and UTC with the current
offset instead of the offset in effect at the date of the file. Times in the
other daylight saving period were stored one hour off, and the computed time of
an unchanged file changed when the system switched between standard and
daylight saving time, so that synch considered it changed.

`test_utctime.lpr` uses the gulp units directly: the time returned by
`gettimeutc` for files and links dated in winter and in summer must equal the
Unix time reported by `lstat`, and `settimeutc` must set exactly that time.
The program also restores such files through the command line and compares
their times. Unix only; it is meaningful with a time zone that has daylight
saving time.
