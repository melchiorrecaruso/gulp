# Last version reported by list (#15)

`list` reported the highest version among the listed items as the last
version. Versions that only delete items, or whose items were replaced later,
were not counted. The message also read "Lastest version".

`test_lastversion.lpr` creates a second version that only deletes a file and
checks that `list` reports "Last version 2", also with `--until`, `--only` and
an include pattern.
