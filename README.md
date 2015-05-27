GULP stores and extracts files from a disk archive.

A GULP archive is a sequence of timestamped updates, each listing the files 
and directories that have been added, changed, or deleted since the previous 
transacted update, normally based on changes to the last-modified dates.

1.0 - GENERAL FILE FORMAT FOR VERSION 0.0.2 

- BEGIN FILE
- CENTRAL DIRECTORY OFFSET-1
  - MARKER                       64            x47/x55/x4C/x50/x00/x00/x02/x00
  - OFFSET                       64
  - DIGEST LEN                   32
  - DIGEST                        8 * DIGEST LEN
- DATA-1
- CENTRAL DIRECTORY-1
  - HEADER-1
    - MARKER                     64            x47/x55/x4C/x50/x00/x00/x02/x00
    - FLAGS   (see note)         32
    - NAME LEN                   32                         if bit 03 in FLAGS
    - NAME                        8 * NAME LEN              if bit 03 in FLAGS
    - TIME                       64                         if bit 04 in FLAGS
    - ATTRIBUTES                 32                         if bit 05 in FLAGS
    - MODE                       32                         if bit 06 in FLAGS
    - SIZE                       64                         if bit 07 in FLAGS
    - LINK NAME LEN              32                         if bit 08 in FLAGS
    - LINK NAME                   8 * LINK LEN              if bit 08 in FLAGS
    - USER ID                    32                         if bit 08 in FLAGS
    - USER NAME LEN              32                         if bit 10 in FLAGS
    - USER NAME                   8 * USER NAME LEN         if bit 10 in FLAGS
    - GROUP ID                   32                         if bit 11 in FLAGS
    - GROUP NAME LEN             32                         if bit 12 in FLAGS
    - GROUP NAME                  8 * USER NAME LEN         if bit 12 in FLAGS
    - STORAGE OFFSET             64                         if bit 13 in FLAGS
    - STORAGE SIZE               64                         if bit 14 in FLAGS
    - STORAGE DIGEST LEN         32                         if bit 15 in FLAGS
    - STORAGE DIGEST              8 * STORAGE DIGEST LEN    if bit 15 in FLAGS
    - COMMENT LEN                32                         if bit 16 in FLAGS
    - COMMENT                     8 * COMMENT LEN           if bit 16 in FLAGS
    - PLATFORM                   32                         if bit 17 in FLAGS
    - HEADER DIGEST LEN          32
      HEADER DIGEST               8 * DIGEST LEN
  - HEADER-2
  - ...
  - HEADER-N
- CENTRAL DIRECTORY OFFSET-2
- DATA-2
- CENTRAL DIRECTORY-2
- ...
- CENTRAL DIRECTORY OFFSET-N
- DATA-N
- CENTRAL DIRECTORY-N
- END FILE

 FLAGS NOTE:
 - bit 00 is reserved for mark last header in current central directory;
 - bit 01 is reserved for mark header added or deleted from archive;
 - bit 02 is reserved but unused.
