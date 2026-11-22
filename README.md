#GULP - A simple journaling archiver.   

**GULP** is a free and open source incremental, journaling command-line archiver for Windows and Linux. Journaling means that the archive is append-only. When you add files or directories to the archive, both the old and new versions are saved. You can recover old versions by specifying version number.

## Build

Source code is written in pascal and it can be compiled with Lazarus/FreePascal.

###Requirements

- FreePascal >= 3.0
- Lazarus >= 1.6

###Instructions

If you have problems with permissions don't forget to prefix with `sudo`

1. Clone the Gulp repository :

```sh
  git clone https://github.com/melchiorrecaruso/gulp.git
  cd gulp
```

2. Checkout the latest Gulp release:

```sh
  git fetch -p
  git checkout $(git describe --tags `git rev-list --tags --max-count=1`)
```

3. Build Gulp:

```sh
  lazbuild gulp.lpi
```
  This will create the gulp application at `build/$(TargetCPU)-$(TargetOS)/bin`.

4. Install the `Gulp` to `/usr/local/bin` by executing:

```sh
  sudo cp build/$(TargetCPU)-$(TargetOS)/bin/gulp /usr/local/bin/gulp
```

To use the newly installed Gulp, quit and restart all running Gulp instances.

##Usage
Show the program version with `gulp --version`.

The first argument to gulp should be a function; either one of the letters or one of the long function names. A function letter needs to be prefixed with "-", and can't be combined with other single letter options. A long function name must be prefixed with "--".  Some options take a parameter; with the single-letter form these must be given as separate arguments. With the long form, they must be given by appending "=value" to the option.

```sh
SYNOPSIS

	gulp [-] s --synch | r --restore | p --purge | l --list |
	    	 c --check | f --fix [options ...] [ files ...]
	gulp --version
```

##Main function mode
                                           
####Synchronize archive (*-s*, *--synch*)
Append changes in files  to archive, or create archive if it does not exist. "files" is a list of file and directory names separated by spaces. If a name is a directory, then it recursively includes all files and subdirectories within. "files" may contain wildcards "\*" and "?" in the last component of the path (after the last slash). "\*" matches any string and "?" matches any character. 

A change is an addition, update, or deletion of any file or directory in "files" or any of its subdirectories to any depth. A file or directory is considered changed if its type, size, last-modified date, Unix/Linux permissions or Windows attributes, or symbolic link target differ between the internal and external versions. File contents are not compared. Only files and directories within "files" are marked as deleted when they no longer exist.

For each added or updated file or directory, the following information is saved in the archive: the contents, the file or directory name as it appears in "files" plus any trailing path, the last-modified date (UTC) and the Unix/Linux permissions or Windows attributes. Other metadata such as owner name, group name, last access time, etc. are not saved. Symbolic links are saved. Hard links are followed as if they were ordinary files. Special file types such as devices, named pipes, and named sockets are not saved. If any file cannot be read (e.g. permission denied), then it is skipped with a warning and retried at the next synch. However, other files are still added and the update is still valid. The archive itself is never added.

###
> **Note** :
>
> - Updates are transacted. If an update fails, it is removed from the archive. If GULP is interrupted before completing the update, then the archive can be repaired with fix function.
>

###
> **Example** :
>
> Create archive.gulp from directories foo and bar
>
	gulp -s archive.gulp foo bar
>

#### Restore files and directories (*-r*, *--restore*)
Restore "files" (including the contents of directories), or extract the whole archive contents if "files" is omitted. The file names, last-modified date, and permissions or attributes are set as saved in the archive. 

###
> **Note** : 
>
> - If there are multiple versions of a file stored, then only the latest version is extracted. 
>
> - If a stored file has been marked as deleted, then it is deleted. Existing files are overwritten if they are considered changed; a file is replaced only after its data has been verified.
>
> - Unless *--nodelete* is given, external files and directories within "files" that do not exist in the archive are deleted. Excluded items and the archive itself are never deleted.
>

###
>**Example** :
>
> Restore all files from archive.gulp into current directories
>
	gulp -r archive.gulp
>

#### Purge archive (*-p*, *--purge*)
Purge archive, remove old files archived. The archive is replaced only when the purged copy has been completely written.

#### List archive contents (*-l*, *--list*)
List "files" within the archive, or list the entire archive contents if "files" is omitted. For each file or directory, show version number, marker, Unix/Linux permissions, attributes, last-modified date, size and name. 

###
> **Note** : 
>
> - Permissions are listed as an octal number (as per chmod(1)) and attributes with the letters R, H, S, V, D, A, L (read-only, hidden, system, volume, directory, archive, symbolic link).
>
> - The last line shows the last version of the archive.
>

###
>**Example** :
>
> List all files in archive.gulp
>
	gulp -l archive.gulp
>

#### Check archive integrity (*-c*, *--check*)
Check archive integrity by verifying that the data agrees with the stored SHA-1 hashes and sizes and that the data otherwise conforms to the gulp standard.

#### Fix damaged archive (*-f*, *--fix*)
Truncates any data added after last valid update.

</br>
## Options

#### Exclude pattern (*-e*, *--exclude=PATTERN*)
Avoid operating on files whose names match filename pattern PATTERN. The option prevents any file or member whose name matches the shell wildcard (pattern) from being operated on. Multiple exclude options are supported. A directory excludes all its content; a trailing slash is ignored ("dir" and "dir/" are the same). 

>**Example** :
>
> List all files in archive.gulp but exclude .pas files
>
	gulp -l archive.gulp --exclude='*.pas'
>

#### Exclude attributes (*--excludeattr=ATTR*)
Avoid operating on files whose attributes include attributes ATTR. 

>**Example** :
>
> List all files in archive.gulp but exclude directories and hidden files
>
	gulp -l archive.gulp --excludeattr=DH
>

#### Exclude modes (*--excludemode=MODE*)
Avoid operating on files whose modes include modes MODE. 

>**Example** :
>
> List all files in archive.gulp but exclude executables
>
	gulp -l archive.gulp --excludemode=111
>

#### Force path (*--forcepath*)
It's recommended store files in archive with relative path names. This option forces GULP to operate on files with absolute path.

#### Include option (*-i*, *--include=PATTERN*)
Operating on files whose names match filename pattern PATTERN.
Multiple include options are supported. A directory includes all its content; a trailing slash is ignored ("dir" and "dir/" are the same). 

>**Example** :
>
> List *.pas files in archive.gulp
>
	$ gulp -l archive.gulp --include=*.pas
>

#### Not delete files (*--nodelete*)
With synch command (***-s***) , do not mark files in the archive as deleted when the corresponding external file does not exist. With restore command (***-r***), do not delete external files when the corresponding file in archive does not exist. 

>**Note**: 
>
> - This makes **GULP** consistent with the behavior of most non-journaling archivers.

#### Only option (*--only=VERSION*)
With list, show only the changes made in version VERSION. 

>**Example** :
>
> - show files added or deleted in version 20
>
		gulp -l archive.gulp --only=20
> 

#### Until option (*-u*, *--until=VERSION*)
Ignore any part of the archive updated after version number. 

>**Example** :
>
> - restore content of archive until version 120 into current directories
>
		gulp -r archivename.gulp -u 120

> - show files added before version 21
>
		gulp -l archive.gulp --until=20
> 

</br>
##General file format version 0.5

A **GULP** archive is a sequence of timestamped updates, each listing the files and directories that have been deleted or added since the previous transacted update.

#### Archive content:
``` sh
	- Slice 1
 	- Slice 2
	- ...
	- Slice N
```
#### Slice content:
``` sh
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

#### Header content:
``` sh
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

>
> **Note:** 
> 
> - rawbytestring is stored as its length (longint, 4 bytes) followed by its bytes;
> - mTime and sTime are TDateTime values (days since 1899-12-30) in UTC;
> - Offset1 and Offset2 are the start and end positions of the stored data in the archive;
> - Checksum1 is the SHA-1 digest of the header fields from Name to Offset2;
> - Checksum2 is the SHA-1 digest of the stored data;
> - bit 1 in Flags is used to mark header added to archive;
> - bit 2 in Flags is used to mark header deleted from archive;
> - bit 3 in Flags is used to mark last header in current slice.
>

</br>

##Credits

**GULP** is copyright (c) 2014-2026, Melchiorre Caruso. It is  licensed under The GNU General Public License v2. For information on the license, see [GPLv2](http://www.gnu.org/copyleft/gpl.html) . Program was written by Melchiorre Caruso,  melchiorrecaruso (at) gmail (dot) com
