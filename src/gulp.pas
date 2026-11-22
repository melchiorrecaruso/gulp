{ Description: Shell application unit.

  Copyright (C) 2014-2022 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

program gulp;

{$mode objfpc}
{$H+}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFDEF UNIX}
  baseunix,
{$ENDIF}
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  classes,
  gulpcommandline,
  gulpcommon,
  gulpfixes,
  gulplibrary,
  gulplist,
  gulpmessages,
  sysutils;

type
  { gulp shell application }

  tshellapplication = class
  protected
    procedure showitem(p: pointer);
    procedure showmessage(const message: rawbytestring);
    procedure abort;
  public
    constructor create;
    destructor destroy; override;
    procedure run;
    procedure help;
  end;

  constructor tshellapplication.create;
  begin
    inherited create;
    defaultformatsettings.longdateformat  := 'yyyy-mm-dd';
    defaultformatsettings.shortdateformat := 'yyyy-mm-dd';
    exitcode := 1;
  end;

  destructor tshellapplication.destroy;
  begin
    inherited destroy;
  end;

  procedure tshellapplication.showmessage(const message: rawbytestring);
  begin
    writeln(message);
  end;

  procedure tshellapplication.showitem(p: pointer);
  begin
    with tgulpitem(p^) do
      if gfadd in flags then
      begin
        if isdirectory(attr) then
          writeln(format('%4s %3s %3s %7s %19s %12s %s',
            [version2str(version),
             flags2str(flags),
             mode2str(l2smode(mode)),
             attr2str(l2sattr(attr)),
             time2str(universaltime2local(mtime)),
             '', name]))
        else
          writeln(format('%4s %3s %3s %7s %19s %12s %s',
            [version2str(version),
             flags2str(flags),
             mode2str(l2smode(mode)),
             attr2str(l2sattr(attr)),
             time2str(universaltime2local(mtime)),
             size2str(size), name]));
      end else
      if isdirectory(attr) then
        writeln(format('%4s %3s %3s %7s %19s %12s %s',
          [version2str(version), flags2str(flags), '', '', '', '', name]))
      else
        writeln(format('%4s %3s %3s %7s %19s %12s %s',
          [version2str(version), flags2str(flags), '', '', '', '', name]));
  end;

  procedure tshellapplication.run;
  // A trailing path delimiter does not change the meaning of a path:
  // "dir/" is the same as "dir" (the directory and all its content).
  function normalizepath(const path: rawbytestring): rawbytestring;
  begin
    result := path;
    while (length(result) > 1) and (result[length(result)] in allowdirectoryseparators) do
      setlength(result, length(result) - 1);
  end;

  var
    app:   tgulpapplication;
    files: trawbytestringlist;
    i:     longint;
    operationcount: longint;
    s:     rawbytestring;
    start: tdatetime;
  begin
    start := now;
    app   := tgulpapplication.create;
    files := trawbytestringlist.create;
    app.onshowitem     := @showitem;
    app.onshowmessage1 := @showmessage;
    app.onshowmessage2 := nil;
    app.onshowwarning  := @showmessage;

    try
      s := checkoptions('-s: -r: -p: -l: -c: -f: -u: -i: -e: -h        ',
        '--synch: --restore: --purge:   --list:    --check:    --fix:  ' +
        '--until: --include: --exclude: --help     --verbose   --only: ' +
        '--excludeattr: --excludemode:  --nodelete --forcepath         ',
        files);

      if s = '' then
      begin
        operationcount := length(getoptionvalues('-s', '--synch')) +
          length(getoptionvalues('-r', '--restore')) +
          length(getoptionvalues('-p', '--purge')) +
          length(getoptionvalues('-c', '--check')) +
          length(getoptionvalues('-f', '--fix')) +
          length(getoptionvalues('-l', '--list'));
        if hasoption('-h', '--help') then inc(operationcount);
        if operationcount > 1 then
          raise exception.create('Specify only one main operation');

        for i := 0 to files.count - 1 do
          app.include.add(normalizepath(files[i]));

        for s in getoptionvalues('-e', '--exclude') do
          if s <> '' then
            app.exclude.add(normalizepath(s));
        if hasoption('', '--excludeattr') = true then
        begin
          app.excludeattr := str2attr(getoptionvalue('', '--excludeattr'));
        end;
        if hasoption('', '--excludemode') = true then
        begin
          app.excludemode := str2mode(getoptionvalue('', '--excludemode'));
        end;
        for s in getoptionvalues('-i', '--include') do
          if s <> '' then
            app.include.add(normalizepath(s));
        if hasoption('', '--only') = true then
        begin
          app.onlyversion := strtoint(getoptionvalue('', '--only'));
        end;
        if hasoption('-u', '--until') = true then
        begin
          if getoptionvalue('-u', '--until') = 'last' then
            app.untilversion := $ffffffff
          else
            app.untilversion := strtoint(getoptionvalue('-u', '--until'));
        end;
        if hasoption('', '--verbose') then
        begin
          app.onshowmessage2 := @showmessage;
        end;

        app.nodelete  := hasoption('', '--nodelete');
        app.forcepath := hasoption('', '--forcepath');

        if hasoption('-s', '--synch') then
          app.sync(getoptionvalue('-s', '--synch'))
        else
        if hasoption('-r', '--restore') then
          app.restore(getoptionvalue('-r', '--restore'))
        else
        if hasoption('-p', '--purge') then
          app.purge(getoptionvalue('-p', '--purge'))
        else
        if hasoption('-c', '--check') then
          app.check(getoptionvalue('-c', '--check'))
        else
        if hasoption('-f', '--fix') then
          app.fix(getoptionvalue('-f', '--fix'))
        else
        if hasoption('-l', '--list') then
          app.list(getoptionvalue('-l', '--list'))
        else
        if hasoption('-h', '--help') then
          help
        else
          help;

        exitcode := 0;
      end else
        writeln(#13, #13: 80, s);

    except
      on e: exception do
        writeln(#13, #13: 80,
          format('An exception was raised: "%s"', [e.message]));
    end;

    files.destroy;
    app.destroy;
    writeln(#13, #13: 80, 'Elapsed ',
      format('%0.2f', [(now - start) * (24 * 60 * 60)]), ' sec');
  end;

  procedure tshellapplication.abort;
  begin
    raiseexception('User abort');
  end;

  procedure tshellapplication.help;
  begin
    writeln;
    writeln('NAME');
    writeln('       gulp - A simple user level journaling archiver.');
    writeln;
    writeln('SYNOPSIS');
    writeln('       gulp [-] s --synch | r --restore | p --purge | l --list |');
    writeln('                c --check | f --fix | h --help [ options ...] [ files ...]');
    writeln;
    writeln('DESCRIPTION');
    writeln('       gulp stores and extracts files from a disk archive. A GULP archive');
    writeln('       is a sequence of timestamped updates, each listing the files and');
    writeln('       directories that have been added, changed, or deleted since the');
    writeln('       previous transacted update.');
    writeln;
    writeln('       The first argument to gulp should be a function; either one of the');
    writeln('       letters srplcfh, or one of the long function names. A function');
    writeln('       letter needs to be prefixed with ''-'', and can''t be combined with');
    writeln('       other single-letter options. A long function name must be prefixed');
    writeln('       with ''--''. Some options take a parameter; with the single-letter');
    writeln('       form these must be given as separate arguments. With the long form,');
    writeln('       they must be given by appending =value to the option.');
    writeln;
    writeln('FUNCTION LETTERS');
    writeln('       Main operation mode:');
    writeln;
    writeln('       -s, --synch');
    writeln('              append changes in files to archive, or create archive if it');
    writeln('              does not exist. files is a list of file and directory names');
    writeln('              separated by spaces. If a name is a directory, then it');
    writeln('              recursively includes all files and subdirectories within.');
    writeln('              files may contain wildcards * and ? in the last component of');
    writeln('              the path (after the last slash). * matches any string and ?');
    writeln('              matches any character.');
    writeln;
    writeln('              A change is an addition, update, or deletion of any file or');
    writeln('              directory in files or any of its subdirectories to any depth.');
    writeln('              A file or directory is considered changed if its type, size,');
    writeln('              last-modified date, Unix/Linux permissions or Windows');
    writeln('              attributes, or symbolic link target differ between the');
    writeln('              internal and external versions. File contents are not');
    writeln('              compared. Only files and directories within files are marked');
    writeln('              as deleted when they no longer exist.');
    writeln;
    writeln('              For each added or updated file or directory, the following');
    writeln('              information is saved in the archive: the contents, the file');
    writeln('              or directory name as it appears in files plus any trailing');
    writeln('              path, the last-modified date (UTC), and the Unix/Linux');
    writeln('              permissions or Windows attributes. Other metadata such as');
    writeln('              owner, group, last access time, etc. are not saved. Symbolic');
    writeln('              links are saved. Hard links are followed as if they were');
    writeln('              ordinary files. Special file types such as devices, named');
    writeln('              pipes, and named sockets are not saved. If any file cannot be');
    writeln('              read (e.g. permission denied), then it is skipped with a');
    writeln('              warning and retried at the next synch. However, other files');
    writeln('              are still added and the update is still valid. The archive');
    writeln('              itself is never added.');
    writeln;
    writeln('              Updates are transacted. If an update fails, it is removed');
    writeln('              from the archive. If gulp is interrupted before completing');
    writeln('              the update, then the archive can be repaired with the fix');
    writeln('              function.');
    writeln;
    writeln('       -r, --restore');
    writeln('              restore files (including the contents of directories), or');
    writeln('              extract the whole archive contents if files is omitted. The');
    writeln('              file names, last-modified date, and permissions or attributes');
    writeln('              are set as saved in the archive. If there are multiple');
    writeln('              versions of a file stored, then only the latest version is');
    writeln('              extracted. If a stored file has been marked as deleted, then');
    writeln('              it is deleted. Existing files are overwritten if they are');
    writeln('              considered changed; a file is replaced only after its data');
    writeln('              has been verified.');
    writeln;
    writeln('              Unless --nodelete is given, external files and directories');
    writeln('              within files that do not exist in the archive are deleted.');
    writeln('              Excluded items and the archive itself are never deleted.');
    writeln;
    writeln('       -p, --purge');
    writeln('              purge archive, remove old files archived. The archive is');
    writeln('              replaced only when the purged copy has been completely');
    writeln('              written.');
    writeln;
    writeln('       -l, --list');
    writeln('              list files within the archive, or list the entire archive');
    writeln('              contents if files is omitted. For each file or directory,');
    writeln('              show version number, marker, Unix/Linux permissions,');
    writeln('              attributes, last-modified date, size and name. Permissions');
    writeln('              are listed as an octal number (as per chmod(1)) and');
    writeln('              attributes with the letters R, H, S, V, D, A, L (read-only,');
    writeln('              hidden, system, volume, directory, archive, symbolic link).');
    writeln;
    writeln('       -c, --check');
    writeln('              check archive integrity by verifying that the data agrees');
    writeln('              with the stored SHA-1 hashes and sizes and that the data');
    writeln('              otherwise conforms to the gulp standard.');
    writeln;
    writeln('       -f, --fix');
    writeln('              truncates any data added after last valid update.');
    writeln;
    writeln('       -h, --help');
    writeln('              show this help.');
    writeln;
    writeln('OPTIONS');
    writeln('       Operation modifiers:');
    writeln;
    writeln('       -e, --exclude=pattern');
    writeln('              exclude files in pattern. Can be repeated. A directory');
    writeln('              excludes all its content; a trailing slash is ignored.');
    writeln;
    writeln('       --excludeattr=attributes');
    writeln('              exclude files with any of the attributes, given with the');
    writeln('              letters R, H, S, V, D, A, L.');
    writeln;
    writeln('       --excludemode=mode');
    writeln('              exclude files with any of the permission bits in mode, given');
    writeln('              as an octal number.');
    writeln;
    writeln('       --forcepath');
    writeln('              allow files with absolute path names.');
    writeln;
    writeln('       -i, --include=pattern');
    writeln('              include files in pattern. Can be repeated. A directory');
    writeln('              includes all its content; a trailing slash is ignored.');
    writeln;
    writeln('       --nodelete');
    writeln('              with synch, do not mark files in the archive as deleted when');
    writeln('              the corresponding external file does not exist. With');
    writeln('              restore, do not delete external files when the corresponding');
    writeln('              file in archive does not exist. This makes gulp consistent');
    writeln('              with the behavior of most non-journaling archivers.');
    writeln;
    writeln('       --only=version');
    writeln('              with list, show only the changes made in version.');
    writeln;
    writeln('                     gulp -l backup --only=20');
    writeln;
    writeln('              show files added or deleted in version 20.');
    writeln;
    writeln('       -u, --until=version');
    writeln('              ignore any part of the archive updated after version number.');
    writeln;
    writeln('                     gulp -l backup --until=20');
    writeln;
    writeln('              show files added before version 21.');
    writeln;
    writeln('       --verbose');
    writeln('              show each processed file.');
    writeln;
    writeln('EXAMPLES');
    writeln('       Create archive from files foo and bar:');
    writeln;
    writeln('              gulp -s archive foo bar');
    writeln;
    writeln('       List all files in archive:');
    writeln;
    writeln('              gulp -l archive');
    writeln;
    writeln('       Restore all files from archive:');
    writeln;
    writeln('              gulp -r archive');
    writeln;
    writeln('EXIT STATUS');
    writeln('       Returns 0 if successful or 1 in case of an error.');
    writeln;
    writeln('BUGS');
    writeln('       The archive format does not save sufficient information for backing');
    writeln('       up and restoring the operating system.');
    writeln;
    writeln('AUTHOR');
    writeln('       gulp is copyright (c) 2014-2022, Melchiorre Caruso. It is licensed');
    writeln('       under GPL v2. For information on the license, see');
    writeln('       <http://www.gnu.org/copyleft/gpl.html>. Program was written by');
    writeln('       Melchiorre Caruso <melchiorrecaruso at gmail dot com>');
    writeln;
  end;

var
  shell: tshellapplication;
begin
  shell := tshellapplication.create;
  shell.run;
  shell.destroy;
end.
