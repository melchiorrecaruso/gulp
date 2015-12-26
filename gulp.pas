{
  Copyright (c) 2014-2015 Melchiorre Caruso.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  Contains:

    The journaling archiver utility.

  Modified:

    v0.0.3 - 2015.12.26 by Melchiorre Caruso.
}

program Gulp;

{$mode objfpc}

uses
  {$IFDEF UNIX} cthreads, BaseUnix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Classes, CustApp, GulpCommon, GulpLibrary, SysUtils;

type
  TShellApplication = class(TObject)
  protected
    procedure   ShowItem(const Item: TGulpItem);
    procedure   ShowMessage(const Message: ansistring);
    procedure   Abort;
  public
    constructor Create(AOwner: TComponent);
    destructor  Destroy; override;
    procedure   Run;
    procedure   Help;
  end;

constructor TShellApplication.Create(AOwner: TComponent);
begin
  inherited Create;
  DefaultFormatSettings. LongDateFormat := 'yyyy-mm-dd';
  DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  ExitCode := 255;
end;

destructor TShellApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TShellApplication.ShowMessage(const Message: ansistring);
begin
  write(Message);
end;

procedure TShellApplication.ShowItem(const Item: TGulpItem);
begin
  if gfAdd in Item.Flags then
  begin

    if Item.Attr and faDirectory = faDirectory then
      writeln(Format('%4s %3s %3s %7s %19s %12s %s', [
         VerTostring(Item.Version),
        FlagToString(Item.Flags),
        ModeToString(Item.Mode),
        AttrToString(Item.Attr),
        TimeToString(Item.Time),
        '',
        Item.Name]))
    else
      writeln(Format('%4s %3s %3s %7s %19s %12s %s', [
         VerTostring(Item.Version),
        FlagToString(Item.Flags),
        ModeToString(Item.Mode),
        AttrToString(Item.Attr),
        TimeToString(Item.Time),
        SizeToString(Item.Size),
        Item.Name]));

  end else
  begin

    if Item.Attr and faDirectory = faDirectory then
      writeln(Format('%4s %3s %3s %7s %19s %12s %s', [
         VerTostring(Item.Version),
        FlagToString(Item.Flags),
        '',
        '',
        '',
        '',
        Item.Name]))
    else
      writeln(Format('%4s %3s %3s %7s %19s %12s %s', [
         VerTostring(Item.Version),
        FlagToString(Item.Flags),
        '',
        '',
        '',
        '',
        Item.Name]));

  end;
end;

procedure TShellApplication.Run;
var
  App   : TGulpApplication;
  S     : ansistring;
  Start : TDateTime;
begin
  Start := Now;
  App   := TGulpApplication.Create;
  App.OnShowMessage := @ShowMessage;
  App.OnShowItem    := @ShowItem;
  try
    S := CheckOptions(
      '-s: -r: -p: -l: -c: -f: -u: -m: -i: -e: h ',
      '--synch:   --restore: --purge: --list:    ' +
      '--check:   --fix:     --until: --method:  ' +
      '--include: --exclude: --help   --nodelete ' ,
      App.Include);

    if S = '' then
    begin
      if HasOption('-i', '--include') = TRUE then
      begin
        S := GetOptionValue('-i', '--include');
        if S[Length(S)] = PathDelim then
          App.Include.Add(S + '*')
        else
          App.Include.Add(S);
      end;

      if HasOption('-e', '--exclude') = TRUE then
      begin
        S := GetOptionValue('-e', '--exclude');
        if S[Length(S)] = PathDelim then
          App.Exclude.Add(S + '*')
        else
          App.Exclude.Add(S);
      end;

      if HasOption('-u', '--until'  ) = TRUE then
      begin
        if GetOptionValue('-u', '--until') = 'last' then
          App.UntilVersion := $FFFFFFFF
        else
          App.UntilVersion := StrToInt(GetOptionValue('-u', '--until'));
      end;

      if HasOption('-s', '--synch'  ) then App.Sync   (GetOptionValue('-s', '--synch'  )) else
        if HasOption('-r', '--restore') then App.Restore(GetOptionValue('-r', '--restore')) else
          if HasOption('-p', '--purge'  ) then App.Purge  (GetOptionValue('-p', '--purge'  )) else
            if HasOption('-c', '--check'  ) then App.Check  (GetOptionValue('-c', '--check'  )) else
              if HasOption('-f', '--fix'    ) then App.Fix    (GetOptionValue('-f', '--fix'    )) else
                if HasOption('-l', '--list'   ) then App.List   (GetOptionValue('-l', '--list'   )) else
                  if HasOption('-h', '--help'   ) then
                    Help
                  else
                    Help;

      ExitCode := 0;
    end else
      writeln(#13, #13: 80, S);
  except
    on E: Exception do
      writeln(#13, #13: 80, Format('An exception was raised: "%s"', [E.Message]));
  end;
  FreeAndNil(App);
  writeln(#13, #13: 80, 'Elapsed ', Format('%0.2f', [(Now - Start) * (24 * 60 * 60)]) , ' sec');
end;

procedure TShellApplication.Abort;
begin
  raise Exception.Create('User abort');
end;

procedure TShellApplication.Help;
begin
  writeln;
  writeln('NAME                                                                      ');
  writeln('       gulp - A simple journaling archiver.                               ');
  writeln('                                                                          ');
  writeln('SYNOPSIS                                                                  ');
  writeln('       gulp [-] s --synch | r --restore | p --purge | l --list |          ');
  writeln('                c --check | f --fix [ options ...] [ files ...]           ');
  writeln('                                                                          ');
  writeln('DESCRIPTION                                                               ');
  writeln('       gulp stores and extracts files from a disk archive. A GULP  archive');
  writeln('       is  a  sequence  of timestamped updates, each listing the files and');
  writeln('       directories that have been added, changed,  or  deleted  since  the');
  writeln('       previous  transacted update, normally based on changes to the last-');
  writeln('       modified dates.                                                    ');
  writeln('                                                                          ');
  writeln('       The first argument to gulp should be a function; either one of  the');
  writeln('       letters srplcfh, or one of the long function names. A function let‐');
  writeln('       ter need be prefixed with ''-'', and can''t  be  combined  with  other');
  writeln('       single-letter  options.  A long function name must be prefixed with');
  writeln('       ''--''.  Some options take a parameter; with the  single-letter  form');
  writeln('       these must be given as separate arguments. With the long form, they');
  writeln('       may be given by appending =value to the option.                    ');
  writeln('                                                                          ');
  writeln('FUNCTION LETTERS                                                          ');
  writeln('       Main operation mode:                                               ');
  writeln('                                                                          ');
  writeln('       -s, --synch                                                        ');
  writeln('              append changes in files to archive, or create archive if  it');
  writeln('              does  not exist. files is a list of file and directory names');
  writeln('              separated by spaces. If a  name  is  a  directory,  then  it');
  writeln('              recursively includes all files and subdirectories within. In');
  writeln('              Windows, files may contain wildcards * and  ?  in  the  last');
  writeln('              component  of the path (after the last slash). * matches any');
  writeln('              string and ? matches any character.                         ');
  writeln('                                                                          ');
  writeln('              A change is an addition, update, or deletion of any file  or');
  writeln('              directory  in  files  or  any  of  its subdirectories to any');
  writeln('              depth. A file or directory  is  considered  changed  if  its');
  writeln('              last-modified  date differ between the internal and external');
  writeln('              versions. File contents are not compared.                   ');
  writeln('                                                                          ');
  writeln('              For each added or updated file or directory,  the  following');
  writeln('              information  is saved in the archive: the contents, the file');
  writeln('              or directory name as it appears in files plus  any  trailing');
  writeln('              path, the last-modified date, and the Unix/Linux permissions');
  writeln('              or Windows attributes. Other metadata such as owner,  group,');
  writeln('              last  access  time,  etc.  are not saved. Symbolic links are');
  writeln('              saved. Hard links are followed  as  if  they  were  ordinary');
  writeln('              files.  Special file types such as devices, named pipes, and');
  writeln('              named sockets are not saved. If  any  file  cannot  be  read');
  writeln('              (e.g. permission denied), then it is skipped. However, other');
  writeln('              files are still added and the update is still valid.        ');
  writeln('                                                                          ');
  writeln('              Updates are transacted. If gulp is interrupted  before  com‐');
  writeln('              pleting  the update, then the archive can be repair with fix');
  writeln('              function.                                                   ');
  writeln('                                                                          ');
  writeln('       -r, --restore                                                      ');
  writeln('              restore files (including the contents  of  directories),  or');
  writeln('              extract  the whole archive contents if files is omitted. The');
  writeln('              file  names,  last-modified   date,   and   permissions   or');
  writeln('              attributes  are  set  as  saved in the archive. If there are');
  writeln('              multiple versions of a file stored,  then  only  the  latest');
  writeln('              version  is  extracted.  If a stored file has been marked as');
  writeln('              deleted, then it is deleted. Existing files are  overwritten');
  writeln('              if they are considered changed.                             ');
  writeln('                                                                          ');
  writeln('       -p, --purge                                                        ');
  writeln('              purge archive, remove old files archived.                   ');
  writeln('                                                                          ');
  writeln('       -l, --list                                                         ');
  writeln('              list  files  within  the archive, or list the entire archive');
  writeln('              contents if files is omitted. For each  file  or  directory,');
  writeln('              show   version   number,   marker,   Windows  attributes  or');
  writeln('              Unix/Linux permissions, last modified date, size  and  name.');
  writeln('              Attributes  are  listed as an octal number in Unix/Linux (as');
  writeln('              per chmod(1)) or with the letters D, A, S, H, R, I  in  Win‐');
  writeln('              dows (as per the attrib command).                           ');
  writeln('                                                                          ');
  writeln('       -c, --check                                                        ');
  writeln('              check  archive  integrity  by verifying that the data agrees');
  writeln('              with the stored SHA-1 hashes and sizes  and  that  the  data');
  writeln('              otherwise conforms to the gulp standard.                    ');
  writeln('                                                                          ');
  writeln('       -f, --fix                                                          ');
  writeln('              truncates any data added after last valid update.           ');
  writeln('                                                                          ');
  writeln('OPTIONS                                                                   ');
  writeln('       Operation modifiers:                                               ');
  writeln('                                                                          ');
  writeln('       -e, --exclude  [ pattern ]                                         ');
  writeln('              exclude files in pattern.                                   ');
  writeln('                                                                          ');
  writeln('       -i, --include  [ pattern ]                                         ');
  writeln('              include files in pattern.                                   ');
  writeln('                                                                          ');
  writeln('       -m, --method [ gzfast | gz | gzmax ]                               ');
  writeln('              with synch, select a compression method.                    ');
  writeln('                                                                          ');
  writeln('                     gulp -s backup files -m gzfast                       ');
  writeln('                                                                          ');
  writeln('              store files with gzfast compression method.                 ');
  writeln('                                                                          ');
  writeln('       --nodelete                                                         ');
  writeln('              with synch, do not mark files in the archive as deleted when');
  writeln('              the  corresponding  external file does not exist. With rest-');
  writeln('              ore, do not delete external files when the corresponding fi-');
  writeln('              le in  archive does  not exist. This  makes gulp  consistent');
  writeln('              with the behavior of most non-journaling archivers.         ');
  writeln('                                                                          ');
  writeln('       -u, --until                                                        ');
  writeln('              ignore any part of the archive updated after version number.');
  writeln('                                                                          ');
  writeln('                     gulp -l backup files -until 20                       ');
  writeln('                                                                          ');
  writeln('              show files added before version 21.                         ');
  writeln('                                                                          ');
  writeln('EXAMPLES                                                                  ');
  writeln('       Create archive from files foo and bar:                             ');
  writeln('                                                                          ');
  writeln('              gulp -s archive foo bar                                     ');
  writeln('                                                                          ');
  writeln('       List all files in archive:                                         ');
  writeln('                                                                          ');
  writeln('              gulp -l archive                                             ');
  writeln('                                                                          ');
  writeln('       Restore all files from archive:                                    ');
  writeln('                                                                          ');
  writeln('              gulp -r archive                                             ');
  writeln('                                                                          ');
  writeln('EXIT STATUS                                                               ');
  writeln('       Returns 0 if successful or 1 in case of an error.                  ');
  writeln('                                                                          ');
  writeln('BUGS                                                                      ');
  writeln('       The archive format does not save sufficient information for backing');
  writeln('       up and restoring the operating system.                             ');
  writeln('                                                                          ');
  writeln('AUTHOR                                                                    ');
  writeln('       gulp is copyright (c) 2014-2015, Melchiorre Caruso. It is  licensed');
  writeln('       under    GPL    v2.   For   information   on   the   license,   see');
  writeln('       <http://www.gnu.org/copyleft/gpl.html>. Program was written by Mel‐');
  writeln('       chiorre Caruso <melchiorrecaruso at gmail dot com>"                ');
  writeln;
end;

var
  Shell: TShellApplication;

begin
  Shell := TShellApplication.Create(nil);
  Shell.Run;
  Shell.Destroy;
end.
