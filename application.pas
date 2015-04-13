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
}

{
  Contains:

    The journaling archiver shell.

  Modified:

    v0.0.2 - 2015.04.13 by Melchiorre Caruso.
}

unit Application;

interface

uses
  Classes,
  Common,
  CustApp;

type
  { TGulpApplication class }

  TGulpApplication = class(TCustomApplication)
  private
    FFileNames : TStringList;
    FSwitches  : TStringList;
    FScanner   : TSysScanner;
    procedure Synch;
    procedure Restore;
    procedure Purge;
    procedure List;
    procedure Fix;
    procedure Check;
    procedure Help;
  protected
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Abort;
  end;

implementation

uses
  LibGulp,
  SysUtils;

const
  Description = 'GULP v0.0.2 journaling archiver, copyright (c) 2014-2015 Melchiorre Caruso.' + LineEnding +
                'GULP archiver for user-level incremental backups with rollback capability.';

// =============================================================================
// TGulpApplication
// =============================================================================

constructor TGulpApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF UNIX}
    CaseSensitiveOptions := TRUE;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      CaseSensitiveOptions := FALSE;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  StopOnException := TRUE;
  FFileNames      := TStringList.Create;
  FSwitches       := TStringList.Create;
  FScanner        := TSysScanner.Create;
end;

destructor TGulpApplication.Destroy;
begin
  FScanner.Destroy;
  FSwitches.Destroy;
  FFileNames.Destroy;
  inherited Destroy;
end;

procedure TGulpApplication.Synch;
var
     I, J : longint;
  GulpLib : TGulpLib;
     Size : int64;
   Stream : TStream;
begin
  writeln(Description);
  writeln(#13, #13: 80, 'Synch the content of ' + GetOptionValue('s', 'synch'));
  write  (#13, #13: 80, 'Scanning filesystem... ');
  if FFileNames.Count = 0 then
    FFileNames.Add('*');
  for I := 0 to FFileNames.Count - 1 do
    FScanner.Add(FFileNames[I]);
  J := FScanner.Find(GetOptionValue('s', 'synch'));
  if J <> -1 then
    FScanner.Delete(J);

  write(#13, #13: 80, 'Opening archive... ');
  if FileExists(GetOptionValue('s', 'synch')) then
    Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmOpenReadWrite)
  else
    Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmCreate);;

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(longword(-2)) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  Size := Stream.Seek(0, soEnd);

  write(#13, #13: 80, 'Deleting records... ');
  for I := 0 to GulpLib.Count - 1 do
  begin
    J := FScanner.Find(GulpLib.Items[I].Name);
    if J = -1 then
    begin
      if HasOption('nodelete') = FALSE then
        GulpLib.Delete(I);
    end else
      if GetTime(FScanner.Items[J]) <> GulpLib.Items[I].Time then
      begin
        GulpLib.Delete(I);
      end else
        FScanner.Delete(J);
  end;

  if GetOptionValue('m', 'method') <> '' then
  begin
    if GetOptionValue('m', 'method') = 'gzfast' then GulpLib.Method := gmGZFast   else
    if GetOptionValue('m', 'method') = 'gz'     then GulpLib.Method := gmGZNormal else
    if GetOptionValue('m', 'method') = 'gzmax'  then GulpLib.Method := gmGZMax    else
      raise Exception.Create('Wrong method value');
  end;

  write(#13, #13: 80, 'Adding records... ');
  for I := 0 to FScanner.Count - 1 do
    GulpLib.Add(FScanner.Items[I]);
  GulpLib.CloseArchive;

  write(#13, #13: 80, 'Finished (', Stream.Seek(0, soEnd) - Size, ' added bytes)');
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  writeln;
end;

procedure TGulpApplication.Restore;
var
  I, J, K : longint;
  GulpLib : TGulpLib;
  GulpRec : TGulpRec;
     Size : int64 = 0;
   Stream : TStream;
  Version : longword;
begin
  writeln(Description);
  writeln(#13, #13: 80, 'Restore the content of ' + GetOptionValue('r', 'restore'));
  write  (#13, #13: 80, 'Scanning filesystem... ');
  if FFileNames.Count = 0 then
    FFileNames.Add('*');
  FScanner.Add('*');
  J := FScanner.Find(GetOptionValue('r', 'restore'));
  if J <> -1 then
    FScanner.Delete(J);

  Version := longword(-2);
  if GetOptionValue('u', 'until') <> '' then
  begin
    if lowercase(GetOptionValue('u', 'until')) = 'last' then
      Version := longword(-2)
    else
      Version := StrToInt(GetOptionValue('u', 'until'));
  end;

  write(#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create(GetOptionValue('r', 'restore'), fmOpenRead);

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(Version) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

  write(#13, #13: 80, 'Deleting records... ');
  if HasOption('nodelete') = FALSE then
    for I := FScanner.Count - 1 downto 0 do
    begin
      J := GulpLib.Find(FScanner.Items[I]);
      if (J = -1) or (FileNameMatch(GulpLib.Items[J].Name, FScanner.Items[I]) = FALSE) then
      begin
        if DirectoryExists(FScanner.Items[I]) = TRUE then
          RemoveDir(FScanner.Items[I])
        else
          DeleteFile(FScanner.Items[I]);
      end;
    end;

  write(#13, #13: 80, 'Extracting records... ');
  for I := GulpLib.Count - 1 downto 0 do
  begin
    GulpRec := GulpLib.Items[I];
    for J := 0 to FFileNames.Count - 1 do
      if FileNameMatch(GulpRec.Name, FFileNames[J]) = TRUE then
      begin
        K := FScanner.Find(GulpRec.Name);
        if K = -1 then
        begin
          GulpLib.Extract(I);
          Inc(Size, GulpRec.Size);
        end else
          if GetTime(FScanner.Items[K]) <> GulpRec.Time then
          begin
            GulpLib.Extract(I);
            Inc(Size, GulpRec.Size);
          end;
        Break;
      end;
  end;

  write(#13, #13: 80, 'Finished (', Size, ' extracted bytes)');
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  writeln;
end;

procedure TGulpApplication.Fix;
var
    Size : int64;
  Stream : TStream;
begin
  writeln(Description);
  writeln(#13, #13: 80, 'Fix the content of ' + GetOptionValue('f', 'fix'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create(GetOptionValue('f', 'fix'), fmOpenReadWrite);
  Size   := Stream.Seek(0, soEnd);

  write(#13, #13: 80, 'Fixing archive... ');
  FixArchive(Stream);

  write(#13, #13: 80, 'Finished (', Size - Stream.Size, ' removed bytes)');
  FreeAndNil(Stream);
  writeln;
end;

procedure TGulpApplication.Check;
var
        I : longint;
  GulpLib : TGulpLib;
      Nul : TStream;
   Stream : TStream;
begin
  writeln(Description);
  writeln(#13, #13: 80, 'Check the content of ' + GetOptionValue('c', 'check'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create (GetOptionValue('c', 'check'), fmOpenRead);
  Nul    := TNulStream.Create;

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(longword(-1)) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

  write(#13, #13: 80, 'Checking records... ');
  for I := 0 to GulpLib.Count - 1 do
    GulpLib.ExtractTo(I, Nul);
  GulpLib.CloseArchive;

  write(#13, #13: 80, 'Finished (', Stream.Size, ' checked bytes)');
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  FreeAndNil(Nul);
  writeln;
end;

procedure TGulpApplication.Purge;
var
   Stream : TStream;
      Tmp : TStream;
  TmpName : string;
begin
  writeln(Description);
  writeln(#13, #13: 80, 'Purge the content of ' + GetOptionValue('p', 'purge'));
  write  (#13, #13: 80, 'Opening archive... ');
  TmpName := GetTempFileName(ExtractFileDir(GetOptionValue('p', 'purge')), '');
  Tmp     := TFileStream.Create(TmpName, fmCreate);
  Stream  := TFileStream.Create(GetOptionValue('p', 'purge'), fmOpenRead);

  write(#13, #13: 80, 'Moving records... ');
  PurgeArchive(Stream, Tmp);

  write(#13, #13: 80, 'Finished (', Stream.Size - Tmp.Size, ' removed bytes)');
  FreeAndNil(Stream);
  FreeAndNil(Tmp);

  if DeleteFile(GetOptionValue('p', 'purge')) = FALSE then
    raise Exception.CreateFmt('Unable to delete file "%s"', [GetOptionValue('p', 'purge')])
  else
    if RenameFile(TmpName, GetOptionValue('p', 'purge'))= FALSE then
      raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);
  writeln;
end;

procedure TGulpApplication.List;
var
     I, J : longint;
    Count : longint;
  GulpLib : TGulpLib;
  GulpRec : TGulpRec;
   Stream : TStream;
  Version : longword;
begin
  writeln(Description);
  writeln(#13, #13: 80, 'List the content of ' + GetOptionValue('l', 'list'));
  if FFileNames.Count = 0 then
    FFileNames.Add('*');

  Version := longword(-1);
  if GetOptionValue('u', 'until') <> '' then
  begin
    if lowercase(GetOptionValue('u', 'until')) = 'last' then
      Version := longword(-2)
    else
      Version := StrToInt(GetOptionValue('u', 'until'));
  end;

  write(#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create (GetOptionValue('l', 'list'), fmOpenRead);

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(Version) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

  write (#13, #13: 80, 'Reading records... ');
  Count := 0;
  for I := 0 to GulpLib.Count - 1 do
  begin
    GulpRec := GulpLib.Items[I];
    for J := 0 to FFileNames.Count - 1 do
       if FileNameMatch(GulpRec.Name, FFileNames[J])  then
       begin
         writeln(#13, #13: 80, Format('%4s %3s %3s %7s %19s %12s %s', [
            VerTostring(GulpRec),
           FlagToString(GulpRec),
           ModeToString(GulpRec),
           AttrToString(GulpRec),
           TimeToString(GulpRec),
           SizeToString(GulpRec),
           GulpRec.Name]));
         Inc(Count);
         Break;
       end;
  end;

  writeln(#13, #13: 80, 'Finished (', Count, ' listed records)');
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  writeln;
end;

procedure TGulpApplication.Help;
begin
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
writeln('       -u, --until                                                        ');
writeln('              ignore any part of the archive updated after version number.');
writeln('                                                                          ');
writeln('                     gulp -l backup files -until 20                       ');
writeln('                                                                          ');
writeln('              show files added before version 21.                         ');
writeln('                                                                          ');
writeln('EXAMPLES                                                                  ');
writeln('       Create archive.gulp from files foo and bar.                        ');
writeln('              gulp -s archive.gulp foo bar                                ');
writeln('       List all files in archive.gulp.                                    ');
writeln('              gulp -l archive.gulp                                        ');
writeln('       Restore all files from archive.gulp.                               ');
writeln('              gulp -r archive.gulp                                        ');
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
end;

procedure TGulpApplication.Abort;
begin
  raise Exception.Create('User abort');
end;

procedure TGulpApplication.DoRun;
var
          Error : string;
   LongSwitches : TStringList;
  ShortSwitches : string;
      StartTime : TDateTime;
begin
  inherited DoRun;
  StartTime := Now;

  DefaultFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
  DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  ShortSwitches := 's:r:p:l:c:f:u:m:h';
  LongSwitches  :=  TStringList.Create;
  LongSwitches.Add('synch:');
  LongSwitches.Add('restore:');
  LongSwitches.Add('purge:');
  LongSwitches.Add('list:');
  LongSwitches.Add('check:');
  LongSwitches.Add('fix:');
  LongSwitches.Add('until:');
  LongSwitches.Add('method:');
  LongSwitches.Add('help');
  LongSwitches.Add('nodelete');

  ExitCode  := 1;
  try
    Error   := CheckOptions(ShortSwitches, LongSwitches, FSwitches, FFileNames);
    if Error = '' then
    begin
      if HasOption('s', 'synch'  ) then Synch   else
      if HasOption('r', 'restore') then Restore else
      if HasOption('p', 'purge'  ) then Purge   else
      if HasOption('l', 'list'   ) then List    else
      if HasOption('c', 'check'  ) then Check   else
      if HasOption('f', 'fix'    ) then Fix     else
      if HasOption('h', 'help'   ) then Help    else Help;

      ExitCode := 0;
    end else
    begin
      writeln(Description);
      writeln(#13, #13: 80, Error);
    end;

  except
    on E: Exception do
      writeln(#13, #13: 80, Format('An exception was raised: "%s"', [E.Message]));
  end;

  FreeAndNil(LongSwitches);
  writeln(#13, #13: 80, 'Elapsed ',
    Format('%0.2f', [(Now - StartTime) * (24 * 60 * 60)]) , ' sec');
  Terminate;
end;

end.
