program TestDeleteDir;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  SysUtils, RegressionUtils;

{$IFDEF UNIX}
var
  info: Stat;
  dirname: string = 'd';
{$ENDIF}
begin
  EnterNewDir('work');
  WriteTextFile('d', 'f');
  GulpOk(['-s', '../arc.gulp']);

  EnterNewDir('restored');
  WriteTextFile('d' + PathDelim + 'y', 'y');
  WriteTextFile('d' + PathDelim + '.hidden', 'h');
  WriteTextFile('d' + PathDelim + 'sub' + PathDelim + 'z', 'z');
  WriteTextFile('outside' + PathDelim + 'keep', 'k');
  {$IFDEF UNIX}
  FpSymlink('../outside', 'd/lnk');
  {$ENDIF}
  FileSetDate('d', DateTimeToFileDate(EncodeDate(2020, 1, 1)));
  GulpOk(['-r', '../arc.gulp', '--nodelete']);
  Check(FileExists('d') and not DirectoryExists('d'), 'the non-empty directory is replaced by the file');
  Check(ReadTextFile('d') = 'f', 'the file content is restored');
  Check(FileExists('outside' + PathDelim + 'keep'), 'symbolic links are not followed');

  {$IFDEF UNIX}
  if not IsRoot then
  begin
    DeleteFile('d');
    WriteTextFile('d' + PathDelim + 'ro' + PathDelim + 'q', 'q');
    FpChmod('d/ro', &555);
    FpChmod('d', &755);
    FileSetDate('d', DateTimeToFileDate(EncodeDate(2020, 1, 1)));
    GulpOk(['-r', '../arc.gulp', '--nodelete']);
    Check(DirectoryExists('d'), 'a directory that cannot be deleted is kept');
    Check((FpStat(dirname, info) = 0) and ((info.st_mode and &777) = &755),
      'the file metadata are not applied to the directory');
    Check(not HasTempFiles, 'no temporary file is left');
  end;
  {$ENDIF}

  WriteLn('PASS: non-empty directories are deleted, failed restores keep metadata');
end.
