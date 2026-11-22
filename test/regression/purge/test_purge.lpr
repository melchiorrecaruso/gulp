program TestPurge;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  SysUtils, RegressionUtils;

var
  before: string;
begin
  EnterNewDir('work');
  WriteTextFile('a', 'a');
  GulpOk(['-s', 'arc.gulp']);
  WriteTextFile('a', 'bb');
  FileSetDate('a', DateTimeToFileDate(EncodeDate(2021, 1, 1)));
  GulpOk(['-s', 'arc.gulp']);

  // the temporary file must not depend on the system temporary directory,
  // which may be on another file system
  SetGulpEnvironment('TMPDIR', IncludeTrailingPathDelimiter(GetCurrentDir) + 'missing');
  GulpOk(['-p', 'arc.gulp']);
  SetGulpEnvironment('TMPDIR', '');
  Check(FileExists('arc.gulp'), 'the archive exists after purge');
  Check(ListedNames('arc.gulp') = 'a', 'only the latest version is kept');
  Check(ListedNames('arc.gulp', 'ADD', ['--only=1']) = 'a', 'the purged archive has the latest items in version 1');
  Check(ListedNames('arc.gulp', 'ADD', ['--only=2']) = '', 'the purged archive has no version 2');
  GulpOk(['-c', 'arc.gulp']);
  Check(not HasTempFiles, 'no temporary file is left');

  ForceDirectories('r');
  SetCurrentDir('r');
  GulpOk(['-r', '../arc.gulp']);
  Check(ReadTextFile('a') = 'bb', 'the purged archive restores the latest content');
  SetCurrentDir('..');

  {$IFDEF UNIX}
  if not IsRoot then
  begin
    before := ReadTextFile('arc.gulp');
    FpChmod('.', &555);
    try
      Check(RunGulp(['-p', 'arc.gulp']) <> 0, 'purge fails in a read-only directory');
    finally
      FpChmod('.', &755);
    end;
    Check(ReadTextFile('arc.gulp') = before, 'a failed purge keeps the archive unchanged');
  end;
  {$ENDIF}

  WriteLn('PASS: purge replaces the archive safely');
end.
