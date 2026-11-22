program TestChangeDetect;
{$mode objfpc}{$H+}

uses
  BaseUnix, SysUtils, RegressionUtils;

const
  Time = 43831; // 2020-01-01

procedure SetTime(const AName: string);
begin
  Check(FileSetDate(AName, DateTimeToFileDate(Time)) = 0, 'set time of ' + AName);
end;

var
  version: integer = 1;

procedure CheckChanged(const AName, ADescription: string);
begin
  GulpOk(['-s', '../arc.gulp']);
  Inc(version);
  Check(ListedNames('../arc.gulp', 'ADD', ['--only=' + IntToStr(version)]) = AName, ADescription);
end;

begin
  EnterNewDir('work');
  WriteTextFile('f', 'a');
  WriteTextFile('g', 'b');
  SetTime('f');
  SetTime('g');
  GulpOk(['-s', '../arc.gulp']);
  Check(Pos('Finished (0 ', GulpOk(['-s', '../arc.gulp'])) > 0, 'no change found in unchanged items');

  FpChmod('g', &600);
  CheckChanged('g', 'permission change with the same time');

  WriteTextFile('f', 'longer');
  SetTime('f');
  CheckChanged('f', 'size change with the same time');

  DeleteFile('f');
  ForceDirectories('f');
  SetTime('f');
  CheckChanged('f', 'file replaced by a directory with the same time');

  EnterNewDir('restored');
  GulpOk(['-r', '../arc.gulp']);
  Check(Pos('Finished (0 ', GulpOk(['-r', '../arc.gulp'])) > 0, 'a second restore finds no change');
  RemoveDir('f');
  WriteTextFile('f', 'x');
  SetTime('f');
  GulpOk(['-r', '../arc.gulp', '--nodelete']);
  Check(DirectoryExists('f'), 'restore replaces a file with the archived directory');

  WriteLn('PASS: type, size and permission changes are detected');
end.
