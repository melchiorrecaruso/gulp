program TestRestoreInclude;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

begin
  EnterNewDir('work');
  WriteTextFile('a.txt', 'a');
  WriteTextFile('b.txt', 'b');
  WriteTextFile('d' + PathDelim + 'x', 'x');
  GulpOk(['-s', '../arc.gulp']);

  WriteTextFile('a.txt', 'changed');
  FileSetDate('a.txt', DateTimeToFileDate(EncodeDate(2020, 1, 1)));
  GulpOk(['-r', '../arc.gulp', 'a.txt']);
  Check(ReadTextFile('a.txt') = 'a', 'requested file is restored');
  Check(TreeNames = 'a.txt b.txt d d/x', 'restoring a file keeps the other items');

  WriteTextFile('d' + PathDelim + 'extra', 'e');
  WriteTextFile('other', 'o');
  GulpOk(['-r', '../arc.gulp', 'd']);
  Check(not FileExists('d' + PathDelim + 'extra'), 'item not in the archive inside the requested directory is deleted');
  Check(FileExists('other'), 'item outside the requested directory is kept');

  GulpOk(['-r', '../arc.gulp']);
  Check(TreeNames = 'a.txt b.txt d d/x', 'full restore deletes the items not in the archive');

  WriteLn('PASS: restore only deletes items within the requested patterns');
end.
