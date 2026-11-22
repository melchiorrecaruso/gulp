program TestSyncInclude;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

begin
  EnterNewDir('work');
  WriteTextFile('a.txt', 'a');
  WriteTextFile('b.txt', 'b');
  WriteTextFile('d' + PathDelim + 'y', 'y');
  WriteTextFile('d' + PathDelim + 'e' + PathDelim + 'x', 'x');
  GulpOk(['-s', '../arc.gulp']);

  GulpOk(['-s', '../arc.gulp', 'a.txt']);
  Check(ListedNames('../arc.gulp', 'DEL', ['--only=2']) = '', 'synch of an unchanged subset deletes nothing');
  Check(ListedNames('../arc.gulp') = 'a.txt b.txt d d/e d/e/x d/y', 'all files are still archived');

  DeleteFile('a.txt');
  GulpOk(['-s', '../arc.gulp', 'a.txt']);
  Check(ListedNames('../arc.gulp', 'DEL', ['--only=2']) = 'a.txt', 'only the given file is deleted');

  DeleteTree('d');
  GulpOk(['-s', '../arc.gulp', 'd']);
  Check(ListedNames('../arc.gulp', 'DEL', ['--only=3']) = 'd d/e d/e/x d/y', 'a deleted directory is deleted with its content');

  WriteTextFile('c.txt', 'c');
  GulpOk(['-s', '../arc.gulp']);
  DeleteFile('c.txt');
  GulpOk(['-s', '../arc.gulp', '-e', 'c.txt']);
  Check(ListedNames('../arc.gulp') = 'b.txt c.txt', 'an excluded file is not marked as deleted');
  GulpOk(['-s', '../arc.gulp']);
  Check(ListedNames('../arc.gulp') = 'b.txt', 'a full synch records the deletion');

  WriteLn('PASS: synch only marks as deleted items within the include patterns');
end.
