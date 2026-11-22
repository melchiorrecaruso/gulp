program TestSelfArchive;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

procedure CheckNotSelf(const AArchive, AName: string);
begin
  Check(Pos(AName, ListedNames(AArchive)) = 0, AArchive + ' does not contain itself');
end;

var
  dir: string;
begin
  dir := EnterNewDir('work');
  WriteTextFile('a.txt', 'a');
  ForceDirectories('sub');

  // every synch archives the whole directory, other archives included
  GulpOk(['-s', './arc1.gulp']);
  CheckNotSelf('arc1.gulp', 'arc1.gulp');
  Check(Pos('Finished (0 ', GulpOk(['-s', './arc1.gulp'])) > 0, 'a second synch adds nothing');
  CheckNotSelf('arc1.gulp', 'arc1.gulp');
  GulpOk(['-s', IncludeTrailingPathDelimiter(dir) + 'arc2.gulp']);
  CheckNotSelf('arc2.gulp', 'arc2.gulp');
  GulpOk(['-s', 'sub/../arc3.gulp']);
  CheckNotSelf('arc3.gulp', 'arc3.gulp');

  WriteTextFile('sub' + PathDelim + 's', 's');
  GulpOk(['-s', './sub/arc4.gulp', 'sub']);
  Check(ListedNames('sub/arc4.gulp') = 'sub sub/s', 'archive inside a synchronized directory');

  WriteLn('PASS: the archive is never added to itself');
end.
