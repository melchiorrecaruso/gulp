program TestSymlink;
{$mode objfpc}{$H+}

uses
  BaseUnix, SysUtils, RegressionUtils;

function LinkTime(const AName: string): int64;
var
  info: Stat;
begin
  Check(FpLStat(AName, info) = 0, 'lstat ' + AName);
  result := info.st_mtime;
end;

var
  output, work: string;
  t1, t2: int64;
begin
  work := EnterNewDir('work');
  WriteTextFile('t', 't');
  Check(FpSymlink('t', 'l') = 0, 'create link');
  Check(FpSymlink('missing', 'broken') = 0, 'create dangling link');
  t1 := LinkTime('l');
  t2 := LinkTime('broken');
  GulpOk(['-s', '../arc.gulp']);

  Sleep(1100);
  EnterNewDir('restored');
  output := GulpOk(['-r', '../arc.gulp']);
  Check(Pos('Unable', output) = 0, 'no warning while restoring links');
  Check(FpReadLink('l') = 't', 'link target');
  Check(FpReadLink('broken') = 'missing', 'dangling link target');
  Check(LinkTime('l') = t1, 'link time is restored');
  Check(LinkTime('broken') = t2, 'dangling link time is restored');

  WriteLn('PASS: symbolic links are restored with their time and no warning');
end.
