program TestLastVersion;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

begin
  EnterNewDir('work');
  WriteTextFile('k1', '1');
  WriteTextFile('k2', '2');
  GulpOk(['-s', '../arc.gulp']);
  DeleteFile('k2');
  GulpOk(['-s', '../arc.gulp']);

  Check(Pos('Last version 2', GulpOk(['-l', '../arc.gulp'])) > 0, 'list');
  Check(Pos('Last version 2', GulpOk(['-l', '../arc.gulp', '--until=1'])) > 0, 'list --until');
  Check(Pos('Last version 2', GulpOk(['-l', '../arc.gulp', '--only=2'])) > 0, 'list --only');
  Check(Pos('Last version 2', GulpOk(['-l', '../arc.gulp', '-i', 'k1'])) > 0, 'list -i');

  WriteTextFile('../empty.gulp', '');
  Check(Pos('Last version 0', GulpOk(['-l', '../empty.gulp'])) > 0, 'empty archive');

  WriteLn('PASS: list reports the last version of the archive');
end.
