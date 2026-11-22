program TestUnreadable;
{$mode objfpc}{$H+}

uses
  SysUtils, BaseUnix, RegressionUtils;

var
  output: string;
begin
  if IsRoot then
  begin
    WriteLn('SKIP: permissions are not enforced for root');
    exit;
  end;
  EnterNewDir('work');
  WriteTextFile('s.txt', 'secret');
  WriteTextFile('o.txt', 'ok');
  FpChmod('s.txt', 0);
  output := GulpOk(['-s', '../arc.gulp']);
  Check(Pos('s.txt', output) > 0, 'a warning reports the unreadable file');
  Check(ListedNames('../arc.gulp') = 'o.txt', 'the unreadable file is not stored');

  EnterNewDir('restored');
  GulpOk(['-r', '../arc.gulp']);
  Check(ReadTextFile('o.txt') = 'ok', 'restore works');

  SetCurrentDir('..' + PathDelim + 'work');
  FpChmod('s.txt', &644);
  GulpOk(['-s', '../arc.gulp']);
  Check(ListedNames('../arc.gulp') = 'o.txt s.txt', 'the file is added when readable again');
  GulpOk(['-c', '../arc.gulp']);

  WriteLn('PASS: unreadable files are skipped and retried');
end.
