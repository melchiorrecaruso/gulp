program TestTrailingSlash;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

var
  n: integer = 0;

procedure CheckSync(const AArgs: array of string; const AExpected, ADescription: string);
var
  args: array of string;
  i: integer;
  archive: string;
begin
  Inc(n);
  archive := '../a' + IntToStr(n) + '.gulp';
  SetLength(args, Length(AArgs) + 2);
  args[0] := '-s';
  args[1] := archive;
  for i := 0 to High(AArgs) do
    args[i + 2] := AArgs[i];
  GulpOk(args);
  Check(ListedNames(archive) = AExpected, ADescription);
end;

begin
  EnterNewDir('work');
  WriteTextFile('d1' + PathDelim + 'a', 'a');
  WriteTextFile('d2' + PathDelim + 'b', 'b');

  CheckSync(['-i', 'd2'], 'd2 d2/b', '-i d2');
  CheckSync(['-i', 'd2/'], 'd2 d2/b', '-i d2/');
  CheckSync(['-i', 'd2//'], 'd2 d2/b', '-i d2//');
  CheckSync(['--include=d2/'], 'd2 d2/b', '--include=d2/');
  CheckSync(['d2'], 'd2 d2/b', 'd2');
  CheckSync(['d2/'], 'd2 d2/b', 'd2/');
  CheckSync(['-e', 'd1'], 'd2 d2/b', '-e d1');
  CheckSync(['-e', 'd1/'], 'd2 d2/b', '-e d1/');
  CheckSync(['--exclude=d1/'], 'd2 d2/b', '--exclude=d1/');

  Check(ListedNames('../a1.gulp', 'ADD', ['-i', 'd2/']) = 'd2 d2/b', 'list -i d2/');
  EnterNewDir('restored');
  GulpOk(['-r', '../a1.gulp', '-i', 'd2/']);
  Check(TreeNames = 'd2 d2/b', 'restore -i d2/');

  WriteLn('PASS: a trailing slash does not change the meaning of a path');
end.
