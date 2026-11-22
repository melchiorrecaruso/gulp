program TestAtomicRestore;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, RegressionUtils;

var
  data: string;
  i: integer;
  s: TFileStream;
  b: byte;
begin
  EnterNewDir('work');
  SetLength(data, 300000);
  for i := 1 to Length(data) do
    data[i] := chr(i mod 251);
  WriteTextFile('big', data);
  GulpOk(['-s', '../arc.gulp']);

  // corrupt one byte of the stored data, which is at the end of the archive
  CopyTextFile('../arc.gulp', '../bad.gulp');
  s := TFileStream.Create('../bad.gulp', fmOpenReadWrite);
  try
    s.Seek(-1000, soEnd);
    s.ReadBuffer(b, 1);
    b := not b;
    s.Seek(-1, soCurrent);
    s.WriteBuffer(b, 1);
  finally
    s.Free;
  end;

  EnterNewDir('restored');
  WriteTextFile('big', 'old');
  Check(RunGulp(['-r', '../bad.gulp']) <> 0, 'restore of corrupted data fails');
  Check(ReadTextFile('big') = 'old', 'the existing file is left untouched');
  Check(TreeNames = 'big', 'no partial or temporary file is left');

  GulpOk(['-r', '../arc.gulp']);
  Check(ReadTextFile('big') = data, 'the intact archive is restored');
  Check(TreeNames = 'big', 'no temporary file is left');

  WriteLn('PASS: restore replaces files only with verified data');
end.
