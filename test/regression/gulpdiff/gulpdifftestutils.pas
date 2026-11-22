unit GulpDiffTestUtils;
{$mode objfpc}{$H+}

interface

procedure NewPair(const AName: string);
procedure PairFile(const AName: string; const AContent: string = 'same');
function Diff(const AArgs: array of string; AStatus: integer = 0): string;
function ComparePair(AStatus: integer = 0): string;

implementation

uses SysUtils, RegressionUtils;

procedure NewPair(const AName: string);
begin
  EnterNewDir(AName);
  Check(CreateDir('left'), 'create left directory');
  Check(CreateDir('right'), 'create right directory');
end;

procedure PairFile(const AName: string; const AContent: string);
var left, right: string;
begin
  left := 'left'+PathDelim+AName;
  right := 'right'+PathDelim+AName;
  WriteTextFile(left,AContent);
  CopyTextFile(left,right);
  Check(FileSetDate(right,FileAge(left)) = 0, 'copy file modification time');
end;

function Diff(const AArgs: array of string; AStatus: integer): string;
var status: integer; diagnostics: string;
begin
  // The runner supplies gulpdiff as the executable to the shared helper.
  status := RunGulp(AArgs,result);
  Check(status = AStatus, 'gulpdiff exit code '+IntToStr(status)+
    ', expected '+IntToStr(AStatus)+LineEnding+result);
  diagnostics := StringReplace(result,'0 unfreed memory blocks : 0','',[rfReplaceAll]);
  Check(Pos('unfreed memory blocks :',diagnostics) = 0, 'no memory leaks'+LineEnding+result);
  Check(Pos('Runtime error',result) = 0, 'no runtime errors'+LineEnding+result);
end;

function ComparePair(AStatus: integer): string;
begin
  result := Diff(['left','right'],AStatus);
end;

end.
