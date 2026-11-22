program TestGulpDiff;
{$mode objfpc}{$H+}

uses SysUtils, RegressionUtils, GulpDiffTestUtils;

var output: string;
begin
  NewPair('identical');
  PairFile('file');
  output := ComparePair;
  Check(Pos('(DATA)',output) = 0, 'identical files have no DATA differences');
  Check(Pos('not found',output) = 0, 'identical files are found');
  Check(Pos('Unable',output) = 0, 'identical files are readable');

  NewPair('empty');
  output := ComparePair;
  Check(Pos('0 object(s) in folder "left"',output) > 0, 'empty left directory');
  Check(Pos('0 object(s) in folder "right"',output) > 0, 'empty right directory');

  NewPair('data');
  PairFile('file with spaces');
  WriteTextFile('right'+PathDelim+'file with spaces','different');
  output := ComparePair;
  Check(Pos('(DATA) "left'+PathDelim+'file with spaces"',output) > 0,
    'left DATA output is quoted');
  Check(Pos('(DATA) "right'+PathDelim+'file with spaces"',output) > 0,
    'right DATA output is quoted');

  NewPair('nested');
  PairFile('nested'+PathDelim+'file');
  WriteTextFile('left'+PathDelim+'only-left','left');
  WriteTextFile('right'+PathDelim+'only-right','right');
  output := ComparePair;
  Check(Pos('"left'+PathDelim+'only-left" not found in "right"',output) > 0,
    'missing file on right');
  Check(Pos('"right'+PathDelim+'only-right" not found in "left"',output) > 0,
    'missing file on left');
  Check(Pos('(DATA)',output) = 0, 'nested identical file has no DATA differences');

  NewPair('arguments');
  Diff([],2);
  Diff(['left'],2);
  Diff(['left','right'+PathDelim+'missing'],2);
  Diff(['left','right','extra'],2);

  NewPair('type');
  WriteTextFile('left'+PathDelim+'item','file');
  Check(CreateDir('right'+PathDelim+'item'), 'create directory opposite file');
  output := ComparePair;
  Check(Pos('(ATTR',output) > 0, 'file/directory attributes differ');
  Check(Pos('(DATA)',output) = 0, 'directories are not hashed');

  WriteLn('PASS: gulpdiff core cases (6)');
end.
