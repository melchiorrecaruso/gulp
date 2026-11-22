program TestGulpDiffUnix;
{$mode objfpc}{$H+}

uses BaseUnix, SysUtils, RegressionUtils, GulpDiffTestUtils;

var output, blocked: string;
begin
  NewPair('dangling');
  Check(FpSymlink('missing','left/broken') = 0, 'create left dangling link');
  Check(FpSymlink('missing','right/broken') = 0, 'create right dangling link');
  output := ComparePair;
  Check(Pos('Unable',output) = 0, 'dangling links are not read as files');
  Check(Pos('(DATA)',output) = 0, 'dangling links are not hashed');
  Check(Pos('(LINKNAME',output) = 0, 'identical link targets');

  NewPair('targets');
  WriteTextFile('target-left','left');
  WriteTextFile('target-right','right');
  Check(FpSymlink('../target-left','left/link') = 0, 'create left link');
  Check(FpSymlink('../target-right','right/link') = 0, 'create right link');
  output := ComparePair;
  Check(Pos('(LINKNAME',output) > 0, 'different stored link targets');
  Check(Pos('(DATA)',output) = 0, 'link target contents are not hashed');
  Check(Pos('Unable',output) = 0, 'no link read errors');

  NewPair('unreadable');
  if IsRoot then
    WriteLn('SKIP: unreadable-file case requires non-root user')
  else
  begin
    PairFile('readable');
    PairFile('unreadable');
    blocked := 'left/unreadable';
    WriteTextFile('right/unreadable','different');
    Check(FpChmod(blocked,0) = 0, 'remove read permission');
    try
      output := ComparePair(2);
      Check(Pos('Unable to read stream',output) > 0, 'read error is reported');
      Check(Pos('(DATA)',output) = 0, 'failed read does not compare stale hashes');
    finally
      Check(FpChmod(blocked,&600) = 0, 'restore file permission');
    end;
  end;

  NewPair('mode');
  PairFile('file');
  Check(FpChmod('left/file',&600) = 0, 'set left permissions');
  Check(FpChmod('right/file',&644) = 0, 'set right permissions');
  Check(Pos('(MODE',ComparePair) > 0, 'permission difference is reported');

  WriteLn('PASS: gulpdiff Unix cases (4; unreadable-file case skipped for root)');
end.
