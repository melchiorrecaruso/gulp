program TestRestoreArchive;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

begin
  EnterNewDir('samedir');
  WriteTextFile('a.txt', 'a');
  GulpOk(['-s', 'arc.gulp']);
  WriteTextFile('new.txt', 'n');
  GulpOk(['-r', 'arc.gulp']);
  Check(FileExists('arc.gulp'), 'archive in the restored directory survives');
  Check(not FileExists('new.txt'), 'items not in the archive are deleted');
  GulpOk(['-r', './arc.gulp']);
  Check(FileExists('arc.gulp'), 'archive given as ./arc.gulp survives');

  EnterNewDir('subdir');
  WriteTextFile('a.txt', 'a');
  GulpOk(['-s', 'arc.gulp']);
  ForceDirectories('bk');
  RenameFile('arc.gulp', 'bk' + PathDelim + 'arc.gulp');
  WriteTextFile('junk', 'j');
  WriteTextFile('keep.log', 'k');
  GulpOk(['-r', 'bk/arc.gulp', '-e', 'keep.log']);
  Check(FileExists('bk' + PathDelim + 'arc.gulp'), 'archive in a non archived directory survives');
  Check(FileExists('keep.log'), 'excluded files are not deleted');
  Check(not FileExists('junk'), 'items not in the archive are deleted');

  WriteLn('PASS: restore keeps the archive, its directory and excluded files');
end.
