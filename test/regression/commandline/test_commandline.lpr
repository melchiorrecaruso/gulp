program TestCommandLine;
{$mode objfpc}{$H+}

uses
  SysUtils, RegressionUtils;

procedure CheckError(const AArgs: array of string; const AMessage, ADescription: string);
var
  output: string;
begin
  Check(RunGulp(AArgs, output) <> 0, ADescription + ': failure exit code');
  Check(Pos(AMessage, output) > 0, ADescription + ': message "' + AMessage + '"');
end;

const
  Operations: array[0..5] of string = ('-s', '-r', '-p', '-c', '-f', '-l');
var
  output: string;
  i, j: integer;
begin
  EnterNewDir('work');
  WriteTextFile('in', 'i');
  WriteTextFile('k1', '1');
  WriteTextFile('k2', '2');
  WriteTextFile('k3', '3');

  // #9
  Check(RunGulp(['-h'], output) = 0, '-h succeeds');
  Check(Pos('SYNOPSIS', output) > 0, '-h shows the help');

  // #10
  GulpOk(['-s', 'a']);
  GulpOk(['-s', 's', '-e', 'in']);
  Check(Pos(' in ', ' ' + ListedNames('s') + ' ') = 0, 'an option argument equal to part of an option name');
  CheckError(['-s', 'x', '-e', '-s'], 'needs an argument', 'an option as argument');
  CheckError(['-s', 'x', '--exclud=k1'], 'Invalid option', 'a partial long option');
  CheckError(['-q'], 'Invalid option', 'an unknown option');

  // #11
  GulpOk(['-s', 'e.gulp', '-e', 'k1', '--exclude=k2', '-e', '*.gulp', '-e', 'a', '-e', 's']);
  Check(ListedNames('e.gulp') = 'in k3', 'all -e options are used');
  GulpOk(['-s', 'i.gulp', '-i', 'k1', '--include=k3']);
  Check(ListedNames('i.gulp') = 'k1 k3', 'all -i options are used');

  // #12
  CheckError(['--synch', 'l.gulp'], 'in the form --synch=value', 'a long option without =value');
  Check(not FileExists('l.gulp'), 'nothing is done for an invalid command line');
  GulpOk(['--synch=l.gulp', '--nodelete']);

  // #13
  CheckError(['-s'], 'needs an argument', 'a missing short option argument');
  CheckError(['--synch='], 'needs an argument', 'an empty long option argument');
  CheckError(['-s', 'l.gulp', '--nodelete=yes'], 'does not allow an argument', 'an argument for an option without argument');

  // #14
  Check(RunGulp(['-l', 'missing.gulp']) = 1, 'a runtime error returns exit code 1');
  Check(RunGulp(['-x']) = 1, 'an invalid option returns exit code 1');
  Check(RunGulp(['-l', 'l.gulp']) = 0, 'success returns exit code 0');

  // #20: an empty short-option value must never broaden a restore.
  WriteTextFile('unrelated', 'keep me');
  CheckError(['-r', 'i.gulp', '-i', ''], 'needs an argument', 'empty include');
  Check(FileExists('unrelated'), 'invalid restore preserves unrelated files');
  CheckError(['-s', 'empty.gulp', '-e', ''], 'needs an argument', 'empty exclude');
  Check(not FileExists('empty.gulp'), 'invalid sync does not create archive');
  CheckError(['-s', ''], 'needs an argument', 'empty archive name');

  // #21: reject every pair before opening either archive.
  for i := low(Operations) to high(Operations) do
    for j := low(Operations) to high(Operations) do
      CheckError([Operations[i], 'conflict.gulp', Operations[j], 'other.gulp'],
        'only one main operation', 'conflicting operations');
  CheckError(['--list=i.gulp', '--synch=conflict.gulp'],
    'only one main operation', 'conflicting long operations');
  CheckError(['-s', 'conflict.gulp', '-h'],
    'only one main operation', 'help with a modifying operation');
  Check(not FileExists('conflict.gulp'), 'conflict does not create first archive');
  Check(not FileExists('other.gulp'), 'conflict does not create second archive');

  // #22: validate the version before any operation; retain unsigned range.
  CheckError(['-r', 'i.gulp', '--until=-2'], 'Invalid version', 'negative until');
  Check(FileExists('unrelated'), 'invalid version does not restore or delete');
  CheckError(['-l', 'i.gulp', '--only=-1'], 'Invalid version', 'negative only');
  CheckError(['-l', 'i.gulp', '-u', '-2147483649'], 'Invalid version', 'negative short until');
  CheckError(['-l', 'i.gulp', '--until=4294967296'], 'Invalid version', 'until overflow');
  CheckError(['-l', 'i.gulp', '--only=4294967296'], 'Invalid version', 'only overflow');
  CheckError(['-l', 'i.gulp', '--until=garbage'], 'Invalid version', 'nonnumeric version');
  GulpOk(['-l', 'i.gulp', '--until=4294967295']);
  GulpOk(['-l', 'i.gulp', '--only=4294967295']);
  GulpOk(['-l', 'i.gulp', '--until=last']);
  GulpOk(['-l', 'i.gulp', '--until=0']);

  WriteLn('PASS: command line options, arguments, messages and exit codes');
end.
