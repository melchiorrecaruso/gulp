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

var
  output: string;
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

  WriteLn('PASS: command line options, arguments, messages and exit codes');
end.
