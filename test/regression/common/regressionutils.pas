{ Helpers shared by the regression programs.

  Each program receives the absolute path of the gulp executable as its first
  argument and runs with an empty, writable results directory as cwd. }
unit RegressionUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringArray = array of string;

{ Raises an exception with ADescription when ACondition is false. }
procedure Check(ACondition: boolean; const ADescription: string);

{ Sets an environment variable for the next gulp runs ('' value removes it). }
procedure SetGulpEnvironment(const AName, AValue: string);

{ Runs gulp in the current directory; returns the exit code and the output. }
function RunGulp(const AArgs: array of string; out AOutput: string): integer;
function RunGulp(const AArgs: array of string): integer;
{ Runs gulp and fails if the exit code is not zero. }
function GulpOk(const AArgs: array of string): string;

{ Names listed by "gulp -l" with the given marker ('ADD' or 'DEL'), sorted. }
function ListedNames(const AArchive, AMarker: string; const AExtra: array of string): string;
function ListedNames(const AArchive: string): string;

{ Creates a clean directory below the results directory and enters it. }
function EnterNewDir(const AName: string): string;
procedure DeleteTree(const APath: string);

procedure WriteTextFile(const AFileName, AContent: string);
function ReadTextFile(const AFileName: string): string;
procedure CopyTextFile(const ASource, ADest: string);
{ Sorted names of the items below the current directory, '/' separated. }
function TreeNames: string;
{ True if the current directory tree contains a gulp temporary file. }
function HasTempFiles: boolean;

{ True if running as root on Unix: permission checks are then meaningless. }
function IsRoot: boolean;

implementation

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  Process;

var
  GulpExe, ResultsDir: string;
  Environment: TStringList;

procedure SetGulpEnvironment(const AName, AValue: string);
var
  i: integer;
begin
  if Environment.Count = 0 then
    for i := 1 to GetEnvironmentVariableCount do
      Environment.Add(GetEnvironmentString(i));
  i := Environment.IndexOfName(AName);
  if i <> -1 then
    Environment.Delete(i);
  if AValue <> '' then
    Environment.Add(AName + '=' + AValue);
end;

procedure Check(ACondition: boolean; const ADescription: string);
begin
  if not ACondition then
    raise Exception.Create('Check failed: ' + ADescription);
end;

function RunGulp(const AArgs: array of string; out AOutput: string): integer;
var
  child: TProcess;
  output: TStringStream;
  buffer: array[0..4095] of char;
  count, i: integer;

  procedure ReadOutput;
  begin
    while child.Output.NumBytesAvailable > 0 do
    begin
      count := child.Output.Read(buffer, SizeOf(buffer));
      if count <= 0 then break;
      output.WriteBuffer(buffer, count);
    end;
  end;

begin
  child := TProcess.Create(nil);
  output := TStringStream.Create('');
  try
    child.Executable := GulpExe;
    for i := 0 to High(AArgs) do
      child.Parameters.Add(AArgs[i]);
    child.CurrentDirectory := GetCurrentDir;
    if Environment.Count > 0 then
      child.Environment.Assign(Environment);
    child.Options := [poUsePipes, poStderrToOutPut];
    child.Execute;
    while child.Running do
    begin
      ReadOutput;
      Sleep(5);
    end;
    ReadOutput;
    AOutput := output.DataString;
    result := child.ExitCode;
  finally
    output.Free;
    child.Free;
  end;
end;

function RunGulp(const AArgs: array of string): integer;
var
  dummy: string;
begin
  result := RunGulp(AArgs, dummy);
end;

function GulpOk(const AArgs: array of string): string;
var
  i: integer;
  line: string;
begin
  if RunGulp(AArgs, result) <> 0 then
  begin
    line := 'gulp';
    for i := 0 to High(AArgs) do
      line := line + ' ' + AArgs[i];
    raise Exception.Create('Command failed: ' + line + LineEnding + result);
  end;
end;

function ListedNames(const AArchive, AMarker: string; const AExtra: array of string): string;
var
  args: array of string;
  lines, names, fields: TStringList;
  i: integer;
begin
  SetLength(args, 2 + Length(AExtra));
  args[0] := '-l';
  args[1] := AArchive;
  for i := 0 to High(AExtra) do
    args[2 + i] := AExtra[i];
  lines := TStringList.Create;
  names := TStringList.Create;
  fields := TStringList.Create;
  try
    lines.Text := GulpOk(args);
    fields.Delimiter := ' ';
    for i := 0 to lines.Count - 1 do
    begin
      fields.DelimitedText := Trim(lines[i]);
      if (fields.Count >= 3) and (fields[1] = AMarker) then
        names.Add(fields[fields.Count - 1]);
    end;
    names.Sort;
    names.Delimiter := ' ';
    result := names.DelimitedText;
  finally
    fields.Free;
    names.Free;
    lines.Free;
  end;
end;

function ListedNames(const AArchive: string): string;
begin
  result := ListedNames(AArchive, 'ADD', []);
end;

procedure DeleteTree(const APath: string);
var
  sr: TSearchRec;
  name: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile or faDirectory or faSymLink, sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then continue;
      name := IncludeTrailingPathDelimiter(APath) + sr.Name;
      {$IFDEF UNIX}
      if (sr.Attr and faSymLink) = 0 then
        FpChmod(name, &700);
      {$ENDIF}
      if ((sr.Attr and faDirectory) <> 0) and ((sr.Attr and faSymLink) = 0) then
        DeleteTree(name)
      else
        DeleteFile(name);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
  RemoveDir(APath);
end;

function EnterNewDir(const AName: string): string;
begin
  result := IncludeTrailingPathDelimiter(ResultsDir) + AName;
  SetCurrentDir(ResultsDir);
  if DirectoryExists(result) then
    DeleteTree(result);
  if not ForceDirectories(result) then
    raise Exception.Create('Cannot create ' + result);
  SetCurrentDir(result);
end;

procedure WriteTextFile(const AFileName, AContent: string);
var
  s: TFileStream;
begin
  ForceDirectories(ExtractFileDir(ExpandFileName(AFileName)));
  s := TFileStream.Create(AFileName, fmCreate);
  try
    if AContent <> '' then
      s.WriteBuffer(AContent[1], Length(AContent));
  finally
    s.Free;
  end;
end;

function ReadTextFile(const AFileName: string): string;
var
  s: TFileStream;
begin
  s := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(result, s.Size);
    if s.Size > 0 then
      s.ReadBuffer(result[1], s.Size);
  finally
    s.Free;
  end;
end;

procedure CopyTextFile(const ASource, ADest: string);
begin
  WriteTextFile(ADest, ReadTextFile(ASource));
end;

procedure AddTree(const APath: string; AList: TStringList);
var
  sr: TSearchRec;
  name: string;
begin
  if FindFirst(APath + '*', faAnyFile or faDirectory or faSymLink, sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then continue;
      name := APath + sr.Name;
      AList.Add(StringReplace(name, PathDelim, '/', [rfReplaceAll]));
      if ((sr.Attr and faDirectory) <> 0) and ((sr.Attr and faSymLink) = 0) then
        AddTree(name + PathDelim, AList);
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

function TreeNames: string;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    AddTree('', list);
    list.Sort;
    list.Delimiter := ' ';
    result := list.DelimitedText;
  finally
    list.Free;
  end;
end;

function HasTempFiles: boolean;
begin
  result := Pos('TMP', UpperCase(TreeNames)) > 0;
end;

function IsRoot: boolean;
begin
  {$IFDEF UNIX}
  result := FpGetEUid = 0;
  {$ELSE}
  result := false;
  {$ENDIF}
end;

initialization
  GulpExe := ParamStr(1);
  if (GulpExe = '') or not FileExists(GulpExe) then
  begin
    WriteLn(StdErr, 'Usage: ', ExtractFileName(ParamStr(0)), ' <absolute path of gulp executable>');
    Halt(2);
  end;
  ResultsDir := GetCurrentDir;
  Environment := TStringList.Create;

finalization
  Environment.Free;
end.
