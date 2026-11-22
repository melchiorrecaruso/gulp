program Regression;
{$mode objfpc}{$H+}

uses {$IFDEF UNIX}BaseUnix,{$ENDIF} Classes, SysUtils, StrUtils, Process;

var
  Root, BuildDir, FPC, OnlyPrefix, GulpExe: string;
  TestTimeout: QWord = 120000;
  Passed, Failed, Skipped, Selected: integer;

function Option(const AName, ADefault: string): string;
var i: integer; prefix: string;
begin
  result := ADefault;
  prefix := '--'+AName+'=';
  for i := 1 to ParamCount do
    if AnsiStartsStr(prefix,ParamStr(i)) then
      result := Copy(ParamStr(i),Length(prefix)+1,MaxInt);
end;

function NativePath(const S: string): string;
begin
  result := StringReplace(S,'/',PathDelim,[rfReplaceAll]);
end;

{ Drain the pipe while the child runs, so a verbose compiler cannot block.
  A failed launch, nonzero exit, signal or timeout must fail the run. }
function Execute(const AExecutable: string; AArgs: TStrings;
  const ADirectory, ALog: string; ATimeout: QWord): boolean;
var
  child: TProcess;
  output: TStringStream;
  buffer: array[0..4095] of char;
  count: integer;
  started: QWord;
  timedOut: boolean;

  procedure ReadOutput;
  begin
    while child.Output.NumBytesAvailable > 0 do
    begin
      count := child.Output.Read(buffer,SizeOf(buffer));
      if count <= 0 then break;
      output.WriteBuffer(buffer,count);
    end;
  end;

begin
  result := false;
  child := TProcess.Create(nil);
  output := TStringStream.Create('');
  try
    child.Executable := AExecutable;
    child.Parameters.Assign(AArgs);
    child.CurrentDirectory := ADirectory;
    child.Options := [poUsePipes,poStderrToOutPut];
    started := GetTickCount64;
    timedOut := false;
    try
      child.Execute;
      while child.Running do
      begin
        ReadOutput;
        if GetTickCount64-started >= ATimeout then
        begin
          timedOut := true;
          child.Terminate(124);
          child.WaitOnExit(5000);
          break;
        end;
        Sleep(10);
      end;
      ReadOutput;
      result := not timedOut and (child.ExitStatus = 0);
      if timedOut then
        WriteLn('FAIL: timed out after ',ATimeout div 1000,' seconds')
      else if not result then
        WriteLn('FAIL: exit code ',child.ExitCode);
    except
      on E: Exception do
        output.WriteString(E.ClassName+': '+E.Message+LineEnding);
    end;
    output.SaveToFile(ALog);
    if not result then
      WriteLn(output.DataString);
  finally
    output.Free;
    child.Free;
  end;
end;

{ Results of a previous run are removed, so each test starts in an empty directory.
  A failed test may leave items without permissions, which are restored first. }
procedure DeleteTree(const APath: string);
var
  sr: TSearchRec;
  name: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath)+'*',faAnyFile or faDirectory or faSymLink,sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then continue;
      name := IncludeTrailingPathDelimiter(APath)+sr.Name;
      {$IFDEF UNIX}
      if (sr.Attr and faSymLink) = 0 then FpChmod(name,&700);
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

procedure BuildGulp;
var
  args: TStringList;
  outputDir, unitsDir: string;
begin
  outputDir := BuildDir+PathDelim+'gulp';
  unitsDir := outputDir+PathDelim+'units';
  if not ForceDirectories(unitsDir) then
    raise Exception.Create('Cannot create '+unitsDir);
  GulpExe := outputDir+PathDelim+'gulp'+{$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF};
  args := TStringList.Create;
  try
    WriteLn('BUILD: src/gulp.pas');
    args.Add('-Fu'+Root+PathDelim+'src');
    args.Add('-FU'+unitsDir);
    args.Add('-o'+GulpExe);
    args.Add(Root+PathDelim+'src'+PathDelim+'gulp.pas');
    if not Execute(FPC,args,Root,outputDir+PathDelim+'build.log',600000) then
      raise Exception.Create('Cannot build gulp (logs: '+outputDir+')');
  finally
    args.Free;
  end;
end;

procedure RunTest(const ASource, AMode: string);
var
  source, executable, name, outputDir, resultsDir, unitsDir: string;
  args: TStringList;
  success: boolean;
begin
  if (AMode <> 'core') and (AMode <> 'unix') then
    raise Exception.Create('Unknown regression mode: '+AMode);
  source := ExpandFileName(Root+PathDelim+'test'+PathDelim+'regression'+PathDelim+NativePath(ASource));
  if not FileExists(source) then
    raise Exception.Create('Missing test source: '+source);
  Inc(Selected);
  {$IFNDEF UNIX}
  if AMode = 'unix' then
  begin
    WriteLn('SKIP: ',ASource,' requires Unix');
    Inc(Skipped);
    exit;
  end;
  {$ENDIF}
  name := ChangeFileExt(ExtractFileName(source),'');
  outputDir := BuildDir+PathDelim+AMode+PathDelim+name;
  resultsDir := outputDir+PathDelim+'results';
  unitsDir := BuildDir+PathDelim+'units';
  if DirectoryExists(resultsDir) then
    DeleteTree(resultsDir);
  if not ForceDirectories(resultsDir) or not ForceDirectories(unitsDir) then
    raise Exception.Create('Cannot create '+resultsDir);
  executable := outputDir+PathDelim+name+{$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF};
  args := TStringList.Create;
  try
    WriteLn('BUILD [',AMode,']: ',ASource);
    args.Add('-Fu'+Root+PathDelim+'src');
    args.Add('-Fu'+Root+PathDelim+'test'+PathDelim+'regression'+PathDelim+'common');
    args.Add('-FU'+unitsDir);
    args.Add('-o'+executable);
    args.Add(source);
    success := Execute(FPC,args,Root,outputDir+PathDelim+'build.log',600000);
    if success then
    begin
      WriteLn('RUN: ',ASource);
      args.Clear;
      args.Add(GulpExe);
      success := Execute(executable,args,resultsDir,outputDir+PathDelim+'run.log',TestTimeout);
    end;
    if success then
    begin
      Inc(Passed);
      WriteLn('PASS: ',ASource);
    end else
    begin
      Inc(Failed);
      WriteLn('FAIL: ',ASource,' (logs: ',outputDir,')');
    end;
  finally
    args.Free;
  end;
end;

var manifest, fields: TStringList; fileName, line: string; i: integer;
begin
  try
    Root := ExpandFileName(Option('root',GetCurrentDir));
    BuildDir := ExpandFileName(Option('build-dir',Root+PathDelim+'.regression-build'));
    FPC := Option('fpc','fpc');
    OnlyPrefix := Option('only','');
    TestTimeout := StrToQWord(Option('timeout','120'))*1000;
    if TestTimeout = 0 then raise Exception.Create('Timeout must be positive');
    fileName := Option('manifest',Root+PathDelim+'test'+PathDelim+'regression'+PathDelim+'tests.txt');
    manifest := TStringList.Create;
    fields := TStringList.Create;
    try
      manifest.LoadFromFile(fileName);
      fields.StrictDelimiter := true;
      fields.Delimiter := '|';
      BuildGulp;
      for i := 0 to manifest.Count-1 do
      begin
        line := Trim(manifest[i]);
        if (line = '') or (line[1] = '#') then continue;
        fields.DelimitedText := line;
        if fields.Count <> 2 then raise Exception.Create('Invalid manifest line: '+line);
        if (OnlyPrefix <> '') and not AnsiStartsStr(OnlyPrefix,fields[0]) then continue;
        RunTest(Trim(fields[0]),Trim(fields[1]));
      end;
      if Selected = 0 then raise Exception.Create('No regression test selected');
    finally
      fields.Free;
      manifest.Free;
    end;
    WriteLn('Regression results: ',Passed,' passed, ',Failed,' failed, ',Skipped,' skipped');
    if Failed <> 0 then Halt(1);
  except
    on E: Exception do
    begin
      WriteLn(StdErr,'FAIL: ',E.ClassName,': ',E.Message);
      Halt(1);
    end;
  end;
end.
