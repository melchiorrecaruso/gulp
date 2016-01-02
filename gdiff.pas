program gdiff;

{$mode objfpc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, GulpCommon, GulpLibrary, SHA1, SysUtils;

var
  I, J : longint;
  S    : ansistring;
  Scan : array[1..2] of TSysScanner;

begin
  writeln('GDIFF v0.0.3 - Compare FOLDERS file by file.');
  if ParamCount <> 2 then Halt;
  if DirectoryExists(ParamStr(1)) = FALSE then Halt;
  if DirectoryExists(ParamStr(2)) = FALSE then Halt;

  for I := 1 to 2 do
  begin
    Scan[I] := TSysScanner.Create;
    Scan[I].Add(IncludeTrailingPathDelimiter(ParamStr(I)) + '*');
    writeln(Format('%d object(s) in folder "%s"', [Scan[I].Count, ParamStr(I)]));
  end;

  for I := 0 to Scan[1].Count - 1 do
  begin
    S := Scan[1][I];
    Delete(S, 1, Length(IncludeTrailingPathDelimiter(ParamStr(1))));
    S := IncludeTrailingPathDelimiter(ParamStr(2)) + S;

    J := Scan[2].Find(S);
    if J <> -1 then
    begin

      if (GetAttr(Scan[1][I]) and faDirectory = 0) and
         (GetAttr(Scan[2][J]) and faDirectory = 0) then
        if SHA1Match(
             SHA1File(Scan[1][I], 4096),
             SHA1File(Scan[2][J], 4096)) = FALSE then
          writeln(Format('"%s" "%s" differ', [Scan[1][I], Scan[2][J]]));

      if GetTime(Scan[1][I]) <> GetTime(Scan[2][J]) then writeln(Format('"%s" and "%s" differ in time', [Scan[1][I], Scan[2][J]]));
      if GetSize(Scan[1][I]) <> GetSize(Scan[2][J]) then writeln(Format('"%s" and "%s" differ in size', [Scan[1][I], Scan[2][J]]));
      if GetAttr(Scan[1][I]) <> GetAttr(Scan[2][J]) then writeln(Format('"%s" and "%s" differ in attr', [Scan[1][I], Scan[2][J]]));
      if GetMode(Scan[1][I]) <> GetMode(Scan[2][J]) then writeln(Format('"%s" and "%s" differ in mode', [Scan[1][I], Scan[2][J]]));
      if GetLink(Scan[1][I]) <> GetLink(Scan[2][J]) then writeln(Format('"%s" and "%s" differ in link', [Scan[1][I], Scan[2][J]]));
      if GetUID (Scan[1][I]) <> GetUID (Scan[2][J]) then writeln(Format('"%s" and "%s" differ in uid ', [Scan[1][I], Scan[2][J]]));
      if GetUNM (Scan[1][I]) <> GetUNM (Scan[2][J]) then writeln(Format('"%s" and "%s" differ in unm ', [Scan[1][I], Scan[2][J]]));
      if GetGID (Scan[1][I]) <> GetGID (Scan[2][J]) then writeln(Format('"%s" and "%s" differ in gid ', [Scan[1][I], Scan[2][J]]));
      if GetGNM (Scan[1][I]) <> GetGNM (Scan[2][J]) then writeln(Format('"%s" and "%s" differ in gnm ', [Scan[1][I], Scan[2][J]]));

      Scan[2].Delete(J);
    end else
      writeln(Format('"%s" not founded in "%s"',[Scan[1][I], ParamStr(2)]));
  end;

  for J := 0 to Scan[2].Count - 1 do
    writeln(Format('"%s" not founded in "%s"',[Scan[2][J], ParamStr(1)]));

  for I := 1 to 2 do
    Scan[I].Destroy;
end.

