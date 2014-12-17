{
  Copyright (c) 2014 Melchiorre Caruso.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{
  Contains:

    Various helper routines.

  Modifyed:

}

unit Common;

{$I gulp.inc}

interface

const
  { Default file names }

  DefaultConfigFileName = 'bx.ini';

  {$IFDEF MSWINDOWS}
    DefaultSfxFileName = 'bxwin.sfx';
  {$ENDIF}
  {$IFDEF UNIX}
    DefaultSfxFileName = 'bxlinux.sfx';
  {$ENDIF}
  {$IFDEF MAC}
    TODO...
  {$ENDIF}

function SelfName: string;
function SelfPath: string;

{ disk/file routines }

function GetDriveFreeSpace(const FileName: string): int64;
function SizeOfFile(const FileName: string): int64;

{ filename handling routines }

function DeleteFileDrive(const FileName: string): string;
function DeleteFilePath(const FilePath, FileName: string): string;

//function FileNameIsValid(const FileName: string): boolean;
//function FilePathIsValid(const FileName: string): boolean;

function DirectoryNameMatch(const DirName, DirMask: string): boolean;
function FileNameMatch(const FileName, FileMask: string): boolean;
function FileMaskHasWildcards(const FileMask: string): boolean;
function FileNamePos(const FilePath, FileName: string): longint;


function GenerateAltFileName(const FileName: string; Index: longint): string;
function GenerateFileName(const FilePath: string): string;

{ time handling routines }

function DateTimeToString(X: TDateTime): string; overload;
function DateTimeToString(X: TDateTime; const Format: string): string; overload;
function FileTimeToString(X: longint): string; overload;
function FileTimeToString(X: longint; const Format: string): string; overload;
function FileTimeToUnix(X: longint): int64;
function UnixToFileTime(X: int64): longint;
function TimeDifference(X: double): string;
function TimeToStr(T: longint): string;

{ hex routines }

function Hex(const Data; Count: longint): string;
function HexToData(const S: string; var Data; Count: longint): boolean;

{ oem-ansi charset functions }

function OemToParam(const Param: string): string;
function ParamToOem(const Param: string): string;

{ system control }

procedure SetCtrlCHandler(CtrlHandler: pointer);
function  SetIdlePriority: boolean;
function  SetNormalPriority: boolean;

implementation

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  DateUtils,
  SysUtils;

const
  HexaDecimals: array [0..15] of char = '0123456789ABCDEF';
  HexValues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

function SelfName: string;
begin
  Result := ExtractFileName(ParamStr(0));
end;

function SelfPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

{ file routines }

function GetDriveFreeSpace(const FileName: string): int64;
var
  S: string;
begin
  S := GetCurrentDir;
  if SetCurrentDir(ExtractFileDir(ExpandFileName(FileName))) then
    Result := DiskFree(0)
  else
    Result := -1;
  SetCurrentDir(S);
end;

function SizeOfFile(const FileName: string): int64;
var
  Err: longint;
  Rec: TSearchRec;
begin
  Err := SysUtils.FindFirst(FileName, faAnyFile, Rec);
  if (Err = 0) and ((Rec.Attr and faDirectory) = 0) then
    Result := Rec.Size
  else
    Result := -1;
  SysUtils.FindClose(Rec);
end;

{ filename handling routines }

function DeleteFileDrive(const FileName: string): string;
var
  Drive: string;
begin
  Result := FileName;
  if Length(Result) > 0 then
  begin
    Drive := ExtractFileDrive(FileName);
    System.Delete(Result, 1, Length(Drive));
    while Pos(PathDelim, Result) = 1 do
    begin
      Delete(Result, 1, 1);
    end;
  end;
end;

function DeleteFilePath(const FilePath, FileName: string): string;
begin
  Result := FileName;
  if FileNamePos(FilePath, Result) = 1 then
  begin
    Delete(Result, 1, Length(FilePath));
  end;
end;

function FileNameIsValid(const FileName: string): boolean;
const
  {$IFDEF MSWINDOWS}
    InvalidChars: set of char = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  {$ENDIF}
  {$IFDEF UNIX}
    InvalidChars: set of char = [PathDelim];
  {$ENDIF}
  {$IFDEF MAC}
    TODO...
  {$ENDIF}
var
  I: longint;
begin
  Result := Length(FileName) > 0;
  if Result then
    for I := 1 to Length(FileName) do
    begin
      Result := not (FileName[I] in InvalidChars);
      if Result = FALSE then
      begin
        Break;
      end;
    end;
end;

function FilePathIsValid(const FilePath: string): boolean;
const
  {$IFDEF MSWINDOWS}
    InvalidChars: set of char = ['*', '?', '"', '<', '>', '|'];
  {$ENDIF}
  {$IFDEF UNIX}
    InvalidChars: set of char = [];
  {$ENDIF}
  {$IFDEF MAC}
    TODO...
  {$ENDIF}
var
  I: longint;
begin
  Result := TRUE;
  for I := 1 to Length(FilePath) do
  begin
    Result := not (FilePath[I] in InvalidChars);
    if Result = FALSE then
    begin
      Break;
    end;
  end;

  if Result = TRUE then
  begin
    Result := Pos(PathDelim + PathDelim, FilePath) = 0;
  end;
end;

function FileMaskHasWildcards(const FileMask: string): boolean;
const
  WildcardCharacters: set of char = ['*', '?'];
var
  I: longint;
begin
  Result := FALSE;
  for I := 1 to Length(FileMask) do
  begin
    Result := FileMask[I] in WildcardCharacters;
    if Result = TRUE then
    begin
      Break;
    end;
  end;
end;

function MatchPattern(Element, Pattern: PChar): boolean;
begin
  if 0 = StrComp(Pattern, '*') then
    Result := True
  else
    if (Element^ = Chr(0)) and (Pattern^ <> Chr(0)) then
      Result := False
    else
      if Element^ = Chr(0) then
        Result := True
      else
        case Pattern^ of
          '*': if MatchPattern(Element, @Pattern[1]) then
                 Result := True
               else
                 Result := MatchPattern(@Element[1], Pattern);

          '?': Result := MatchPattern(@Element[1], @Pattern[1]);

        else
          if Element^ = Pattern^ then
            Result := MatchPattern(@Element[1], @Pattern[1])
          else
            Result := False;
        end;
end;

function DirectoryNameMatch(const DirName, DirMask: string): boolean;
begin
  Result := MatchPattern(PChar(DirName), PChar(DirMask));
end;

function FileNameMatch(const FileName, FileMask: string): boolean;
begin
  Result := MatchPattern(PChar(FileName), PChar(FileMask));
end;

function FileNamePos(const FilePath, FileName: string): longint;
begin
  {$IFDEF MSWINDOWS}
    Result := System.Pos(UpperCase(FilePath), UpperCase(FileName));
  {$ENDIF}
  {$IFDEF UNIX}
    Result := System.Pos(FilePath, FileName);
  {$ENDIF}
end;

function GenerateAltFileName(const FileName: string; Index: longint): string;
begin
  Result := ChangeFileExt(FileName, '_' +
    IntToStr(Index) + ExtractFileExt(FileName));
end;

function GenerateFileName(const FilePath: string): string;
var
  I: longint;
begin
  repeat
    Result := '????????.$$$';
    for I := 1 to 8 do
      Result[I] := char(byte('A') + Random(byte('Z') - byte('A')));

    if FilePath <> '' then
      Result := IncludeTrailingBackSlash(FilePath) + Result;

  until FileAge(Result) = -1;
end;

{ time handling routines }

function DateTimeToString(X: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', X);
end;

function DateTimeToString(X: TDateTime; const Format: string): string;
begin
  Result := FormatDateTime(Format, X);
end;

function FileTimeToString(X: longint): string;
begin
  try
    Result := Common.DateTimeToString(SysUtils.FileDateToDateTime(X));
  except
    Result := '????-??-?? ??:??:??';
  end;
end;

function FileTimeToString(X: longint; const Format: string): string;
begin
  try
    Result := Common.DateTimeToString(SysUtils.FileDateToDateTime(X), Format);
  except
    Result := '????-??-?? ??:??:??';
  end;
end;

function FileTimeToUnix(X: longint): int64;
begin
  Result := DateTimeToUnix(FileDateToDateTime(X));
end;

function UnixToFileTime(X: int64): longint;
begin
  Result := DateTimeToFileDate(UnixToDateTime(X));
end;

function TimeDifference(X: double): string;
begin
  Result := Format('%0.2f', [(Now - X) * (24 * 60 * 60)]);
end;

function TimeToStr(T: longint): string;
var
  H, M, S:    string;
  ZH, ZM, ZS: longint;
begin
  ZH := T div 3600;
  ZM := T div 60 - ZH * 60;
  ZS := T - (ZH * 3600 + ZM * 60);

  if ZH < 10 then
    H := '0' + IntToStr(ZH)
  else
    H := IntToStr(ZH);

  if ZM < 10 then
    M := '0' + IntToStr(ZM)
  else
    M := IntToStr(ZM);

  if ZS < 10 then
    S := '0' + IntToStr(ZS)
  else
    S := IntToStr(ZS);

  Result := H + ':' + M + ':' + S;
end;

{ hex routines }

function Hex(const Data; Count: longint): string;
var
  I, J: longint;
  K:    longword;
begin
  SetLength(Result, Count shl 1);
  J := 1;
  for I := 0 to Count - 1 do
  begin
    K := TByteArray(Data) [I];
    Result[J] := HexaDecimals[K shr 4];
    Inc(J);
    Result[J] := HexaDecimals[K and $f];
    Inc(J);
  end;
end;

function HexToData(const S: string; var Data; Count: longint): boolean;
var
  I: longint;
begin
  Result := False;
  if Length(S) < Count * 2 then Exit;

  for I := 0 to Count - 1 do
  begin
    if (S[I * 2 + 1] in ['0'..'9', 'A'..'F']) and (S[I * 2 + 2] in ['0'..'9', 'A'..'F']) then
    begin
      TByteArray(Data)[I] := HexValues[S[I * 2 + 1]] shl 4 + HexValues[S[I * 2 + 2]]
    end else
      Exit;
  end;
  Result := True;
end;

{ oem-ansi charset functions }

function OemToParam(const Param: string): string;
begin
  {$IFDEF MSWINDOWS}
    if Length(Param) > 0 then
    begin
      SetLength(Result, Length(Param));
      OemToChar(PChar(Param), PChar(Result));
    end else
      Result := '';
  {$ELSE}
    Result := Param;
  {$ENDIF}
end;

function ParamToOem(const Param: string): string;
begin
  {$IFDEF MSWINDOWS}
    if Length(Param) > 0 then
    begin
      SetLength(Result, Length(Param));
      CharToOem(PChar(Param), PChar(Result));
    end else
      Result := '';
  {$ELSE}
    Result := Param;
  {$ENDIF}
end;

{ system control }

procedure SetCtrlCHandler(CtrlHandler: pointer);
{$IFDEF UNIX}
var
  oa, na: SigActionRec;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    SetConsoleCtrlHandler(CtrlHandler, TRUE);
  {$ENDIF}
  {$IFDEF UNIX}
    na.sa_handler := SigActionHandler(CtrlHandler);
    FillChar(na.sa_mask, SizeOf(na.sa_mask), #0);
    na.sa_flags    := SA_ONESHOT;
    na.sa_restorer := nil;
    fpSigAction(SIGINT, @na, @oa);
  {$ENDIF}
  {$IFDEF MAC}
    TODO...
  {$ENDIF}
end;

function SetIdlePriority: boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := FALSE;
  {$ENDIF}
  {$IFDEF MAC}
    TODO...
  {$ENDIF}
end;

function SetNormalPriority: boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := FALSE;
  {$ENDIF}
  {$IFDEF MAC}
    TODO...
  {$ENDIF}
end;

end.