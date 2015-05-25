{
  Copyright (c) 2014-2015 Melchiorre Caruso.

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

  Contains:

    Various helper routines.

  Modified:

    v0.0.2 - 2015.05.23 by Melchiorre Caruso.
}

unit GulpCommon;

interface

uses
  Classes, SysUtils;

type
  { TSysScanner }

  TSysScanner = class(TObject)
  private
    FList: TStringList;
    function GetCount: integer;
    function GetItem(Index: longint): string;
    procedure AddItem(const FileName: string);
    procedure Scan(const FileMask: string; Recursive: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const FileMask: string);
    procedure Delete(Index: longint);
    function Find(const FileName: string): longint;
    procedure Clear;
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: string read GetItem;
  end;

  { TNulStream }

  TNulStream = class(TStream)
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { Matching routines }

  function FileNameMatch(const FileName, FileMask: string): boolean; overload;
  function FileNameMatch(const FileName: string; FileMasks: TStringList): boolean; overload;

  { Priority routines }

  function SetNormalPriority: boolean;
  function SetIdlePriority: boolean;

implementation

uses
  {$IFDEF UNIX} BaseUnix; {$ENDIF}
  {$IFDEF MSWINDOWS} Windows; {$ENDIF}

{ TSysScanner class }

constructor TSysScanner.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  {$IFDEF UNIX}
    FList.CaseSensitive := TRUE;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      FList.CaseSensitive := FALSE;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  FList.Duplicates  := dupIgnore;
  FList.Sorted      := TRUE;
end;

destructor TSysScanner.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TSysScanner.Clear;
begin
  FList.Clear;
end;

procedure TSysScanner.AddItem(const FileName: string);
begin
  if FList.IndexOf(FileName) = -1 then
  begin
    FList.Add(FileName);
  end;
end;

procedure TSysScanner.Scan(const FileMask: string; Recursive: boolean);
var
     Error : longint;
       Rec : TSearchRec;
  ScanMask : string;
  ScanPath : string;
begin
  ScanPath := ExtractFilePath(FileMask);
  ScanMask := ExtractFileName(FileMask);
  // Search filemask...
  Error := SysUtils.FindFirst(ScanPath + '*',
    faReadOnly  or faHidden  or faSysFile or faVolumeId  or
    faDirectory or faArchive or faSymLink or faAnyFile,  Rec);
  while Error = 0 do
  begin
    if Rec.Attr and faDirectory = faDirectory then
    begin
      if (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        AddItem(ScanPath + Rec.Name);
        if Recursive then
          if Rec.Attr and faSymLink = 0 then
            Scan(ScanPath + IncludeTrailingPathDelimiter(Rec.Name) + ScanMask, TRUE);
      end;
    end else
      if FileNameMatch(ScanPath + Rec.Name, FileMask) then
        AddItem(ScanPath + Rec.Name);

    Error := FindNext(Rec);
  end;
  SysUtils.FindClose(Rec);
end;

procedure TSysScanner.Add(const FileMask: string);
begin
  if FileMask = '' then Exit;
  if DirectoryExists(FileMask) then
    AddItem(FileMask)
  else
    if FileExists(FileMask) then
      AddItem(FileMask)
    else
      Scan(FileMask, TRUE);
end;

procedure TSysScanner.Delete(Index: longint);
begin
  FList.Delete(Index);
end;

function TSysScanner.Find(const FileName: string): longint;
begin
  if FList.Find(FileName, Result) = FALSE then
  begin
    Result := -1;
  end;
end;

function TSysScanner.GetItem(Index: longint): string;
begin
  Result := FList[Index];
end;

function TSysScanner.GetCount: longint;
begin
  Result := FList.Count;
end;

{ TNulStream class }

function TNulStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

function TNulStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

{ Matching routines }

function MatchPattern(Element, Pattern: pchar): boolean;
begin
  if 0 = StrComp(Pattern, '*') then
    Result := TRUE
  else
    if (Element^ = Chr(0)) and (Pattern^ <> Chr(0)) then
      Result := FALSE
    else
      if Element^ = Chr(0) then
        Result := TRUE
      else
        case Pattern^ of
          '*': if MatchPattern(Element, @Pattern[1]) then
                 Result := TRUE
               else
                 Result := MatchPattern(@Element[1], Pattern);
          '?': Result := MatchPattern(@Element[1], @Pattern[1]);
        else
          if Element^ = Pattern^ then
            Result := MatchPattern(@Element[1], @Pattern[1])
          else
            Result := FALSE;
        end;
end;

function FileNameMatch(const FileName, FileMask: string): boolean;
begin
  Result := MatchPattern(PChar(FileName), PChar(FileMask));
end;

function FileNameMatch(const FileName: string; FileMasks: TStringList): boolean;
var
  I : longint;
begin
  Result := FALSE;
  for I := 0 to FileMasks.Count - 1 do
    if FileNameMatch(FileName, FileMasks[I]) = TRUE then
    begin
      Result := TRUE;
      Break;
    end;
end;

{ Priority routines }

function SetIdlePriority: boolean;
begin
  {$IFDEF UNIX}
    Result := FpNice(5) = 0;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
end;

function SetNormalPriority: boolean;
begin
  {$IFDEF UNIX}
    Result := FpNice(10) = 0;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
end;

end.

