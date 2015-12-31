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

    v0.0.2 - 2015.12.30 by Melchiorre Caruso.
}

unit GulpCommon;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
  { TGenericList }

  generic TGenericList<TGenericItem> = class(TObject)
  public type
    TGenericCompare = function(Item1, Item2: TGenericItem): longint;
  private
    FList       : TList;
    FCompare    : TGenericCompare;
    function    GetCount: longint;
    function    GetItem(Index: longint): TGenericItem;
    procedure   SetCompare(Value: TGenericCompare);
  public
    constructor Create(Compare: TGenericCompare);
    destructor  Destroy; override;
    function    Add (Item: TGenericItem): longint;
    function    Find(Item: TGenericItem): longint;
    procedure   Delete(Index: longint);
    procedure   Clear;
  public
    property Compare: TGenericCompare write SetCompare;
    property Items[Index: longint]: TGenericItem read GetItem; default;
    property Count: longint read GetCount;
  end;

  { TStrList }

  TPCharList = specialize TGenericList<PChar>;

  TStrList = class(TObject)
  private
    FList       : TPCharList;
    function    GetCount: longint;
    function    Get(Index: longint): ansistring;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add (const S: ansistring);
    function    Find(const S: ansistring): longint;
    procedure   Delete(Index: longint);
    procedure   Clear;
  public
    property Items[Index: longint]: ansistring read Get; default;
    property Count: longint read GetCount;
  end;

  { TSysScanner }

  TSysScanner = class(TObject)
  private
    FList       : TStrList;
    procedure   Scan(const FileMask: ansistring; Recursive: boolean);
    function    GetCount: integer;
    function    GetItem(Index: longint): ansistring;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(const FileMask: ansistring);
    function    Find(const FileName: ansistring): longint;
    procedure   Delete(Index: longint);
    procedure   Clear;
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: ansistring read GetItem; default;
  end;

  { TNulStream }

  TNulStream = class(TStream)
  public
    function Read (var   Buffer; Count: Longint): longint; override;
    function Write(const Buffer; Count: Longint): longint; override;
  end;

  { Matching routines }

  function FileNameMatch(const FileName: ansistring; const FileMask: ansistring): boolean; overload;
  function FileNameMatch(const FileName: ansistring; FileMasks: TStrList): boolean; overload;

  { Check command line }

  function CheckOptions  (const ShortOpts, LongOpts: ansistring;  FileMasks: TStrList): ansistring;
  function HasOption     (const ShortOpt,  LongOpt:  ansistring): boolean;
  function GetOptionValue(const ShortOpt,  LongOpt:  ansistring): ansistring;

  { Priority routines }

  function SetNormalPriority: boolean;
  function SetIdlePriority: boolean;

implementation

uses
  {$IFDEF UNIX} BaseUnix; {$ENDIF}
  {$IFDEF MSWINDOWS} Windows; {$ENDIF}

{ TGenList class }

constructor TGenericList.Create(Compare: TGenericCompare);
begin
  inherited Create;
  FCompare := Compare;
  FList    := TList.Create;
end;

destructor TGenericList.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGenericList.Delete(Index: longint);
begin
  FList.Delete(Index);
end;

function TGenericList.Add(Item: TGenericItem): longint;
var
  L, M, H, I : longint;
begin
  Result := -1;
  if FList.Count > 0 then
  begin
    L := 0;
    H := FList.Count - 1;
    while H >= L do
    begin
      M := (L + H) div 2;
      I := FCompare(GetItem(M), Item);
      if I < 0 then
        L := M + 1
      else
        if I > 0 then
          H := M - 1
        else
          H := -2;
    end;

    if I < 0 then
    begin
      Result := M + 1;
      FList.Insert(Result, Item);
    end else
      if I > 0 then
      begin
        Result := M;
        FList.Insert(Result, Item);
      end;

  end else
    Result := FList.Add(Item);
end;

function TGenericList.Find(Item: TGenericItem): longint;
var
  L, M, H, I : longint;
begin
  Result := -1;
  if FList.Count > 0 then
  begin
    L := 0;
    H := FList.Count - 1;
    while H >= L do
    begin
      M := (L + H) div 2;
      I := FCompare(GetItem(M), Item);
      if I < 0 then
        L := M + 1
      else
        if I > 0 then
          H := M - 1
        else
          H := -2;
    end;

    if H = -2 then
      Result := M;
  end;
end;

procedure TGenericList.Clear;
begin
  while FList.Count > 0 do Delete(0);
end;

function TGenericList.GetItem(Index: longint): TGenericItem;
begin
  Result := TGenericItem(FList[Index]);
end;

function TGenericList.GetCount: longint;
begin
  Result := FList.Count;
end;

procedure TGenericList.SetCompare(Value: TGenericCompare);
begin
  if FList.Count = 0 then
    FCompare := Value;
end;

{ TStrList class }

function Compare(Item1, Item2: pchar): longint;
begin
  {$IFDEF UNIX}
    Result := strcomp(Item1, Item2);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := stricomp(Item1, Item2);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
end;

constructor TStrList.Create;
begin
  FList := TPCharList.Create(@Compare);
end;

destructor TStrList.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TStrList.Delete(Index: longint);
begin
  StrDispose(FList[Index]);
  FList.Delete(Index);
end;

procedure TStrList.Clear;
begin
  while FList.Count > 0 do Delete(0);
end;

procedure TStrList.Add(const S: ansistring);
var
  P : PChar;
begin
  P := StrAlloc(Length(S) + 1);
  StrPCopy(P, S);
  if FList.Add(P) = -1 then
  begin
    StrDispose(P);
  end;
end;

function TStrList.Find(const S: ansistring): longint;
var
  P : PChar;
begin
  P := StrAlloc(Length(S) + 1);
  StrPCopy(P, S);
  begin
    Result := FList.Find(P);
  end;
  StrDispose(P);
end;

function TStrList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TStrList.Get(Index: longint): ansistring;
begin
  Result := ansistring(FList[Index]);
end;

{ TSysScanner class }

constructor TSysScanner.Create;
begin
  inherited Create;
  FList := TStrList.Create;
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

procedure TSysScanner.Scan(const FileMask: ansistring; Recursive: boolean);
var
     Error : longint;
       Rec : TSearchRec;
  ScanMask : ansistring;
  ScanPath : ansistring;
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
        FList.Add(ScanPath + Rec.Name);
        if Recursive then
          if Rec.Attr and faSymLink = 0 then
            Scan(ScanPath + IncludeTrailingPathDelimiter(Rec.Name) + ScanMask, TRUE);
      end;
    end else
      if FileNameMatch(ScanPath + Rec.Name, FileMask) then
        FList.Add(ScanPath + Rec.Name);

    Error := FindNext(Rec);
  end;
  SysUtils.FindClose(Rec);
end;

procedure TSysScanner.Add(const FileMask: ansistring);
begin
  if FileMask = '' then Exit;

  if DirectoryExists(FileMask) then
    FList.Add(FileMask)
  else
    if FileExists(FileMask) then
      FList.Add(FileMask)
    else
      Scan(FileMask, TRUE);
end;

procedure TSysScanner.Delete(Index: longint);
begin
  FList.Delete(Index);
end;

function TSysScanner.Find(const FileName: ansistring): longint;
begin
  Result := FList.Find(FileName);
end;

function TSysScanner.GetItem(Index: longint): ansistring;
begin
  Result := FList[Index];
end;

function TSysScanner.GetCount: longint;
begin
  Result := FList.Count;
end;

{ TNulStream class }

function TNulStream.Read(var Buffer; Count: longint): longint;
begin
  Result := Count;
end;

function TNulStream.Write(const Buffer; Count: longint): longint;
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

function FileNameMatch(const FileName: ansistring; const FileMask: ansistring): boolean;
begin
   Result := MatchPattern(pchar(FileName), pchar(FileMask));
end;

function FileNameMatch(const FileName: ansistring; FileMasks: TStrList): boolean;
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

{ Check command line }

function CheckOptions(const ShortOpts, LongOpts: ansistring; FileMasks: TStrList): ansistring;
var
  I : longint = 1;
  S : ansistring;

  function CheckOption(const Options: string): ansistring;
  begin
    Result := '';
    if Pos(S, Options) <> 0 then
    begin
      if Options[Pos(S, Options) + Length(S)] = ':' then
      begin
        Inc(I);
        if (I > ParamCount) then
          Result := Format('Option at position %d does not allow an argument: "%s"', [I - 1, S])
        else
          if (Pos(ParamStr(I), ShortOpts) <> 0) or
             (Pos(ParamStr(I), LongOpts ) <> 0) then
            Result := Format('Option at position %d needs an argument : "%s"', [I - 1, S]);
      end else
        if Options[Pos(S, Options) + Length(S)] <> ' ' then
          Result := Format('Invalid option at position %d: "%s"', [I, S]);
    end else
      Result := Format('Invalid option at position %d: "%s"', [I, S]);
  end;

begin
  Result := '';
  while (I <= ParamCount) and (Result = '') do
  begin
    S := ParamStr(I);
    if S[1] = '-' then
    begin
      if Length(S) <= 2 then
        Result := CheckOption(ShortOpts)
      else
        if S[2] = '-' then
          Result := CheckOption(LongOpts)
        else
          Result := Format('Invalid option at position %d: "%s"', [I, S]);
    end else
      if Assigned(FileMasks) then
        FileMasks.Add(S);
    Inc(I);
  end;
end;

function HasOption(const ShortOpt, LongOpt: ansistring): boolean;
var
  I : longint = 1;
  S : ansistring;
begin
  Result := FALSE;
  while (I <= ParamCount) and (Result = FALSE) do
  begin
    S := ParamStr(I);
    if S = ShortOpt then
      Result := TRUE
    else
      if S = LongOpt then
        Result := TRUE;
    Inc(I);
  end;
end;

function GetOptionValue(const ShortOpt, LongOpt: ansistring): ansistring;
var
  I : longint = 1;
  S : ansistring;
begin
  Result := '';
  while (I <= ParamCount) and (Result = '') do
  begin
    S := ParamStr(I);
    if S = ShortOpt then
      Result := ParamStr(I + 1)
    else
      if S = LongOpt then
        Result := ParamStr(I + 1)   ;
    Inc(I);
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

