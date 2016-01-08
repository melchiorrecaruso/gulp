{
  Copyright (c) 2014-2016 Melchiorre Caruso.

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

    The journaling archiver library.

  Modified:

    v0.0.3 - 2016.01.07 by Melchiorre Caruso.
}

unit GulpLibrary;

{$mode objfpc}

interface

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF} Classes, GulpCommon, GulpFixes,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} Sha1, SysUtils;

type 
  // --- Gulp flags
  TGulpFlag = (gfADD,  gfDEL, gfLAST,  gfNAME, gfTIME,  gfSIZE, gfATTR, gfMODE,
               gfLINK, gfUID, gfUNAME, gfGID,  gfGNAME, gfCOMM);

  TGulpFlags = set of TGulpFlag;

  // --- Gulp item ---
  TGulpItem = class(TObject)
  private
    FFlags     : TGulpFlags;    // Flags
    FName      : ansistring;    // Path and name
    FTime      : TDateTime;     // Last modification date and time (UTC)
    FSize      : int64;         // Size in bytes
    FAttr      : longint;       // Attributes (MSWindows)
    FMode      : longint;       // Mode (Unix)
    FLink      : ansistring;    // Name of link
    FUserID    : longword;      // User ID
    FUserName  : ansistring;    // User Name
    FGroupID   : longword;      // Group ID
    FGroupName : ansistring;    // Group Name
    FComment   : ansistring;    // Comment
    FStartPos  : int64;         // Stream start position (reserved)
    FEndPos    : int64;         // Stream end position   (reserved)
    FVersion   : longword;      // Version               (reserved)
  public
    property Flags     : TGulpFlags    read FFlags;
    property Name      : ansistring    read FName;
    property Time      : TDateTime     read FTime;
    property Size      : int64         read FSize;
    property Attr      : longint       read FAttr;
    property Mode      : longint       read FMode;
    property Link      : ansistring    read FLink;
    property UserID    : longword      read FUserID;
    property UserName  : ansistring    read FUserName;
    property GroupID   : longword      read FGroupID;
    property GroupName : ansistring    read FGroupName;
    property Comment   : ansistring    read FComment;
    property Version   : longword      read FVersion;
  end;

  // --- The Gulp Application events ---
  TGulpShowItem    = procedure(const Item: TGulpItem) of object;
  TGulpShowMessage = procedure(const Message: ansistring) of object;
  TGulpTerminate   = function : boolean of object;

  // --- The Gulp Application class ---
  TGulpApplication = class(TObject)
  private
    FExclude       : TStrList;
    FInclude       : TStrList;
    FNodelete      : boolean;
    FUntilVersion  : longword;
    FOnShowItem    : TGulpShowItem;
    FOnShowMessage : TGulpShowMessage;
    procedure ShowItem(Item: TGulpItem);
    procedure ShowMessage(const Message: ansistring);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Sync    (const FileName: ansistring);
    procedure   Restore (const FileName: ansistring);
    procedure   Purge   (const FileName: ansistring);
    procedure   List    (const FileName: ansistring);
    procedure   Fix     (const FileName: ansistring);
    procedure   Check   (const FileName: ansistring);
    procedure   Reset;
  public
    property Exclude       : TStrList         read FExclude;
    property Include       : TStrList         read FInclude;
    property NoDelete      : boolean          read FNoDelete      write FNoDelete;
    property UntilVersion  : longword         read FUntilVersion  write FUntilVersion;
    property OnShowItem    : TGulpShowItem    read FOnShowItem    write FOnShowItem;
    property OnShowMessage : TGulpShowMessage read FOnShowMessage write FOnShowMessage;
  end;

// --- Some useful routines ---

function GetTime(const FileName: ansistring): TDateTime;  overload;
function GetTime(var   SR: TSearchRec      ): TDateTime;  overload;
function GetSize(const FileName: ansistring): int64;      overload;
function GetSize(var   SR: TSearchRec      ): int64;      overload;
function GetAttr(const FileName: ansistring): longint;    overload;
function GetAttr(var   SR: TSearchRec      ): longint;    overload;

function GetMode(const FileName: ansistring): longint;
function GetUID (const FileName: ansistring): longword;
function GetUNM (const FileName: ansistring): ansistring;
function GetGID (const FileName: ansistring): longword;
function GetGNM (const FileName: ansistring): ansistring;
function GetLink(const FileName: ansistring): ansistring;

function  VerToString(const Version: longword): ansistring;
function AttrToString(const Attr: longint    ): ansistring;
function SizeToString(const Size: int64      ): ansistring;
function TimeToString(const T: TDateTime     ): ansistring;
function ModeToString(const Mode: longint    ): ansistring;

function StringToAttr(const S: ansistring    ): longint;
function StringToMode(const S: ansistring    ): longint;
function FlagToString(const F: TGulpFlags    ): ansistring;

// =============================================================================
// IMPLEMENTATION
// =============================================================================

implementation

uses
  DateUtils, Math;

const
  GulpMarker003 : TSHA1Digest = (255, 210, 119, 9, 180, 210, 231, 123,
    25, 91, 110, 159, 243,  53, 246, 80, 215, 248, 114, 172);

  GulpDescription =
      'GULP v0.0.3 journaling archiver, copyright (c) 2014-2016 Melchiorre Caruso.' + LineEnding +
      'GULP archiver for user-level incremental backups with rollback capability.'  + LineEnding ;

type
  TGulpList = specialize TGenericList<TGulpItem>;

  // --- The Gulp Reader class ---
  TGulpReader = class(TObject)
  private
    FList       : TGulpList;
    FStream     : TStream;
    function    Read(Item: TGulpItem): TGulpItem;
    function    GetItem(Index: longint): TGulpItem;
    function    GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor  Destroy; override;
    procedure   Load(UntilVersion: longword);
    procedure   Extract(Index: longint; Stream: TStream); overload;
    procedure   Extract(Index: longint); overload;
    function    Find(const FileName: ansistring): longint;
    procedure   Clear;
  public
    property Items[Index: longint]: TGulpItem read GetItem; default;
    property Count: longint read GetCount;
  end;

  // --- The Gulp Writer class ---
  TGulpWriter = class(TObject)
  private
    FList       : TGulpList;
    FStream     : TStream;
    procedure   Write(Item: TGulpItem); overload;
    procedure   Write(Stream: TStream; Size: int64); overload;
    function    GetItem(Index: longint): TGulpItem;
    function    GetCount: longint;
  public
    constructor Create   (Stream: TStream);
    destructor  Destroy; override;
    procedure   Delete   (const FileName: ansistring);
    procedure   Add      (const FileName: ansistring);
    procedure   Clear;
  public
    property Items[Index: longint]: TGulpItem read GetItem; default;
    property Count: longint read GetCount;
  end;

// =============================================================================
// Internal rutines
// =============================================================================

function Clear(Item: TGulpItem): TGulpItem; inline;
begin
  Item.FFlags     := [];
  Item.FName      := '';
  Item.FTime      := 0.0;
  Item.FSize      := 0;
  Item.FAttr      := 0;
  Item.FMode      := 0;
  Item.FLink  := '';
  Item.FUserID    := 0;
  Item.FUserName  := '';
  Item.FGroupID   := 0;
  Item.FGroupName := '';
  Item.FComment   := '';
  Item.FStartPos  := 0;
  Item.FEndPos    := 0;
  Item.FVersion   := 0;
  Result := Item;
end;

function GetDigest(Item: TGulpItem): TSHA1Digest; inline;
var
  Context : TSHA1Context;
begin
  SHA1Init(Context);
                                 SHA1Update(Context,         Item.FFlags,       SizeOf(Item.FFlags    ));
  if gfNAME  in Item.FFlags then SHA1Update(Context, Pointer(Item.FName)^,      Length(Item.FName     ));
  if gfTIME  in Item.FFlags then SHA1Update(Context,         Item.FTime,        SizeOf(Item.FTime     ));
  if gfSIZE  in Item.FFlags then SHA1Update(Context,         Item.FSize,        SizeOf(Item.FSize     ));
  if gfATTR  in Item.FFlags then SHA1Update(Context,         Item.FAttr,        SizeOf(Item.FAttr     ));
  if gfMODE  in Item.FFlags then SHA1Update(Context,         Item.FMode,        SizeOf(Item.FMode     ));
  if gfLINK  in Item.FFlags then SHA1Update(Context, Pointer(Item.FLink)^,  Length(Item.FLink ));
  if gfUID   in Item.FFlags then SHA1Update(Context,         Item.FUserID,      SizeOf(Item.FUserID   ));
  if gfUNAME in Item.FFlags then SHA1Update(Context, Pointer(Item.FUserName)^,  Length(Item.FUserName ));
  if gfGID   in Item.FFlags then SHA1Update(Context,         Item.FGroupID,     SizeOf(Item.FGroupID  ));
  if gfGNAME in Item.FFlags then SHA1Update(Context, Pointer(Item.FGroupName)^, Length(Item.FGroupName));
  if gfCOMM  in Item.FFlags then SHA1Update(Context, Pointer(Item.FComment)^,   Length(Item.FComment  ));
  if gfSIZE  in Item.FFlags then SHA1Update(Context,         Item.FStartPos,    SizeOf(Item.FStartPos ));
  if gfSIZE  in Item.FFlags then SHA1Update(Context,         Item.FEndPos,      SizeOf(Item.FEndPos   ));
  SHA1Final(Context, Result);
end;

// =============================================================================
// Library routines
// =============================================================================

function GetTime(var SR: TSearchRec): TDateTime;
begin
  Result := GulpFixes.LocalTime2Universal(FileDateToDateTime(SR.Time));
end;

function GetTime(const FileName: ansistring): TDateTime;
var
  SR : TSearchRec;
begin
  Result := 0.0;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GetTime(SR);
  end;
  SysUtils.FindClose(SR);
end;

function GetSize(var SR: TSearchRec): int64;
begin
  Result := 0;
  if SR.Attr and (faDirectory or faVolumeId or faSymLink) = 0 then
  begin
    Result := SR.Size;
  end;
end;

function GetSize(const FileName: ansistring): int64;
var
  SR : TSearchRec;
begin
  Result := 0;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GetSize(SR);
  end;
  SysUtils.FindClose(SR);
end;

function GetAttr(var SR: TSearchRec): longint;
begin
  Result := SR.Attr;
end;

function GetAttr(const FileName: ansistring): longint;
var
  SR : TSearchRec;
begin
  Result := 0;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GetAttr(SR);
  end;
  SysUtils.FindClose(SR);
end;

function GetLink(const FileName: ansistring): ansistring;
begin
  Result := '';
  {$IFDEF UNIX}
  if GetAttr(FileName) and faSymLink = faSymLink then
    Result := fpReadLink(FileName);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function GetMode(const FileName: ansistring): longint;
{$IFDEF UNIX}
var
  Info : stat;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF UNIX}
  if fpLstat(FileName, Info) = 0 then
    Result := Info.st_mode
  else
    if fpstat(FileName, Info) = 0 then
      Result := Info.st_mode;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function GetUID(const FileName: ansistring): longword;
{$IFDEF UNIX}
var
  Info : stat;
{$ENDIF}
begin
  Result := $FFFFFFFF;
  {$IFDEF UNIX}
  if fpLstat(FileName, Info) = 0 then
    Result := Info.st_uid
  else
    if fpstat(FileName, Info) = 0 then
      Result := Info.st_uid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function GetUNM(const FileName: ansistring): ansistring;
begin
  Result := '';
end;

function GetGID(const FileName: ansistring): longword;
{$IFDEF UNIX}
var
  Info : stat;
{$ENDIF}
begin
  Result := $FFFFFFFF;
  {$IFDEF UNIX}
  if fpLstat(FileName, Info) = 0 then
    Result := Info.st_gid
  else
    if fpstat(FileName, Info) = 0 then
      Result := Info.st_gid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function GetGNM(const FileName: ansistring): ansistring;
begin
  Result := '';
end;

function VerToString(const Version: longword): ansistring;
begin
  Result := IntTostr(Version);
end;

function FlagToString(const F: TGulpFlags): ansistring;
begin
  if (gfAdd in F) and (gfDEL in F) then
    Result := 'UPD'
  else
    if (gfAdd in F) then
      Result := 'ADD'
    else
      if (gfDEL in F) then
        Result := 'DEL'
      else
        raise Exception.Create('Invalid marker value (ex0001)');
end;

function TimeToString(const T: TDateTime): ansistring;
begin
  Result := FormatDateTime(
    DefaultFormatSettings.LongDateFormat + ' ' +
    DefaultFormatSettings.LongTimeFormat, T);
end;

function SizeToString(const Size: int64): ansistring;
begin
  Result := Format('%u', [Size]);
end;

function AttrToString(const Attr: longint): ansistring;
begin
  Result := '       ';
  if Attr and faReadOnly  <> 0 then Result[1] := 'R';
  if Attr and faHidden    <> 0 then Result[2] := 'H';
  if Attr and faSysFile   <> 0 then Result[3] := 'S';
  if Attr and faVolumeId  <> 0 then Result[4] := 'V';
  if Attr and faDirectory <> 0 then Result[5] := 'D';
  if Attr and faArchive   <> 0 then Result[6] := 'A';
  if Attr and faSymLink   <> 0 then Result[7] := 'L';
end;

function StringToAttr(const S: ansistring): longint;
begin
  Result := 0;
  if Length(S) = 7 then
  begin
    if Upcase(S[1]) = 'R' then  Result := Result or faReadOnly;
    if Upcase(S[2]) = 'H' then  Result := Result or faHidden;
    if Upcase(S[3]) = 'S' then  Result := Result or faSysFile;
    if Upcase(S[4]) = 'V' then  Result := Result or faVolumeId;
    if Upcase(S[5]) = 'D' then  Result := Result or faDirectory;
    if Upcase(S[6]) = 'A' then  Result := Result or faArchive;
    if Upcase(S[7]) = 'L' then  Result := Result or faSymLink;
  end;
end;

function ModeToString(const Mode: longint): ansistring;
begin
  {$IFDEF UNIX}
    Result := OctStr(Mode, 3);
  {$ELSE}
  {$IFDEF MSWINDOWS}
    Result := '...';
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function StringToMode(const S: ansistring): longint;
{$IFDEF UNIX}
var
  I : longint;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF UNIX}
    for I  := 1 to Length(S) do
      Result := Result * 8 + StrToInt(Copy(S, I, 1));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

// =============================================================================
// TGulpReader
// =============================================================================

function Compare41(Item1, Item2: TGulpItem): longint; inline;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(Item1.FName, Item2.FName);
  {$ELSE}
  {$IFDEF MSWINDOWS}
    Result := AnsiCompareText(Item1.FName, Item2.FName);
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function Compare40(Item1, Item2: TGulpItem): longint; inline;
begin
  Result := Compare41(Item1, Item2);

  if Result = 0 then
  begin
    if Item1.Version < Item2.Version then
      Result := - 1
    else
      if Item1.Version > Item2.Version then
        Result := 1;
  end;

  if Result = 0 then
  begin
    if (gfDEL in Item1.FFlags) and
       (gfADD in Item2.FFlags) then
      Result := - 1
    else
      if (gfDEL in Item2.FFlags) and
         (gfADD in Item1.FFlags) then
        Result := 1;
  end;
end;

constructor TGulpReader.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FList   := TGulpList.Create(@Compare41);
end;

destructor TGulpReader.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpReader.Clear;
begin
  while FList.Count > 0 do
  begin
    FList.Items [0].Destroy;
    FList.Delete(0);
  end;
end;

function TGulpReader.Read(Item: TGulpItem): TGulpItem;
var
  Digest : TSHA1Digest;
begin
  Result := GulpLibrary.Clear(Item);
  if FStream.Read(Digest, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0002)');
  if SHA1Match(Digest, GulpMarker003) = FALSE then
    raise Exception.Create('Invalid marker value (ex0003)');

  FStream.Read(Item.FFlags, SizeOf(Item.FFlags));
  if gfNAME  in Item.Flags then Item.FName      := FStream.ReadAnsiString;
  if gfTIME  in Item.Flags then FStream.Read(Item.FTime,     SizeOf(Item.FTime    ));
  if gfSIZE  in Item.Flags then FStream.Read(Item.FSize,     SizeOf(Item.FSize    ));
  if gfATTR  in Item.Flags then FStream.Read(Item.FAttr,     SizeOf(Item.FAttr    ));
  if gfMODE  in Item.Flags then FStream.Read(Item.FMode,     SizeOf(Item.FMode    ));
  if gfLINK  in Item.Flags then Item.FLink  := FStream.ReadAnsiString;
  if gfUID   in Item.Flags then FStream.Read(Item.FUserID,   SizeOf(Item.FUserID  ));
  if gfUNAME in Item.Flags then Item.FUserName  := FStream.ReadAnsiString;
  if gfGID   in Item.Flags then FStream.Read(Item.FGroupID,  SizeOf(Item.FGroupID ));
  if gfGNAME in Item.Flags then Item.FGroupName := FStream.ReadAnsiString;
  if gfCOMM  in Item.Flags then Item.FComment   := FStream.ReadAnsiString;
  if gfSIZE  in Item.Flags then FStream.Read(Item.FStartPos, SizeOf(Item.FStartPos));
  if gfSIZE  in Item.Flags then FStream.Read(Item.FEndPos,   SizeOf(Item.FEndPos  ));

  if ((gfDEL in Item.FFlags) = FALSE) and ((gfADD in Item.FFlags) = FALSE) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0004)');
  if ((Item.FEndPos = 0) or (Item.FStartPos = 0)) and (Item.FSize <> 0) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0005)');
  if FStream.Read(Digest, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0006)');
  if SHA1Match(Digest, GetDigest(Item)) = FALSE then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0007)');

  DoDirSeparators(Item.FLink);
  DoDirSeparators(Item.FName);
end;

procedure TGulpReader.Load(UntilVersion: longword);
var
  I       : longint;
  Item    : TGulpItem;
  OffSet  : int64    = 0;
  Size    : int64    = 0;
  Version : longword = 1;
begin
  Clear;
  if 0 = UntilVersion then
    FList.Compare := @Compare40
  else
    FList.Compare := @Compare41;

  Item := TGulpItem.Create;
  try
    while TRUE do
    begin
      Read(Item).FVersion := Version;
      OffSet := Max(OffSet, Item.FEndPos);
      if gfLAST in Item.Flags then
      begin
        OffSet := Max(OffSet, FStream.Seek(0, soCurrent));
        if FStream.Seek(OffSet, soBeginning) <> OffSet then
          raise Exception.Create('Stream is not a valid archive (ex0008)');
        Size := OffSet;
        Inc(Version);
      end;

      if 0 = UntilVersion then
      begin
        if FList.Add(Item) = -1 then
          raise Exception.Create('Duplicates non allowed (ex0009)');
        Item := TGulpItem.Create;
      end else
        if Version <= UntilVersion then
        begin
          if gfDEL in Item.FFlags then
          begin
            I := FList.Find(Item);
            if I <> -1 then
            begin
              FList.Items [I].Destroy;
              FList.Delete(I);
            end;
          end;

          if gfADD in Item.FFlags then
          begin
            if FList.Add(Item) = -1 then
              raise Exception.Create('Duplicates non allowed (ex0010)');
            Item := TGulpItem.Create;
          end;
        end;
    end;

  except
  end;
  FreeAndNil(Item);

  if Size <> FStream.Seek(0, soEnd) then
    raise Exception.Create('Archive is broken, try with fix command (ex0011)');
end;

procedure TGulpReader.Extract(Index: longint; Stream: TStream);
var
  Buffer  : array[0..$FFFF] of byte;
  Context : TSHA1Context;
  Digest1 : TSHA1Digest;
  Digest2 : TSHA1Digest;
  Readed  : longint;
  Size    : int64;
begin
  FStream.Seek(Items[Index].FStartPos, soBeginning);
  Size := Items[Index].Size;
  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := FStream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0012)');
    SHA1Update  (Context, Buffer, Readed);
    Stream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest1);
  if FStream.Read(Digest2, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0013)');
  if SHA1Match(Digest1, Digest2) = FALSE then
    raise Exception.CreateFmt('Mismatched checksum for "%s"', [Items[Index].Name]);
end;

procedure TGulpReader.Extract(Index: longint);
var
  Check    : boolean;
  Item     : TGulpItem;
  ItemPath : ansistring;
  Stream   : TFileStream;
begin
  Item := Items[Index];
  if Item.Attr and (faSysFile or faVolumeId) = 0 then
  begin
    ItemPath := ExtractFileDir(Item.Name);
    if (ItemPath <> '') and (ForceDirectories(ItemPath) = FALSE) then
      raise Exception.CreateFmt('Unable to create path "%s"', [ItemPath]);

    Check := FALSE;
    if (Item.Attr and faSymLink) = faSymLink then
    begin
      {$IFDEF UNIX}
      Check := FpSymLink(pchar(Item.Link), pchar(Item.Name)) = 0;
      {$ELSE}
      {$IFDEF MSWINDOWS}
      {$ELSE}
        Unsupported platform...
      {$ENDIF}
      {$ENDIF}
    end else

      if (Item.Attr and faDirectory) = faDirectory then
      begin
        Check := DirectoryExists(Item.Name);
        if Check = FALSE then
          Check := CreateDir(Item.Name);
      end else

        if (gfSIZE in Item.Flags) then
        begin
          if FileExists(Item.Name) = FALSE then
            Stream := TFileStream.Create(Item.Name, fmCreate)
          else
            Stream := TFileStream.Create(Item.Name, fmOpenWrite);
          Extract(Index, Stream);
          FreeAndNil(Stream);
          Check := TRUE;
        end;

    if Check = TRUE then
    begin
      {$IFDEF UNIX}
      if (gfUID in Item.Flags) or (gfGID in Item.Flags) then
        if FpChown(Item.Name, Item.UserID, Item.GroupID) <> 0 then
          raise Exception.CreateFmt('Unable to set user/group id for "%s"', [Item.Name]);

      if gfMODE in Item.Flags then
        if FpChmod(Item.Name, Item.Mode) <> 0 then
          raise Exception.CreateFmt('Unable to set mode for "%s"', [Item.Name]);
      {$ELSE}
      {$IFDEF MSWINDOWS}
        if FileSetAttr(Item.Name, Item.Attr) <> 0 then
          raise Exception.CreateFmt('Unable to set attributes for "%s"', [Item.Name]);
      {$ELSE}
        Unsupported platform...
      {$ENDIF}
      {$ENDIF}
      if FileSetDate(Item.Name, DateTimeToFileDate(
        GulpFixes.UniversalTime2Local(Item.Time))) <> 0 then
          raise Exception.CreateFmt('Unable to set date for "%s"', [Item.Name]);
    end else
      raise Exception.CreateFmt('Unable to restore item "%s"', [Item.Name]);

  end;
end;

function  TGulpReader.Find(const FileName: ansistring): longint;
var
  Item : TGulpItem;
begin
  Item := GulpLibrary.Clear(TGulpItem.Create);
  Item.FFlags := [gfADD, gfNAME];
  Item.FName  := FileName;
  begin
    Result := FList.Find(Item);
  end;
  FreeAndNil(Item);
end;

function TGulpReader.GetItem(Index: longint): TGulpItem;
begin
  Result := FList.Items[Index];
end;

function TGulpReader.GetCount: longint;
begin
  Result := FList.Count;
end;

// =============================================================================
// TGulpWriter
// =============================================================================

function Compare42(Item1, Item2: TGulpItem): longint; inline;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(Item1.FName, Item2.FName);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := AnsiCompareText(Item1.FName, Item2.FName);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  if Result = 0 then
  begin
    if (gfDEL in Item1.FFlags) and
       (gfADD in Item2.FFlags) then
      Result := - 1
    else
      if (gfDEL in Item2.FFlags) and
         (gfADD in Item1.FFlags) then
        Result := 1;
  end;
end;

constructor TGulpWriter.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FList   := TGulpList.Create(@Compare42);
end;

destructor TGulpWriter.Destroy;
var
  I      : longint;
  Size   : int64;
  Source : TStream;
begin
  if FList.Count > 0 then
  begin
    Size  := FStream.Seek(0, soEnd);
    Include(FList[FList.Count - 1].FFlags, gfLAST);
    for I := 0 to FList.Count - 1 do Write(FList[I]);
    for I := 0 to FList.Count - 1 do
      if FList[I].Attr and (faSysFile or faVolumeId ) = 0 then
        if FList[I].Attr and (faSymLink or faDirectory) = 0 then
          if gfSIZE in FList[I].FFlags then
          begin
            FList[I].FStartPos := FStream.Seek(0, soCurrent);
            Source := TFileStream.Create(FList[I].FName, fmOpenRead or fmShareDenyNone);
            Write(Source, FList[I].FSize);
            FreeAndNil(Source);
            FList[I].FEndPos   := FStream.Seek(0, soCurrent);
          end;
    FStream.Seek(Size , soBeginning);
    for I := 0 to FList.Count - 1 do Write(FList[I]);
    FStream.Seek(0 , soEnd);
    Clear;
  end;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpWriter.Clear;
begin
  while FList.Count > 0 do
  begin
    FList.Items [0].Destroy;
    FList.Delete(0);
  end;
end;

procedure TGulpWriter.Add(const FileName: ansistring);
var
  Item : TGulpItem;
  SR   : TSearchRec;
begin
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
    if GetAttr(SR) and (faSysFile or faVolumeId) = 0 then
    begin
      Item          := GulpLibrary.Clear(TGulpItem.Create);
      Item.FName    := FileName;
      Item.FTime    := GetTime(SR);
      Item.FSize    := GetSize(SR);
      Item.FAttr    := GetAttr(SR);
      Include(Item.FFlags, gfNAME);
      Include(Item.FFlags, gfTIME);
      if Item.Attr and faSymLink = 0 then
        if Item.Attr and faDirectory = 0 then
          Include(Item.FFlags, gfSIZE);
      Include(Item.FFlags, gfATTR);
      {$IFDEF UNIX}
      Item.FMode    := GetMode(FileName);
      Item.FLink    := GetLink(FileName);
      Item.FUserID  := GetUID (FileName);
      Item.FGroupID := GetGID (FileName);
      Include(Item.FFLags, gfMODE);
      Include(Item.FFLags, gfLINK);
      Include(Item.FFLags, gfUID );
      Include(Item.FFLags, gfGID );
      {$ELSE}
      {$IFDEF MSWINDOWS}
      {$ELSE}
        Unsupported platform...
      {$ENDIF}
      {$ENDIF}
      Include(Item.FFlags, gfADD);
      if FList.Add(Item) = -1 then
        raise Exception.Create('Duplicates non allowed (ex0014)');
    end;
  SysUtils.FindClose(SR);
end;

procedure TGulpWriter.Delete(const FileName: ansistring);
var
  Item : TGulpItem;
begin
  Item        := GulpLibrary.Clear(TGulpItem.Create);
  Item.FFlags := [gfDEL, gfNAME];
  Item.FName  := FileName;
  if FList.Add(Item) = -1 then
    raise Exception.Create('Duplicates non allowed (ex0015)');
end;

procedure TGulpWriter.Write(Stream: TStream; Size: int64);
var
  Buffer  : array[0..$FFFF] of byte;
  Context : TSHA1Context;
  Digest  : TSHA1Digest;
  Readed  : longint;
begin
  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := Stream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0016)');
    SHA1Update   (Context, Buffer, Readed);
    FStream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest);
  FStream.Write(Digest, SizeOf(TSHA1Digest));
end;

procedure TGulpWriter.Write(Item: TGulpItem);
begin
  FStream.Write(GulpMarker003,  SizeOf(GulpMarker003));
  FStream.Write(Item.FFlags, SizeOf(Item.FFlags));
  if gfNAME  in Item.FFlags then FStream.WriteAnsiString(Item.FName);
  if gfTIME  in Item.FFlags then FStream.Write(Item.FTime, SizeOf(Item.FTime));
  if gfSIZE  in Item.FFlags then FStream.Write(Item.FSize, SizeOf(Item.FSize));
  if gfATTR  in Item.FFlags then FStream.Write(Item.FAttr, SizeOf(Item.FAttr));
  if gfMODE  in Item.FFlags then FStream.Write(Item.FMode, SizeOf(Item.FMode));
  if gfLINK  in Item.FFlags then FStream.WriteAnsiString(Item.FLink);
  if gfUID   in Item.FFlags then FStream.Write(Item.FUserID, SizeOf(Item.FUserID));
  if gfUNAME in Item.FFlags then FStream.WriteAnsiString(Item.FUserName);
  if gfGID   in Item.FFlags then FStream.Write(Item.FGroupID, SizeOf(Item.FGroupID));
  if gfGNAME in Item.FFlags then FStream.WriteAnsiString(Item.FGroupName);
  if gfCOMM  in Item.FFlags then FStream.WriteAnsiString(Item.FComment);
  if gfSIZE  in Item.FFlags then FStream.Write(Item.FStartPos, SizeOf(Item.FStartPos));
  if gfSIZE  in Item.FFlags then FStream.Write(Item.FEndPos,   SizeOf(Item.FEndPos));
  FStream.Write(GetDigest(Item), SizeOf(TSHA1Digest));
end;

function TGulpWriter.GetItem(Index: longint): TGulpItem;
begin
  Result := FList.Items[Index];
end;

function TGulpWriter.GetCount: longint;
begin
  Result := FList.Count;
end;

// =============================================================================
// TGulpApplication
// =============================================================================

constructor TGulpApplication.Create;
begin
  inherited Create;
  FOnShowItem    := nil;
  FOnShowMessage := nil;
  FExclude       := TStrList.Create;
  FInclude       := TStrList.Create;
  FNodelete      := FALSE;
  FUntilVersion  := $FFFFFFFF;
end;

destructor TGulpApplication.Destroy;
begin
  FreeAndNil(FExclude);
  FreeAndNil(FInclude);
  inherited Destroy;
end;

procedure TGulpApplication.Reset;
begin
  FExclude.Clear;
  FInclude.Clear;
  FNodelete     := FALSE;
  FUntilVersion := $FFFFFFFF;
end;

procedure TGulpApplication.ShowItem(Item: TGulpItem);
begin
  if Assigned(FOnShowItem) then
    FOnShowItem(Item);
end;

procedure TGulpApplication.ShowMessage(const Message: ansistring);
begin
  if Assigned(FOnShowMessage) then
    FOnShowMessage(Message);
end;

procedure TGulpApplication.Sync(const FileName: ansistring);
var
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Sync the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...      ', [#13]));
  if FileExists(FileName) then
    Stream := TFileStream.Create(FileName, fmOpenReadWrite)
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  LibReader:= TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  Size := Stream.Seek(0, soEnd);

  ShowMessage(Format('%sScanning filesystem...   ', [#13]));
  Scan  := TSysScanner.Create;
  for I := FInclude.Count - 1 downto 0 do
    if DirectoryExists(FInclude[I]) = TRUE then
      FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*');

  if FInclude.Count = 0 then
    FInclude.Add('*');
  for I := FInclude.Count - 1 downto 0 do
    Scan.Add(FInclude[I]);

  for I := FExclude.Count - 1 downto 0 do
    if DirectoryExists(FExclude[I]) = TRUE then
      FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*');

  FExclude.Add(FileName);
  for I := Scan.Count - 1 downto 0 do
    if FileNameMatch(Scan[I], FExclude) = TRUE then
      Scan.Delete(I);

  ShowMessage(Format('%sSyncing items...         ', [#13]));
  LibWriter := TGulpWriter.Create(Stream);
  if FNoDelete = FALSE then
    for I := 0 to LibReader.Count - 1 do
      if Scan.Find(LibReader[I].Name) = -1 then
        LibWriter.Delete(LibReader[I].Name);

  for I := 0 to Scan.Count - 1 do
  begin
    J := LibReader.Find(Scan[I]);
    if J = -1 then
      LibWriter.Add(Scan[I])
    else
      if GetTime(Scan[I]) <> LibReader[J].Time then
      begin
        LibWriter.Delete(Scan[I]);
        LibWriter.Add   (Scan[I]);
      end;
  end;
  FreeAndNil(LibWriter);
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeandNil(Scan);

  ShowMessage(Format('%sFinished (%u added bytes) %s',
    [#13, GetSize(FileName) - Size, LineEnding]));
end;

procedure TGulpApplication.Restore(const FileName: ansistring);
var
  LibReader : TGulpReader;
     LibRec : TGulpItem;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64 = 0;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Restore the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...         ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  ShowMessage(Format('%sScanning filesystem...      ', [#13]));
  Scan := TSysScanner.Create;
  Scan.Add('*');
  for I := FInclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FInclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
  end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FExclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
  end;
  FExclude.Add(FileName);

  ShowMessage(Format('%sRestoring items...          ', [#13]));
  if FNoDelete = FALSE then
    for I := Scan.Count - 1 downto 0 do
    begin
      J := LibReader.Find(Scan[I]);
      if (J = -1) or
         (FileNameMatch(LibReader[J].Name, FInclude) = FALSE) or
         (FileNameMatch(LibReader[J].Name, FExclude) = TRUE ) then
      begin
        if DirectoryExists(Scan[I]) = TRUE then
          RemoveDir(Scan[I])
        else
          DeleteFile(Scan[I]);
      end;
    end;

  for I := LibReader.Count - 1 downto 0 do
  begin
    LibRec := LibReader[I];
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
      begin
        J := Scan.Find(LibRec.Name);
        if J = -1 then
        begin
          LibReader.Extract(I);
          Inc(Size, LibRec.Size);
        end else
          if GetTime(Scan[J]) <> LibRec.Time then
          begin
            LibReader.Extract(I);
            Inc(Size, LibRec.Size);
          end;
      end;
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Scan);

  ShowMessage(Format('%sFinished (%u extracted bytes) %s',
    [#13, Size, LineEnding]));
end;

procedure TGulpApplication.Check(const FileName: ansistring);
var
  LibReader : TGulpReader;
          I : longint;
        Nul : TStream;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Check the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...       ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(0);

  ShowMessage(Format('%sChecking items...         ', [#13]));
  Nul   := TNulStream.Create;
  for I := 0 to LibReader.Count - 1 do
  begin
    if LibReader[I].Size > 0 then
      LibReader.Extract(I, Nul);
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Nul);

  ShowMessage(Format('%sFinished (%u checked bytes) %s',
    [#13, GetSize(FileName), LineEnding]));
end;

procedure TGulpApplication.Fix(const FileName: ansistring);
var
  LibReader : TGulpReader;
     LibRec : TGulpItem;
     OffSet : int64 = 0;
       Size : int64 = 0;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Fix the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sFixing items...         ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenReadWrite);
  LibReader := TGulpReader.Create(Stream);
  LibRec    := TGulpItem.Create;

  try
    while TRUE do
    begin
      LibReader.Read(LibRec);
      OffSet := Max(OffSet, LibRec.FEndPos);
      if gfLAST in LibRec.Flags then
      begin
        OffSet := Max(OffSet, Stream.Seek(0, soCurrent));
        if Stream.Seek(OffSet, soBeginning) <> OffSet then
          raise Exception.Create('Stream is not a valid archive (ex0017)');
        Size := OffSet;
      end;
    end;
  except
    // nothing to do
  end;

  OffSet := Stream.Size - Size;
  if Size > 0 then
    Stream.Size := Size
  else
    raise Exception.Create('Stream is not a valid archive (ex0018)');
  FreeAndNil(LibReader);
  FreeAndNil(LibRec);
  FreeAndNil(Stream);

  ShowMessage(Format('%sFinished (%u removed bytes) %s',
    [#13, OffSet, LineEnding]));
end;

procedure TGulpApplication.Purge(const FileName: ansistring);
var
    LibItem : TGulpItem;
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
          I : longint;
       Size : int64 = 0;
     Stream : TStream;
        Tmp : TStream;
    TmpName : ansistring;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Purge the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...       ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  ShowMessage(Format('%sMoving items...           ', [#13]));
  TmpName   := GetTempFileName(ExtractFileDir(FileName), '');
  Tmp       := TFileStream.Create(TmpName, fmCreate);
  LibWriter := TGulpWriter.Create(Tmp);

  if LibReader.Count > 0 then
  begin
    for I := 0 to LibReader.Count - 1 do LibWriter.Write(LibReader[I]);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibItem := LibReader[I];
      if LibItem.FSize > 0 then
      begin
        Stream.Seek(LibItem.FStartPos, soBeginning);
        Size := LibItem.FEndPos - LibItem.FStartPos;
        LibItem.FStartPos := Tmp.Seek(0, soCurrent);
        Tmp.CopyFrom(Stream, Size);
        LibItem.FEndPos   := Tmp.Seek(0, soCurrent);
      end;
    end;

    Tmp.Seek(0, soBeginning);
    for I := 0 to LibReader.Count -1 do
    begin
      LibItem := LibReader[I];
      if I = LibReader.Count - 1 then
        System.Include(LibItem.FFlags, gfLAST)
      else
        System.Exclude(LibItem.FFlags, gfLAST);
      LibWriter.Write(LibItem);
    end;
    Tmp.Seek(0, soEnd);
  end;

  Size := Stream.Size - Tmp.Size;
  FreeAndNil(LibReader);
  FreeAndNil(LibWriter);
  FreeAndNil(Stream);
  FreeAndNil(Tmp);

  if DeleteFile(FileName) = FALSE then
    raise Exception.CreateFmt('Unable to delete file "%s"', [FileName])
  else
    if RenameFile(TmpName, FileName)= FALSE then
      raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);

  ShowMessage(Format('%sFinished (%u removed bytes) %s',
    [#13, Size, LineEnding]));
end;

procedure TGulpApplication.List(const FileName: ansistring);
var
      Count : longint = 0;
  LibReader : TGulpReader;
     LibRec : TGulpItem;
       I, J : longint;
     Stream : TStream;
    Version : longword = 0;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('List the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...      ', [#13]));
  Stream    := TFileStream.Create (FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  for I := FInclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FInclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
  end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FExclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
  end;

  ShowMessage(Format('%sListing items...       %s', [#13, LineEnding]));
  for I := 0 to LibReader.Count - 1 do
  begin
    LibRec  := LibReader[I];
    Version := Max(Version, LibRec.Version);
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
      begin
        ShowItem(LibRec);
        Inc(Count);
      end;
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);

  ShowMessage(Format('Finished (%u listed items) %s', [Count, LineEnding]));
  ShowMessage(Format('Lastest version %u %s', [Version, LineEnding]));
end;

end.
