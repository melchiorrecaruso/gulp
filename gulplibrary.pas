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

    The journaling archiver library.

  Modified:

    v0.0.2 - 2015.07.12 by Melchiorre Caruso.
}

unit GulpLibrary;

interface

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Classes, GulpCommon, Sha1, SysUtils, Dialogs;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..7] of char;

  // --- Gulp Flags ---
  TGulpFlag = (gfLast,         gfAdd,      gfUnused,
               gfName,         gfTime,     gfAttributes,
               gfMode,         gfSize,     gfLinkName,
               gfUserID,       gfUserName, gfGroupID,
               gfGroupName,    gfOffSet,   gfStoredSize,
               gfStoredDigest, gfComment,  gfPlatform);

  TGulpFlags = set of TGulpFlag;

  // --- Gulp Storage Flags ---
  TGulpStorageFlag = (gsfFASTEST,
                      gsfDEFAULT,
                      gsfMAX,
                      gsfGZ);

  TGulpStorageFlags = set of TGulpStorageFlag;

  // --- Gulp Platforms ---
  TGulpPlatform = (gpUNIX,
                   gpMSWINDOWS,
                   gpMAC);

  // --- Gulp Item class ---
  TGulpItem = class(TObject)
  private
    FFlags        : TGulpFlags;    // Flags
    FVersion      : longword;      // File version       (reserved)
    FName         : string;        // File path and name
    FTime         : TDateTime;     // Last modification date and time (UTC)
    FAttributes   : longint;       // File Attributes (MSWindows)
    FMode         : longint;       // File Mode (Unix)
    FSize         : int64;         // File size in bytes
    FLinkName     : string;        // Name of linked file
    FUserID       : longword;      // User ID
    FUserName     : string;        // User Name
    FGroupID      : longword;      // Group ID
    FGroupName    : string;        // Group Name
    FOffSet       : int64;         // Data offset        (reserved)
    FStoredSize   : int64;         // Stored data size   (reserved)
    FStoredDigest : string;        // Stored data digest (reserved)
    FComment      : string;        // Comment
    FPlatform     : TGulpPlatform; // Platform
    FPosition     : longword;      // Position           (reserved)
  public
    property Flags      : TGulpFlags    read FFlags;
    property Version    : longword      read FVersion;
    property Name       : string        read FName;
    property Time       : TDateTime     read FTime;
    property Attributes : longint       read FAttributes;
    property Mode       : longint       read FMode;
    property Size       : int64         read FSize;
    property LinkName   : string        read FLinkName;
    property UserID     : longword      read FUserID;
    property UserName   : string        read FUserName;
    property GroupID    : longword      read FGroupID;
    property GroupName  : string        read FGroupName;
    property Comment    : string        read FComment;
    property Platform   : TGulpPlatform read FPlatform;
    property Position   : longword      read FPosition;
  end;

  // --- The Gulp Application events ---
  TGulpOnMessage = procedure(const Message: string) of object;
  TGulpOnList    = procedure(const Item: TGulpItem) of object;

  // --- The Gulp Application class ---
  TGulpApplication = class(TObject)
  private
    FExclude      : TStringList;
    FInclude      : TStringList;
    FNodelete     : boolean;
    FStorageFlags : TGulpStorageFlags;
    FUntilVersion : longword;
    FOnList       : TGulpOnList;
    FOnMessage    : TGulpOnMessage;
    procedure DoList(var Item: TGulpItem);
    procedure DoMessage(const Message: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Sync     (const FileName: string);
    procedure   Restore  (const FileName: string);
    procedure   Purge    (const FileName: string);
    procedure   List     (const FileName: string);
    procedure   Fix      (const FileName: string);
    procedure   Check    (const FileName: string);
    procedure   Reset;
  public
    property Exclude      : TStringList       read FExclude;
    property Include      : TStringList       read FInclude;
    property NoDelete     : boolean           read FNoDelete     write FNoDelete;
    property StorageFlags : TGulpStorageFlags read FStorageFlags write FStorageFlags;
    property UntilVersion : longword          read FUntilVersion write FUntilVersion;
    property OnList       : TGulpOnList       read FOnList       write FOnList;
    property OnMessage    : TGulpOnMessage    read FOnMessage    write FOnMessage;
  end;

// --- Some useful routines ---

function GetTime(const Time: TDateTime ): TDateTime; overload;
function GetTime(const FileName: string): TDateTime; overload;
function GetTime(var   SR: TSearchRec  ): TDateTime; overload;
function GetSize(const FileName: string): int64;     overload;
function GetSize(var   SR: TSearchRec  ): int64;     overload;
function GetAttr(const FileName: string): longint;   overload;
function GetAttr(var   SR: TSearchRec  ): longint;   overload;

{$IFDEF UNIX}
function GetMode(const FileName: string): longint;   overload;
function GetMode(var   Info: stat      ): longint;   overload;
function GetUID (const FileName: string): longword;  overload;
function GetUID (var   Info: stat      ): longword;  overload;
function GetGID (const FileName: string): longword;  overload;
function GetGID (var   Info: stat      ): longword;  overload;
{$ENDIF}

function  VerToString(const Version: longword): string;
function FlagToString(const Flags: TGulpFlags): string;
function AttrToString(const Attr: longint    ): string;
function SizeToString(const Size: int64      ): string;
function TimeToString(const T: TDateTime     ): string;
function ModeToString(const Mode: longint    ): string;

function StringToAttr(const S: string        ): longint;
function StringToMode(const S: string        ): longint;
function PlatToString(const P: TGulpPlatform ): string;

// =============================================================================
// IMPLEMENTATION
// =============================================================================

implementation

uses
  DateUtils, Math, ZStream;

const
  GulpMarker : TGulpMarker = ('G','U','L','P',char(0),char(0),char(2),char(0));
  GulpDescription =
      'GULP v0.0.2 journaling archiver, copyright (c) 2014-2015 Melchiorre Caruso.' + LineEnding +
      'GULP archiver for user-level incremental backups with rollback capability.'  + LineEnding ;

type
  // --- The Gulp Reader class ---
  TGulpReader = class(TObject)
  private
    FList   : TList;
    FStream : TStream;
    procedure Add        (Item: TGulpItem);
    procedure Delete     (Index: longint);
    function  Find       (const FileName: string): longint;

    function  ReadOffSet (var OffSet: int64): boolean;
    function  ReadItem   (Item: TGulpItem): boolean;
    function  ReadStream (Stream: TStream; Size: int64): string;
    procedure ExtractTo  (Stream: TStream; Index: longint);
    procedure Extract    (Index: longint);
    function  GetItem    (Index: longint): TGulpItem;
    function  GetCount: longint;
  public
    constructor Create   (Stream: TStream);
    destructor  Destroy; override;
    procedure   Load     (Version: longword);
    procedure   Clear;
  public
    property Items[Index: longint]: TGulpItem read GetItem;
    property Count: longint read GetCount;
  end;

  // --- The Gulp Writer class ---
  TGulpWriter = class(TObject)
  private
    FList         : TList;
    FStream       : TStream;
    FStreamSize   : int64;
    FStorageFlags : TGulpStorageFlags;
    function WriteOffSet (const OffSet: int64): boolean;
    function WriteItem   (Item: TGulpItem): boolean;
    function WriteStream (Stream: TStream; Size: int64): string;
    function GetItem     (Index: longint): TGulpItem;
    function GetCount: longint;
    function GetNew: TGulpItem;
  public
    constructor Create   (Stream: TStream);
    destructor  Destroy; override;
    procedure   Delete   (const FileName: string);
    procedure   Add      (const FileName: string);
  public
    property StorageFlags : TGulpStorageFlags read FStorageFlags write FStorageFlags;
    property Items[Index: longint]: TGulpItem read GetItem;
    property Count: longint read GetCount;
  end;

// =============================================================================
// Internal rutines
// =============================================================================

function ClearItem(Item: TGulpItem): TGulpItem; inline;
begin
  Result               := Item;
  Result.FFlags        := [gfPlatform];
  Result.FVersion      := 0;
  Result.FName         := '';
  Result.FTime         := 0.0;
  Result.FAttributes   := 0;
  Result.FMode         := 0;
  Result.FSize         := 0;
  Result.FLinkName     := '';
  Result.FUserID       := 0;
  Result.FUserName     := '';
  Result.FGroupID      := 0;
  Result.FGroupName    := '';
  Result.FOffSet       := 0;
  Result.FStoredSize   := 0;
  Result.FStoredDigest := '';
  Result.FComment      := '';
  {$IFDEF UNIX}
    Result.FPlatform   := gpUNIX;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result.FPlatform := gpMSWINDOWS;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
end;

function CompareItemName(I1, I2: TGulpItem): longint; inline;
begin
  Result := AnsiCompareFileName(I1.FName, I2.FName);
  if Result = 0 then
  begin
    if I1.FPosition < I2.FPosition then
      Result := - 1
    else
      if I1.FPosition > I2.FPosition then
        Result := 1;
  end;
end;

function GetItemDigest(Item: TGulpItem): string; inline;
var
  Context : TSHA1Context;
   Digest : TSHA1Digest;
begin
  SHA1Init  (Context);
  SHA1Update(Context,         Item.FFlags,          SizeOf(Item.FFlags       ));
  SHA1Update(Context, Pointer(Item.FName)^,         Length(Item.FName        ));
  SHA1Update(Context,         Item.FTime,           SizeOf(Item.FTime        ));
  SHA1Update(Context,         Item.FAttributes,     SizeOf(Item.FAttributes  ));
  SHA1Update(Context,         Item.FMode,           SizeOf(Item.FMode        ));
  SHA1Update(Context,         Item.FSize,           SizeOf(Item.FSize        ));
  SHA1Update(Context, Pointer(Item.FLinkName)^,     Length(Item.FLinkName    ));
  SHA1Update(Context,         Item.FUserID,         SizeOf(Item.FUserID      ));
  SHA1Update(Context, Pointer(Item.FUserName)^,     Length(Item.FUserName    ));
  SHA1Update(Context,         Item.FGroupID,        SizeOf(Item.FGroupID     ));
  SHA1Update(Context, Pointer(Item.FGroupName)^,    Length(Item.FGroupName   ));
  SHA1Update(Context,         Item.FOffSet,         SizeOf(Item.FOffSet      ));
  SHA1Update(Context,         Item.FStoredSize,     SizeOf(Item.FStoredSize  ));
  SHA1Update(Context, Pointer(Item.FStoredDigest)^, Length(Item.FStoredDigest));
  SHA1Update(Context, Pointer(Item.FComment)^,      Length(Item.FComment     ));
  SHA1Update(Context,         Item.FPlatform,       SizeOf(Item.FPlatform    ));
  SHA1Final (Context, Digest);
  Result := SHA1Print(Digest);
end;

// =============================================================================
// Library routines
// =============================================================================

function GetTime(const Time: TDateTime): TDateTime;
begin
  Result := LocalTimeToUniversal(Time);
end;

function GetTime(var SR: TSearchRec): TDateTime;
begin
  Result := GetTime(FileDateToDateTime(SR.Time));
end;

function GetTime(const FileName: string): TDateTime;
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

function GetSize(const FileName: string): int64;
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

function GetAttr(const FileName: string): longint;
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

{$IFDEF UNIX}
function GetMode(const FileName: string): longint;
var
  Info : stat;
begin
  Result := 0;
  if fpLstat(FileName, Info) = 0 then
    Result := GetMode(Info)
  else
    if fpstat(FileName, Info) = 0 then
      Result := GetMode(Info);
end;

function GetMode(var Info: stat): longint;
begin
  Result := Info.st_mode;
end;

function GetUID(const FileName: string): longword;
var
  Info : stat;
begin
  Result := $FFFFFFFF;
  if fpLstat(FileName, Info) = 0 then
    Result := GetUID(Info)
  else
    if fpstat(FileName, Info) = 0 then
      Result := GetUID(Info);
end;

function GetUID(var Info: stat): longword;
begin
  Result := Info.st_uid;
end;

function GetGID(const FileName: string): longword;
var
  Info : stat;
begin
  Result := $FFFFFFFF;
  if fpLstat(FileName, Info) = 0 then
    Result := GetGID(Info)
  else
    if fpstat(FileName, Info) = 0 then
      Result := GetGID(Info);
end;

function GetGID(var Info: stat): longword;
begin
  Result := Info.st_gid;
end;
{$ENDIF}

function VerToString(const Version: longword): string;
begin
  Result := IntTostr(Version);
end;

function FlagToString(const Flags: TGulpFlags): string;
begin
  if gfAdd in Flags then
    Result := 'ADD'
  else
    Result := 'DEL';
end;

function TimeToString(const T: TDateTime): string;
begin
  Result := FormatDateTime(
    DefaultFormatSettings.LongDateFormat + ' ' +
    DefaultFormatSettings.LongTimeFormat, T);
end;

function SizeToString(const Size: int64): string;
begin
  Result := Format('%u', [Size]);
end;

function AttrToString(const Attr: longint): string;
begin
  Result := '.......';
  if Attr and faReadOnly  <> 0 then Result[1] := 'R';
  if Attr and faHidden    <> 0 then Result[2] := 'H';
  if Attr and faSysFile   <> 0 then Result[3] := 'S';
  if Attr and faVolumeId  <> 0 then Result[4] := 'V';
  if Attr and faDirectory <> 0 then Result[5] := 'D';
  if Attr and faArchive   <> 0 then Result[6] := 'A';
  if Attr and faSymLink   <> 0 then Result[7] := 'L';
end;

function StringToAttr(const S: string  ): longint;
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

function ModeToString(const Mode: longint): string;
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

function StringToMode(const S: string): longint;
{$IFDEF UNIX}
var
  I : longint;
{$ENDIF}
begin
  {$IFDEF UNIX}
    Result := 0;
    for I  := 1 to Length(S) do
      Result := Result * 8 + StrToInt(Copy(S, I, 1));
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := 0;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
end;

function PlatToString(const P: TGulpPlatform): string;
begin
  case P of
    gpUNIX      : Result := 'UNIX';
    gpMSWINDOWS : Result := 'WIN';
    gpMAC       : Result := 'MAC';
  else            Result := ''
  end;
end;

// =============================================================================
// TGulpReader
// =============================================================================

constructor TGulpReader.Create(Stream: TStream);
begin
  inherited Create;
  FList   := TList.Create;
  FStream := Stream;
end;

destructor TGulpReader.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TGulpReader.Clear;
begin
  while FList.Count <> 0 do
  begin
    Delete(0);
  end;
end;

procedure TGulpReader.Delete(Index: longint);
begin
  if Assigned(FList[Index]) then
    TGulpItem(FList[Index]).Destroy;
  FList.Delete(Index);
end;

procedure TGulpReader.Add(Item: TGulpItem);
var
  L, M, H, I : longint;
begin
  L := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := CompareItemName(Items[M], Item);
    if I < 0 then
      L := M + 1
    else
      if I > 0 then
        H := M - 1
      else
        H := -2;
  end;

  if FList.Count <> 0 then
  begin
    if I < 0 then
      FList.Insert(M + 1, Item)
    else
      if I > 0 then
        FList.Insert(M, Item)
      else
        raise Exception.Create('Duplicates not allowed (ex0006)');
  end else
    FList.Add(Item);
end;

function TGulpReader.Find(const FileName: string): longint;
var
  L, M, H, I : longint;
begin
  L := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := AnsiCompareFileName(Items[M].Name, FileName);
    if I < 0 then
      L := M + 1
    else
      if I > 0 then
        H := M - 1
      else
        H := -2;
  end;

  if H = -2 then
    Result  := M
  else
    Result := -1;
end;

function TGulpReader.ReadStream(Stream: TStream; Size: int64): string;
var
   Buffer : array[0..$FFFF] of byte;
  Context : TSHA1Context;
   Digest : TSHA1Digest;
    Flags : TGulpStorageFlags = [];
   Readed : longint;
  ZStream : TStream;
begin
  FStream.Read(Flags, SizeOf(TGulpStorageFlags));
  case gsfGZ in Flags of
    FALSE: ZStream := FStream;
    TRUE : ZStream := TDecompressionStream.Create(FStream, FALSE);
  end;

  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := ZStream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0005)');
    SHA1Update  (Context, Buffer, Readed);
    Stream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest);
  case gsfGZ in Flags of
    FALSE: ZStream := nil;
    TRUE : FreeAndNil(ZStream);
  end;
  Result := SHA1Print(Digest);
end;

function TGulpReader.ReadItem(Item: TGulpItem): boolean;
var
  Marker : TGulpMarker;
begin
  FillChar    (Marker, SizeOf(Marker), 0);
  FStream.Read(Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    ClearItem(Item);
    FStream.Read(Item.FFlags, SizeOf(Item.FFlags));

    if gfName         in Item.Flags then Item.FName := FStream.ReadAnsiString;

    if gfTime         in Item.Flags then FStream.Read(Item.FTime, SizeOf(Item.FTime));
    if gfAttributes   in Item.Flags then FStream.Read(Item.FAttributes, SizeOf(Item.FAttributes));
    if gfMode         in Item.Flags then FStream.Read(Item.FMode, SizeOf(Item.FMode));
    if gfSize         in Item.Flags then FStream.Read(Item.FSize, SizeOf(Item.FSize));

    if gfLinkName     in Item.Flags then Item.FLinkName := FStream.ReadAnsiString;

    if gfUserID       in Item.Flags then FStream.Read(Item.FUserID, SizeOf(Item.FUserID));
    if gfUserName     in Item.Flags then Item.FUserName := FStream.ReadAnsiString;

    if gfGroupID      in Item.Flags then FStream.Read(Item.FGroupID, SizeOf(Item.FGroupID));
    if gfGroupName    in Item.Flags then Item.FGroupName := FStream.ReadAnsiString;

    if gfOffSet       in Item.Flags then FStream.Read(Item.FOffSet, SizeOf(Item.FOffSet));
    if gfStoredSize   in Item.Flags then FStream.Read(Item.FStoredSize, SizeOf(Item.FStoredSize));

    if gfStoredDigest in Item.Flags then Item.FStoredDigest := FStream.ReadAnsiString;
    if gfComment      in Item.Flags then Item.FComment      := FStream.ReadAnsiString;

    if gfPlatform     in Item.Flags then FStream.Read(Item.FPlatform, SizeOf(Item.FPlatform));

    Result := FStream.ReadAnsiString = GetItemDigest(Item);

    DoDirSeparators(Item.FLinkName);
    DoDirSeparators(Item.FName    );
  end;
end;

function TGulpReader.ReadOffSet(var OffSet: int64): boolean;
var
     Context : TSHA1Context;
      Digest : TSHA1Digest;
      Marker : TGulpMarker;
  OffSetTime : TDateTime = 0.0;
begin
  FillChar    (Marker, SizeOf(Marker), 0);
  FStream.Read(Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    SHA1Init    (Context);
    FStream.Read(         OffSet,     SizeOf(OffSet));
    SHA1Update  (Context, OffSet,     SizeOf(OffSet));
    FStream.Read(         OffSetTime, SizeOf(OffSetTime));
    SHA1Update  (Context, OffSetTime, SizeOf(OffSetTime));
    SHA1Final   (Context, Digest);

    Result := FStream.ReadAnsiString = SHA1Print(Digest);
    if Result then
    begin
      if OffSet = 0 then
        raise Exception.Create('Archive is damaged, try with the "fix" command (ex0004)');
    end;
  end;
end;

procedure TGulpReader.Load(Version: longword);
var
         I : longint;
      Item : TGulpItem;
   ItemVer : longword = 0;
  LastItem : TGulpItem;
    OffSet : int64 = 0;
      Size : int64 = 0;
begin
  Size := FStream.Seek(0, soBeginning);
  if ReadOffSet(OffSet) = TRUE then
  begin
    FStream.Seek(OffSet, soBeginning);
    Inc(ItemVer);
  end;

  Item := TGulpItem.Create;
  while ReadItem(Item) = TRUE do
  begin
    Item.FPosition := Count;
    Item.FVersion  := ItemVer;

    LastItem := Item;
    if gfLast in Item.FFlags then
    begin
      Size := FStream.Seek(0, soCurrent);
      if ReadOffSet(OffSet) = TRUE then
      begin
        FStream.Seek(OffSet, soBeginning);
        Inc(ItemVer);
      end;
    end;

    if Version = 0 then
    begin
      Add(Item);
      Item := TGulpItem.Create;
    end else
      if Item.FVersion <= Version then
      begin
        if gfAdd in Item.FFlags then
        begin
          Add(Item);
          Item := TGulpItem.Create;
        end else
        begin
          I := Find(Item.FName);
          if I <> - 1 then
            Delete(I);
        end;
      end;
  end;

  if FList.Count = 0 then
  begin
    if FStream.Size <> 0 then
      raise Exception.Create('Invalid signature value (ex0001)');
  end else
    if (gfLast in LastItem.FFlags) = FALSE then
      raise Exception.Create('Archive is broken, try with fix command (ex0002)');

  if FStream.Seek(0, soEnd) <> Size then
    raise Exception.Create('Archive is broken, try with fix command (ex0003)');

  FreeAndNil(Item);
end;

procedure TGulpReader.ExtractTo(Stream: TStream; Index: longint);
var
  Item : TGulpItem;
begin
  Item := Items[Index];
  if gfAdd in Item.FFlags then
    if Item.FSize > 0 then
    begin
      FStream.Seek(Item.FOffset, soBeginning);
      if ReadStream(Stream, Item.FSize) <> Item.FStoredDigest then
        raise Exception.CreateFmt('Mismatched checksum for "%s"', [Item.FName]);
    end;
end;

procedure TGulpReader.Extract(Index: longint);
var
    Item : TGulpItem;
  Stream : TFileStream;
begin
  Item := Items[Index];
  if ForceDirectories(ExtractFileDir(Item.FName)) = FALSE then
    raise Exception.CreateFmt('Unable to create path "%s"',
      [ExtractFileDir(Item.FName)]);
  {$IFDEF UNIX}
    if Item.FAttributes and faSymLink = faSymLink then
    begin
      //if FpLink(RecLinkName, RecName) <> 0 then
      //  raise Exception.CreateFmt('Unable to create hardlink %s', [RecName]);
      if FpSymLink(pchar(Item.FLinkName), pchar(Item.FName)) <> 0 then
        raise Exception.CreateFmt('Unable to create symlink "%s"', [Item.FName]);
    end else
  {$ELSE}
    {$IFDEF MSWINDOWS}
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
      if Item.FAttributes and faDirectory = faDirectory then
      begin
        if DirectoryExists(Item.FName) = FALSE then
          if CreateDir(Item.FName) = FALSE then
            raise Exception.CreateFmt('Unable to create directory "%s"', [Item.FName]);
      end else
      begin
        Stream := TFileStream.Create(Item.FName, fmCreate);
        ExtractTo (Stream, Index);
        FreeAndNil(Stream);
      end;
  {$IFDEF UNIX}
    if Item.FPlatform = gpUNIX then
      if FpChmod(Item.FName, Item.FMode) <> 0 then
        raise Exception.CreateFmt('Unable to set mode for "%s"', [Item.FName]);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      if FileSetAttr(RecName, Rec.FAttributes) <> 0 then
        raise Exception.CreateFmt('Unable to set attrbutes for "%s"', [RecName]);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  if FileSetDate(Item.FName, DateTimeToFileDate(UniversalTimeToLocal(Item.FTime))) <> 0 then
    raise Exception.CreateFmt('Unable to set date for "%s"', [Item.FName]);
end;

function TGulpReader.GetItem(Index: longint): TGulpItem;
begin
  Result := TGulpItem(FList[Index]);
end;

function TGulpReader.GetCount: longint;
begin
  Result := FList.Count;
end;

// =============================================================================
// TGulpWriter
// =============================================================================

constructor TGulpWriter.Create(Stream: TStream);
begin
  inherited Create;
  FList         := TList.Create;
  FStream       := Stream;
  FStreamSize   := Stream.Seek(0, soEnd);
  FStorageFlags := [];
end;

destructor TGulpWriter.Destroy;
var
     I : longint;
  Size : int64;
begin
  if FList.Count <> 0 then
  begin
    Size := FStream.Seek(0, soEnd);
    FStream.Seek(FStreamSize, soBeginning);
    WriteOffSet(Size);

    FStream.Seek(0, soEnd);
    Include(Items[Count - 1].FFlags, gfLast);
    for I := 0 to Count - 1 do
      WriteItem(Items[I]);

    FStreamSize := FStream.Seek(0, soEnd);
    for I := 0 to Count - 1 do
      Items[I].Destroy;
  end;
  FreeandNil(FList);
  inherited Destroy;
end;

procedure TGulpWriter.Add(const FileName: string);
var
    Item : TGulpItem;
      SR : TSearchRec;
  Stream : TStream;
begin
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
    if GetAttr(SR) and (faSysFile or faVolumeId) = 0 then
    begin
      Item := GetNew;

      Include(Item.FFlags, gfAdd);
      Include(Item.FFlags, gfName);        Item.FName       := FileName;
      Include(Item.FFlags, gfTime);        Item.FTime       := GetTime(SR);
      Include(Item.FFlags, gfAttributes);  Item.FAttributes := GetAttr(SR);
      {$IFDEF UNIX}
        Include(Item.FFlags, gfMode    );  Item.FMode       := GetMode(FileName);
        Include(Item.FFlags, gfUserID  );  Item.FUserID     := GetUID (FileName);
        Include(Item.FFlags, gfGroupID );  Item.FGroupID    := GetGID (FileName);
      {$ELSE}
        {$IFDEF MSWINDOWS}
        {$ELSE}
          Unsupported platform...
        {$ENDIF}
      {$ENDIF}

      {$IFDEF UNIX}
        if Item.FAttributes and faSymLink = faSymLink then
        begin
          Include(Item.FFlags, gfLinkName);  Item.FLinkName := fpReadLink(FileName);
        end else
      {$ELSE}
        {$IFDEF MSWINDOWS}
        {$ELSE}
          Unsupported platform...
        {$ENDIF}
      {$ENDIF}
          if Item.FAttributes and faDirectory = 0 then
            if GetSize(SR) > 0 then
            begin
              Stream := TFileStream.Create(Item.FName, fmOpenRead or fmShareDenyNone);
              Include(Item.FFlags, gfSize);          Item.FSize         := GetSize(SR);
              Include(Item.FFlags, gfOffSet);        Item.FOffSet       := FStream.Seek(0, soCurrent);
              Include(Item.FFlags, gfStoredDigest);  Item.FStoredDigest := WriteStream (Stream, Item.FSize);
              Include(Item.FFlags, gfStoredSize);    Item.FStoredSize   := FStream.Seek(0, soCurrent) - Item.FOffSet;
              FreeAndNil(Stream);
            end;
    end;
  SysUtils.FindClose(SR);
end;

procedure TGulpWriter.Delete(const FileName: string);
var
  Item : TGulpItem;
begin
  Item := GetNew;
  Include(Item.FFlags, gfName);
  Item.FName := FileName;
end;

function TGulpWriter.WriteStream(Stream: TStream; Size: int64): string;
var
   Buffer : array[0..$FFFF] of byte;
  Context : TSHA1Context;
   Digest : TSHA1Digest;
   Readed : longint;
  ZStream : TStream;
begin
  FStream.Write(FStorageFlags, SizeOf(TGulpStorageFlags));
  case gsfGZ in FStorageFlags of
    FALSE: ZStream := FStream;
    TRUE : if gsfMAX in FStorageFlags then
             ZStream := TCompressionStream.Create(clMAX, FStream, FALSE)
           else
             if gsfDEFAULT in FStorageFlags then
               ZStream := TCompressionStream.Create(clDEFAULT, FStream, FALSE)
             else
               ZStream := TCompressionStream.Create(clFASTEST, FStream, FALSE);
  end;

  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := Stream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0007)');
    SHA1Update   (Context, Buffer, Readed);
    ZStream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest);
  case gsfGZ in FStorageFlags of
    FALSE: ZStream := nil;
    TRUE : FreeAndNil(ZStream);
  end;
  Result := SHA1Print(Digest);
end;

function TGulpWriter.WriteItem(Item: TGulpItem): boolean;
begin
  FStream.Write(GulpMarker,  SizeOf(GulpMarker));
  FStream.Write(Item.FFlags, SizeOf(Item.FFlags));

  if gfName         in Item.Flags then FStream.WriteAnsiString(Item.FName);

  if gfTime         in Item.Flags then FStream.Write(Item.FTime, SizeOf(Item.FTime));
  if gfAttributes   in Item.Flags then FStream.Write(Item.FAttributes, SizeOf(Item.FAttributes));
  if gfMode         in Item.Flags then FStream.Write(Item.FMode, SizeOf(Item.FMode));
  if gfSize         in Item.Flags then FStream.Write(Item.FSize, SizeOf(Item.FSize));

  if gfLinkName     in Item.Flags then FStream.WriteAnsiString(Item.FLinkName);

  if gfUserID       in Item.Flags then FStream.Write(Item.FUserID, SizeOf(Item.FUserID));
  if gfUserName     in Item.Flags then FStream.WriteAnsiString(Item.FUserName);

  if gfGroupID      in Item.Flags then FStream.Write(Item.FGroupID, SizeOf(Item.FGroupID));
  if gfGroupName    in Item.Flags then FStream.WriteAnsiString(Item.FGroupName);

  if gfOffSet       in Item.Flags then FStream.Write(Item.FOffSet, SizeOf(Item.FOffSet));
  if gfStoredSize   in Item.Flags then FStream.Write(Item.FStoredSize, SizeOf(Item.FStoredSize));

  if gfStoredDigest in Item.Flags then FStream.WriteAnsiString(Item.FStoredDigest);
  if gfComment      in Item.Flags then FStream.WriteAnsiString(Item.FComment);

  if gfPlatform     in Item.Flags then FStream.Write(Item.FPlatform, SizeOf(Item.FPlatform));

  FStream.WriteAnsiString(GetItemDigest(Item));
  Result := TRUE;
end;

function TGulpWriter.WriteOffSet(const OffSet: int64): boolean;
var
     Context : TSHA1Context;
      Digest : TSHA1Digest;
  OffSetTime : TDateTime;
begin
  OffSetTime := GetTime(Now);
  begin
    FStream.Write(GulpMarker, SizeOf(GulpMarker));

    SHA1Init     (Context);
    FStream.Write(         OffSet,     SizeOf(OffSet));
    SHA1Update   (Context, OffSet,     SizeOf(OffSet));
    FStream.Write(         OffSetTime, SizeOf(OffSetTime));
    SHA1Update   (Context, OffSetTime, SizeOf(OffSetTime));
    SHA1Final    (Context, Digest);

    FStream.WriteAnsiString(SHA1Print(Digest));
  end;
  Result := TRUE;
end;

function TGulpWriter.GetNew: TGulpItem;
begin
  if FList.Count = 0 then
  begin
    WriteOffSet(FStreamSize);
  end;
  Result := Items[FList.Add(ClearItem(TGulpItem.Create))];
end;

function TGulpWriter.GetItem(Index: longint): TGulpItem;
begin
  Result := TGulpItem(FList[Index]);
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
  FOnList    := nil;
  FOnMessage := nil;
  FExclude   := TStringList.Create;
  FInclude   := TStringList.Create;
  {$IFDEF UNIX}
    FInclude .CaseSensitive := TRUE;
    FExclude .CaseSensitive := TRUE;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      FInclude .CaseSensitive := FALSE;
      FExclude .CaseSensitive := FALSE;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  FExclude .Duplicates := dupIgnore;
  FInclude .Duplicates := dupIgnore;
  FExclude .Sorted     := TRUE;
  FInclude .Sorted     := TRUE;
  FStorageFlags        := [];
  FNodelete            := FALSE;
  FUntilVersion        := $FFFFFFFF;
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
  FStorageFlags := [];
  FNodelete     := FALSE;
  FUntilVersion := $FFFFFFFF;
end;

procedure TGulpApplication.DoList(var Item: TGulpItem);
begin
  if Assigned(FOnList) then
    FOnList(Item);
end;

procedure TGulpApplication.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Message);
end;

procedure TGulpApplication.Sync(const FileName: string);
var
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  DoMessage(Format('Sync the content of "%s" %s', [FileName, LineEnding]));
  DoMessage(Format('%sScanning archive...      ', [#13]));
  if FileExists(FileName) then
    Stream := TFileStream.Create (FileName, fmOpenReadWrite)
  else
    Stream := TFileStream.Create (FileName, fmCreate);
  LibReader:= TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  Size := Stream.Seek(0, soEnd);

  DoMessage(Format('%sScanning filesystem...   ', [#13]));
  Scan := TSysScanner.Create;
  for I := FInclude.Count - 1 downto 0 do
    if DirectoryExists(FInclude[I]) = TRUE then
      FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
    else
      if FInclude[I][Length(FInclude[I])] = PathDelim then
        FInclude[I] := FInclude[I] + '*';
  if FInclude.Count = 0 then
    FInclude.Add('*');
  for I := FInclude.Count - 1 downto 0 do
    Scan.Add(FInclude[I]);

  for I := FExclude.Count - 1 downto 0 do
    if DirectoryExists(FExclude[I]) = TRUE then
      FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
    else
      if FExclude[I][Length(FExclude[I])] = PathDelim then
        FExclude[I] := FExclude[I] + '*';
  FExclude.Add(FileName);
  for I := Scan.Count - 1 downto 0 do
    if FileNameMatch(Scan.Items[I], FExclude) = TRUE then
      Scan.Delete(I);

  DoMessage(Format('%sSyncing items...         ', [#13]));
  LibWriter := TGulpWriter.Create(Stream);
  if FNoDelete = FALSE then
  begin
    for I := 0 to LibReader.Count - 1 do
      if Scan.Find(LibReader.Items[I].Name) = -1 then
        LibWriter.Delete(LibReader.Items[I].Name);
  end;

  LibWriter.FStorageFlags := FStorageFlags;
  for I := 0 to Scan.Count - 1 do
  begin
    J := LibReader.Find(Scan.Items[I]);
    if J = -1 then
      LibWriter.Add(Scan.Items[I])
    else
      if GetTime(Scan.Items[I]) <> LibReader.Items[J].Time then
      begin
        LibWriter.Delete(Scan.Items[I]);
        LibWriter.Add(Scan.Items[I]);
      end;
  end;
  FreeAndNil(LibWriter);
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeandNil(Scan);

  DoMessage(Format('%sFinished (%u added bytes) %s',
    [#13, GetSize(FileName) - Size, LineEnding]));
end;

procedure TGulpApplication.Restore(const FileName: string);
var
  LibReader : TGulpReader;
     LibRec : TGulpItem;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64 = 0;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  DoMessage(Format('Restore the content of "%s" %s', [FileName, LineEnding]));
  DoMessage(Format('%sScanning archive...         ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  DoMessage(Format('%sScanning filesystem...      ', [#13]));
  Scan := TSysScanner.Create;
  Scan.Add('*');
  for I := FInclude.Count - 1 downto 0 do
    if FInclude[I][Length(FInclude[I])] = PathDelim then
    begin
      FInclude[I] := FInclude[I] + '*'
    end else
    begin
      J := LibReader.Find(FInclude[I]);
      if J <> -1 then
        if LibReader.Items[J].Attributes and faDirectory = faDirectory then
          FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
    end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
    if FExclude[I][Length(FExclude[I])] = PathDelim then
    begin
      FExclude[I] := FExclude[I] + '*'
    end else
    begin
      J := LibReader.Find(FExclude[I]);
      if J <> -1 then
        if LibReader.Items[J].Attributes and faDirectory = faDirectory then
          FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
    end;
  FExclude.Add(FileName);

  DoMessage(Format('%sRestoring items...          ', [#13]));
  if FNoDelete = FALSE then
    for I := Scan.Count - 1 downto 0 do
    begin
      J := LibReader.Find(Scan.Items[I]);
      if (J = -1) or
         (FileNameMatch(LibReader.Items[J].Name, FInclude) = FALSE) or
         (FileNameMatch(LibReader.Items[J].Name, FExclude) = TRUE ) then
      begin
        if DirectoryExists(Scan.Items[I]) = TRUE then
          RemoveDir(Scan.Items[I])
        else
          DeleteFile(Scan.Items[I]);
      end;
    end;

  for I := LibReader.Count - 1 downto 0 do
  begin
    LibRec := LibReader.Items[I];
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
      begin
        J := Scan.Find(LibRec.Name);
        if J = -1 then
        begin
          LibReader.Extract(I);
          Inc(Size, LibRec.Size);
        end else
          if GetTime(Scan.Items[J]) <> LibRec.Time then
          begin
            LibReader.Extract(I);
            Inc(Size, LibRec.Size);
          end;
      end;
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Scan);

  DoMessage(Format('%s Finished (%u extracted bytes) %s',
    [#13, Size, LineEnding]));
end;

procedure TGulpApplication.Check(const FileName: string);
var
  LibReader : TGulpReader;
          I : longint;
        Nul : TStream;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  DoMessage(Format('Check the content of "%s" %s', [FileName, LineEnding]));
  DoMessage(Format('%sScanning archive...       ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(0);

  DoMessage(Format('%sChecking items...         ', [#13]));
  Nul   := TNulStream.Create;
  for I := 0 to LibReader.Count - 1 do
  begin
    LibReader.ExtractTo(Nul, I);
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Nul);

  DoMessage(Format('%sFinished (%u checked bytes) %s',
    [#13, GetSize(FileName), LineEnding]));
end;

procedure TGulpApplication.Fix(const FileName: string);
var
  LibReader : TGulpReader;
     LibRec : TGulpItem;
     OffSet : int64 = 0;
       Size : int64 = 0;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  DoMessage(Format('Fix the content of "%s" %s', [FileName, LineEnding]));
  DoMessage(Format('%sFixing items...         ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenReadWrite);
  LibReader := TGulpReader.Create(Stream);
  LibRec    := TGulpItem.Create;
  try
    if LibReader.ReadOffSet(OffSet) = TRUE then
    begin
      if OffSet <> 0 then
        Stream.Seek(OffSet, soBeginning);
    end;

    while LibReader.ReadItem(LibRec) = TRUE do
      if gfLast in LibRec.FFlags then
      begin
        Size := Stream.Seek(0, soCurrent);
        if LibReader.ReadOffSet(OffSet) = TRUE then
        begin
          if OffSet <> 0 then
            Stream.Seek(OffSet, soBeginning);
        end;
      end;
  except
    // nothing to do
  end;

  OffSet := Stream.Size - Size;
  if Size > 0 then
    Stream.Size := Size
  else
    raise Exception.Create('Stream is not a valid archive (ex0008)');
  FreeAndNil(LibReader);
  FreeAndNil(LibRec);
  FreeAndNil(Stream);

  DoMessage(Format('%sFinished (%u removed bytes) %s',
    [#13, OffSet, LineEnding]));
end;

procedure TGulpApplication.Purge(const FileName: string);
var
    LibItem : TGulpItem;
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
          I : longint;
     OffSet : int64 = 0;
     Stream : TStream;
        Tmp : TStream;
    TmpName : string;
begin
  DoMessage(GulpDescription);
  DoMessage(Format('Purge the content of "%s" %s', [FileName, LineEnding]));
  DoMessage(Format('%sScanning archive...       ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  DoMessage(Format('%sMoving items...           ', [#13]));
  TmpName   := GetTempFileName(ExtractFileDir(FileName), '');
  Tmp       := TFileStream.Create(TmpName, fmCreate);
  LibWriter := TGulpWriter.Create(Tmp);

  if LibReader.Count > 0 then
  begin
    LibWriter.WriteOffSet(0);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibItem := LibReader.Items[I];
      if LibItem.FStoredSize > 0 then
      begin
        Stream.Seek(LibItem.FOffset, soBeginning);
        LibItem.FOffSet := Tmp.Seek(0, soCurrent);
        Tmp.CopyFrom(Stream, LibItem.FStoredSize);
      end;
    end;

    OffSet := Tmp.Seek(0, soEnd);
    Tmp.Seek(0, soBeginning);
    LibWriter.WriteOffSet(OffSet);

    Tmp.Seek(0, soEnd);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibItem := LibReader.Items[I];
      if I = LibReader.Count - 1 then
        System.Include(LibItem.FFlags, gfLast)
      else
        System.Exclude(LibItem.FFlags, gfLast);
      LibWriter.WriteItem(LibItem);
    end;
  end;
  OffSet := Stream.Size - Tmp.Size;
  FreeAndNil(LibReader);
  FreeAndNil(LibWriter);
  FreeAndNil(Stream);
  FreeAndNil(Tmp);

  if DeleteFile(FileName) = FALSE then
    raise Exception.CreateFmt('Unable to delete file "%s"', [FileName])
  else
    if RenameFile(TmpName, FileName)= FALSE then
      raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);

  DoMessage(Format('%sFinished (%u removed bytes) %s',
    [#13, Offset, LineEnding]));
end;

procedure TGulpApplication.List(const FileName: string);
var
      Count : longint = 0;
  LibReader : TGulpReader;
     LibRec : TGulpItem;
       I, J : longint;
     Stream : TStream;
    Version : longword = 0;
begin
  DoMessage(GulpDescription);
  DoMessage(Format('List the content of "%s" %s', [FileName, LineEnding]));
  DoMessage(Format('%sScanning archive...      ', [#13]));
  Stream    := TFileStream.Create (FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  for I := FInclude.Count - 1 downto 0 do
    if FInclude[I][Length(FInclude[I])] = PathDelim then
    begin
      FInclude[I] := FInclude[I] + '*'
    end else
    begin
      J := LibReader.Find(FInclude[I]);
      if J <> -1 then
        if LibReader.Items[J].Attributes and faDirectory = faDirectory then
          FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
    end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
    if FExclude[I][Length(FExclude[I])] = PathDelim then
    begin
      FExclude[I] := FExclude[I] + '*'
    end else
    begin
      J := LibReader.Find(FExclude[I]);
      if J <> -1 then
        if LibReader.Items[J].Attributes and faDirectory = faDirectory then
          FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
    end;

  DoMessage(Format('%sListing items...       %s', [#13, LineEnding]));
  while LibReader.Count <> 0 do
  begin
    LibRec  := LibReader.Items[0];

    Version := Max(Version, LibRec.Version);
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
      begin
        DoList(LibRec);
        Inc(Count);
      end;

    LibReader.Delete(0);
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);

  DoMessage(Format('Finished (%u listed items) %s', [Count, LineEnding]));
  DoMessage(Format('Lastest version %u %s', [Version, LineEnding]));
end;

end.
