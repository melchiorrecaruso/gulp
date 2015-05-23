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

    v0.0.2 - 2015.05.15 by Melchiorre Caruso.
}

unit GulpLibrary;

interface

uses
  {$IFDEF UNIX}
    BaseUnix,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Windows,
    {$ENDIF}
  {$ENDIF}
  Classes,
  GulpCommon,
  SysUtils,
  Sha1;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..7] of char;

  // --- Gulp Flags ---
  TGulpFlag = (gfNew,        gfName,         gfTime,
               gfAttributes, gfMode,         gfSize,
               gfLinkName,   gfUserID,       gfUserName,
               gfGroupID,    gfGroupName,    gfOffSet,
               gfStoredSize, gfStoredDigest, gfComment,
               gfPlatform,   gfLast);

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

  // --- Gulp Record class ---
  TGulpRec = class(TObject)
  private
    FFlags        : TGulpFlags;    // Flags
    FVersion      : int64;         // File version (reserved)
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
  public
    property Flags      : TGulpFlags    read FFlags;
    property Version    : int64         read FVersion;
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
  end;

  TGulpOnMessage = procedure (const Message: string) of object;
  TGulpOnList    = procedure (Rec: TGulpRec) of object;

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
    procedure DoList(Rec: TGulpRec);
    procedure DoMessage(const Message: string);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Synchronize(const FileName: string);
    procedure Restore    (const FileName: string);
    procedure Purge      (const FileName: string);
    procedure List       (const FileName: string);
    procedure Fix        (const FileName: string);
    procedure Check      (const FileName: string);
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

function  VerToString(var Rec: TGulpRec): string;
function FlagToString(var Rec: TGulpRec): string;
function AttrToString(var Rec: TGulpRec): string;
function StringToAttr(const S: string  ): longint;
function SizeToString(var Rec: TGulpRec): string;
function TimeToString(var Rec: TGulpRec): string;
function ModeToString(var Rec: TGulpRec): string;
function StringToMode(const S: string  ): longint;

// =============================================================================
// IMPLEMENTATION
// =============================================================================

implementation

uses
  DateUtils,
  Math,
  ZStream;

const
  GulpMarker : TGulpMarker = ('G','U','L','P',char(0),char(0),char(2),char(0));
  GulpDescription =  'GULP v0.0.2 journaling archiver, copyright (c) 2014-2015 Melchiorre Caruso.' + LineEnding +
                     'GULP archiver for user-level incremental backups with rollback capability.';

type
  // --- The Gulp Reader ---
  TGulpReader = class(TObject)
  private
    FList   : TList;
    FStream : TStream;
    procedure Clear;
    procedure Add(Rec: TGulpRec);
    procedure Delete(Index: longint);
    function Find(const FileName: string): longint;
    function ReadRec(Rec: TGulpRec): boolean;
    function ReadStream (Stream: TStream; Size: int64): string;
    function ReadOffSet(var OffSet: int64): boolean;
    procedure ExtractTo(Index: longint; Stream: TStream);
    procedure Extract(Index: longint);
    function GetItem(Index: longint): TGulpRec;
    function GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Load(Version: longword): boolean;
    property Items[Index: longint]: TGulpRec read GetItem;
    property Count: longint read GetCount;
  end;

  // --- The Gulp Writer ---
  TGulpWriter = class(TObject)
  private
    FList         : TList;
    FStream       : TStream;
    FStreamSize   : int64;
    FStorageFlags : TGulpStorageFlags;
    function WriteRec(Rec: TGulpRec): boolean;
    function WriteStream(Stream: TStream; Size: int64): string;
    function WriteOffSet(const OffSet: int64): boolean;
    function GetItem(Index: longint): TGulpRec;
    function GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure Delete(const FileName: string);
    procedure Add(const FileName: string);
    property Items[Index: longint]: TGulpRec read GetItem;
    property Count: longint read GetCount;
 public
    property StorageFlags : TGulpStorageFlags read FStorageFlags write FStorageFlags;
  end;

// =============================================================================
// Library routines
// =============================================================================

function GetTime(var SR: TSearchRec): TDateTime;
begin
  Result := LocalTimeToUniversal(FileDateToDateTime(SR.Time));
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
  Result := longword(-1);
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
  Result := longword(-1);
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

function VerToString(var Rec: TGulpRec): string;
begin
  Result := IntTostr(Rec.Version);
end;

function FlagToString(var Rec: TGulpRec): string;
begin
  if gfNew in Rec.Flags then
    Result := 'ADD'
  else
    Result := 'DEL';
end;

function TimeToString(var Rec: TGulpRec): string;
begin
  Result := '.......... ........';
  if gfNew in Rec.Flags then
    if gfTime in Rec.Flags then
      Result := FormatDateTime(
        DefaultFormatSettings.LongDateFormat + ' ' +
        DefaultFormatSettings.LongTimeFormat, Rec.Time);
end;

function SizeToString(var Rec: TGulpRec): string;
begin
  Result := '';
  if gfNew in Rec.Flags then
    if gfSize in Rec.Flags then
      Result := Format('%u', [Rec.Size])
end;

function AttrToString(var Rec: TGulpRec): string;
begin
  Result := '.......';
  if gfNew in Rec.Flags then
  begin
    if Rec.Attributes and faReadOnly  <> 0 then Result[1] := 'R';
    if Rec.Attributes and faHidden    <> 0 then Result[2] := 'H';
    if Rec.Attributes and faSysFile   <> 0 then Result[3] := 'S';
    if Rec.Attributes and faVolumeId  <> 0 then Result[4] := 'V';
    if Rec.Attributes and faDirectory <> 0 then Result[5] := 'D';
    if Rec.Attributes and faArchive   <> 0 then Result[6] := 'A';
    if Rec.Attributes and faSymLink   <> 0 then Result[7] := 'L';
  end;
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

function ModeToString(var Rec: TGulpRec): string;
begin
  {$IFDEF UNIX}
    Result := '...';
    if Rec.Platform = gpUNIX then
    begin
      if gfNew in Rec.Flags then
        Result := OctStr(Rec.Mode, 3);
    end;
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

// =============================================================================
// Internal rutines
// =============================================================================

function ClearRec(Rec: TGulpRec): TGulpRec; inline;
begin
  Result               := Rec;
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

function CompareRecName(R1, R2: TGulpRec): longint; inline;
begin
  Result := AnsiCompareFileName(R1.Name, R2.Name);
  if Result = 0 then
  begin
    if R1.Version < R2.Version then
      Result := - 1
    else
      if R1.Version > R2.Version then
        Result := 1;
  end;
end;

function GetRecDigest(Rec: TGulpRec): string; inline;
var
  Context : TSHA1Context;
   Digest : TSHA1Digest;
begin
  SHA1Init  (Context);
  SHA1Update(Context,         Rec.FFlags,          SizeOf(Rec.FFlags       ));
  SHA1Update(Context,         Rec.FVersion,        SizeOf(Rec.FVersion     ));
  SHA1Update(Context, Pointer(Rec.FName)^,         Length(Rec.FName        ));
  SHA1Update(Context,         Rec.FTime,           SizeOf(Rec.FTime        ));
  SHA1Update(Context,         Rec.FAttributes,     SizeOf(Rec.FAttributes  ));
  SHA1Update(Context,         Rec.FMode,           SizeOf(Rec.FMode        ));
  SHA1Update(Context,         Rec.FSize,           SizeOf(Rec.FSize        ));
  SHA1Update(Context, Pointer(Rec.FLinkName)^,     Length(Rec.FLinkName    ));
  SHA1Update(Context,         Rec.FUserID,         SizeOf(Rec.FUserID      ));
  SHA1Update(Context, Pointer(Rec.FUserName)^,     Length(Rec.FUserName    ));
  SHA1Update(Context,         Rec.FGroupID,        SizeOf(Rec.FGroupID     ));
  SHA1Update(Context, Pointer(Rec.FGroupName)^,    Length(Rec.FGroupName   ));
  SHA1Update(Context,         Rec.FOffSet,         SizeOf(Rec.FOffSet      ));
  SHA1Update(Context,         Rec.FStoredSize,     SizeOf(Rec.FStoredSize  ));
  SHA1Update(Context, Pointer(Rec.FStoredDigest)^, Length(Rec.FStoredDigest));
  SHA1Update(Context, Pointer(Rec.FComment)^,      Length(Rec.FComment     ));
  SHA1Update(Context,         Rec.FPlatform,       SizeOf(Rec.FPlatform    ));
  SHA1Final (Context, Digest);
  Result := SHA1Print(Digest);
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
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpReader.Clear;
begin
  while FList.Count <> 0 do
    Delete(0);
end;

procedure TGulpReader.Delete(Index: longint);
begin
  TGulpRec(FList[Index]).Destroy;
  FList.Delete(Index);
end;

procedure TGulpReader.Add(Rec: TGulpRec);
var
  L, M, H, I : longint;
begin
  L := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := CompareRecName(Items[M], Rec);
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
      FList.Insert(M + 1, Rec)
    else
      if I > 0 then
        FList.Insert(M, Rec)
      else
        raise Exception.Create('Duplicates not allowed');
  end else
    FList.Add(Rec);
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
     Method : longint = 0;
     Readed : longint;
    ZStream : TStream;
begin
  FillChar    (Method, SizeOf(Method), 0);
  FStream.Read(Method, SizeOf(Method));
  FStream.ReadAnsiString;

  case Method and $0000FF00 of
    $0000: ZStream := FStream;
    $0100: ZStream := TDecompressionStream.Create(FStream, FALSE);
    else Exception.Create('Internal error');
  end;

  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := ZStream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream');
    SHA1Update  (Context, Buffer, Readed);
    Stream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest);

  case Method and $0000FF00 of
    $0100: FreeAndNil(ZStream);
    $0000: ZStream := nil;
  end;
  Result := SHA1Print(Digest);
end;

function TGulpReader.ReadRec(Rec: TGulpRec): boolean;
var
  Marker : TGulpMarker;
begin
  FillChar    (Marker, SizeOf(Marker), 0);
  FStream.Read(Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    ClearRec(Rec);
    FStream.Read(Rec.FFlags, SizeOf(Rec.FFlags));

    if gfName         in Rec.Flags then Rec.FName := FStream.ReadAnsiString;

    if gfTime         in Rec.Flags then FStream.Read(Rec.FTime, SizeOf(Rec.FTime));
    if gfAttributes   in Rec.Flags then FStream.Read(Rec.FAttributes, SizeOf(Rec.FAttributes));
    if gfMode         in Rec.Flags then FStream.Read(Rec.FMode, SizeOf(Rec.FMode));
    if gfSize         in Rec.Flags then FStream.Read(Rec.FSize, SizeOf(Rec.FSize));

    if gfLinkName     in Rec.Flags then Rec.FLinkName := FStream.ReadAnsiString;

    if gfUserID       in Rec.Flags then FStream.Read(Rec.FUserID, SizeOf(Rec.FUserID));
    if gfUserName     in Rec.Flags then Rec.FUserName := FStream.ReadAnsiString;

    if gfGroupID      in Rec.Flags then FStream.Read(Rec.FGroupID, SizeOf(Rec.FGroupID));
    if gfGroupName    in Rec.Flags then Rec.FGroupName := FStream.ReadAnsiString;

    if gfOffSet       in Rec.Flags then FStream.Read(Rec.FOffSet, SizeOf(Rec.FOffSet));
    if gfStoredSize   in Rec.Flags then FStream.Read(Rec.FStoredSize, SizeOf(Rec.FStoredSize));

    if gfStoredDigest in Rec.Flags then Rec.FStoredDigest := FStream.ReadAnsiString;
    if gfComment      in Rec.Flags then Rec.FComment      := FStream.ReadAnsiString;

    if gfPlatform     in Rec.Flags then FStream.Read(Rec.FPlatform, SizeOf(Rec.FPlatform));

    Result := FStream.ReadAnsiString = GetRecDigest(Rec);

    DoDirSeparators(Rec.FLinkName);
    DoDirSeparators(Rec.FName    );
  end;
end;

function TGulpReader.ReadOffSet(var OffSet: int64): boolean;
var
  Context : TSHA1Context;
   Digest : TSHA1Digest;
   Marker : TGulpMarker;
begin
  FillChar    (Marker, SizeOf(Marker), 0);
  FStream.Read(Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    SHA1Init    (Context);
    FStream.Read(         OffSet, SizeOf(OffSet));
    SHA1Update  (Context, OffSet, SizeOf(OffSet));
    SHA1Final   (Context, Digest);

    Result := FStream.ReadAnsiString = SHA1Print(Digest);
    if Result then
    begin
      if OffSet = 0 then
        raise Exception.Create('Archive is damaged, try with the "fix" command');
    end;
  end;
end;

function TGulpReader.Load(Version: longword): boolean;
var
       I : longint;
     Rec : TGulpRec;
  OffSet : int64 = 0;
    Size : int64 = 0;
begin
  Result := FALSE;
  FStream.Seek(0, soBeginning);
  if ReadOffSet(OffSet) = TRUE then
    FStream.Seek(OffSet, soBeginning);

  Rec := TGulpRec.Create;
  while ReadRec(Rec) = TRUE do
  begin
    Result := gfLast in Rec.Flags;
    if Result then
    begin
      Size := FStream.Seek(0, soCurrent);
      if ReadOffSet(OffSet) = TRUE then
        FStream.Seek(OffSet, soBeginning)
    end;

    if Version = longword(-1) then
    begin
      Add(Rec);
      Rec := TGulpRec.Create;
    end else
      if Rec.Version <= Version then
      begin
        if gfNew in REc.FFlags then
        begin
          Add(Rec);
          Rec := TGulpRec.Create;
        end else
        begin
          I := Find(Rec.FName);
          if I <> - 1 then
            Delete(I);
        end;
      end;
  end;
  FreeAndNil(Rec);

  if Result = FALSE then
  begin
    raise Exception.CreateFmt('Mismatched flags for "%s"', [Rec.Name]);
  end else
  begin
    if FStream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

  if FStream.Seek(0, soEnd) = Size then
    raise Exception.CreateFmt('Mismatched flags for "%s"', [Rec.Name]);
end;

procedure TGulpReader.ExtractTo(Index: longint; Stream: TStream);
var
  LibRec : TGulpRec;
begin
  LibRec := Items[Index];
  if LibRec.FSize > 0 then
  begin
    FStream.Seek(LibRec.FOffset, soBeginning);
    if ReadStream(Stream, LibRec.FSize) <> LibRec.FStoredDigest then
      raise Exception.CreateFmt('Mismatched checksum for "%s"', [LibRec.FName]);
  end;
end;

procedure TGulpReader.Extract(Index: longint);
var
  Dest : TFileStream;
   Rec : TGulpRec;
begin
  Rec  := Items[Index];
  if ForceDirectories(ExtractFileDir(Rec.FName)) = FALSE then
    raise Exception.CreateFmt('Unable to create path "%s"',
      [ExtractFileDir(Rec.FName)]);
  {$IFDEF UNIX}
    if Rec.FAttributes and faSymLink = faSymLink then
    begin
      //if FpLink(RecLinkName, RecName) <> 0 then
      //  raise Exception.CreateFmt('Unable to create hardlink %s', [RecName]);
      if FpSymLink(pchar(Rec.FLinkName), pchar(Rec.FName)) <> 0 then
        raise Exception.CreateFmt('Unable to create symlink "%s"', [Rec.FName]);
    end else
  {$ELSE}
    {$IFDEF MSWINDOWS}
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
      if Rec.FAttributes and faDirectory = faDirectory then
      begin
        if DirectoryExists(Rec.FName) = FALSE then
          if CreateDir(Rec.FName) = FALSE then
            raise Exception.CreateFmt('Unable to create directory "%s"', [Rec.FName]);
      end else
      begin
        Dest := TFileStream.Create(Rec.FName, fmCreate);
        ExtractTo (Index, Dest);
        FreeAndNil(Dest);
      end;
  {$IFDEF UNIX}
    if Rec.FPlatform = gpUNIX then
      if FpChmod(Rec.FName, Rec.FMode) <> 0 then
        raise Exception.CreateFmt('Unable to set mode for "%s"', [Rec.FName]);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      if FileSetAttr(RecName, Rec.FAttributes) <> 0 then
        raise Exception.CreateFmt('Unable to set attrbutes for "%s"', [RecName]);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  if FileSetDate(Rec.FName, DateTimeToFileDate(UniversalTimeToLocal(Rec.FTime))) <> 0 then
    raise Exception.CreateFmt('Unable to set date for "%s"', [Rec.FName]);
end;

function TGulpReader.GetItem(Index: longint): TGulpRec;
begin
  Result := TGulpRec(FList[Index]);
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
    Include(TGulpRec(FList.Last).FFlags, gfLast);
    for I := 0 to FList.Count - 1 do
      WriteRec(TGulpRec(FList.Last));

    FStreamSize := FStream.Seek(0, soEnd);
    for I := 0 to FList.Count - 1 do
      TGulpRec(FList[I]).Destroy;
  end;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpWriter.Add(const FileName: string);
var
     Rec : TGulpRec;
      SR : TSearchRec;
  Source : TStream;
begin
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
    if GetAttr(SR) and (faSysFile or faVolumeId) = 0 then
    begin
      Rec := TGulpRec(FList[FList.Add(ClearRec(TGulpRec.Create))]);

      Include(Rec.FFlags, gfNew);
      Include(Rec.FFlags, gfName);        Rec.FName       := FileName;
      Include(Rec.FFlags, gfTime);        Rec.FTime       := GetTime(SR);
      Include(Rec.FFlags, gfAttributes);  Rec.FAttributes := GetAttr(SR);
      {$IFDEF UNIX}
        Include(Rec.FFlags, gfMode    );  Rec.FMode       := GetMode(FileName);
        Include(Rec.FFlags, gfUserID  );  Rec.FUserID     := GetUID (FileName);
        Include(Rec.FFlags, gfGroupID );  Rec.FGroupID    := GetGID (FileName);
      {$ELSE}
        {$IFDEF MSWINDOWS}
        {$ELSE}
          Unsupported platform...
        {$ENDIF}
      {$ENDIF}

      {$IFDEF UNIX}
        if Rec.FAttributes and faSymLink = faSymLink then
        begin
          Include(Rec.FFlags, gfLinkName);  Rec.FLinkName := fpReadLink(FileName);
        end else
      {$ELSE}
        {$IFDEF MSWINDOWS}
        {$ELSE}
          Unsupported platform...
        {$ENDIF}
      {$ENDIF}
          if Rec.FAttributes and faDirectory = 0 then
            if GetSize(SR) > 0 then
            begin
              Source := TFileStream.Create(Rec.FName, fmOpenRead or fmShareDenyNone);
              Include(Rec.FFlags, gfSize);          Rec.FSize         := GetSize(SR);
              Include(Rec.FFlags, gfOffSet);        Rec.FOffSet       := FStream.Seek(0, soCurrent);
              Include(Rec.FFlags, gfStoredDigest);  Rec.FStoredDigest := WriteStream (Source, Rec.FSize);
              Include(Rec.FFlags, gfStoredSize);    Rec.FStoredSize   := FStream.Seek(0, soCurrent) - Rec.FOffSet;
              FreeAndNil(Source);
            end;
    end;
  SysUtils.FindClose(SR);
end;

procedure TGulpWriter.Delete(const FileName: string);
var
  Rec : TGulpRec;
begin
  Rec := TGulpRec(FList[FList.Add(ClearRec(TGulpRec.Create))]);
  Include(Rec.FFlags, gfName);
  Rec.FName := FileName;
  FList.Add(Rec);
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
      raise Exception.Create('Unable to read stream');
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

function TGulpWriter.WriteRec(Rec: TGulpRec): boolean;
begin
  FStream.Write(GulpMarker, SizeOf(GulpMarker));
  FStream.Write(Rec.FFlags, SizeOf(Rec.FFlags));

  if gfName         in Rec.Flags then FStream.WriteAnsiString(Rec.FName);

  if gfTime         in Rec.Flags then FStream.Write(Rec.FTime, SizeOf(Rec.FTime));
  if gfAttributes   in Rec.Flags then FStream.Write(Rec.FAttributes, SizeOf(Rec.FAttributes));
  if gfMode         in Rec.Flags then FStream.Write(Rec.FMode, SizeOf(Rec.FMode));
  if gfSize         in Rec.Flags then FStream.Write(Rec.FSize, SizeOf(Rec.FSize));

  if gfLinkName     in Rec.Flags then FStream.WriteAnsiString(Rec.FLinkName);

  if gfUserID       in Rec.Flags then FStream.Write(Rec.FUserID, SizeOf(Rec.FUserID));
  if gfUserName     in Rec.Flags then FStream.WriteAnsiString(Rec.FUserName);

  if gfGroupID      in Rec.Flags then FStream.Write(Rec.FGroupID, SizeOf(Rec.FGroupID));
  if gfGroupName    in Rec.Flags then FStream.WriteAnsiString(Rec.FGroupName);

  if gfOffSet       in Rec.Flags then FStream.Write(Rec.FOffSet, SizeOf(Rec.FOffSet));
  if gfStoredSize   in Rec.Flags then FStream.Write(Rec.FStoredSize, SizeOf(Rec.FStoredSize));

  if gfStoredDigest in Rec.Flags then FStream.WriteAnsiString(Rec.FStoredDigest);
  if gfComment      in Rec.Flags then FStream.WriteAnsiString(Rec.FComment);

  if gfPlatform     in Rec.Flags then FStream.Write(Rec.FPlatform, SizeOf(Rec.FPlatform));

  FStream.WriteAnsiString(GetRecDigest(Rec));
  Result := TRUE;
end;

function TGulpWriter.WriteOffSet(const OffSet: int64): boolean;
var
  Context : TSHA1Context;
   Digest : TSHA1Digest;
begin
  FStream.Write(GulpMarker, SizeOf(GulpMarker));

  SHA1Init     (Context);
  FStream.Write(         OffSet, SizeOf(OffSet));
  SHA1Update   (Context, OffSet, SizeOf(OffSet));
  SHA1Final    (Context, Digest);

  FStream.WriteAnsiString(SHA1Print(Digest));
  Result := TRUE;
end;

function TGulpWriter.GetItem(Index: longint): TGulpRec;
begin
  Result := TGulpRec(FList[Index]);
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
  FUntilVersion        := longword(-1);
end;

destructor TGulpApplication.Destroy;
begin
  FreeAndNil(FExclude);
  FreeAndNil(FInclude);
  inherited Destroy;
end;

procedure TGulpApplication.DoList(Rec: TGulpRec);
begin
  if Assigned(FOnList) then
    FOnList(Rec);
end;

procedure TGulpApplication.DoMessage(const Message: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Message);
end;

procedure TGulpApplication.Synchronize(const FileName: string);
var
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('Synch the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage('Scanning archive...      ');
  if FileExists(FileName) then
    Stream := TFileStream.Create (FileName, fmOpenReadWrite)
  else
    Stream := TFileStream.Create (FileName, fmCreate);
  LibReader:= TGulpReader.Create(Stream);
  LibReader.Load(longword(-1));
  Size := Stream.Seek(0, soEnd);
  if LibReader.Count = 0 then
  begin
    if Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage('Scanning filesystem...   ');
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
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage('Update records...      ');
  LibWriter := TGulpWriter.Create(Stream);
  if FNoDelete = FALSE then
  begin
    for I := 0 to LibReader.Count - 1 do
      if Scan.Find(LibReader.Items[I].Name) = -1 then
        LibWriter.Delete(LibReader.Items[I].Name);
  end;

  LibWriter.FStorageFlags := StorageFlags;
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
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage(Format('Finished (%u added bytes)', [Stream.Seek(0, soEnd) - Size]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  FreeAndNil(Stream);
  FreeandNil(Scan);
end;

procedure TGulpApplication.Restore(const FileName: string);
var
  LibReader : TGulpReader;
     LibRec : TGulpRec;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('Restore the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage('Scanning archive...      ');
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(longword(-1));
  if LibReader.Count = 0 then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage('Scanning filesystem...   ');
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
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage('Restore records...    ');
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

  Size  := 0;
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
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage(Format('Finished (%u extracted bytes)', [Size]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Scan);
end;

procedure TGulpApplication.Check(const FileName: string);
var
  LibReader : TGulpReader;
          I : longint;
        Nul : TStream;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('Check the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage('Scanning archive...      ');
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(longword(-1));
  if LibReader.Count = 0 then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage('Checking records...      ');
  Nul   := TNulStream.Create;
  for I := 0 to LibReader.Count - 1 do
  begin
    LibReader.ExtractTo(I, Nul);
  end;
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage(Format('Finished (%u checked bytes)', [Stream.Size]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Nul);
end;

procedure TGulpApplication.Fix(const FileName: string);
var
  LibReader : TGulpReader;
     LibRec : TGulpRec;
     OffSet : int64 = 0;
       Size : int64 = 0;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('Fix the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage('Fixing archive...        ');
  Stream    := TFileStream.Create(FileName, fmOpenReadWrite);
  LibReader := TGulpReader.Create(Stream);
  LibRec    := TGulpRec.Create;
  try
    if LibReader.ReadOffSet(OffSet) = TRUE then
    begin
      if OffSet <> 0 then
        Stream.Seek(OffSet, soBeginning);
    end;

    while LibReader.ReadRec(LibRec) = TRUE do
      if  gfLast in LibRec.FFlags then
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
  FreeAndNil(LibReader);
  FreeAndNil(LibRec);

  if Size > 0 then
    Stream.Size := Size
  else
    raise Exception.Create('Stream is not a valid archive');
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage(Format('Finished (%u removed bytes)', [Size - Stream.Size]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  FreeAndNil(Stream);
end;

procedure TGulpApplication.Purge(const FileName: string);
var
     LibRec : TGulpRec;
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
          I : longint;
     OffSet : int64 = 0;
     Stream : TStream;
        Tmp : TStream;
    TmpName : string;
begin
  DoMessage(GulpDescription);
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('Purge the content of %s', [FileName]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(longword(-1));
  if LibReader.Count = 0 then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage('Moving records...        ');
  TmpName   := GetTempFileName(ExtractFileDir(FileName), '');
  Tmp       := TFileStream.Create(TmpName, fmCreate);
  LibWriter := TGulpWriter.Create(Tmp);

  if LibReader.Count > 0 then
  begin
    LibWriter.WriteOffSet(0);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibRec := LibReader.Items[I];
      if LibRec.FStoredSize > 0 then
      begin
        Stream.Seek(LibRec.FOffset, soBeginning);
        LibRec.FOffSet := Tmp.Seek(0, soCurrent);
        Tmp.CopyFrom(Stream, LibRec.FStoredSize);
      end;
    end;

    OffSet := Tmp.Seek(0, soEnd);
    Tmp.Seek(0, soBeginning);
    LibWriter.WriteOffSet(OffSet);

    Tmp.Seek(0, soEnd);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibRec := LibReader.Items[I];
      LibWriter.WriteRec(LibRec);
    end;
  end;
  FreeAndNil(LibReader);
  FreeAndNil(LibWriter);
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage(Format('Finished (%u removed bytes)', [Stream.Size - Tmp.Size]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  FreeAndNil(Stream);
  FreeAndNil(Tmp);

  if DeleteFile(FileName) = FALSE then
    raise Exception.CreateFmt('Unable to delete file "%s"', [FileName])
  else
    if RenameFile(TmpName, FileName)= FALSE then
      raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);
end;

procedure TGulpApplication.List(const FileName: string);
var
      Count : longint = 0;
  LibReader : TGulpReader;
     LibRec : TGulpRec;
       I, J : longint;
     Stream : TStream;
begin
  DoMessage(GulpDescription);
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('List the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage('Scanning archive...      ');
  Stream    := TFileStream.Create (FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);
  if LibReader.Count = 0 then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

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
  {$IFDEF CONSOLEAPP} DoMessage(#13); {$ENDIF}
  DoMessage('Listing records...       ');
  for I := 0 to LibReader.Count - 1 do
  begin
    LibRec := LibReader.Items[I];
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
      begin
        DoList(LibRec);
        Inc(Count);
      end;
  end;
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  DoMessage(Format('Finished (%u listed records)', [Count]));
  {$IFDEF CONSOLEAPP} DoMessage(LineEnding); {$ENDIF}
  FreeAndNil(LibReader);
  FreeAndNil(LibRec);
  FreeAndNil(Stream);
end;

end.
