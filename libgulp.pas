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
}

{
  Contains:

    The journaling archiver library.

  Modified:

    v0.0.2 - 2015.04.13 by Melchiorre Caruso.
}

unit LibGulp;

interface

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  Classes,
  Sha1,
  SysUtils;

const
  // --- Gulp Flags ---
  gfADD          = $00000000;
  gfDEL          = $00000001;

  gfVersion      = $00000010;
  gfName         = $00000020;
  gfTime         = $00000040;
  gfAttributes   = $00000080;
  gfMode         = $00000100;
  gfSize         = $00000200;
  gfLinkName     = $00000400;
  gfUserID       = $00000800;
  gfUserName     = $00001000;
  gfGroupID      = $00002000;
  gfGroupName    = $00004000;
  gfOffSet       = $00008000;
  gfStoredSize   = $00010000;
  gfStoredDigest = $00020000;
  gfComment      = $00040000;
  gfPlatform     = $00080000;
  gfLast         = $40000000;

  // --- Gulp Methods ---
  gmGZFast       = $00000101;
  gmGZNormal     = $00000102;
  gmGZMax        = $00000103;

  // --- Gulp Platforms ---
  gpUNIX         = $00000100;
  gpMSWINDOWS    = $00000200;
  gpMAC          = $00000400;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..7] of char;

  // --- Gulp Record ---
  TGulpRec = class(TObject)
  private
    FFlags        : longword;  // Flags
    FVersion      : longword;  // File version
    FName         : string;    // File path and name
    FTime         : TDateTime; // Last modification date and time (UTC)
    FAttributes   : longint;   // File Attributes (MSWindows)
    FMode         : longint;   // File Mode (Unix)
    FSize         : int64;     // File size in bytes
    FLinkName     : string;    // Name of linked file
    FUserID       : longword;  // User ID
    FUserName     : string;    // User Name
    FGroupID      : longword;  // Group ID
    FGroupName    : string;    // Group Name
    FOffSet       : int64;     // Data offset        (reserved)
    FStoredSize   : int64;     // Stored data size   (reserved)
    FStoredDigest : string;    // Stored data digest (reserved)
    FComment      : string;    // Comment
    FPlatform     : longint;   // Platform
  public
    property Flags      : longword  read FFlags;
    property Version    : longword  read FVersion;
    property Name       : string    read FName;
    property Time       : TDateTime read FTime;
    property Attributes : longint   read FAttributes;
    property Mode       : longint   read FMode;
    property Size       : int64     read FSize;
    property LinkName   : string    read FLinkName;
    property UserID     : longword  read FUserID;
    property UserName   : string    read FUserName;
    property GroupID    : longword  read FGroupID;
    property GroupName  : string    read FGroupName;
    property Comment    : string    read FComment;
    property Platform   : longint   read FPlatform;
  end;

  // --- The Gulp Library ---
  TGulpLib = class(TObject)
  private
    FAdd         : TList;
    FCurrVersion : longword;
    FFilter      : string;
    FFullLoad    : boolean;
    FList        : TList;
    FMethod      : longword;
    FStream      : TStream;
    FStreamSize  : int64;
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddItem   (Rec: TGulpRec);
    procedure InsertItem(Rec: TGulpRec);
    procedure DeleteItem(Index: longint);
    function  FindItem  (const FileName: string): longint;

    function GetItem(Index: longint): TGulpRec;
    function GetCount: longint;

    function ReadStream (Stream: TStream; Size: int64): string;
    function WriteStream(Stream: TStream; Size: int64): string;
  public
    constructor Create(Stream: TStream);
    destructor  Destroy; override;

    function  OpenArchive(Version: longword): boolean;
    procedure CloseArchive;

    procedure Add (const FileName: string);
    function  Find(const FileName: string): longint;
    procedure ExtractTo  (Index: longint; Stream: TStream);
    procedure Extract    (Index: longint);
    procedure Delete     (Index: longint);

    property Items[Index: longint]: TGulpRec read GetItem;
    property Method: longword read FMethod   write FMethod;
    property Count:  longint  read GetCount;
  end;

  procedure FixArchive(Stream: TStream);
  procedure PurgeArchive(Source, Dest: TStream);

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
  // BlowFish,
  DateUtils,
  Math,
  ZStream;

const
  GulpMarker : TGulpMarker = ('G','U','L','P',char(0),char(0),char(2),char(0));

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

function  VerToString(var Rec: TGulpRec): string;
begin
  if (Rec.FFlags and gfVersion) = gfVersion then
    Result := IntTostr(Rec.Version)
  else
    Result := '?';
end;

function FlagToString(var Rec: TGulpRec): string;
begin
  case Rec.Flags and $F of
    gfADD: Result := 'ADD';
    gfDEL: Result := 'DEL';
    else   Result := '???';
  end;
end;

function TimeToString(var Rec: TGulpRec): string;
begin
  Result := '.......... ........';
  if (Rec.Flags and $F) in [gfADD] then
    if (Rec.Flags and gfTime) = gfTime then
      Result := FormatDateTime(
        DefaultFormatSettings.LongDateFormat + ' ' +
        DefaultFormatSettings.LongTimeFormat, Rec.Time);
end;

function SizeToString(var Rec: TGulpRec): string;
begin
  Result := '';
  if (Rec.Flags and $F) in [gfADD] then
    if (Rec.Flags and gfSize) = gfSize then
      Result := Format('%u', [Rec.Size])
end;

function AttrToString(var Rec: TGulpRec): string;
begin
  Result := '.......';
  if (Rec.Flags and $F) in [gfADD] then
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
  if (Rec.Flags and $F) in [gfADD] then
    Result := OctStr(Rec.Mode, 3)
  else
    Result := '...';
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

procedure IncludeFlag(var Flags: longword; Flag: longword); inline;
begin
  Flags := Flags or Flag;
end;

procedure ExcludeFlag(var Flags: longword; Flag: longword); inline;
begin
  Flags := Flags and (Flag xor $FFFFFFFF);
end;

function CompareRec(const Rec1, Rec2: TGulpRec): longint; inline;
begin
  Result := AnsiCompareFileName(Rec1.Name, Rec2.Name);
  if Result = 0 then
  begin
    if Rec1.Version < Rec2.Version then
      Result := - 1
    else
      if Rec1.Version > Rec2.Version then
        Result := 1;
  end;
end;

procedure ClearRec(var Rec: TGulpRec); inline;
begin
  Rec.FFlags        := 0;
  Rec.FVersion      := 0;
  Rec.FName         := '';
  Rec.FTime         := 0.0;
  Rec.FAttributes   := 0;
  Rec.FMode         := 0;
  Rec.FSize         := 0;
  Rec.FLinkName     := '';
  Rec.FUserID       := 0;
  Rec.FUserName     := '';
  Rec.FGroupID      := 0;
  Rec.FGroupName    := '';
  Rec.FOffSet       := 0;
  Rec.FStoredSize   := 0;
  Rec.FStoredDigest := '';
  Rec.FComment      := '';
  Rec.FPlatform     := 0;
end;

function DigestRec(const Rec: TGulpRec): string; inline;
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

function ReadRec(Source: TStream; var Rec: TGulpRec): boolean; inline;
var
  Marker : TGulpMarker;
begin
  FillChar   (Marker, SizeOf(Marker), 0);
  Source.Read(Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    ClearRec(Rec);
    Source.Read(Rec.FFlags, SizeOf(Rec.FFlags));

    if gfVersion      and Rec.Flags <> 0 then Source.Read(Rec.FVersion, SizeOf(Rec.FVersion));
    if gfName         and Rec.Flags <> 0 then Rec.FName := Source.ReadAnsiString;

    if gfTime         and Rec.Flags <> 0 then Source.Read(Rec.FTime, SizeOf(Rec.FTime));
    if gfAttributes   and Rec.Flags <> 0 then Source.Read(Rec.FAttributes, SizeOf(Rec.FAttributes));
    if gfMode         and Rec.Flags <> 0 then Source.Read(Rec.FMode, SizeOf(Rec.FMode));
    if gfSize         and Rec.Flags <> 0 then Source.Read(Rec.FSize, SizeOf(Rec.FSize));

    if gfLinkName     and Rec.Flags <> 0 then Rec.FLinkName := Source.ReadAnsiString;

    if gfUserID       and Rec.Flags <> 0 then Source.Read(Rec.FUserID, SizeOf(Rec.FUserID));
    if gfUserName     and Rec.Flags <> 0 then Rec.FUserName := Source.ReadAnsiString;

    if gfGroupID      and Rec.Flags <> 0 then Source.Read(Rec.FGroupID, SizeOf(Rec.FGroupID));
    if gfGroupName    and Rec.Flags <> 0 then Rec.FGroupName := Source.ReadAnsiString;

    if gfOffSet       and Rec.Flags <> 0 then Source.Read(Rec.FOffSet, SizeOf(Rec.FOffSet));
    if gfStoredSize   and Rec.Flags <> 0 then Source.Read(Rec.FStoredSize, SizeOf(Rec.FStoredSize));

    if gfStoredDigest and Rec.Flags <> 0 then Rec.FStoredDigest := Source.ReadAnsiString;
    if gfComment      and Rec.Flags <> 0 then Rec.FComment      := Source.ReadAnsiString;

    if gfPlatform     and Rec.Flags <> 0 then Source.Read(Rec.FPlatform, SizeOf(Rec.FPlatform));

    Result := Source.ReadAnsiString = DigestRec(Rec);

    DoDirSeparators(Rec.FLinkName);
    DoDirSeparators(Rec.FName    );
  end;
end;

function WriteRec(Dest: TStream; const Rec: TGulpRec): boolean; inline;
begin
  Dest.Write(GulpMarker, SizeOf(GulpMarker));
  Dest.Write(Rec.FFlags, SizeOf(Rec.FFlags));

  if gfVersion      and Rec.Flags <> 0 then Dest.Write(Rec.FVersion, SizeOf(Rec.FVersion));
  if gfName         and Rec.Flags <> 0 then Dest.WriteAnsiString(Rec.FName);

  if gfTime         and Rec.Flags <> 0 then Dest.Write(Rec.FTime, SizeOf(Rec.FTime));
  if gfAttributes   and Rec.Flags <> 0 then Dest.Write(Rec.FAttributes, SizeOf(Rec.FAttributes));
  if gfMode         and Rec.Flags <> 0 then Dest.Write(Rec.FMode, SizeOf(Rec.FMode));
  if gfSize         and Rec.Flags <> 0 then Dest.Write(Rec.FSize, SizeOf(Rec.FSize));

  if gfLinkName     and Rec.Flags <> 0 then Dest.WriteAnsiString(Rec.FLinkName);

  if gfUserID       and Rec.Flags <> 0 then Dest.Write(Rec.FUserID, SizeOf(Rec.FUserID));
  if gfUserName     and Rec.Flags <> 0 then Dest.WriteAnsiString(Rec.FUserName);

  if gfGroupID      and Rec.Flags <> 0 then Dest.Write(Rec.FGroupID, SizeOf(Rec.FGroupID));
  if gfGroupName    and Rec.Flags <> 0 then Dest.WriteAnsiString(Rec.FGroupName);

  if gfOffSet       and Rec.Flags <> 0 then Dest.Write(Rec.FOffSet, SizeOf(Rec.FOffSet));
  if gfStoredSize   and Rec.Flags <> 0 then Dest.Write(Rec.FStoredSize, SizeOf(Rec.FStoredSize));

  if gfStoredDigest and Rec.Flags <> 0 then Dest.WriteAnsiString(Rec.FStoredDigest);
  if gfComment      and Rec.Flags <> 0 then Dest.WriteAnsiString(Rec.FComment);

  if gfPlatform     and Rec.Flags <> 0 then Dest.Write(Rec.FPlatform, SizeOf(Rec.FPlatform));

  Dest.WriteAnsiString(DigestRec(Rec));
  Result := TRUE;
end;

function ReadOffSet(Stream: TStream; var OffSet: int64): boolean; inline;
var
  Context : TSHA1Context;
   Digest : TSHA1Digest;
   Marker : TGulpMarker;
begin
  FillChar   (Marker, SizeOf(Marker), 0);
  Stream.Read(Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    SHA1Init   (Context);
    Stream.Read(         OffSet, SizeOf(OffSet));
    SHA1Update (Context, OffSet, SizeOf(OffSet));
    SHA1Final  (Context, Digest);

    Result := Stream.ReadAnsiString = SHA1Print(Digest);
  end;
end;

function WriteOffSet(Stream: TStream; const OffSet: int64): boolean; inline;
var
  Context : TSHA1Context;
   Digest : TSHA1Digest;
begin
  Stream.Write(GulpMarker, SizeOf(GulpMarker));

  SHA1Init    (Context);
  Stream.Write(         OffSet, SizeOf(OffSet));
  SHA1Update  (Context, OffSet, SizeOf(OffSet));
  SHA1Final   (Context, Digest);

  Stream.WriteAnsiString(SHA1Print(Digest));
  Result := TRUE;
end;

// =============================================================================
// TGulpLib
// =============================================================================

constructor TGulpLib.Create(Stream: TStream);
begin
  inherited Create;
  FAdd         := TList.Create;
  FList        := TList.Create;
  FCurrVersion :=  0;
  FMethod      :=  0;
  FStreamSize  := -1;
  FStream      := Stream;
  FFullLoad    := FALSE;
end;

destructor TGulpLib.Destroy;
begin
  CloseArchive;
  FAdd.Destroy;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpLib.BeginUpdate;
begin
  if FStreamSize = -1 then
  begin
    FStreamSize := FStream.Seek(0, soEnd);
    WriteOffSet(FStream, 0);
  end;
end;

procedure TGulpLib.EndUpdate;
var
     I : longint;
   Rec : TGulpRec;
  Size : int64;
begin
  if FStreamSize <> -1 then
  begin
    Size := FStream.Seek(0, soEnd);
    FStream.Seek(FStreamSize, soBeginning);
    WriteOffSet (FStream, Size);

    FStream.Seek(0, soEnd);
    for I := 0 to FAdd.Count - 1 do
    begin
      Rec := TGulpRec(FAdd[I]);
      if I <> FAdd.Count - 1 then
        ExcludeFlag(Rec.FFlags, gfLast)
      else
        IncludeFlag(Rec.FFlags, gfLast);
      WriteRec(FStream, Rec);
    end;
    FStreamSize := -1;
  end;
end;

function TGulpLib.WriteStream(Stream: TStream; Size: int64): string;
var
   Buffer : array[0..$FFFF] of byte;
  Context : TSHA1Context;
   Digest : TSHA1Digest;
   Readed : longint;
  ZStream : TStream;
begin
  FStream.Write(FMethod, SizeOf(FMethod));
  FStream.WriteAnsiString(FFilter);

  case FMethod and $0000FF00 of
    $0000: ZStream := FStream;
    $0100: ZStream := TCompressionStream.Create(
         TCompressionLevel(Method and $000000FF), FStream, FALSE);
  else Exception.Create('Internal error');
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

  case FMethod and $0000FF00 of
    $0100: FreeAndNil(ZStream);
    $0000: ZStream := nil;
  end;
  Result := SHA1Print(Digest);
end;

function TGulpLib.ReadStream(Stream: TStream; Size: int64): string;
var
     Buffer : array[0..$FFFF] of byte;
    Context : TSHA1Context;
     Digest : TSHA1Digest;
     Readed : longint;
    ZStream : TStream;
begin
  FillChar    (FMethod, SizeOf(FMethod), 0);
  FStream.Read(FMethod, SizeOf(FMethod));
  FFilter := FStream.ReadAnsiString;

  case FMethod and $0000FF00 of
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

procedure TGulpLib.AddItem(Rec: TGulpRec);
begin
  if FFullLoad = TRUE then
    FList.Add(Rec)
  else
    InsertItem(Rec);
end;

procedure TGulpLib.InsertItem(Rec: TGulpRec);
var
  L, M, H, I: longint;
begin
  L := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := CompareRec(Items[M], Rec);
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

procedure TGulpLib.DeleteItem(Index: longint);
begin
  Items[Index].Destroy;
  FList.Delete(Index);
end;

function TGulpLib.FindItem(const FileName: string): longint;
var
  L, M, H, I: longint;
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

function TGulpLib.OpenArchive(Version: longword): boolean;
var
       I : longint;
     Rec : TGulpRec;
  OffSet : int64 = 0;
    Size : int64 = 0;
begin
  Result := FALSE;

  CloseArchive;
  FFullLoad := Version = longword(-1);
  FStream.Seek(0, soBeginning);
  if ReadOffSet(FStream, OffSet) = TRUE then
  begin
    if OffSet = 0 then
      raise Exception.Create('Archive is damaged, try with the "fix" command');
    FStream.Seek(OffSet, soBeginning)
  end;

  Rec := TGulpRec.Create;
  while ReadRec(FStream, Rec) = TRUE do
  begin
    Result := (Rec.Flags and gfLast) = gfLast;
    if Result then
    begin
      Size := FStream.Seek(0, soCurrent);
      if ReadOffSet(FStream, OffSet) = TRUE then
      begin
        if OffSet = 0 then
          raise Exception.Create('Archive is damaged, try with the "fix" command');
        FStream.Seek(OffSet, soBeginning)
      end;
    end;

    FCurrVersion := Max(FCurrVersion, Rec.Version);
    if FFullLoad = TRUE then
    begin
      AddItem(Rec);
      Rec := TGulpRec.Create;
    end else
      if Rec.Version <= Version then
        case (Rec.Flags and $F) of
          gfADD: begin
            AddItem(Rec);
            Rec := TGulpRec.Create;
          end;
          gfDEL: begin
            I := Find(Rec.Name);
            if I <> - 1 then DeleteItem(I);
          end;
        else raise Exception.CreateFmt('Mismatched flags for "%s"', [Rec.Name]);
        end;
  end;
  FreeAndNil(Rec);

  if Result = TRUE then
    Result := FStream.Seek(0, soEnd) = Size;
end;

procedure TGulpLib.CloseArchive;
var
  I : longint;
begin
  EndUpdate;
  for I := 0 to FAdd .Count - 1 do
    TGulpRec(FAdd [I]).Destroy;
  FAdd.Clear;

  for I := 0 to FList.Count - 1 do
    TGulpRec(FList[I]).Destroy;
  FList.Clear;

  FCurrVersion :=  0;
  FStreamSize  := -1;
  FFullLoad    := FALSE;
end;

procedure TGulpLib.Add(const FileName: string);
var
     Rec : TGulpRec;
      SR : TSearchRec;
  Stream : TStream;
begin
  if FFullLoad = TRUE then
    raise Exception.Create('Internal error');
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
    if GetAttr(SR) and (faSysFile or faVolumeId) = 0 then
    begin
      BeginUpdate;
      Rec := TGulpRec.Create; ClearRec(Rec);

      IncludeFlag(Rec.FFlags, gfADD);
      IncludeFlag(Rec.FFlags, gfVersion);      Rec.FVersion    := FCurrVersion + 1;
      IncludeFlag(Rec.FFlags, gfName);         Rec.FName       := FileName;
      IncludeFlag(Rec.FFlags, gfTime);         Rec.FTime       := GetTime(SR);
      IncludeFlag(Rec.FFlags, gfAttributes);   Rec.FAttributes := GetAttr(SR);
      {$IFDEF UNIX}
        IncludeFlag(Rec.FFlags, gfMode    );   Rec.FMode       := GetMode(FileName);
        IncludeFlag(Rec.FFlags, gfUserID  );   Rec.FUserID     := GetUID (FileName);
        IncludeFlag(Rec.FFlags, gfGroupID );   Rec.FGroupID    := GetGID (FileName);
        IncludeFlag(Rec.FFlags, gfPlatform);   Rec.FPlatform   := gpUNIX;
      {$ELSE}
        {$IFDEF MSWINDOWS}
          IncludeFlag(Rec.FFlags, gfPlatform); Rec.FPlatform   := gpMSWINDOWS;
        {$ELSE}
          Unsupported platform...
        {$ENDIF}
      {$ENDIF}

      {$IFDEF UNIX}
        if Rec.FAttributes and faSymLink = faSymLink then
        begin
          IncludeFlag(Rec.FFlags, gfLinkName); Rec.FLinkName   := fpReadLink(FileName);
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
              Stream := TFileStream.Create(Rec.FName, fmOpenRead or fmShareDenyNone);
              IncludeFlag(Rec.FFlags, gfSize);           Rec.FSize         := GetSize(SR);
              IncludeFlag(Rec.FFlags, gfOffSet);         Rec.FOffSet       := FStream.Seek(0, soCurrent);
              IncludeFlag(Rec.FFlags, gfStoredDigest);   Rec.FStoredDigest := WriteStream (Stream, Rec.FSize);
              IncludeFlag(Rec.FFlags, gfStoredSize);     Rec.FStoredSize   := FStream.Seek(0, soCurrent) - Rec.FOffSet;
              FreeAndNil(Stream);
            end;

      FAdd.Add(Rec);
    end;
  SysUtils.FindClose(SR);
end;

procedure TGulpLib.Delete(Index: longint);
var
  Rec : TGulpRec;
begin
  if FFullLoad = TRUE then
    raise Exception.Create('Internal error');
  BeginUpdate;
  Rec := TGulpRec.Create; ClearRec(Rec);
  IncludeFlag(Rec.FFlags, gfDEL);
  IncludeFlag(Rec.FFlags, gfVersion);  Rec.FVersion := FCurrVersion + 1;
  IncludeFlag(Rec.FFlags, gfName);     Rec.FName    := Items[Index].Name;
  FAdd.Add(Rec);
end;

function TGulpLib.Find(const FileName: string): longint;
var
  I : longint;
begin
  if FFullLoad = TRUE then
  begin
    Result := -1;
    for I  := FList.Count - 1 downto 0 do
      if AnsiCompareFileName(FileName, Items[I].Name) = 0 then
      begin
        Result := I;
        Break;
      end;
  end else
    Result := FindItem(FileName);
end;

procedure TGulpLib.ExtractTo(Index: longint; Stream: TStream);
var
  Rec : TGulpRec;
begin
  Rec := Items[Index];
  if Rec.FSize > 0 then
  begin
    FStream.Seek(Rec.FOffset, soBeginning);
    if ReadStream(Stream, Rec.FSize) <> Rec.FStoredDigest then
      raise Exception.CreateFmt('Mismatched checksum for "%s"', [Rec.FName]);
  end;
end;

procedure TGulpLib.Extract(Index: longint);
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

function TGulpLib.GetItem(Index: longint): TGulpRec;
begin
  Result := TGulpRec(FList[Index]);
end;

function TGulpLib.GetCount: longint;
begin
  Result := FList.Count;
end;

// =============================================================================
// Advanced routines
// =============================================================================

procedure FixArchive(Stream: TStream);
var
  OffSet : int64 = 0;
     Rec : TGulpRec;
    Size : int64 = 0;
begin
  Stream.Seek(0, soBeginning);
  if ReadOffSet(Stream, OffSet) = TRUE then
  begin
    if OffSet <> 0 then
      Stream.Seek(OffSet, soBeginning);
  end;

  Rec := TGulpRec.Create;
  try
    while ReadRec(Stream, Rec) = TRUE do
      if (Rec.FFlags and gfLast) = gfLast then
      begin
        Size := Stream.Seek(0, soCurrent);
        if ReadOffSet(Stream, OffSet) = TRUE then
        begin
          if OffSet <> 0 then
            Stream.Seek(OffSet, soBeginning);
        end;
      end;
  except
    // nothing to do
  end;
  FreeAndNil(Rec);

  if Size > 0 then
    Stream.Size := Size
  else
    raise Exception.Create('Stream is not a valid archive');
end;

procedure PurgeArchive(Source, Dest: TStream);
var
       I : longint;
     Lib : TGulpLib;
  OffSet : int64 = 0;
     Rec : TGulpRec;
begin
  Dest.Seek(0, soBeginning);

  Lib:= TGulpLib.Create(Source);
  if Lib.OpenArchive(longword(-2)) = FALSE then
  begin
    if Source.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

  WriteOffSet(Dest, 0);
  for I := 0 to Lib.Count - 1 do
  begin
    Rec := Lib.Items[I];
    if Rec.FStoredSize > 0 then
    begin
      Source.Seek (Rec.FOffset, soBeginning);
      Rec.FOffSet := Dest.Seek(0, soCurrent);
      Dest.CopyFrom(Source, Rec.FStoredSize);
    end;
    Rec.FVersion := 1;
  end;

  OffSet := Dest.Seek(0, soEnd);
  Dest.Seek(0, soBeginning);
  WriteOffSet(Dest, OffSet);

  Dest.Seek(0, soEnd);
  for I := 0 to Lib.Count - 1 do
  begin
    Rec := Lib.Items[I];
    if I <> Lib.Count - 1 then
      ExcludeFlag(Rec.FFlags, gfLast)
    else
      IncludeFlag(Rec.FFlags, gfLast);
    WriteRec(Dest, Rec);
  end;
  FreeAndNil(Lib);
end;

end.
