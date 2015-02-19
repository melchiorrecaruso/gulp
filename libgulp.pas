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

    v0.0.2 - 2015.02.14 by Melchiorre Caruso.
}

unit LibGulp;

interface

uses
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes,
  SysUtils,
  SHA1;

const
  // --- Gulp Flags ---
  gfFIX        = $00000000;
  gfADD        = $00000001;
  gfDEL        = $00000002;

  gfFlags      = $00000100;
  gfVersion    = $00000200;
  gfName       = $00000400;
  gfTime       = $00000800;
  gfAttributes = $00001000;
  gfMode       = $00002000;
  gfSize       = $00004000;
  gfLinkName   = $00008000;
  gfUserID     = $00010000;
  gfUserName   = $00020000;
  gfGroupID    = $00040000;
  gfGroupName  = $00080000;
  gfOffSet     = $00100000;
  gfStoredSize = $00200000;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..9] of char;

  // --- Gulp Record CLASS ---
  TGulpRec = class(TObject)
  private
    FFlags      : longword;   // Flags
    FVersion    : longword;   // File version
    FName       : ansistring; // File path and name
    FTime       : TDateTime;  // Last modification date and time (UTC)
    FAttributes : longint;    // File Attributes (Windows)
    FMode       : longword;   // File Mode (Unix)
    FSize       : int64;      // File size in bytes
    FLinkName   : ansistring; // Name of linked file
    FUserID     : longword;   // User ID
    FUserName   : ansistring; // User Name
    FGroupID    : longword;   // Group ID
    FGroupName  : ansistring; // Group Name
                              // Rec SHA digest
    FOffSet     : int64;      // Data stream offset
    FStoredSize : int64;      // Data stream size
                              // Data SHA digest
  public
    property Flags      : longword   read FFlags;
    property Version    : longword   read FVersion;
    property Name       : ansistring read FName;
    property Time       : TDateTime  read FTime;
    property Attributes : longint    read FAttributes;
    property Mode       : longword   read FMode;
    property Size       : int64      read FSize;
    property LinkName   : ansistring read FLinkName;
    property UserID     : longword   read FUserID;
    property UserName   : ansistring read FUserName;
    property GroupID    : longword   read FGroupID;
    property GroupName  : ansistring read FGroupName;
    property Offset     : int64      read FOffSet;
    property StoredSize : int64      read FStoredSize;
  end;

  // --- The Gulp Stream CLASS ---
  TGulpStream = class(TObject)
  protected
    FCTX        : TSHA1Context;
    FStream     : TStream;
    FStreamSize : int64;
  public
    constructor Create(Stream: TStream);
    destructor  Destroy; override;
    function Read(var Buffer: string): longint; overload;
    function Read(var Buffer; Count: longint): longint; overload;
    function Read(Rec: TGulpRec; Stream: TStream; Size: int64): boolean; overload;
    function Read(Rec: TGulpRec): boolean; overload;
    function ReadOffSet(var OffSet: int64): boolean; overload;
    function Write(const Buffer: string): longint; overload;
    function Write(const Buffer; Count: longint): longint; overload;
    function Write(Rec: TGulpRec; Stream: TStream; Size: int64): boolean; overload;
    function Write(Rec: TGulpRec): boolean; overload;
    function WriteOffSet(const OffSet: int64): boolean; overload;
  end;

  // --- The Gulp Lib CLASS ---
  TGulpLib = class(TGulpStream)
  private
    FAdd         : TList;
    FList        : TList;
    FFullLoad    : boolean;
    FCurrVersion : longword;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AddItem(Rec: TGulpRec);
    procedure InsertItem(Rec: TGulpRec);
    procedure DeleteItem(Index: longint);
    function  FindItem(const FileName: string): longint;
    function GetItem(Index: longint): TGulpRec;
    function GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor  Destroy; override;

    function  OpenArchive(Version: longword): boolean;
    procedure CloseArchive;

    procedure Add(const FileName: string);
    procedure Delete(Index: longint);
    function  Find(const FileName: string): longint;
    procedure ExtractTo(Index: longint; Stream: TStream);
    procedure Extract(Index: longint);

    property Items[Index: longint]: TGulpRec read GetItem;
    property Count: longint read GetCount;
  end;

  procedure FixArchive(Stream: TStream);
  procedure PurgeArchive(Source, Dest: TStream);

// --- Some useful routines ---

function GetName(const FileName: string): string;
function GetTime(const FileName: string): TDateTime; overload;
function GetTime(var   SR: TSearchRec  ): TDateTime; overload;
function GetSize(const FileName: string): int64;     overload;
function GetSize(var   SR: TSearchRec  ): int64;     overload;
function GetAttr(const FileName: string): longint;   overload;
function GetAttr(var   SR: TSearchRec  ): longint;   overload;

{$IFDEF UNIX}
function GetMode(const FileName: string): longword;  overload;
function GetMode(var   Info: stat      ): longword;  overload;
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
  Math;

const
  GulpMarker : TGulpMarker = ('G', 'U','L','P','/', '0', '0', '2',' ',' ');

// =============================================================================
// Library routines
// =============================================================================

{$IFDEF MSWINDOWS}
function GetName(const FileName: string): string;
begin
  Result := StringReplace(Filename, '/', '\', [rfReplaceAll]);
end;
{$ENDIF}

{$IFDEF UNIX}
function GetName(const FileName: string): string;
begin
  Result := StringReplace(FileName, '\', '/', [rfReplaceAll]);
end;
{$ENDIF}

function GetTime(var SR: TSearchRec): TDateTime;
begin
  Result := LocalTimeToUniversal(FileDateToDateTime(SR.Time));
end;

function GetTime(const FileName: string): TDateTime;
var
  SR: TSearchRec;
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
  SR: TSearchRec;
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
  SR: TSearchRec;
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
function GetMode(const FileName: string): longword;
var
  Info: stat;
begin
  Result := 0;
  if fpLstat (FileName, Info) = 0 then
    Result := GetMode(Info)
  else
    if fpstat (FileName, Info) = 0 then
      Result := GetMode(Info);
end;

function GetMode(var Info: stat): longword;
begin
  Result := Info.st_mode;
end;

function GetUID(const FileName: string): longword;
var
  Info: stat;
begin
  Result := longword(-1);
  if fpLstat (FileName, @Info) = 0 then
    Result := GetUID(Info)
  else
    if fpstat (FileName, Info) = 0 then
      Result := GetUID(Info);
end;

function GetUID(var Info: stat): longword;
begin
  Result := Info.st_uid;
end;

function GetGID(const FileName: string): longword;
var
  Info: stat;
begin
  Result := longword(-1);
  if fpLstat (FileName, @Info) = 0 then
    Result := GetGID(Info)
  else
    if fpstat (FileName, Info) = 0 then
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
  case Rec.Flags and $FF of
    gfFIX: Result := 'FIX';
    gfADD: Result := 'ADD';
    gfDEL: Result := 'DEL';
    else   Result := '???';
  end;
end;

function TimeToString(var Rec: TGulpRec): string;
begin
  if Rec.Flags and $FF in [gfADD] then
    Result := FormatDateTime(
      DefaultFormatSettings.LongDateFormat + ' ' +
      DefaultFormatSettings.LongTimeFormat, Rec.Time)
  else
    Result := '.......... ........';
end;

function SizeToString(var Rec: TGulpRec): string;
begin
  Result := '';
  if (Rec.Flags and $FF) in [gfADD] then
    if (Rec.Flags and gfSize) = gfSize then
    begin
      Result := Format('%u', [Rec.Size])
    end;
end;

function AttrToString(var Rec: TGulpRec): string;
begin
  Result := '.......';
  if Rec.Flags and $FF in [gfADD] then
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

{$IFDEF MSWINDOWS}
function ModeToString(var Rec: TGulpRec): string;
begin
  Result := '.........';
end;

function StringToMode(const S: string): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF UNIX}
function ModeToString(var Rec: TGulpRec): string;
begin
  Result := '...';
  if Rec.Flags and $FF in [gfADD] then
  begin
    Result := OctStr(Rec.Mode, 3);
  end;
end;

function StringToMode(const S: string): longint;
var
  I : longint;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    Result := Result * 8 + StrToInt(Copy(S, I, 1));
  end;
end;
{$ENDIF}

// =============================================================================
// Internal rutines
// =============================================================================

procedure Clear_(var Rec: TGulpRec);
begin
  Rec.FFlags      := 0;
  Rec.FVersion    := 0;
  Rec.FName       := '';
  Rec.FTime       := 0.0;
  Rec.FAttributes := 0;
  Rec.FMode       := 0;
  Rec.FSize       := 0;
  Rec.FLinkName   := '';
  Rec.FUserID     := 0;
  Rec.FUserName   := '';
  Rec.FGroupID    := 0;
  Rec.FGroupName  := '';
  Rec.FOffSet     := 0;
  Rec.FStoredSize := 0;
end;

procedure Include_(var Flags: longword; Flag: longword);
begin
  Flags := Flags or Flag;
end;

procedure Exclude_(var Flags: longword; Flag: longword);
begin
  Flags := Flags and (Flag xor $FFFFFFFF);
end;

function Compare_(Item1, Item2: TGulpRec): longint;
begin
  Result := AnsiCompareFileName(Item1.Name, Item2.Name);
  if Result = 0 then
  begin
    if Item1.Version < Item2.Version then
      Result := - 1
    else
      if Item1.Version > Item2.Version then
        Result := 1;
  end;
end;

// =============================================================================
// TGulpStream
// =============================================================================

constructor TGulpStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream     := Stream;
  FStreamSize := -1;
end;

destructor TGulpStream.Destroy;
begin
  inherited Destroy;
end;

function TGulpStream.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
  SHA1Update(FCTX, Buffer, Result);
end;

function TGulpStream.Read(var Buffer: string): longint;
begin
  Read(Result, SizeOf(Result));
  SetLength(Buffer, Result);
  Read(Pointer(Buffer)^, Result);
end;

function TGulpStream.Read(Rec: TGulpRec): boolean;
var
     Marker : TGulpMarker;
     Digest : TSHA1Digest;
  AuxDigest : TSHA1Digest;
begin
  Clear_(Rec);
  SHA1Init (FCTX);
  FillChar (Marker, SizeOf(Marker), 0);
  Read     (Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    Read (Rec.FFlags, SizeOf (Rec.FFlags));

    if gfVersion    and Rec.Flags <> 0 then Read(Rec.FVersion,    SizeOf(Rec.FVersion));
    if gfName       and Rec.Flags <> 0 then Read(Rec.FName);
    if gfTime       and Rec.Flags <> 0 then Read(Rec.FTime,       SizeOf(Rec.FTime));
    if gfAttributes and Rec.Flags <> 0 then Read(Rec.FAttributes, SizeOf(Rec.FAttributes));
    if gfMode       and Rec.Flags <> 0 then Read(Rec.FMode,       SizeOf(Rec.FMode));
    if gfSize       and Rec.Flags <> 0 then Read(Rec.FSize,       SizeOf(Rec.FSize));
    if gfLinkName   and Rec.Flags <> 0 then Read(Rec.FLinkName);
    if gfUserID     and Rec.Flags <> 0 then Read(Rec.FUserID,     SizeOf(Rec.FUserID));
    if gfUserName   and Rec.Flags <> 0 then Read(Rec.FUserName);
    if gfGroupID    and Rec.Flags <> 0 then Read(Rec.FGroupID,    SizeOf(Rec.FGroupID));
    if gfGroupName  and Rec.Flags <> 0 then Read(Rec.FGroupName);
    if gfOffSet     and Rec.Flags <> 0 then Read(Rec.FOffSet,     SizeOf(Rec.FOffSet));
    if gfStoredSize and Rec.Flags <> 0 then Read(Rec.FStoredSize, SizeOf(Rec.FStoredSize));

    Rec.FName := GetName(Rec.FName);

    SHA1Final(FCTX, Digest);
    FillChar(AuxDigest, SizeOf(AuxDigest), 0);
    FStream.Read(AuxDigest, SizeOf(AuxDigest));
    Result := SHA1Match(AuxDigest, Digest);
  end;
end;

function TGulpStream.Read(Rec: TGulpRec; Stream: TStream; Size: int64): boolean;
var
     Buffer : array[0..$FFFF] of byte;
     Readed : longint;
     Digest : TSHA1Digest;
  AuxDigest : TSHA1Digest;
begin
  SHA1Init(FCTX);
  while Size > 0 do
  begin
    Readed := Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.CreateFmt('Unable to read file "%s"', [Rec.Name]);

    Stream.Write(Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(FCTX, Digest);
  FillChar(AuxDigest, SizeOf(AuxDigest), 0);
  FStream.Read(AuxDigest, SizeOf(AuxDigest));
  Result := SHA1Match(AuxDigest, Digest);
end;

function TGulpStream.ReadOffSet(var OffSet: int64): boolean;
var
     Marker : TGulpMarker;
     Digest : TSHA1Digest;
  AuxDigest : TSHA1Digest;
begin
  SHA1Init(FCTX);
  FillChar(Marker, SizeOf(Marker), 0);
  Read    (Marker, SizeOf(Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    Read(OffSet, SizeOf(OffSet));

    SHA1Final(FCTX, Digest);
    FillChar(AuxDigest, SizeOf(AuxDigest), 0);
    FStream.Read(AuxDigest, SizeOf(AuxDigest));
    Result := SHA1Match(AuxDigest, Digest);
  end;
end;

function TGulpStream.Write(const Buffer; Count: longint): longint;
begin
  Result := FStream.Write(Buffer, Count);
  SHA1Update(FCTX, Buffer, Result);
end;

function TGulpStream.Write(const Buffer: string): longint;
begin
  Result := Length(Buffer);
  Write(Result, SizeOf(Result));
  Write(Pointer(Buffer)^, Result);
end;

function TGulpStream.Write(Rec: TGulpRec): boolean;
var
  Digest : TSHA1Digest;
begin
  Result := TRUE;
  SHA1Init(FCTX);
  Write   (GulpMarker, SizeOf(GulpMarker));
  Write   (Rec.FFlags, SizeOf(Rec.FFlags));

  if gfVersion    and Rec.Flags <> 0 then Write(Rec.FVersion,    SizeOf(Rec.FVersion));
  if gfName       and Rec.Flags <> 0 then Write(Rec.FName);
  if gfTime       and Rec.Flags <> 0 then Write(Rec.FTime,       SizeOf(Rec.FTime));
  if gfAttributes and Rec.Flags <> 0 then Write(Rec.FAttributes, SizeOf(Rec.FAttributes));
  if gfMode       and Rec.Flags <> 0 then Write(Rec.FMode,       SizeOf(Rec.FMode));
  if gfSize       and Rec.Flags <> 0 then Write(Rec.FSize,       SizeOf(Rec.FSize));
  if gfLinkName   and Rec.Flags <> 0 then Write(Rec.FLinkName);
  if gfUserID     and Rec.Flags <> 0 then Write(Rec.FUserID,     SizeOf(Rec.FUserID));
  if gfUserName   and Rec.Flags <> 0 then Write(Rec.FUserName);
  if gfGroupID    and Rec.Flags <> 0 then Write(Rec.FGroupID,    SizeOf(Rec.FGroupID));
  if gfGroupName  and Rec.Flags <> 0 then Write(Rec.FGroupName);
  if gfOffSet     and Rec.Flags <> 0 then Write(Rec.FOffSet,     SizeOf(Rec.FOffSet));
  if gfStoredSize and Rec.Flags <> 0 then Write(Rec.FStoredSize, SizeOf(Rec.FStoredSize));

  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
end;

function TGulpStream.Write(Rec: TGulpRec; Stream: TStream; Size: int64): boolean;
var
  Buffer : array[0..$FFFF] of byte;
  Readed : longint;
  Digest : TSHA1Digest;
begin
  Result      := TRUE;
  Rec.FOffSet := FStream.Seek(0, soCurrent);
  SHA1Init(FCTX);
  while Size > 0 do
  begin
    Readed := Stream.Read (Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.CreateFmt('Unable to write record "%s"', [Rec.Name]);

    Write(Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
  Rec.FStoredSize := FStream.Seek(0, soCurrent) - Rec.FOffSet;
end;

function TGulpStream.WriteOffSet(const OffSet: int64): boolean;
var
  Digest : TSHA1Digest;
begin
  Result := TRUE;

  SHA1Init(FCTX);
  Write(GulpMarker, SizeOf(GulpMarker));
  Write(OffSet,     SizeOf(OffSet));
  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
end;

// =============================================================================
// TGulpLib
// =============================================================================

constructor TGulpLib.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FAdd         := TList.Create;
  FList        := TList.Create;
  FFullLoad    := FALSE;
  FCurrVersion := 0;
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
    WriteOffSet(FStreamSize);
  end;
end;

procedure TGulpLib.EndUpdate;
var
     I : longint;
   Rec : TGulpRec;
  Size : int64;
begin
  if not (FStreamSize = -1) then
  begin
    Size := FStream.Seek(0, soEnd);
    FStream.Seek(FStreamSize, soBeginning);
    WriteOffSet(Size);

    Rec := TGulpRec.Create;
    Clear_(Rec);

    Include_(Rec.FFlags, gfFIX);
    Include_(Rec.FFlags, gfVersion);
    Rec.FVersion := FCurrVersion + 1;
    FAdd.Add(Rec);

    FStream.Seek(0, soEnd);
    for I := 0 to FAdd.Count - 1 do
      Write(TGulpRec(FAdd[I]));
    FStreamSize := -1;
  end;
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
    I := Compare_(Items[M], Rec);
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
  if ReadOffSet(OffSet) = TRUE then
    FStream.Seek(OffSet, soBeginning);

  Rec := TGulpRec.Create;
  while Read(Rec) = TRUE do
  begin
    Result := (Rec.Flags and $FF) = gfFIX;
    if Result then
    begin
      Size := FStream.Seek(0, soCurrent);
      if ReadOffSet(OffSet) = TRUE then
        FStream.Seek(OffSet, soBeginning);
    end;

    FCurrVersion := Max(FCurrVersion, Rec.Version);
    if FFullLoad = TRUE then
    begin
      AddItem(Rec);
      Rec := TGulpRec.Create;
    end else
      if Rec.Version <= Version then
        case Rec.Flags and $FF of
          gfADD: begin
            AddItem(Rec);
            Rec := TGulpRec.Create;
          end;
          gfDEL: begin
            I := Find(Rec.Name);
            if I <> - 1 then DeleteItem(I);
          end;
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

  FFullLoad    := FALSE;
  FCurrVersion :=  0;
  FStreamSize  := -1;
end;

{$IFDEF MSWINDOWS}
procedure TGulpLib.Add(const FileName: string);
var
  Rec : TGulpRec;
  SR  : TSearchRec;
begin
  if FFullLoad = TRUE then
    raise Exception.Create('Internal error');

  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId  or
    faDirectory or faArchive or faSymLink or faAnyFile,  SR) = 0 then
  begin
    BeginUpdate;

    Rec := TGulpRec.Create;
    Clean(Rec);

    IncludeFlag(Rec.FFlags, gfADD  );
    IncludeFlag(Rec.FFlags, gfVer  );  Rec.FVer   := FVersion + 1;
    IncludeFlag(Rec.FFlags, gfName );  Rec.FName  := FileName;

    IncludeFlag(Rec.FFlags, gfMTime);  Rec.FMTime := GetTime(SR);
    IncludeFlag(Rec.FFlags, gfAttr );  Rec.FAttr  := GetAttr(SR);
    (*
    Stream := nil;
    if SR.Attr and (faDirectory or faVolumeId or faSymLink) = 0 then
      try
        Stream := TFileStream.Create(FileName, fmOpenRead);

        IncludeFlag(Rec.FFlags, gfSize);  Rec.FSize := SR.Size;
      except
        Stream := nil;
      end;
    *)
    FToDo.Add(Rec);
  end;
  SysUtils.FindClose(SR);
end;
{$ENDIF}

{$IFDEF UNIX}
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
  begin
    BeginUpdate;

    Rec := TGulpRec.Create;
    Clear_(Rec);

    Include_(Rec.FFlags, gfADD);
    Include_(Rec.FFlags, gfVersion);     Rec.FVersion    := FCurrVersion + 1;
    Include_(Rec.FFlags, gfName);        Rec.FName       := FileName;
    Include_(Rec.FFlags, gfTime);        Rec.FTime       := GetTime(SR);
    Include_(Rec.FFlags, gfAttributes);  Rec.FAttributes := GetAttr(SR);
    Include_(Rec.FFlags, gfMode);        Rec.FMode       := GetMode(FileName);

    Include_(Rec.FFlags, gfUserID);      Rec.FUserID     := GetUID(FileName);
    Include_(Rec.FFlags, gfGroupID);     Rec.FGroupID    := GetGID(FileName);

    if FpS_ISREG(Rec.FMode) then
    begin
      Stream := TFileStream.Create(Rec.Name, fmOpenRead);
      Include_(Rec.FFlags, gfSize);
      Rec.FSize := GetSize(SR);

      Include_(Rec.FFlags, gfOffSet);
      Include_(Rec.FFlags, gfStoredSize);
      Write(Rec, Stream, Rec.FSize);
      FreeAndNil(Stream);
    end else
      if FpS_ISDIR(Rec.FMode) then
      begin
        // nothing to do
      end else
        if FpS_ISLNK(Rec.FMode) then
        begin
          Include_(Rec.FFlags, gfLinkName);
          Rec.FLinkName := fpReadLink(FileName);
        end else
          raise Exception.CreateFmt('Unsupported file "%s"', [Rec.Name]);

    FAdd.Add(Rec);
  end;
  SysUtils.FindClose(SR);
end;
{$ENDIF}

procedure TGulpLib.Delete(Index: longint);
var
  Rec : TGulpRec;
begin
  if FFullLoad = TRUE then
    raise Exception.Create('Internal error');

  BeginUpdate;

  Rec := TGulpRec.Create;
  Clear_(Rec);

  Include_(Rec.FFlags, gfDEL);
  Include_(Rec.FFlags, gfVersion);  Rec.FVersion := FCurrVersion + 1;
  Include_(Rec.FFlags, gfName);     Rec.FName    := Items[Index].Name;

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
  FStream.Seek(Rec.Offset, soBeginning);
  if Rec.Size > 0 then
  begin
    if Read(Rec, Stream, Rec.Size) = FALSE then
      raise Exception.CreateFmt('Mismatched checksum for "%s"', [Rec.Name]);
  end;
end;

procedure TGulpLib.Extract(Index: longint);
var
  Rec  : TGulpRec;
  Dest : TFileStream;
begin
  Rec := Items[Index];
  if FpS_ISREG(Rec.FMode) then
  begin
    if DirectoryExists(ExtractFileDir(Rec.Name)) then
      ForceDirectories(ExtractFileDir(Rec.Name));
    Dest := TFileStream.Create(Rec.Name, fmCreate);

    FStream.Seek(Rec.Offset, soBeginning);
    if Read(Rec, Dest, Rec.Size) = FALSE then
      raise Exception.CreateFmt('Mismatched checksum for "%s"', [Rec.Name]);
    FreeAndNil(Dest);
  end else
    if FpS_ISDIR(Rec.Mode) then
    begin
      if DirectoryExists(ExtractFileDir(Rec.Name)) then
        ForceDirectories(ExtractFileDir(Rec.Name));
      ForceDirectories(Rec.Name);
    end else
      if FpS_ISLNK(Rec.Mode) then
      begin


      end else
        raise Exception.CreateFmt('Unsupported file "%s"', [Rec.Name]);

  FpChmod    (Rec.Name, Rec.Mode);
  FileSetAttr(Rec.Name, Rec.Attributes);
  FileSetDate(Rec.Name, DateTimeToFileDate(UniversalTimeToLocal(Rec.Time)));
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
     Lib : TGulpLib;
     Rec : TGulpRec;
  OffSet : int64 = 0;
    Size : int64 = 0;
begin
  Stream.Seek(0, soBeginning);
  Lib := TGulpLib.Create(Stream);
  if Lib.ReadOffSet(OffSet) = TRUE then
    Stream.Seek(OffSet, soBeginning);

  Rec := TGulpRec.Create;
  try
    while Lib.Read(Rec) = TRUE do
      if (Rec.FFlags and $FF) = gfFIX then
      begin
        Size := Stream.Seek(0, soCurrent);
        if Lib.ReadOffSet(OffSet) = TRUE then
          Stream.Seek(OffSet, soBeginning);
      end;
  except
    // nothing to do
  end;
  FreeAndNil(Rec);
  FreeAndNil(Lib);

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
  Stream : TGulpStream;
begin
  Dest.Seek(0, soBeginning);
  Stream := TGulpStream.Create(Dest);

  Lib:= TGulpLib.Create(Source);
  if Lib.OpenArchive(longword(-2)) = FALSE then
  begin
    if Source.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;

  Stream.WriteOffSet(OffSet);
  for I := 0 to Lib.Count - 1 do
  begin
    Rec := Lib.Items[I];
    if Rec.StoredSize > 0 then
    begin
      Source.Seek(Rec.FOffset, soBeginning);
      Rec.FOffSet := Dest.Seek(0, soCurrent);
      Dest.CopyFrom(Source, Rec.FStoredSize);
    end;
    Rec.FVersion := 1;
  end;

  OffSet := Dest.Seek(0, soEnd);
  Dest.Seek(0, soBeginning);
  Stream.WriteOffSet(OffSet);

  Dest.Seek(0, soEnd);
  for I := 0 to Lib.Count - 1 do
    Stream.Write(Lib.Items[I]);

  Rec := TGulpRec.Create;
  Clear_(Rec);

  Include_(Rec.FFlags, gfFIX);
  Include_(Rec.FFlags, gfVersion);
  Rec.FVersion := 1;
  Stream.Write(Rec);

  FreeAndNil(Rec);
  FreeAndNil(Lib);
  FreeAndNil(Stream);
end;

end.
