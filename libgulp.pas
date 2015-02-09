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

  Modifyed:

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
  gfFIX   = $00000000;
  gfADD   = $00000001;
  gfDEL   = $00000002;

  gfVer   = $00000100;
  gfName  = $00000200;
  gfFlags = $00000400;
  gfTime  = $00000800;
  gfAttr  = $00001000;
  gfMode  = $00002000;
  gfSize  = $00004000;
  gfLName = $00008000;
  gfUID   = $00010000;
  gfUName = $00020000;
  gfGID   = $00040000;
  gfGName = $00080000;
  gfSSeek = $00100000;
  gfESeek = $00200000;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..9] of char;

  // --- Gulp Record CLASS ---
  TGulpRec = class(TObject)
  private
    FFlags : longword;   // Flags
    FVer   : longword;   // File version
    FName  : ansistring; // File path and name
    FTime  : TDateTime;  // Last modification date and time (UTC)
    FAttr  : longint;    // File Attr
    FMode  : longword;   // File Mode
    FSize  : int64;      // File size in bytes
    FLName : ansistring; // Name of linked file
    FUID   : longword;   // User ID
    FUName : ansistring; // User Name
    FGID   : longword;   // Group ID
    FGName : ansistring; // Group Name
                         // Rec SHA digest
    FSSeek : int64;      // Data start postion
    FESeek : int64;      // Data end position
                         // Data SHA digest
  public
    property Flags : longword   read FFlags;
    property Ver   : longword   read FVer;
    property Name  : ansistring read FName;
    property Time  : TDateTime  read FTime;
    property Attr  : longint    read FAttr;
    property Mode  : longword   read FMode;
    property Size  : int64      read FSize;
    property LName : ansistring read FLName;
    property UID   : longword   read FUID;
    property UName : ansistring read FUName;
    property GID   : longword   read FGID;
    property GName : ansistring read FGName;
    property SSeek : int64      read FSSeek;
    property ESeek : int64      read FESeek;
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
    function Write(const Buffer: string): longint; overload;
    function Write(const Buffer; Count: longint): longint; overload;
    function Write(Rec: TGulpRec; Stream: TStream; Size: int64): boolean; overload;
    function Write(Rec: TGulpRec): boolean; overload;
  end;

  // --- The Gulp Lib CLASS ---
  TGulpLib = class(TGulpStream)
  private
    FAdd        : TList;
    FList       : TList;
    FSimpleList : boolean;
    FVersion    : longword;
    function GetCount: longint;
    function GetItem(Index: longint): TGulpRec;
    procedure AddItem(Rec: TGulpRec);
    procedure InsertItem(Rec: TGulpRec);
    procedure DeleteItem(Index: longint);
    function  FindItem(const FileName: string): longint;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create(Stream: TStream);
    destructor  Destroy; override;

    function  OpenArchive(Version: longword): boolean;
    procedure CloseArchive;

    procedure Add(const FileName: string);
    procedure Delete(Index: longint);
    function  Find(const FileName: string): longint;

    procedure Extract(Index: longint);
    procedure Check(Index: longint);

    property Items[Index: longint]: TGulpRec read GetItem;
    property Count: longint read GetCount;
  end;

  // --- The Nul Stream CLASS ---
  TNulStream = class(TStream)
  public
    function Read (var   Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
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
  GulpMarker: TGulpMarker = ('G', 'U','L','P',' ', ' ', ' ', '0','0','2');

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

function VerToString(var Rec: TGulpRec): string;
begin
  if (Rec.FFlags and gfVer) = gfVer then
    Result := IntTostr(Rec.Ver)
  else
    Result := '?';
end;

function FlagToString(var Rec: TGulpRec): string;
begin
  case Rec.Flags and $FF of
    gfFix: Result := 'FIX';
    gfAdd: Result := 'ADD';
    gfDel: Result := 'DEL';
    else   Result := '???';
  end;
end;

function TimeToString(var Rec: TGulpRec): string;
begin
  if Rec.Flags and $FF in [gfAdd] then
    Result := FormatDateTime(
      DefaultFormatSettings.LongDateFormat + ' ' +
      DefaultFormatSettings.LongTimeFormat, Rec.Time)
  else
    Result := '.......... ........';
end;

function SizeToString (var Rec: TGulpRec): string;
begin
  Result := '';
  if (Rec.Flags and $FF) in [gfAdd] then
    if (Rec.Flags and gfSize) = gfSize then
    begin
      Result := Format('%u', [Rec.Size])
    end;
end;

function AttrToString (var Rec: TGulpRec): string;
begin
  Result := '.......';
  if Rec.Flags and $FF in [gfAdd] then
  begin
    if Rec.Attr and faReadOnly  <> 0 then Result[1] := 'R';
    if Rec.Attr and faHidden    <> 0 then Result[2] := 'H';
    if Rec.Attr and faSysFile   <> 0 then Result[3] := 'S';
    if Rec.Attr and faVolumeId  <> 0 then Result[4] := 'V';
    if Rec.Attr and faDirectory <> 0 then Result[5] := 'D';
    if Rec.Attr and faArchive   <> 0 then Result[6] := 'A';
    if Rec.Attr and faSymLink   <> 0 then Result[7] := 'L';
  end;
end;

function StringToAttr (const S: string  ): longint;
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
function ModeToString (var Rec: TGulpRec): string;
begin
  Result := '.........';
end;

function StringToMode (const S: string): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF UNIX}
function ModeToString (var Rec: TGulpRec): string;
begin
  Result := '.........';
  if Rec.Flags and $FF in [gfAdd] then
  begin
    if Rec.FMode and S_IRUSR <> 0 then Result[1] := 'R';
    if Rec.FMode and S_IWUSR <> 0 then Result[2] := 'W';
    if Rec.FMode and S_IXUSR <> 0 then Result[3] := 'X';
    if Rec.FMode and S_IRGRP <> 0 then Result[4] := 'R';
    if Rec.FMode and S_IWGRP <> 0 then Result[5] := 'W';
    if Rec.FMode and S_IXGRP <> 0 then Result[6] := 'X';
    if Rec.FMode and S_IROTH <> 0 then Result[7] := 'R';
    if Rec.FMode and S_IWOTH <> 0 then Result[8] := 'W';
    if Rec.FMode and S_IXOTH <> 0 then Result[9] := 'X';
  end;
end;

function StringToMode (const S: string): longint;
begin
  Result := 0;
  if Length(S) = 9 then
  begin
    if UpCase(S[1]) = 'R' then Result := Result or S_IRUSR;
    if UpCase(S[2]) = 'W' then Result := Result or S_IWUSR;
    if UpCase(S[3]) = 'X' then Result := Result or S_IXUSR;
    if UpCase(S[4]) = 'R' then Result := Result or S_IRGRP;
    if UpCase(S[5]) = 'W' then Result := Result or S_IWGRP;
    if UpCase(S[6]) = 'X' then Result := Result or S_IXGRP;
    if UpCase(S[7]) = 'R' then Result := Result or S_IROTH;
    if UpCase(S[8]) = 'W' then Result := Result or S_IWOTH;
    if UpCase(S[9]) = 'X' then Result := Result or S_IXOTH;
  end;
end;
{$ENDIF}

// =============================================================================
// Internal rutines
// =============================================================================

procedure Clear_(var Rec: TGulpRec);
begin
  Rec.FFlags := 0;
  Rec.FVer   := 0;
  Rec.FName  := '';
  Rec.FTime  := 0.0;
  Rec.FAttr  := 0;
  Rec.FMode  := 0;
  Rec.FSize  := 0;
  Rec.FLName := '';
  Rec.FUID   := 0;
  Rec.FUName := '';
  Rec.FGID   := 0;
  Rec.FGName := '';
  Rec.FSSeek := 0;
  Rec.FESeek := 0;
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
    if Item1.Ver < Item2.Ver then
      Result := - 1
    else
      if Item1.Ver > Item2.Ver then
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
  FStream := nil;
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
  Marker    : TGulpMarker;
  Digest    : TSHA1Digest;
  DigestAux : TSHA1Digest;
begin
  Clear_(Rec);
  SHA1Init (FCTX);
  FillChar (Marker, SizeOf (Marker), 0);
  Read     (Marker, SizeOf (Marker));

  Result := Marker = GulpMarker;
  if Result then
  begin
    Read (Rec.FFlags, SizeOf (Rec.FFlags));

    if gfVer   and Rec.Flags <> 0 then Read(Rec.FVer,   SizeOf (Rec.FVer));
    if gfName  and Rec.Flags <> 0 then Read(Rec.FName);
    if gfTime  and Rec.Flags <> 0 then Read(Rec.FTime,  SizeOf (Rec.FTime ));
    if gfAttr  and Rec.Flags <> 0 then Read(Rec.FAttr,  SizeOf (Rec.FAttr ));
    if gfMode  and Rec.Flags <> 0 then Read(Rec.FMode,  SizeOf (Rec.FMode ));
    if gfSize  and Rec.Flags <> 0 then Read(Rec.FSize,  SizeOf (Rec.FSize ));
    if gfLName and Rec.Flags <> 0 then Read(Rec.FLName);
    if gfUID   and Rec.Flags <> 0 then Read(Rec.FUID,   SizeOf (Rec.FUID));
    if gfUName and Rec.Flags <> 0 then Read(Rec.FUName);
    if gfGID   and Rec.Flags <> 0 then Read(Rec.FGID,   SizeOf (Rec.FGID));
    if gfGName and Rec.Flags <> 0 then Read(Rec.FGName);
    if gfSSeek and Rec.Flags <> 0 then Read(Rec.FSSeek, SizeOf (Rec.FSSeek));
    if gfESeek and Rec.Flags <> 0 then Read(Rec.FESeek, SizeOf (Rec.FESeek));

    Rec.FName := GetName(Rec.FName);

    SHA1Final(FCTX, Digest);
    FStream.Read(DigestAux, SizeOf(DigestAux));
    Result := SHA1Match(DigestAux, Digest);
  end;
end;

function TGulpStream.Read(Rec: TGulpRec; Stream: TStream; Size: int64): boolean;
var
  Buffer    : array[0..$FFFF] of byte;
  Readed    : longint;
  Digest    : TSHA1Digest;
  DigestAux : TSHA1Digest;
begin
  SHA1Init(FCTX);
  while Size > 0 do
  begin
    Readed := Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Stream read error');

    Stream.Write(Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(FCTX, Digest);
  FStream.Read(DigestAux, SizeOf(DigestAux));
  Result := SHA1Match(DigestAux, Digest);
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
  SHA1Init (FCTX);
  Write    (GulpMarker, SizeOf (GulpMarker));
  Write    (Rec.FFlags, SizeOf (Rec.FFlags));

  if gfVer   and Rec.Flags <> 0 then Write(Rec.FVer,   SizeOf (Rec.FVer));
  if gfName  and Rec.Flags <> 0 then Write(Rec.FName);
  if gfTime  and Rec.Flags <> 0 then Write(Rec.FTime,  SizeOf (Rec.FTime));
  if gfAttr  and Rec.Flags <> 0 then Write(Rec.FAttr,  SizeOf (Rec.FAttr));
  if gfMode  and Rec.Flags <> 0 then Write(Rec.FMode,  SizeOf (Rec.FMode));
  if gfSize  and Rec.Flags <> 0 then Write(Rec.FSize,  SizeOf (Rec.FSize));
  if gfLName and Rec.Flags <> 0 then Write(Rec.FLName);
  if gfUID   and Rec.Flags <> 0 then Write(Rec.FUID,   SizeOf (Rec.FUID));
  if gfUName and Rec.Flags <> 0 then Write(Rec.FUName);
  if gfGID   and Rec.Flags <> 0 then Write(Rec.FGID,   SizeOf (Rec.FGID));
  if gfGName and Rec.Flags <> 0 then Write(Rec.FGName);
  if gfSSeek and Rec.Flags <> 0 then Write(Rec.FSSeek, SizeOf (Rec.FSSeek));
  if gfESeek and Rec.Flags <> 0 then Write(Rec.FESeek, SizeOf (Rec.FESeek));

  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
end;

function TGulpStream.Write(Rec: TGulpRec; Stream: TStream; Size: int64): boolean;
var
  Buffer : array[0..$FFFF] of byte;
  Readed : longint;
  Digest : TSHA1Digest;
begin
  Result := TRUE;
  SHA1Init(FCTX);
  while Size > 0 do
  begin
    Readed := Stream.Read (Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Stream read error');

    Write(Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
end;

// =============================================================================
// TGulpLib
// =============================================================================

constructor TGulpLib.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FAdd        := TList.Create;
  FList       := TList.Create;
  FSimpleList := FALSE;
  FVersion    := 0;
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
    FStream.Write(FStreamSize, SizeOf(FStreamSize));
  end;
end;

procedure TGulpLib.EndUpdate;
var
  I    : longint;
  Rec  : TGulpRec;
  Size : int64;
begin
  if not (FStreamSize = -1) then
  begin
    Size := FStream.Seek(0, soEnd);
    FStream.Seek (FStreamSize, soBeginning);
    FStream.Write(Size, SizeOf(Size));
    FStream.Seek(0, soEnd);

    Rec := TGulpRec.Create;
    Clear_(Rec);

    Include_(Rec.FFlags, gfFIX  );
    Include_(Rec.FFlags, gfVer  );
    Rec.FVer := FVersion + 1;
    FAdd.Add(Rec);

    for I := 0 to FAdd.Count - 1 do
      Write(TGulpRec(FAdd[I]));
    FStreamSize := -1;
  end;
end;

procedure TGulpLib.CloseArchive;
var
  I: longint;
begin
  EndUpdate;
  for I := 0 to FAdd .Count - 1 do TGulpRec(FAdd [I]).Destroy;
  for I := 0 to FList.Count - 1 do TGulpRec(FList[I]).Destroy;
  FAdd.Clear;
  FList.Clear;

  FSimpleList := FALSE;
  FStreamSize := -1;
  FVersion    :=  0;
end;

function TGulpLib.OpenArchive(Version: longword): boolean;
var
  I    : longint;
  Rec  : TGulpRec;
  Seek : int64 = 0;
  Size : int64 = 0;
begin
  Result := FALSE;

  CloseArchive;
  FStream.Seek(0, soBeginning);
  if FStream.Read(Seek, Sizeof(Seek)) = SizeOf(Seek) then
    FStream.Seek(Seek, soBeginning);

  FSimpleList := Version = longword(-1);

  Rec := TGulpRec.Create;
  while Read(Rec) = TRUE do
  begin
    Result := (Rec.Flags and $FF) = gfFIX;
    if Result then
    begin
      Size := FStream.Seek(0, soCurrent);
      if FStream.Read(Seek, Sizeof(Seek)) = SizeOf(Seek) then
        FStream.Seek(Seek, soBeginning);
    end;

    FVersion := Max(FVersion, Rec.Ver);
    if FSimpleList = TRUE then
    begin
      AddItem(Rec);
      Rec := TGulpRec.Create;
    end else
      if Rec.Ver <= Version then
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

  if Result = TRUE then
  begin
    Result := FStream.Seek(0, soEnd) = Size;
  end;
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
        raise Exception.Create('Duplicated item error.');
  end else
    FList.Add(Rec);
end;

procedure TGulpLib.DeleteItem(Index: longint);
begin
  Items[Index].Destroy;
  FList.Delete(Index);
end;

procedure TGulpLib.AddItem(Rec: TGulpRec);
begin
  if FSimpleList = FALSE then
    InsertItem(Rec)
  else
    FList.Add(Rec);
end;

function TGulpLib.Find(const FileName: string): longint;
var
  I : longint;
begin
  if FSimpleList = TRUE then
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

{$IFDEF MSWINDOWS}
procedure TGulpLib.Add(const FileName: string);
var
  Rec : TGulpRec;
  SR  : TSearchRec;
begin
  BeginUpdate;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId  or
    faDirectory or faArchive or faSymLink or faAnyFile,  SR) = 0 then
  begin
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
  Rec    : TGulpRec;
  SR     : TSearchRec;
  Stream : TStream;
begin
  BeginUpdate;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Rec := TGulpRec.Create;
    Clear_(Rec);

    Include_(Rec.FFlags, gfADD  );
    Include_(Rec.FFlags, gfVer  );  Rec.FVer   := FVersion + 1;
    Include_(Rec.FFlags, gfName );  Rec.FName  := FileName;
    Include_(Rec.FFlags, gfTime );  Rec.FTime  := GetTime(SR);
    Include_(Rec.FFlags, gfAttr );  Rec.FAttr  := GetAttr(SR);
    Include_(Rec.FFlags, gfMode );  Rec.FMode  := GetMode(FileName);

    if FpS_ISLNK(Rec.FMode) then
    begin
      Include_(Rec.FFlags, gfLName);
      Rec.FLName := fpReadLink(FileName);
    end;
    Include_(Rec.FFlags, gfUID);  Rec.FUID := GetUID(FileName);
    Include_(Rec.FFlags, gfGID);  Rec.FGID := GetGID(FileName);

    Stream := nil;
    if FpS_ISREG(Rec.FMode) then
      try
        Stream := TFileStream.Create(Rec.Name, fmOpenRead);

        Include_(Rec.FFlags, gfSize );
        Rec.FSize  := GetSize(SR);
      except
        Stream := nil;
      end;

    if Assigned(Stream) then
    begin
      Include_(Rec.FFlags, gfSSeek);
      Include_(Rec.FFlags, gfESeek);
      Rec.FSSeek := FStream.Seek(0, soCurrent);
      Write(Rec, Stream, Rec.FSize);
      Rec.FESeek := FStream.Seek(0, soCurrent);
      FreeAndNil(Stream);
    end;
    FAdd.Add(Rec);
  end;
  SysUtils.FindClose(SR);
end;
{$ENDIF}

procedure TGulpLib.Delete(Index: longint);
var
  Rec : TGulpRec;
begin
  BeginUpdate;
  Rec := TGulpRec.Create;
  Clear_(Rec);

  Include_(Rec.FFlags, gfDEL  );
  Include_(Rec.FFlags, gfVer  );  Rec.FVer   := FVersion + 1;
  Include_(Rec.FFlags, gfName );  Rec.FName  := Items[Index].Name;

  FAdd.Add(Rec);
end;

procedure TGulpLib.Extract(Index: longint);
begin


end;

procedure TGulpLib.Check(Index: longint);
var
  Nul : TNulStream;
  Rec : TGulpRec;
begin
  Rec := Items[Index];
  FStream.Seek(Rec.SSeek, soBeginning);
  if Rec.Size > 0 then
  begin
    Nul := TNulStream.Create;
    if Read(Rec, Nul, Rec.Size) = FALSE then
      raise Exception.Create('Mismatched checksum for ' + Rec.Name);
    FreeAndNil(Nul);
  end;
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
// TNulStream class
// =============================================================================

function TNulStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

function TNulStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

// =============================================================================
// Advanced routines
// =============================================================================

procedure FixArchive(Stream: TStream);
var
   Lib : TGulpLib;
   Rec : TGulpRec;
  Seek : int64 = 0;
  Size : int64 = 0;
begin
  Stream.Seek(0, soBeginning);
  if Stream.Read(Seek, Sizeof(Seek)) = SizeOf(Seek) then
    Stream.Seek(Seek, soBeginning);

  Lib := TGulpLib.Create(Stream);
  Rec := TGulpRec.Create;
  try
    while Lib.Read(Rec) = TRUE do
      if (Rec.FFlags and $FF) = gfFIX then
      begin
        Size := Stream.Seek(0, soCurrent);
        if Stream.Read(Seek, Sizeof(Seek)) = SizeOf(Seek) then
          Stream.Seek(Seek, soBeginning);
      end;

  except
    // nothing to do
  end;
  FreeAndNil(Rec);
  FreeAndNil(Lib);

  if Size > 0 then
    Stream.Size := Size
  else
    raise Exception.Create('Zero size error');
end;

procedure PurgeArchive(Source, Dest: TStream);
var
     I : longint;
   Lib : TGulpLib;
   Rec : TGulpRec;
  Size : int64 = 0;
  Strm : TGulpStream;
begin
  Lib:= TGulpLib.Create(Source);
  if Lib.OpenArchive(longword(-2)) = FALSE then
  begin
    if Source.Size <> 0 then
      raise Exception.Create('FIX point error');
  end;

  Dest.Seek (0, soBeginning);
  Dest.Write(Size, SizeOf(Size));
  for I := 0 to Lib.Count - 1 do
  begin
    Rec  := Lib.Items[I];
    Size := Rec.ESeek - Rec.SSeek;
    if Size > 0 then
    begin
      Source.Seek(Rec.SSeek, soBeginning);
      Rec.FSSeek := Dest.Seek(0, soCurrent);
      Dest.CopyFrom(Source, Size);
      Rec.FESeek := Dest.Seek(0, soCurrent);
    end;
  end;

  Size := Dest.Seek(0, soEnd);
  Dest.Seek (0, soBeginning);
  Dest.Write(Size, SizeOf(Size));
  Dest.Seek (0, soEnd);

  Strm  := TGulpStream.Create(Dest);
  for I := 0 to Lib.Count - 1 do
    Strm.Write(Lib.Items[I]);

  Rec := TGulpRec.Create;
  Clear_(Rec);

  Include_(Rec.FFlags, gfFIX);
  Include_(Rec.FFlags, gfVer);
  Rec.FVer := Lib.FVersion;
  Strm.Write(Rec);
  FreeAndNil(Rec);
  FreeAndNil(Strm);
  FreeAndNil(Lib);
end;

end.
