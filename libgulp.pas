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

unit Libgulp;

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

  gfName  = $00000100;
  gfFlags = $00000200;
  gfSTime = $00000400;
  gfMTime = $00000800;
  gfAttr  = $00001000;
  gfMode  = $00002000;
  gfSize  = $00004000;
  gfLName = $00008000;
  gfUID   = $00010000;
  gfUName = $00020000;
  gfGID   = $00040000;
  gfGName = $00080000;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..7] of char;

  // --- Gulp Record CLASS ---
  TGulpRec = class(TObject)
  private
    FFlags         : longword;   // Flags
    FName          : ansistring; // File path and name
    FSTime         : TDateTime;  // Stored date time
    FMTime         : TDateTime;  // Last modification date and time
    FAttr          : longint;    // File Attr
    FMode          : longword;   // File Mode
    FSize          : int64;      // File size in bytes
    FLName         : ansistring; // Name of linked file
    FUID           : longword;   // User ID
    FUName         : ansistring; // User Name
    FGID           : longword;   // Group ID
    FGName         : ansistring; // Group Name
                                 // Data bytes
                                 // SHA digest
    FChecksumOK    : boolean;    // SHA checksum ok (reserved)
    FHandle        : longword;   // Handle (reserved)
  public
    property Flags        : longword   read FFlags;
    property Name         : ansistring read FName;
    property STime        : TDateTime  read FSTime;
    property MTime        : TDateTime  read FMTime;
    property Attr         : longint    read FAttr;
    property Mode         : longword   read FMode;
    property Size         : int64      read FSize;
    property LName        : ansistring read FLName;
    property UID          : longword   read FUID;
    property UName        : ansistring read FUName;
    property GID          : longword   read FGID;
    property GName        : ansistring read FGName;
    property ChecksumOK   : boolean    read FChecksumOK;
    property Handle       : longword   read FHandle;
  end;

  // --- The Gulp Reader CLASS ---
  TGulpReader = class(TObject)
  protected
    FChecked : boolean;
    FCount   : longword;
    FCTX     : TSHA1Context;
    FStream  : TStream;
    function ReadStream (var Rec: TGulpRec; Stream: TStream): boolean;
    procedure ReadString (var Buffer: string);
    function Read (var Buffer; Count: longint): longint;
  public
    constructor Create (Stream: TStream);
    destructor Destroy; override;
    function ReadNext (var Rec: TGulpRec; Stream: TStream): boolean;
    property Count: longword read FCount;
  end;

  // --- The Gulp Writer CLASS ---
  TGulpWriter = class(TObject)
  protected
    FCount      : longword;
    FCTX        : TSHA1Context;
    FStream     : TStream;
    FStoredTime : TDateTime;
    procedure WriteStream (Rec: TGulpRec; Stream: TStream);
    procedure WriteString (const Buffer: string);
    procedure Write (const Buffer; Count: longint);
  public
    constructor Create (Stream: TStream);
    destructor Destroy; override;
    procedure Add    (const FileName: string);
    procedure Delete (const FileName: string);
    procedure Copy   (const Rec: TGulpRec; Stream: TStream);
    property Count: longword read FCount;
  end;

  // --- The Gulp List CLASS ---
  TGulpList = class(TObject)
  private
    FList: TList;
    FHistoryMode: boolean;
    FStoredTime:TDateTime;
    function GetCount: longint;
    function Get(Index: longint): TGulpRec; overload;
    procedure BinInsert(const Rec: TGulpRec);
    function BinSearch(const FileName: string): longint;
  public
    constructor Create(StoredTime: TDateTime);
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: longint);
    procedure Add(var Rec: TGulpRec);
    function Find(const FileName: string): longint; overload;
    function Find(const Handle: longword): longint; overload;
  public
    property Items[Index: longint]: TGulpRec read Get;
    property Count: longint read GetCount;
  end;

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
function  TagToString (var Rec: TGulpRec): string;
function AttrToString (var Rec: TGulpRec): string;
function SizeToString (var Rec: TGulpRec): string;
function TimeToString (var Rec: TGulpRec): string;
function ModeToString (var Rec: TGulpRec): string;
function StringToAttr (const S: string  ): longint;
function StringToMode (const S: string  ): longint;

// =============================================================================
// IMPLEMENTATION
// =============================================================================

implementation

uses
  Math;

const
  GulpMarker: TGulpMarker = ('G', 'U','L','P','/', '0','0','2');

// =============================================================================
// GULP Lib Routines
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
  Result := FileDateToDateTime(SR.Time);
end;

function GetTime(const FileName: string): TDateTime;
var
  SR: TSearchRec;
begin
  Result := 0.0;
  if FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GetTime(SR);
  end;
  FindClose(SR);
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
  if FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GetSize(SR);
  end;
  FindClose(SR);
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
  if FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GetAttr(SR);
  end;
  FindClose(SR);
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

function TagToString(var Rec: TGulpRec): string;
begin
  case Rec.Flags and $FF of
    gfFix: Result := 'FIX';
    gfAdd: Result := 'ADD';
    gfDel: Result := 'DEL';
    else   Result := '???';
  end;
end;

function TimeToString (var Rec: TGulpRec): string;
begin
  if Rec.Flags and $FF in [gfAdd] then
    Result := FormatDateTime(
      DefaultFormatSettings.LongDateFormat + ' ' +
      DefaultFormatSettings.LongTimeFormat, Rec.MTime)
  else
    Result := '.......... ........';
end;

function SizeToString (var Rec: TGulpRec): string;
begin
  Result := '';
  if Rec.Flags and $FF in [gfAdd] then
    if Rec.Flags and gfSize <> 0 then
    begin
      Result := Format('%u', [Rec.Size])
    end;
end;

function AttrToString (var Rec: TGulpRec): string;
begin
  Result := '.......';
  if Rec.Flags and $FF in [gfAdd] then
  begin
    if Rec.Attr and faReadOnly  <> 0 then Result[1]  := 'R';
    if Rec.Attr and faHidden    <> 0 then Result[2]  := 'H';
    if Rec.Attr and faSysFile   <> 0 then Result[3]  := 'S';
    if Rec.Attr and faVolumeId  <> 0 then Result[4]  := 'V';
    if Rec.Attr and faDirectory <> 0 then Result[5]  := 'D';
    if Rec.Attr and faArchive   <> 0 then Result[6]  := 'A';
    if Rec.Attr and faSymLink   <> 0 then Result[7]  := 'L';
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
// GULP Format
// =============================================================================


// =============================================================================
// TGulpRec
// =============================================================================

function Clear (const Rec: TGulpRec): TGulpRec;
begin
  Result             := Rec;
  Result.FFlags      := 0;
  Result.FName       := '';
  Result.FSTime      := 0.0;
  Result.FMTime      := 0.0;
  Result.FAttr       := 0;
  Result.FMode       := 0;
  Result.FSize       := 0;
  Result.FLName      := '';
  Result.FUID        := 0;
  Result.FUName      := '';
  Result.FGID        := 0;
  Result.FGName      := '';
  Result.FChecksumOK := FALSE;
  Result.FHandle     := 0;
end;

function GetLength (const Rec: TGulpRec): int64;
begin
  Result := 0;
  Inc(Result, SizeOf (TGulpMarker));
  Inc(Result, SizeOf (Rec.FFlags));

  if gfName  and Rec.Flags <> 0 then Inc(Result, 4 + Length (Rec.FName));
  if gfSTime and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FSTime));
  if gfMTime and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FMTime));
  if gfAttr  and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FAttr));
  if gfMode  and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FMode));
  if gfSize  and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FSize));
  if gfLName and Rec.Flags <> 0 then Inc(Result, 4 + Length (Rec.FLName));
  if gfUID   and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FUID));
  if gfUName and Rec.Flags <> 0 then Inc(Result, 4 + Length (Rec.FUName));
  if gfGID   and Rec.Flags <> 0 then Inc(Result,     SizeOf (Rec.FGID));
  if gfGName and Rec.Flags <> 0 then Inc(Result, 4 + Length (Rec.FGName));
  if gfSize  and Rec.Flags <> 0 then Inc(Result,             Rec.FSize);

  Inc(Result, SizeOf(TSHA1Digest));
end;

procedure IncludeFlag(var Flags: longword; Flag: longword);
begin
  Flags := Flags or Flag;
end;

procedure ExcludeFlag(var Flags: longword; Flag: longword);
begin
  Flags := Flags and (Flag xor $FFFFFFFF);
end;

// =============================================================================
// TGulpReader
// =============================================================================

constructor TGulpReader.Create (Stream : TStream);
begin
  inherited Create;
  FCount   := 0;
  FStream  := Stream;
  FChecked := FStream.Size = 0;
  FStream.Seek(0, soBeginning);
end;

destructor TGulpReader.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

function TGulpReader.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
  SHA1Update(FCTX, Buffer, Result);
end;

procedure TGulpReader.ReadString (var Buffer: string);
var
  Len: longint;
begin
  Read(Len, SizeOf(Len));
  SetLength(Buffer, Len);
  Read(Pointer(Buffer)^, Len);
end;

function TGulpReader.ReadStream (var Rec: TGulpRec; Stream: TStream): boolean;
var
  Size: int64;
  Readed: longint;
  Digest: TSHA1Digest;
  DigestAux: TSHA1Digest;
  Buffer: array[0..$FFFF] of byte;
  Marker: TGulpMarker;
begin
  SHA1Init (FCTX);
  FillChar (Marker, SizeOf (Marker), 0);
  Read     (Marker, SizeOf (Marker));
  Result := Marker = GulpMarker;
  if Result then
  begin
    Read (Rec.FFlags, SizeOf (Rec.FFlags));

    if gfName  and Rec.Flags <> 0 then ReadString(Rec.FName);
    if gfSTime and Rec.Flags <> 0 then Read      (Rec.FSTime, SizeOf (Rec.FSTime));
    if gfMTime and Rec.Flags <> 0 then Read      (Rec.FMTime, SizeOf (Rec.FMTime));
    if gfAttr  and Rec.Flags <> 0 then Read      (Rec.FAttr,  SizeOf (Rec.FAttr));
    if gfMode  and Rec.Flags <> 0 then Read      (Rec.FMode,  SizeOf (Rec.FMode));
    if gfSize  and Rec.Flags <> 0 then Read      (Rec.FSize,  SizeOf (Rec.FSize));
    if gfLName and Rec.Flags <> 0 then ReadString(Rec.FLName);
    if gfUID   and Rec.Flags <> 0 then Read      (Rec.FUID,   SizeOf (Rec.FUID));
    if gfUName and Rec.Flags <> 0 then ReadString(Rec.FUName);
    if gfGID   and Rec.Flags <> 0 then Read      (Rec.FGID,   SizeOf (Rec.FGID));
    if gfGName and Rec.Flags <> 0 then ReadString(Rec.FGName);

    if Assigned(Stream) then
    begin
      Size := Rec.Size;
      while Size <> 0 do
      begin
        Readed := Read(Buffer, Min(SizeOf(Buffer), Size));
        if Readed = 0 then
          raise Exception.Create('Stream read error');
        Stream.Write(Buffer, Readed);
        Dec(Size, Readed);
      end;
    end else
      FStream.Seek(Rec.Size, soCurrent);

    SHA1Final(FCTX, Digest);
    Fstream.Read(DigestAux, SizeOf(DigestAux));
    Rec.FChecksumOK := SHA1Match(DigestAux, Digest);
  end;
  Rec.FName := GetName(Rec.FName);
end;

function TGulpReader.ReadNext (var Rec: TGulpRec; Stream: TStream): boolean;
begin
  Clear(Rec);
  Result := ReadStream (Rec, Stream);
  if Result then
  begin
    FChecked := (Rec.Flags and $FF) = gfFIX;
    if FChecked and (Rec.ChecksumOK = FALSE)then
      raise Exception.Create('Fix point checksum mismatched');

    Rec.FHandle := FCount;
    Inc(FCount);
  end else
    if FChecked = FALSE then
      raise Exception.Create('Fix point error');
end;

// =============================================================================
// TGulpWriter
// =============================================================================

constructor TGulpWriter.Create (Stream: TStream);
begin
  inherited Create;
  FCount      := 0;
  FStoredTime := Now;
  FStream     := Stream;
  FStream.Seek (0, soFromEnd);
end;

destructor TGulpWriter.Destroy;
var
  Flags  : longword;
  Digest : TSHA1Digest;
begin
  if FCount <> 0 then
  begin
    Flags := 0;
    IncludeFlag(Flags, gfFIX);
    IncludeFlag(Flags, gfSTime);
    SHA1Init (FCTX);
    Write    (GulpMarker,  SizeOf (GulpMarker));
    Write    (Flags,       SizeOf (Flags));
    Write    (FStoredTime, SizeOf (FStoredTime));
    SHA1Final(FCTX, Digest);
    FStream.Write(Digest, SizeOf(Digest));
  end;
  FStream := nil;
  inherited Destroy;
end;

procedure TGulpWriter.Write (const Buffer; Count: longint);
begin
  FStream.Write(Buffer, Count);
  SHA1Update(FCTX, Buffer, Count);
end;

procedure TGulpWriter.WriteString (const Buffer: string);
var
  Len : longint;
begin
  Len := Length(Buffer);
  Write(Len, SizeOf(Len));
  Write(Pointer(Buffer)^, Len);
end;

procedure TGulpWriter.WriteStream (Rec: TGulpRec; Stream: TStream);
var
  Size   : int64;
  Readed : longint;
  Digest : TSHA1Digest;
  Buffer : array[0..$FFFF] of byte;
begin
  Inc(FCount);
  SHA1Init (FCTX);
  Write    (GulpMarker, SizeOf (GulpMarker));
  Write    (Rec.FFlags, SizeOf (Rec.FFlags));

  if gfName  and Rec.Flags <> 0 then WriteString(Rec.FName);
  if gfSTime and Rec.Flags <> 0 then Write      (Rec.FSTime, SizeOf (Rec.FSTime));
  if gfMTime and Rec.Flags <> 0 then Write      (Rec.FMTime, SizeOf (Rec.FMTime));
  if gfAttr  and Rec.Flags <> 0 then Write      (Rec.FAttr,  SizeOf (Rec.FAttr));
  if gfMode  and Rec.Flags <> 0 then Write      (Rec.FMode,  SizeOf (Rec.FMode));
  if gfSize  and Rec.Flags <> 0 then Write      (Rec.FSize,  SizeOf (Rec.FSize));
  if gfLName and Rec.Flags <> 0 then WriteString(Rec.FLName);
  if gfUID   and Rec.Flags <> 0 then Write      (Rec.FUID,   SizeOf (Rec.FUID));
  if gfUName and Rec.Flags <> 0 then WriteString(Rec.FUName);
  if gfGID   and Rec.Flags <> 0 then Write      (Rec.FGID,   SizeOf (Rec.FGID));
  if gfGName and Rec.Flags <> 0 then WriteString(Rec.FGName);

  if Assigned(Stream) then
  begin
    Size := Rec.Size;
    while Size > 0 do
    begin
      Readed := Stream.Read (Buffer, Min(SizeOf(Buffer), Size));
      if Readed = 0 then
        raise Exception.Create('Stream read error');
      Write (Buffer, Readed);
      Dec(Size, Readed);
    end;
  end;
  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
end;

procedure TGulpWriter.Delete (const FileName: string);
var
  Rec : TGulpRec;
begin
  Rec := Clear(TGulpRec.Create);
  IncludeFlag(Rec.FFlags, gfDEL);
  IncludeFlag(Rec.FFlags, gfName);
  IncludeFlag(Rec.FFlags, gfSTime);

  Rec.FName  := FileName;
  Rec.FSTime := FStoredTime;
  WriteStream (Rec, nil);
  FreeAndNil (Rec);
end;

procedure TGulpWriter.Copy(const Rec: TGulpRec; Stream: TStream);
begin
  Inc(FCount);
  FStream.CopyFrom(Stream, GetLength(Rec));
end;

{$IFDEF MSWINDOWS}

{$ENDIF}

{$IFDEF UNIX}
procedure TGulpWriter.Add (const FileName: string);
var
  Rec    : TGulpRec;
  SR     : TSearchRec;
  Stream : TStream;
begin
  Rec := Clear(TGulpRec.Create);
  if FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId  or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    IncludeFlag(Rec.FFlags, gfADD);
    IncludeFlag(Rec.FFlags, gfName);
    IncludeFlag(Rec.FFlags, gfSTime);
    IncludeFlag(Rec.FFlags, gfMTime);
    IncludeFlag(Rec.FFlags, gfAttr);
    IncludeFlag(Rec.FFlags, gfMode);

    Rec.FName  := FileName;
    Rec.FSTime := FStoredTime;
    Rec.FMTime := GetTime(SR);
    Rec.FAttr  := GetAttr(SR);
    Rec.FMode  := GetMode(FileName);

    Stream := nil;
    if FpS_ISREG(Rec.FMode) then
      try
        Stream := TFileStream.Create(FileName, fmOpenRead);
      except
        Stream := nil;
      end;

    if Assigned(Stream) then
    begin
      IncludeFlag(Rec.FFlags, gfSize);
      Rec.FSize := SR.Size;
    end;

    if FpS_ISLNK(Rec.FMode) then
    begin
      IncludeFlag(Rec.FFlags, gfLName);
      Rec.FLName := fpReadLink(FileName);
    end;
    IncludeFlag(Rec.FFlags, gfUID);
    IncludeFlag(Rec.FFlags, gfGID);
    Rec.FUID := GetUID(FileName);
    Rec.FGID := GetGID(FileName);

    WriteStream(Rec, Stream);
    if Assigned(Stream) then
      FreeAndNil(Stream);
  end;
  FindClose(SR);
  FreeAndNil(Rec);
end;
{$ENDIF}

// =============================================================================
// TGulpList
// =============================================================================

constructor TGulpList.Create(StoredTime: TDateTime);
begin
  inherited Create;
  FList        := TList.Create;
  FStoredTime  := StoredTime;
  FHistoryMode := FStoredTime = 0.0;
end;

destructor TGulpList.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpList.Clear;
var
  I: longint;
begin
  for I := 0 to FList.Count - 1 do
    Items[I].Destroy;
  FList.Clear;
end;

procedure TGulpList.Delete(Index: longint);
begin
  Items[Index].Destroy;
  FList.Delete(Index);
end;

procedure TGulpList.BinInsert(const Rec: TGulpRec);
var
  L, M, H, I: longint;
begin
  L := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := AnsiCompareFileName(Items[M].Name, Rec.Name);
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
        FList.Insert(M, Rec);
  end else
    FList.Add(Rec);
end;

function TGulpList.BinSearch(const FileName: string): longint;
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

function TGulpList.Find(const FileName: string): longint;
var
  I: longint;
begin
  if FHistoryMode = TRUE then
  begin
    Result := -1;
    for I := FList.Count - 1 downto 0 do
      if ((Items[I].Flags and $FF) in [gfFIX, gfDEL]) = FALSE then
        if AnsiCompareFileName(Items[I].Name, FileName) = 0 then
        begin
          Result := I;
          Break;
        end;
  end else
    Result := BinSearch(FileName);
end;

function TGulpList.Find(const Handle: longword): longint;
var
  I: longint;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
    if Items[I].Handle = Handle then
    begin
      Result := I;
      Break;
    end;
end;

procedure TGulpList.Add(var Rec: TGulpRec);
var
  I: longint;
begin
  if FHistoryMode = FALSE then
  begin
    if Rec.STime < FStoredTime then
      case Rec.Flags and $FF of
      //gfFix: nothing to do
        gfAdd: BinInsert(Rec);
        gfDel: begin
          I := BinSearch(Rec.Name);
          if I <> - 1 then Delete(I);
        end;
      end;
  end else
    FList.Add(Rec);
end;

function TGulpList.Get(Index: longint): TGulpRec;
begin
  Result := TGulpRec(FList[Index]);
end;

function TGulpList.GetCount: longint;
begin
  Result := FList.Count;
end;

end.
