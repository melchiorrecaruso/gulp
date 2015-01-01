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
  gfFix            = $00000000;
  gfAdd            = $00000001;
  gfDelete         = $00000002;

  gfName           = $00000100;
  gfFlags          = $00000200;
  gfStoredTime     = $00000400;
  gfModifiedTime   = $00000800;
  gfAttributes     = $00001000;
  gfSize           = $00002000;
  gfLinkName       = $00004000;
  gfUID            = $00008000;
  gfUserName       = $00010000;
  gfGID            = $00020000;
  gfGroupName      = $00040000;

  // --- Gulp Attributes ---
  gaReadOnly       = $00000001;
  gaHidden         = $00000002;
  gaSysFile        = $00000004;
  gaVolumeId       = $00000008;
  gaDirectory      = $00000010;
  gaArchive        = $00000020;
  gaSymLink        = $00000040;

  gaReadByOwner    = $00001000;
  gaWriteByOwner   = $00002000;
  gaExecuteByOwner = $00004000;
  gaReadByGroup    = $00008000;
  gaWriteByGroup   = $00010000;
  gaExecuteByGroup = $00020000;
  gaReadByOther    = $00040000;
  gaWriteByOther   = $00080000;
  gaExecuteByOther = $00100000;

type
  // --- Gulp Marker ---
  TGulpMarker = array [0..7] of char;

  // --- Gulp Record CLASS ---
  TGulpRec = class(TObject)
  private
    FFlags         : longword;   // Flags
    FName          : ansistring; // File path and name
    FStoredTime    : TDateTime;  // Stored date time
    FModifiedTime  : TDateTime;  // Last modification date and time
    FAttr          : longword;   // File mode
    FSize          : int64;      // File size in bytes
    FLinkName      : ansistring; // Name of linked file
    FUID           : longint;    // User ID
    FUserName      : ansistring; // User Name
    FGID           : longint;    // Group ID
    FGroupName     : ansistring; // Group Name
                                 // Data bytes
                                 // SHA digest
    FChecksumOK    : boolean;    // SHA checksum ok (reserved)
    FStartPosition : int64;      // Start position  (reserved)
    FEndPosition   : int64;      // End position    (reserved)
  public
    property Flags        : longword   read FFlags;
    property Name         : ansistring read FName;
    property StoredTime   : TDateTime  read FStoredTime;
    property ModifiedTime : TDateTime  read FModifiedTime;
    property Attr         : longword   read FAttr;
    property Size         : int64      read FSize;
    property LinkName     : ansistring read FLinkName;
    property UID          : longint    read FUID;
    property UserName     : ansistring read FUserName;
    property GID          : longint    read FGID;
    property GroupName    : ansistring read FGroupName;
    property ChecksumOK   : boolean    read FChecksumOK;
  end;

  // --- The Gulp Reader CLASS ---
  TGulpReader = class(TObject)
  protected
    FCTX            : TSHA1Context;
    FStream         : TStream;
    function ReadStream (var Rec: TGulpRec; Stream: TStream): boolean;
    function ReadString (var Buffer: string): longint;
    function Read (var Buffer; Count: longint): longint;
  public
    constructor Create (Stream: TStream);
    destructor Destroy; override;
    procedure Reset;
    function  FindNext (var Rec: TGulpRec; Stream: TStream): boolean;
  end;

  // --- The Gulp Writer CLASS ---
  TGulpWriter = class(TObject)
  protected
    FModified       : boolean;
    FCTX            : TSHA1Context;
    FStream         : TStream;
    FStoredTime     : TDateTime;
    procedure WriteStream (Rec: TGulpRec; Stream: TStream);
    procedure WriteString (const Buffer: string);
    procedure Write (const Buffer; Count: longint);
  public
    constructor Create  (Stream: TStream);
    destructor Destroy; override;
    procedure Add      (const FileName: string);
    procedure Delete   (const FileName: string);
    procedure CopyFrom (var Rec: TGulpRec; Stream: TStream);
  end;

  // --- The Gulp List CLASS ---
  TGulpList = class(TObject)
  private
    FList: TList;
    FBinaryMode: boolean;
    FStoredTime:TDateTime;
    function Get(Index: longint): TGulpRec;
    function GetCount: longint;
    procedure Delete(Index: longint);
    procedure BinInsert(const Rec: TGulpRec);
    function BinSearch(const Filename: string): longint;
  public
    constructor Create(StoredTime: TDateTime);
    destructor Destroy; override;
    function Find(const FileName: string): longint;
    procedure Add(var Rec: TGulpRec);
    procedure Clear;
  public
    property Items[Index: longint]: TGulpRec read Get;
    property Count: longint read GetCount;
  end;

// --- Some useful routines

function ConvertFileName(const FileName: string): string;
function CompareFileTime(const FileName: string; const Rec: TGulpRec): longint;
function CompareFileSize(const FileName: string; const Rec: TGulpRec): longint;
function CompareFileAttr(const FileName: string; const Rec: TGulpRec): longint;

function CommandToString(Rec: TGulpRec): string;
function AttrToString   (Rec: TGulpRec): string;
function SizeToString   (Rec: TGulpRec): string;
function TimeToString   (Rec: TGulpRec): string;

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
function ConvertFileName(const FileName: string): string;
begin
  Result := StringReplace(Filename, '/', '\', [rfReplaceAll]);
end;
{$ENDIF}

{$IFDEF UNIX}
function ConvertFileName(const FileName: string): string;
begin
  Result := StringReplace(Filename, '\', '/', [rfReplaceAll]);
end;
{$ENDIF}

function CompareFileTime(const FileName: string; const Rec: TGulpRec): longint;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    if FileDateToDateTime(SR.Time) > Rec.ModifiedTime then
      Result := - 1
    else
      if FileDateToDateTime(SR.Time) < Rec.ModifiedTime then
        Result := 1;
  end;
  FindClose(SR);
end;

function CompareFileSize(const FileName: string; const Rec: TGulpRec): longint;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    if (SR.Attr and faDirectory = 0) or (Rec.Attr and gaDirectory = 0) then
    begin
      if SR.Size > Rec.Size then
        Result := - 1
      else
        if SR.Size < Rec.Size then
          Result := 1;
    end;
  end;
  FindClose(SR);
end;

{$IFDEF MSWINDOWS}
function CompareFileAttr(const FileName: string; const Rec: TGulpRec): longint;
var
  SR: TSearchRec;
  SRA:  longword;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    SRA := 0;
    if faReadOnly  and SR.Attr <> 0 then SRA := SRA or gaReadOnly;
    if faHidden    and SR.Attr <> 0 then SRA := SRA or gaHidden;
    if faSysFile   and SR.Attr <> 0 then SRA := SRA or gaSysFile;
    if faVolumeId  and SR.Attr <> 0 then SRA := SRA or gaVolumeId;
    if faDirectory and SR.Attr <> 0 then SRA := SRA or gaDirectory;
    if faArchive   and SR.Attr <> 0 then SRA := SRA or gaArchive;
    if faSymLink   and SR.Attr <> 0 then SRA := SRA or gaSymLink;

    if SRA > Rec.Attributes then
      Result := - 1
    else
      if SRA < Rec.Attributes then
        Result := 1;
  end;
  FindClose(SR);
end;
{$ENDIF}

{$IFDEF UNIX}
function CompareFileAttr(const FileName: string; const Rec: TGulpRec): longint;
var
  SR: TSearchRec;
  SRA:  longword;
  info: stat;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    SRA := 0;
    if faReadOnly  and SR.Attr <> 0 then SRA := SRA or gaReadOnly;
    if faHidden    and SR.Attr <> 0 then SRA := SRA or gaHidden;
    if faSysFile   and SR.Attr <> 0 then SRA := SRA or gaSysFile;
    if faVolumeId  and SR.Attr <> 0 then SRA := SRA or gaVolumeId;
    if faDirectory and SR.Attr <> 0 then SRA := SRA or gaDirectory;
    if faArchive   and SR.Attr <> 0 then SRA := SRA or gaArchive;
    if faSymLink   and SR.Attr <> 0 then SRA := SRA or gaSymLink;

    if fpstat (FileName, info) = 0 then
    begin
      if info.st_mode and $0100 <> 0 then SRA := SRA or gaReadByOwner;
      if info.st_mode and $0080 <> 0 then SRA := SRA or gaWriteByOwner;
      if info.st_mode and $0040 <> 0 then SRA := SRA or gaExecuteByOwner;
      if info.st_mode and $0020 <> 0 then SRA := SRA or gaReadByGroup;
      if info.st_mode and $0010 <> 0 then SRA := SRA or gaWriteByGroup;
      if info.st_mode and $0008 <> 0 then SRA := SRA or gaExecuteByGroup;
      if info.st_mode and $0004 <> 0 then SRA := SRA or gaReadByOther;
      if info.st_mode and $0002 <> 0 then SRA := SRA or gaWriteByOther;
      if info.st_mode and $0001 <> 0 then SRA := SRA or gaExecuteByOther;
    end;

    if SRA > Rec.Attr then
      Result := - 1
    else
      if SRA < Rec.Attr then
        Result := 1;
  end;
  FindClose(SR);
end;
{$ENDIF}

function CommandToString(Rec: TGulpRec): string;
begin
  case Rec.Flags and $FF of
    gfFix:    Result := 'LST';
    gfAdd:    Result := 'ADD';
    gfDelete: Result := 'DEL';
    else      Result := '???';
  end;
end;

function SizeToString (Rec: TGulpRec): string;
begin
  Result := ' ';
  if Rec.Flags and $FF in [gfAdd] then
    if Rec.Flags and gfSize <> 0 then
    begin
      Result := Format('%u', [Rec.Size])
    end;
end;

function AttrToString (Rec: TGulpRec): string;
begin
  Result := '....... .........';
  if Rec.Flags and $FF in [gfAdd] then
  begin
    if Rec.Attr and faReadOnly       <> 0 then Result[1]  := 'R';
    if Rec.Attr and faHidden         <> 0 then Result[2]  := 'H';
    if Rec.Attr and faSysFile        <> 0 then Result[3]  := 'S';
    if Rec.Attr and faVolumeId       <> 0 then Result[4]  := 'V';
    if Rec.Attr and faDirectory      <> 0 then Result[5]  := 'D';
    if Rec.Attr and faArchive        <> 0 then Result[6]  := 'A';
    if Rec.Attr and faSymLink        <> 0 then Result[7]  := 'L';

    if Rec.Attr and gaExecuteByOwner <> 0 then Result[9]  := 'X';
    if Rec.Attr and gaReadByOwner    <> 0 then Result[10] := 'R';
    if Rec.Attr and gaWriteByOwner   <> 0 then Result[11] := 'W';
    if Rec.Attr and gaExecuteByGroup <> 0 then Result[12] := 'X';
    if Rec.Attr and gaReadByGroup    <> 0 then Result[13] := 'R';
    if Rec.Attr and gaWriteByGroup   <> 0 then Result[14] := 'W';
    if Rec.Attr and gaExecuteByOther <> 0 then Result[15] := 'X';
    if Rec.Attr and gaReadByOther    <> 0 then Result[16] := 'R';
    if Rec.Attr and gaWriteByOther   <> 0 then Result[17] := 'W';
  end;
end;

function TimeToString (Rec: TGulpRec): string;
begin
  if Rec.Flags and $FF in [gfAdd] then
    Result := FormatDateTime(
      DefaultFormatSettings.LongDateFormat + ' ' +
      DefaultFormatSettings.LongTimeFormat, Rec.ModifiedTime)
  else
    Result := '.......... ........';
end;

// =============================================================================
// GULP Format
// =============================================================================


// =============================================================================
// TGulpRec
// =============================================================================

function ClearRec(const Rec: TGulpRec) :TGulpRec;
begin
  Result                := Rec;
  Result.FFlags         := 0;
  Result.FName          := '';
  Result.FStoredTime    := 0.0;
  Result.FModifiedTime  := 0.0;
  Result.FAttr    := 0;
  Result.FSize          := 0;
  Result.FLinkName      := '';
  Result.FUID           := 0;
  Result.FUserName      := '';
  Result.FGID           := 0;
  Result.FGroupName     := '';
  Result.FChecksumOK    := FALSE;
  Result.FStartPosition := 0;
  Result.FEndPosition   := 0;
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
  FStream := Stream;
  Reset;
end;

destructor TGulpReader.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

procedure TGulpReader.Reset;
begin
  FStream.Seek(0, soBeginning);
end;

function TGulpReader.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
  SHA1Update(FCTX, Buffer, Result);
end;

function TGulpReader.ReadString (var Buffer: string): longint;
begin
  Read(Result, SizeOf(Result));
  SetLength(Buffer, Result);
  Read(Pointer(Buffer)^, Result);
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

    if gfName         and Rec.Flags <> 0 then ReadString(Rec.FName);
    if gfStoredTime   and Rec.Flags <> 0 then Read      (Rec.FStoredTime,   SizeOf (Rec.FStoredTime));
    if gfModifiedTime and Rec.Flags <> 0 then Read      (Rec.FModifiedTime, SizeOf (Rec.FModifiedTime));
    if gfAttributes   and Rec.Flags <> 0 then Read      (Rec.FAttr,         SizeOf (Rec.FAttr));
    if gfSize         and Rec.Flags <> 0 then Read      (Rec.FSize,         SizeOf (Rec.FSize));
    if gfLinkName     and Rec.Flags <> 0 then ReadString(Rec.FLinkName);
    if gfUID          and Rec.Flags <> 0 then Read      (Rec.FUID,          SizeOf (Rec.FUID));
    if gfUserName     and Rec.Flags <> 0 then ReadString(Rec.FUserName);
    if gfGID          and Rec.Flags <> 0 then Read      (Rec.FGID,          SizeOf (Rec.FGID));
    if gfGroupName    and Rec.Flags <> 0 then ReadString(Rec.FGroupName);

    if Assigned(Stream) then
    begin
      Size := Rec.Size;
      while Size <> 0 do
      begin
          Readed := Read(Buffer, Min(SizeOf(Buffer), Size));
            Stream.Write(Buffer, Readed);
        SHA1Update(FCTX, Buffer, Readed);
        Dec(Size, Readed);
      end;
    end else
      if Rec.Size <> 0 then
        FStream.Seek(Rec.Size, soCurrent);

    SHA1Final(FCTX, Digest);
    Fstream.Read(DigestAux, SizeOf(DigestAux));
    Rec.FChecksumOK := SHA1Match(DigestAux, Digest);
  end;
end;

function TGulpReader.FindNext (var Rec: TGulpRec; Stream: TStream): boolean;
begin
  Result := FALSE;
  ClearRec(Rec);
  if FStream.Position < FStream.Size then
  begin
    Rec.FStartPosition := FStream.Seek(0, soCurrent);
    Result             := ReadStream  (Rec, Stream );
    Rec.FEndPosition   := FStream.Seek(0, soCurrent);
  end;
end;

// =============================================================================
// TGulpWriter
// =============================================================================

constructor TGulpWriter.Create (Stream: TStream);
begin
  inherited Create;
  FModified   := FALSE;
  FStoredTime := Now;
  FStream     := Stream;
  FStream.Seek (0, soFromEnd);
end;

destructor TGulpWriter.Destroy;
var
  Flags: longword;
  Digest: TSHA1Digest;
begin
  if FModified = TRUE then
  begin
    Flags := gfFix or gfStoredTime;
    SHA1Init (FCTX);
    Write    (GulpMarker,  SizeOf (GulpMarker));
    Write    (Flags,       SizeOf (Flags));
    Write    (FStoredTime, SizeOf(FStoredTime));
    SHA1Final(FCTX, Digest);
    FStream.Write(Digest, SizeOf(Digest));
  end;
  FStream := nil;
  inherited Destroy;
end;

procedure TGulpWriter.Write(const Buffer; Count: longint);
begin
  FStream.Write(Buffer, Count);
  SHA1Update(FCTX, Buffer, Count);
end;

procedure TGulpWriter.WriteString(const Buffer: string);
var
  Count: longint;
begin
  Count := Length(Buffer);
  Write(Count, SizeOf(Count));
  Write(Pointer(Buffer)^, Count);
end;

procedure TGulpWriter.WriteStream (Rec: TGulpRec; Stream: TStream);
var
  Count: int64;
  Readed: longint;
  Digest: TSHA1Digest;
  Buffer: array[0..$FFFF] of byte;
begin
  FModified := TRUE;
  SHA1Init (FCTX);
  Write    (GulpMarker, SizeOf (GulpMarker));
  Write    (Rec.Flags,  SizeOf (Rec.Flags ));

  if gfName         and Rec.Flags <> 0 then WriteString(Rec.Name);
  if gfStoredTime   and Rec.Flags <> 0 then Write      (Rec.StoredTime,   SizeOf (Rec.StoredTime));
  if gfModifiedTime and Rec.Flags <> 0 then Write      (Rec.ModifiedTime, SizeOf (Rec.ModifiedTime));
  if gfAttributes   and Rec.Flags <> 0 then Write      (Rec.Attr,         SizeOf (Rec.Attr));
  if gfSize         and Rec.Flags <> 0 then Write      (Rec.Size,         SizeOf (Rec.Size));
  if gfLinkName     and Rec.Flags <> 0 then WriteString(Rec.LinkName);
  if gfUID          and Rec.Flags <> 0 then Write      (Rec.UID,          SizeOf (Rec.UID));
  if gfUserName     and Rec.Flags <> 0 then WriteString(Rec.UserName);
  if gfGID          and Rec.Flags <> 0 then Write      (Rec.GID,          SizeOf (Rec.GID));
  if gfGroupName    and Rec.Flags <> 0 then WriteString(Rec.GroupName);

  if Assigned(Stream) then
  begin
    Count := Rec.Size;
    while Count <> 0 do
    begin
      Readed := Stream.Read (Buffer, Min(SizeOf(Buffer), Count));
                      Write (Buffer, Readed);
            SHA1Update(FCTX, Buffer, Readed);
      Dec(Count, Readed);
    end;
  end;
  SHA1Final(FCTX, Digest);
  FStream.Write(Digest, SizeOf(Digest));
end;

procedure TGulpWriter.CopyFrom(var Rec: TGulpRec; Stream: TStream);
begin
  FModified := TRUE;
  Stream.Seek(Rec.FStartPosition, soBeginning);
  if Rec.FEndPosition - Rec.FStartPosition > 0 then
  begin
    FStream.CopyFrom(Stream, Rec.FEndPosition - Rec.FStartPosition);
  end;
end;

procedure TGulpWriter.Delete (const FileName: string);
var
  Rec: TGulpRec;
begin
  Rec := ClearRec(TGulpRec.Create);
  IncludeFlag(Rec.FFlags, gfDelete);

  Rec.FName := FileName;
  IncludeFlag(Rec.FFlags, gfName);

  Rec.FStoredTime := FStoredTime;
  IncludeFlag(Rec.FFlags, gfStoredTime);

  WriteStream (Rec, nil);
  FreeAndNil(Rec);
end;

{$IFDEF MSWINDOWS}
procedure TGulpWriter.Add (const FileName: string);
var
  Rec: TGulpRec;
  SR: TSearchRec;
  Stream: TStream;
begin
  Rec := ClearRec(TGulpRec.Create);
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    IncludeFlag(Rec.FFlags, gfAdd);

    Rec.FName := FileName;
    IncludeFlag(Rec.FFlags, gfName);

    Rec.FStoredTime := FStoredTime;
    IncludeFlag(Rec.FFlags, gfStoredTime);

    Rec.FModifiedTime := FileDateToDateTime(SR.Time);
    IncludeFlag(Rec.FFlags, gfModifiedTime);

    if faReadOnly  and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaReadOnly;
    if faHidden    and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaHidden;
    if faSysFile   and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaSysFile;
    if faVolumeId  and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaVolumeId;
    if faDirectory and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaDirectory;
    if faArchive   and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaArchive;
    if faSymLink   and SR.Attr <> 0 then Rec.FAttributes := Rec.FAttributes or gaSymLink;
    IncludeFlag(Rec.FFlags, gfAttributes);

    Stream := nil;
    if SR.Attr and (faDirectory or faVolumeId) = 0 then
      try
        Stream := TFileStream.Create(FileName, fmOpenRead);
      except
        Stream := nil;
      end;

    if Assigned(Stream) then
    begin
      Rec.FSize := SR.Size;
      IncludeFlag(Rec.FFlags, gfSize);
    end;

    if SR.Attr and faSymLink <> 0 then
    begin
      // nothing to do
    end;

    WriteStream(Rec, Stream);
    if Assigned(Stream) then
      FreeAndNil(Stream);
  end;
  FindClose(SR);
  FreeAndNil(Rec);
end;
{$ENDIF}

{$IFDEF UNIX}
procedure TGulpWriter.Add (const FileName: string);
var
  Info: stat;
  Rec: TGulpRec;
  SR: TSearchRec;
  Stream: TStream;
begin
  Rec := ClearRec(TGulpRec.Create);
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    IncludeFlag(Rec.FFlags, gfAdd);

    Rec.FName := StringReplace(Filename, '\', '/', [rfReplaceAll]);
    IncludeFlag(Rec.FFlags, gfName);

    Rec.FStoredTime := FStoredTime;
    IncludeFlag(Rec.FFlags, gfStoredTime);

    Rec.FModifiedTime := FileDateToDateTime(SR.Time);
    IncludeFlag(Rec.FFlags, gfModifiedTime);

    if faReadOnly  and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaReadOnly;
    if faHidden    and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaHidden;
    if faSysFile   and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaSysFile;
    if faVolumeId  and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaVolumeId;
    if faDirectory and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaDirectory;
    if faArchive   and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaArchive;
    if faSymLink   and SR.Attr <> 0 then Rec.FAttr := Rec.FAttr or gaSymLink;

    if SR.Attr and faSymLink = 0 then
    begin
      if fpstat (FileName, Info) = 0 then
      begin
        if Info.st_mode and $0100 <> 0 then Rec.FAttr := Rec.FAttr or gaReadByOwner;
        if Info.st_mode and $0080 <> 0 then Rec.FAttr := Rec.FAttr or gaWriteByOwner;
        if Info.st_mode and $0040 <> 0 then Rec.FAttr := Rec.FAttr or gaExecuteByOwner;
        if info.st_mode and $0020 <> 0 then Rec.FAttr := Rec.FAttr or gaReadByGroup;
        if info.st_mode and $0010 <> 0 then Rec.FAttr := Rec.FAttr or gaWriteByGroup;
        if info.st_mode and $0008 <> 0 then Rec.FAttr := Rec.FAttr or gaExecuteByGroup;
        if info.st_mode and $0004 <> 0 then Rec.FAttr := Rec.FAttr or gaReadByOther;
        if info.st_mode and $0002 <> 0 then Rec.FAttr := Rec.FAttr or gaWriteByOther;
        if info.st_mode and $0001 <> 0 then Rec.FAttr := Rec.FAttr or gaExecuteByOther;

        Rec.FUID := info.st_uid;
        IncludeFlag(Rec.FFlags, gfUID);
        Rec.FGID := info.st_gid;
        IncludeFlag(Rec.FFlags, gfGID);
      end;

    end else
      if fpLstat (FileName, Info) = 0 then
      begin
        if Info.st_mode and $0100 <> 0 then Rec.FAttr := Rec.FAttr or gaReadByOwner;
        if Info.st_mode and $0080 <> 0 then Rec.FAttr := Rec.FAttr or gaWriteByOwner;
        if Info.st_mode and $0040 <> 0 then Rec.FAttr := Rec.FAttr or gaExecuteByOwner;
        if Info.st_mode and $0020 <> 0 then Rec.FAttr := Rec.FAttr or gaReadByGroup;
        if Info.st_mode and $0010 <> 0 then Rec.FAttr := Rec.FAttr or gaWriteByGroup;
        if Info.st_mode and $0008 <> 0 then Rec.FAttr := Rec.FAttr or gaExecuteByGroup;
        if Info.st_mode and $0004 <> 0 then Rec.FAttr := Rec.FAttr or gaReadByOther;
        if Info.st_mode and $0002 <> 0 then Rec.FAttr := Rec.FAttr or gaWriteByOther;
        if Info.st_mode and $0001 <> 0 then Rec.FAttr := Rec.FAttr or gaExecuteByOther;

        Rec.FUID := info.st_uid;
        IncludeFlag(Rec.FFlags, gfUID);
        Rec.FGID := info.st_gid;
        IncludeFlag(Rec.FFlags, gfGID);
      end;

    IncludeFlag(Rec.FFlags, gfAttributes);

    Stream := nil;
    if SR.Attr and (faDirectory or faVolumeId or faSymLink) = 0 then
      try
        Stream := TFileStream.Create(FileName, fmOpenRead);
      except
        Stream := nil;
      end;

    if Assigned(Stream) then
    begin
      Rec.FSize := SR.Size;
      IncludeFlag(Rec.FFlags, gfSize);
    end;

    if SR.Attr and faSymLink <> 0 then
    begin
      Rec.FLinkName := fpReadLink(FileName);
      IncludeFlag(Rec.FFlags, gfLinkName);
    end;

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
  FList       := TList.Create;
  FStoredTime := StoredTime;
  FBinaryMode := FStoredTime <> 0.0;
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
  if FBinaryMode = FALSE then
  begin
    Result := -1;
    for I := FList.Count - 1 downto 0 do
      if AnsiCompareFileName(FileName, Items[I].Name) = 0 then
      begin
        if Items[I].Flags and gfDelete <> 0 then
          Result := I
        else
          Break;
      end;
  end else
    Result := BinSearch(FileName);
end;

procedure TGulpList.Delete(Index: longint);
begin
  Items[Index].Destroy;
  FList.Delete(Index);
end;

procedure TGulpList.Add(var Rec: TGulpRec);
var
  I: longint;
begin
  if FBinaryMode = TRUE then
  begin
    if Rec.StoredTime < FStoredTime + 0.00002 then
    begin
      case Rec.Flags and $FF of
      //gfFix: nothing to do
        gfAdd:  BinInsert(Rec);
        gfDelete: begin
          I := Find(Rec.Name);
          if I <> - 1 then Delete(I);
        end;
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
