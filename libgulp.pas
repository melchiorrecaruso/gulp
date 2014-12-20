unit LibGulp;

interface

uses
  {$IFDEF UNIX} BaseUnix, Unix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF LIBCUNIT} Libc, {$ENDIF}
  SysUtils,
  Classes;

const
  // --- Gulp Flags
  gfClean          = $00000001; // Clean
  gfFile           = $00000002; // Regular file
  gfLink           = $00000004; // Link
  gfSymbolicLink   = $00000008; // Symbolic link
  gfDirectory      = $00000010; // Directory entry

  gfLast           = $00010000; // Last record
  gfModifiedTime   = $00020000; // Last modification date and time
  gfAttributes     = $00040000; // File Attributes
  gfSize           = $00080000; // File size in bytes
  gfLinkName       = $00100000; // Name of linked file
  gfUID            = $00200000; // User ID
  gfUserName       = $00400000; // User Name
  gfGID            = $00800000; // Group ID
  gfGroupName      = $01000000; // Group Name
  gfSeek           = $02000000; // Record seek
  gfChecksum       = $04000000; // Rec SHA checksum

  gfReserved       = $80000000; // Extra flags

  // --- Gulp Attributes
  gaReadOnly       = $00000001; //
  gaHidden         = $00000002; //
  gaSysFile        = $00000004; //
  gaVolumeId       = $00000008; //
  gaDirectory      = $00000010; //
  gaArchive        = $00000020; //
  gaSymLink        = $00000040; //

  gaReadByOwner    = $00001000; //
  gaWriteByOwner   = $00002000; //
  gaExecuteByOwner = $00004000; //
  gaReadByGroup    = $00008000; //
  gaWriteByGroup   = $00010000; //
  gaExecuteByGroup = $00020000; //
  gaReadByOther    = $00040000; //
  gaWriteByOther   = $00080000; //
  gaExecuteByOther = $00100000; //

type
  {$IFNDEF UNICODE} rawbytestring = string; {$ENDIF}

  // --- Gulp Marker
  TGulpMarker = array [0..7] of char;

  // --- Gulp Record CLASS
  TGulpRec = class(TObject)
    Name         : ansistring;       // File path and name
    Flags        : cardinal;         // Flags
    StoredTime   : TDateTime;        // Stored date time
    ModifiedTime : TDateTime;        // Last modification date and time
    Attributes   : cardinal;         // File mode
    Size         : qword;            // File size in bytes
    LinkName     : ansistring;       // Name of linked file
    UID          : longint;          // User ID
    UserName     : ansistring;       // User Name
    GID          : longint;          // Group ID
    GroupName    : ansistring;       // Group Name
    Seek         : qword;            // Record seek
    Checksum     : ansistring;       // Rec SHA checksum
  end;

  // --- The Gulp Reader CLASS
  TGulpReader = class(TObject)
  protected
    FStream         : TStream;
    FCleanCount     : longint;
    FFileCount      : longint;
    FLinkCount      : longint;
    FSymLinkCount   : longint;
    FDirectoryCount : longint;
  public
    constructor Create  (Stream: TStream);
    destructor Destroy; override;
    procedure Reset; 
    function  FindNext   (var Rec: TGulpRec): boolean;
    procedure ReadStream (var Rec: TGulpRec; Stream: TStream);
    procedure ReadFile   (var Rec: TGulpRec);
    function GetFileSize: int64;
    function GetFilePos: int64;
    procedure SetFilePos (const NewPos : int64);
  public
    property CleanCount     : longint read FCleanCount;
    property FileCount      : longint read FFileCount;
    property LinkCount      : longint read FLinkCount;
    property SymLinkCount   : longint read FSymLinkCount;
    property DirectoryCount : longint read FDirectoryCount;
  end;

  // --- The Gulp Writer CLASS
  TGulpWriter = class(TObject)
  protected
    FStream         : TStream;
    FStoredTime     : TDateTime;
    FCleanCount     : longint;
    FFileCount      : longint;
    FLinkCount      : longint;
    FSymLinkCount   : longint;
    FDirectoryCount : longint;
    procedure WriteStream (Rec: TGulpRec; Stream: TStream);
  public
    constructor Create  (Stream: TStream);
    destructor Destroy; override;
    procedure WriteClean     (const FileName: string; LastFlag: boolean);
    procedure WriteFile      (const FileName: string; LastFlag: boolean);
    procedure WriteLink      (const FileName: string; LastFlag: boolean);
    procedure WriteSymLink   (const FileName: string; LastFlag: boolean);
    procedure WriteDirectory (const FileName: string; LastFlag: boolean);
  public
    property CleanCount     : longint read FCleanCount;
    property FileCount      : longint read FFileCount;
    property LinkCount      : longint read FLinkCount;
    property SymLinkCount   : longint read FSymLinkCount;
    property DirectoryCount : longint read FDirectoryCount;
  end;

  // --- The Gulp List CLASS
  TGulpList = class(TObject)
  private
    FList: TList;
    FBinaryMode: boolean;
    FReader: TGulpReader;
    function Get(Index: longint): TGulpRec;
    function GetCount: longint;
    procedure Delete(Index: longint);
    procedure BinInsert(Rec: TGulpRec);
    function BinSearch(const Filename: string): longint;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure LoadAt(StoredTime: TDateTime);
    function Find(const FileName: string): longint;
    property Items[Index: longint]: TGulpRec read Get;
    property Count: longint read GetCount;
  end;

// --- Some useful constants

function ConvertFileName(const FileName: string): string;
function CompareFileTime(const FileName: string; var Rec: TGulpRec): longint;
function CompareFileSize(const FileName: string; var Rec: TGulpRec): longint;
function CompareFileAttr(const FileName: string; var Rec: TGulpRec): longint;

function AttrToString(Rec: TGulpRec): string;
function SizeToString(Rec: TGulpRec): string;
function TimeToString(Rec: TGulpRec): string;

function GetFileFlag(const FileName: string): longword;

// =============================================================================
// IMPLEMENTATION
// =============================================================================

implementation

uses
  Sha1;

const
  HexaDecimals: array [0..15] of char = '0123456789ABCDEF';
  HexValues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

  GulpMarker: TGulpMarker = ('G', 'U','L','P','-', '1','0','0');

// =============================================================================
// GULP Lib Routines
// =============================================================================

function ConvertFileName(const FileName: string): string;
begin
  {$IFDEF MSWINDOWS}
    Result := StringReplace(Filename, '/', '\', [rfReplaceAll]);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := StringReplace(Filename, '\', '/', [rfReplaceAll]);
  {$ENDIF}
end;

function CompareFileTime(const FileName: string; var Rec: TGulpRec): longint;
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

function CompareFileSize(const FileName: string; var Rec: TGulpRec): longint;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    if (SR.Attr and faDirectory = 0) or (Rec.Attributes and gaDirectory = 0) then
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

function CompareFileAttr(const FileName: string; var Rec: TGulpRec): longint;
var
  SR: TSearchRec;
  SRA:  cardinal;
  {$IFDEF UNIX} info: stat; {$ENDIF}
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
    {$IFDEF UNIX}
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
    {$ENDIF}
    if SRA > Rec.Attributes then
      Result := - 1
    else
      if SRA < Rec.Attributes then
        Result := 1;
  end;
  FindClose(SR);
end;

function SizeToString(Rec: TGulpRec): string;
begin
  Result := ' ';
  if Rec.Flags and gfSize <> 0 then
  begin
    Result := Format('%u', [Rec.Size])
  end;
end;

function AttrToString (Rec: TGulpRec): string;
begin
  if Rec.Flags and gfclean = 0 then
  begin
    Result := 'ADD ....... .........';
    if Rec.Attributes and faReadOnly       <> 0 then Result[5]  := 'R';
    if Rec.Attributes and faHidden         <> 0 then Result[6]  := 'H';
    if Rec.Attributes and faSysFile        <> 0 then Result[7]  := 'S';
    if Rec.Attributes and faVolumeId       <> 0 then Result[8]  := 'V';
    if Rec.Attributes and faDirectory      <> 0 then Result[9]  := 'D';
    if Rec.Attributes and faArchive        <> 0 then Result[10] := 'A';
    if Rec.Attributes and faSymLink        <> 0 then Result[11] := 'L';

    if Rec.Attributes and gaExecuteByOwner <> 0 then Result[13] := 'X';
    if Rec.Attributes and gaReadByOwner    <> 0 then Result[14] := 'R';
    if Rec.Attributes and gaWriteByOwner   <> 0 then Result[15] := 'W';
    if Rec.Attributes and gaExecuteByGroup <> 0 then Result[16] := 'X';
    if Rec.Attributes and gaReadByGroup    <> 0 then Result[17] := 'R';
    if Rec.Attributes and gaWriteByGroup   <> 0 then Result[18] := 'W';
    if Rec.Attributes and gaExecuteByOther <> 0 then Result[19] := 'X';
    if Rec.Attributes and gaReadByOther    <> 0 then Result[20] := 'R';
    if Rec.Attributes and gaWriteByOther   <> 0 then Result[21] := 'W';
  end else
    Result := 'DEL ....... .........';
end;

function TimeToString (Rec: TGulpRec): string;
begin
  if Rec.Flags and gfclean = 0 then
    Result := FormatDateTime(
      DefaultFormatSettings.LongDateFormat + ' ' +
      DefaultFormatSettings.LongTimeFormat, Rec.ModifiedTime)
  else
    Result := '.......... ........';
end;

function Hex(const Data; Count: longint): string;
var
  I, J: longint;
  K:    longword;
begin
  SetLength(Result, Count shl 1);
  J := 1;
  for I := 0 to Count - 1 do
  begin
    K := TByteArray(Data) [I];
    Result[J] := HexaDecimals[K shr 4];
    Inc(J);
    Result[J] := HexaDecimals[K and $f];
    Inc(J);
  end;
end;

function HexToData(const S: string; var Data; Count: longint): boolean;
var
  I: longint;
begin
  Result := FALSE;
  if Length(S) < Count * 2 then Exit;

  for I := 0 to Count - 1 do
  begin
    if (S[I * 2 + 1] in ['0'..'9', 'A'..'F']) and (S[I * 2 + 2] in ['0'..'9', 'A'..'F']) then
    begin
      TByteArray(Data)[I] := HexValues[S[I * 2 + 1]] shl 4 + HexValues[S[I * 2 + 2]]
    end else
      Exit;
  end;
  Result := TRUE;
end;

// =============================================================================
// GULP Format
// =============================================================================

procedure ClearRec(var Rec: TGulpRec);
begin
  Rec.Name         := '';
  Rec.Flags        := 0;
  Rec.StoredTime   := 0.0;
  Rec.ModifiedTime := 0.0;
  Rec.Attributes   := 0;
  Rec.Size         := 0;
  Rec.LinkName     := '';
  Rec.UID          := 0;
  Rec.UserName     := '';
  Rec.GID          := 0;
  Rec.GroupName    := '';
  Rec.Seek         := 0;
  Rec.Checksum     := '';
end;


procedure ReadGulpRec (Stream: TStream; var Rec: TGulpRec);
begin
  Rec.Name := Stream.ReadAnsiString;
  Stream.Read (Rec.Flags, SizeOf (Rec.Flags));
  Stream.Read (Rec.StoredTime, SizeOf (Rec.StoredTime));

  if gfModifiedTime and Rec.Flags <> 0 then Stream.Read(Rec.ModifiedTime, SizeOf (Rec.ModifiedTime));
  if gfAttributes   and Rec.Flags <> 0 then Stream.Read(Rec.Attributes, SizeOf (Rec.Attributes));
  if gfSize         and Rec.Flags <> 0 then Stream.Read(Rec.Size, SizeOf (Rec.Size));
  if gfLinkName     and Rec.Flags <> 0 then Rec.LinkName := Stream.ReadAnsiString;
  if gfUID          and Rec.Flags <> 0 then Stream.Read(Rec.UID, SizeOf (Rec.UID));
  if gfUserName     and Rec.Flags <> 0 then Rec.UserName := Stream.ReadAnsiString;
  if gfGID          and Rec.Flags <> 0 then Stream.Read(Rec.GID, SizeOf (Rec.GID));
  if gfGroupName    and Rec.Flags <> 0 then Rec.GroupName := Stream.ReadAnsiString;
  if gfSeek         and Rec.Flags <> 0 then Stream.Read(Rec.Seek, SizeOf (Rec.Seek));
  // ---
  Stream.Seek (Rec.Size, soCurrent);
  // ---
  if gfChecksum     and Rec.Flags <> 0 then Rec.Checksum := Stream.ReadAnsiString;
end;

// =============================================================================
// TGulpRec
// =============================================================================

function GetFileFlag(const FileName: string): longword;
var
  SR: TSearchRec;
begin
  Result := gfClean;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    if SR.Attr and faDirectory <> 0 then Result := gfDirectory else
    if SR.Attr and faSymLink   <> 0 then Result := gfLink      else Result := gfFile;
  end;
  FindClose(SR);
end;

function GetFileAttributes(const FileName: string): longword;
var
  SR: TSearchRec;
  SRA:  cardinal;
  {$IFDEF UNIX} info: stat; {$ENDIF}
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
    {$IFDEF UNIX}
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
    {$ENDIF}
    Result := SRA;
  end;
end;

function GetFileLink(const FileName: string): string;
begin
  Result := fpReadLink(FileName);
end;

function GetFileUID(const FileName: string): longword;
var
  info: stat;
begin
  Result := longword(-1);
  if fpstat (FileName, info) <> 0 then
    Result := info.st_uid;
end;

function GetFileGID(const FileName: string): longword;
var
  info: stat;
begin
  Result := longword(-1);
  if fpstat (FileName, info) <> 0 then
    Result := info.st_gid;
end;

// =============================================================================
// TGulpReader
// =============================================================================

constructor TGulpReader.Create (Stream : TStream);
begin
  inherited Create;
  FStream := Stream;
end;

destructor TGulpReader.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

procedure TGulpReader.Reset;
begin
  FStream.Seek (0, soBeginning);
  FCleanCount     := 0;
  FFileCount      := 0;
  FLinkCount      := 0;
  FSymLinkCount   := 0;
  FDirectoryCount := 0;
end;

function TGulpReader.FindNext (var Rec: TGulpRec): boolean;
var
  Marker: TGulpMarker;
begin
  Result := FALSE;
  if FStream.Position < FStream.Size then
  begin
    FStream.Read(Marker, SizeOf (Marker));
    Result := Marker = GulpMarker;
    if Result then
    begin
      ClearRec (Rec);
      ReadGulpRec (FStream, Rec);
      if gfClean        and Rec.Flags <> 0 then Inc(FCleanCount)     else
      if gfFile         and Rec.Flags <> 0 then Inc(FFileCount)      else
      if gfLink         and Rec.Flags <> 0 then Inc(FLinkCount)      else
      if gfSymbolicLink and Rec.Flags <> 0 then Inc(FSymLinkCount)   else
      if gfDirectory    and Rec.Flags <> 0 then Inc(FDirectoryCount);
    end;
  end;
end;

procedure TGulpReader.ReadStream (var Rec: TGulpRec; Stream: TStream);
begin
  if Rec.Flags and gfSize <> 0 then
    if Rec.Size <> 0 then
    begin
      FStream.Seek (Rec.Seek, soBeginning);
      Stream.CopyFrom (FStream, Rec.Size);
    end;
end;

procedure TGulpReader.ReadFile (var Rec: TGulpRec);
var
  S: TFileStream;
begin
  try
    S := TFileStream.Create (Rec.Name, fmCreate);
  except
    S := nil;
  end;

  if Assigned(S) then
  begin
    ReadStream (Rec, S);
    FreeAndNil(S);
  end;
end;

function TGulpReader.GetFileSize: int64;
begin
  Result := FStream.Size;
end;

function TGulpReader.GetFilePos: int64;
begin
  Result := FStream.Position;
end;

procedure TGulpReader.SetFilePos (const NewPos: int64);
begin
  FStream.Seek (NewPos, soBeginning);
end;

// =============================================================================
// TGulpWriter
// =============================================================================

constructor TGulpWriter.Create (Stream: TStream);
begin
  inherited Create;
  FStream         := Stream;
  FStream.Seek(0, soFromEnd);
  FStoredTime     := Now;
  FCleanCount     := 0;
  FFileCount      := 0;
  FLinkCount      := 0;
  FSymLinkCount   := 0;
  FDirectoryCount := 0;
end;

destructor TGulpWriter.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

procedure TGulpWriter.WriteStream (Rec: TGulpRec; Stream: TStream);
var
  Count: longint;
  CTX: TSHA1Context;
  Digest: TSHA1Digest;
  Buffer: array[0..$FFFF] of byte;
begin
  FStream.Write(GulpMarker, SizeOf (TGulpMarker));

  FStream.WriteAnsiString(Rec.Name);
  FStream.Write (Rec.Flags, SizeOf (Rec.Flags));
  FStream.Write (Rec.StoredTime, SizeOf (Rec.StoredTime));

  if gfModifiedTime and Rec.Flags <> 0 then
    FStream.Write(Rec.ModifiedTime, SizeOf (Rec.ModifiedTime));

  if gfAttributes   and Rec.Flags <> 0 then
    FStream.Write(Rec.Attributes, SizeOf (Rec.Attributes));

  if gfSize         and Rec.Flags <> 0 then
    FStream.Write(Rec.Size, SizeOf (Rec.Size));

  if gfLinkName     and Rec.Flags <> 0 then
    FStream.WriteAnsiString(Rec.LinkName);

  if gfUID          and Rec.Flags <> 0 then
    FStream.Write(Rec.UID, SizeOf (Rec.UID));

  if gfUserName     and Rec.Flags <> 0 then
    FStream.WriteAnsiString(Rec.UserName);

  if gfGID          and Rec.Flags <> 0 then
    FStream.Write(Rec.GID, SizeOf (Rec.GID));

  if gfGroupName    and Rec.Flags <> 0 then
    FStream.WriteAnsiString(Rec.GroupName);

  if gfSeek         and Rec.Flags <> 0 then
    FStream.Write(Rec.Seek, SizeOf (Rec.Seek));

  if Rec.Flags and gfSize <> 0 then
  begin
    SHA1Init(CTX);
    Count := Stream.Read (Buffer[0], SizeOf(Buffer));
    while Count <> 0 do
    begin
            SHA1Update(CTX, Buffer[0], Count);
             FStream.Write (Buffer[0], Count);
      Count := Stream.Read (Buffer[0], SizeOf(Buffer));
    end;
    SHA1Final(CTX, Digest);
    Rec.Checksum := SHA1Print(Digest);
  end;

  if gfChecksum  and Rec.Flags <> 0 then
    FStream.WriteAnsiString(Rec.Checksum);
end;

procedure TGulpWriter.WriteClean (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
begin
  Rec := TGulpRec.Create;
  ClearRec(Rec);
  Rec.Name       := FileName;
  Rec.Flags      := gfClean;
  if LastFlag then
    Rec.Flags := Rec.Flags or gfLast;

  Rec.StoredTime := FStoredTime;

  WriteStream (Rec, nil);
  FreeAndNil(Rec);

  Inc(FCleanCount);
end;

procedure TGulpWriter.WriteFile (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
  Stream: TFileStream;
begin
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    Rec := TGulpRec.Create;
    ClearRec(Rec);
    Rec.Name  := FileName;
    Rec.Flags :=
      gfFile         or
      gfModifiedTime or
      gfAttributes   or
      gfSize         or
      gfUID          or
      gfGID          or
      gfSeek         or
      gfChecksum;

    if LastFlag then
      Rec.Flags := Rec.Flags or gfLast;

    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.Size         := SR.Size;
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);
    Rec.Seek         := FStream.Seek(0, soCurrent);

    Stream := TFileStream.Create (FileName, fmOpenRead or fmShareDenyWrite);
    WriteStream(Rec, Stream);
    FreeAndNil(Stream);
    FreeAndNil(Rec);

    Inc(FFileCount);
  end;
  FindClose(SR);
end;

procedure TGulpWriter.WriteLink (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    Rec := TGulpRec.Create;
    ClearRec(Rec);
    Rec.Name  := FileName;
    Rec.Flags :=
      gfLink         or
      gfModifiedTime or
      gfAttributes   or
      gfLinkName     or
      gfUID          or
      gfGID;

    if LastFlag then
      Rec.Flags := Rec.Flags or gfLast;

    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.LinkName     := fpReadLink(FileName);
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);

    WriteStream (Rec, nil);
    FreeAndNil(Rec);

    Inc(FLinkCount);
  end;
  FindClose(SR);
end;

procedure TGulpWriter.WriteSymLink (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    Rec := TGulpRec.Create;
    ClearRec(Rec);
    Rec.Name  := FileName;
    Rec.Flags :=
      gfSymbolicLink or
      gfModifiedTime or
      gfAttributes   or
      gfLinkName     or
      gfUID          or
      gfGID;

    if LastFlag then
      Rec.Flags := Rec.Flags or gfLast;

    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.LinkName     := fpReadLink(FileName);
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);

    WriteStream (Rec, nil);
    FreeAndNil (Rec);

    Inc(FSymLinkCount);
  end;
  FindClose(SR);
end;

procedure TGulpWriter.WriteDirectory (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    Rec := TGulpRec.Create;
    ClearRec(Rec);
    Rec.Name  := FileName;
    Rec.Flags :=
      gfDirectory    or
      gfModifiedTime or
      gfAttributes   or
      gfUID          or
      gfGID;

    if LastFlag then
      Rec.Flags := Rec.Flags or gfLast;

    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);

    WriteStream (Rec, nil);
    FreeAndNil(Rec);

    Inc(FDirectoryCount);
  end;
  FindClose(SR);
end;

// =============================================================================
// TGulpList
// =============================================================================

constructor TGulpList.Create(Stream: TStream);
begin
  inherited Create;
  FReader     := TGulpReader.Create(Stream);
  FList       := TList.Create;
  FBinaryMode := TRUE;
end;

destructor TGulpList.Destroy;
begin
  Clear;
  FList.Destroy;
  FReader.Destroy;
  inherited Destroy;
end;

procedure TGulpList.Clear;
var
  I: longint;
begin
  for I := 0 to FList.Count - 1 do
    TGulpRec(FList[I]).Destroy;
  FList.Clear;
end;

function TGulpList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TGulpList.Get(Index: longint): TGulpRec;
begin
  Result := TGulpRec(FList[Index]);
end;

procedure TGulpList.BinInsert(Rec: TGulpRec);
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
        if Items[I].Flags and gfClean <> 0 then
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

procedure TGulpList.LoadAt(StoredTime: TDateTime);
var
  I: longint;
  Rec: TGulpRec;
begin
  Clear;
  FReader.Reset;
  Rec := TGulpRec.Create;
  while FReader.FindNext(Rec) do
  begin
    if Rec.StoredTime < StoredTime + 0.00002 then
    begin
      if Rec.Flags and gfClean = 0 then
      begin
        BinInsert(Rec);
        Rec := TGulpRec.Create;
      end else
      begin
        I := Find(Rec.Name);
        if I <> - 1 then Delete(I);
      end;
    end;
  end;
  Rec.Destroy;
  FBinaryMode := TRUE;
end;

procedure TGulpList.Load;
var
  Rec: TGulpRec;
begin
  Clear;
  FReader.Reset;
  Rec := TGulpRec.Create;
  while FReader.FindNext(Rec) do
  begin
    FList.Add(Rec);
    Rec := TGulpRec.Create;
  end;
  Rec.Destroy;
  FBinaryMode := FALSE;
end;

end.
