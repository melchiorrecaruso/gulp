unit LibGulp;

interface

uses
  {$IFDEF UNIX} BaseUnix, Unix, {$ENDIF}
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF LIBCUNIT} Libc, {$ENDIF}
  SysUtils,
  Classes,
  SHA1;

const
  // --- Gulp Flags ---
  gfUnknow         = $00000000;
  gfAdd            = $00000001; // Add command
  gfDelete         = $00000002; // Delete command
  gfUpdate         = $00000004; // Update command

  gfFile           = $00000100; // Regular file
  gfLink           = $00000200; // Link
  gfSymbolicLink   = $00000400; // Symbolic link
  gfDirectory      = $00000800; // Directory entry

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

  // --- Gulp Attributes ---
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

  // --- Gulp Marker ---
  TGulpMarker = array [0..7] of char;

  // --- Gulp Record CLASS ---
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
    // ---                           // Record data
    Checksum     : ansistring;       // Record SHA checksum
    ChecksumAux  : ansistring;       // Record SHA checksum aux (reserved)
  end;

  // --- The Gulp Reader CLASS ---
  TGulpReader = class(TObject)
  protected
    FCTX            : TSHA1Context;
    FStream         : TStream;
    FFileCount      : longint;
    FLinkCount      : longint;
    FSymLinkCount   : longint;
    FDirectoryCount : longint;
    FDeletionCount  : longint;
    function  ReadString (var Buffer: string): longint;
    function  Read (var Buffer; Count: longint): longint;
  public
    constructor Create  (Stream: TStream);
    destructor Destroy; override;
    procedure Reset;

    function  ReadStream (var Rec: TGulpRec; Stream: TStream): boolean;

    function  FindNext  (var Rec: TGulpRec): boolean;
    procedure ReadRec   (var Rec: TGulpRec);
    function  CheckRec  (var Rec: TGulpRec): boolean;


    function GetFileSize: int64;
    function GetFilePos: int64;
    procedure SetFilePos (const NewPos : int64);
  public
    property FileCount      : longint read FFileCount;
    property LinkCount      : longint read FLinkCount;
    property SymLinkCount   : longint read FSymLinkCount;
    property DirectoryCount : longint read FDirectoryCount;
    property DeletionCount  : longint read FDeletionCount;
  end;

  // --- The Gulp Writer CLASS ---
  TGulpWriter = class(TObject)
  protected
    FCTX            : TSHA1Context;
    FStoredTime     : TDateTime;
    FStream         : TStream;
    FFileCount      : longint;
    FLinkCount      : longint;
    FSymLinkCount   : longint;
    FDirectoryCount : longint;
    FDeletionCount   : longint;
    procedure WriteStream (Rec: TGulpRec; Stream: TStream);
    procedure WriteString (const Buffer: string);
    procedure Write (const Buffer; Count: longint);
  public
    constructor Create  (Stream: TStream);
    destructor Destroy; override;
    procedure Delete       (const FileName: string; LastFlag: boolean);
    procedure AddFile      (const FileName: string; LastFlag: boolean);
    procedure AddLink      (const FileName: string; LastFlag: boolean);
    procedure AddSymLink   (const FileName: string; LastFlag: boolean);
    procedure AddDirectory (const FileName: string; LastFlag: boolean);
  public
    property FileCount      : longint read FFileCount;
    property LinkCount      : longint read FLinkCount;
    property SymLinkCount   : longint read FSymLinkCount;
    property DirectoryCount : longint read FDirectoryCount;
    property DeletionCount  : longint read FDeletionCount;
  end;

  // --- The Gulp List CLASS ---
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

    function GetDeletionCount: longint;
    function GetFileCount: longint;
    function GetLinkCount: longint;
    function GetSymLinkCount: longint;
    function GetDirectoryCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure LoadAt(StoredTime: TDateTime);
    function Find(const FileName: string): longint;
  public
    property FileCount      : longint read GetFileCount;
    property LinkCount      : longint read GetLinkCount;
    property SymLinkCount   : longint read GetSymLinkCount;
    property DirectoryCount : longint read getDirectoryCount;
    property DeletionCount  : longint read GetDeletionCount;

    property Items[Index: longint]: TGulpRec read Get;
    property Count: longint read GetCount;
  end;

// --- Some useful constants

function ConvertFileName(const FileName: string): string;
function CompareFileTime(const FileName: string; var Rec: TGulpRec): longint;
function CompareFileSize(const FileName: string; var Rec: TGulpRec): longint;
function CompareFileAttr(const FileName: string; var Rec: TGulpRec): longint;

function CommToString(Rec: TGulpRec): string;
function AttrToString(Rec: TGulpRec): string;
function SizeToString(Rec: TGulpRec): string;
function TimeToString(Rec: TGulpRec): string;

function GetFileType(const FileName: string): longword;

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

function GetFileType(const FileName: string): longword;
var
  SR: TSearchRec;
begin
  Result := gfUnknow;
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

function GetFileUserName(const FileName: string): string;
begin
  Result := '';
end;

function GetFileGroupName(const FileName: string): string;
begin
  Result := '';
end;

function CommToString(Rec: TGulpRec): string;
begin
  case Rec.Flags and $FF of
    gfAdd:    Result := 'ADD';
    gfDelete: Result := 'DEL';
    gfUpdate: Result := 'UPD';
    else      Result := '???';
  end;
end;

function SizeToString (Rec: TGulpRec): string;
begin
  Result := ' ';
  if Rec.Flags and gfSize <> 0 then
  begin
    Result := Format('%u', [Rec.Size])
  end;
end;

function AttrToString (Rec: TGulpRec): string;
begin
  if Rec.Flags and $FF <> gfDelete then
  begin
    Result := '....... .........';
    if Rec.Attributes and faReadOnly       <> 0 then Result[1]  := 'R';
    if Rec.Attributes and faHidden         <> 0 then Result[2]  := 'H';
    if Rec.Attributes and faSysFile        <> 0 then Result[3]  := 'S';
    if Rec.Attributes and faVolumeId       <> 0 then Result[4]  := 'V';
    if Rec.Attributes and faDirectory      <> 0 then Result[5]  := 'D';
    if Rec.Attributes and faArchive        <> 0 then Result[6]  := 'A';
    if Rec.Attributes and faSymLink        <> 0 then Result[7]  := 'L';

    if Rec.Attributes and gaExecuteByOwner <> 0 then Result[9]  := 'X';
    if Rec.Attributes and gaReadByOwner    <> 0 then Result[10] := 'R';
    if Rec.Attributes and gaWriteByOwner   <> 0 then Result[11] := 'W';
    if Rec.Attributes and gaExecuteByGroup <> 0 then Result[12] := 'X';
    if Rec.Attributes and gaReadByGroup    <> 0 then Result[13] := 'R';
    if Rec.Attributes and gaWriteByGroup   <> 0 then Result[14] := 'W';
    if Rec.Attributes and gaExecuteByOther <> 0 then Result[15] := 'X';
    if Rec.Attributes and gaReadByOther    <> 0 then Result[16] := 'R';
    if Rec.Attributes and gaWriteByOther   <> 0 then Result[17] := 'W';
  end else
    Result := '....... .........';
end;

function TimeToString (Rec: TGulpRec): string;
begin
  if Rec.Flags and gfDelete = 0 then
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
// TGulpRec routines
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
  Rec.ChecksumAux  := '';
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
end;

destructor TGulpReader.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

procedure TGulpReader.Reset;
begin
  FStream.Seek (0, soBeginning);

  FDeletionCount     := 0;
  FFileCount      := 0;
  FLinkCount      := 0;
  FSymLinkCount   := 0;
  FDirectoryCount := 0;
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
  Count: int64;
  Readed: longint;
  Digest: TSHA1Digest;
  Buffer: array[0..$FFFF] of byte;
  Marker: TGulpMarker;
begin
  SHA1Init (FCTX);
  FillChar (Marker, SizeOf (Marker), 0);
  Read     (Marker, SizeOf (Marker));
  Result := Marker = GulpMarker;
  if Result then
  begin
    ClearRec   (Rec);
    ReadString (Rec.Name);
    Read       (Rec.Flags, SizeOf (Rec.Flags));
    Read       (Rec.StoredTime, SizeOf (Rec.StoredTime));

    if gfModifiedTime and Rec.Flags <> 0 then Read      (Rec.ModifiedTime, SizeOf (Rec.ModifiedTime));
    if gfAttributes   and Rec.Flags <> 0 then Read      (Rec.Attributes,   SizeOf (Rec.Attributes));
    if gfSize         and Rec.Flags <> 0 then Read      (Rec.Size,         SizeOf (Rec.Size));
    if gfLinkName     and Rec.Flags <> 0 then ReadString(Rec.LinkName);
    if gfUID          and Rec.Flags <> 0 then Read      (Rec.UID,          SizeOf (Rec.UID));
    if gfUserName     and Rec.Flags <> 0 then ReadString(Rec.UserName);
    if gfGID          and Rec.Flags <> 0 then Read      (Rec.GID,          SizeOf (Rec.GID));
    if gfGroupName    and Rec.Flags <> 0 then ReadString(Rec.GroupName);
    if gfSeek         and Rec.Flags <> 0 then Read      (Rec.Seek,         SizeOf (Rec.Seek));

    if Assigned(Stream) then
    begin
      Count := Rec.Size;
      while Count <> 0 do
      begin
          Readed := Read(Buffer, Min(SizeOf(Buffer), Count));
            Stream.Write(Buffer, Readed);
        SHA1Update(FCTX, Buffer, Readed);
        Dec(Count, Readed);
      end;
    end else
      FStream.Seek(Rec.Size, soCurrent);

    SHA1Final(FCTX, Digest);
    Rec.ChecksumAux := SHA1Print(Digest);
    if gfChecksum and Rec.Flags <> 0 then
      Rec.Checksum := FStream.ReadAnsiString;
  end;
end;

function TGulpReader.FindNext (var Rec: TGulpRec): boolean;
begin
  Result := FALSE;
  if FStream.Position < FStream.Size then
  begin
    Result := ReadStream(Rec, nil);
    if Result then
    begin
      case Rec.Flags and $FF of
      //gfUpdate:
        gfDelete: Inc(FDeletionCount);
        gfAdd: case Rec.Flags and $FF00 of
                 gfFile:         Inc(FFileCount);
                 gfLink:         Inc(FLinkCount);
                 gfSymbolicLink: Inc(FSymLinkCount);
                 gfDirectory:    Inc(FDirectoryCount);
               end;
      end;
    end;
  end;
end;

function TGulpReader.CheckRec(var Rec: TGulpRec): boolean;
begin
  Result := Rec.Checksum = Rec.ChecksumAux;
end;

procedure TGulpReader.ReadRec (var Rec: TGulpRec);
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
  FStoredTime     := Now;
  FStream         := Stream;
  FStream.Seek(0, soFromEnd);

  FDeletionCount  := 0;
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
  SHA1Init    (FCTX);
  Write       (GulpMarker, SizeOf (GulpMarker));
  WriteString (Rec.Name);
  Write       (Rec.Flags, SizeOf (Rec.Flags));
  Write       (Rec.StoredTime, SizeOf (Rec.StoredTime));

  if gfModifiedTime and Rec.Flags <> 0 then Write      (Rec.ModifiedTime, SizeOf (Rec.ModifiedTime));
  if gfAttributes   and Rec.Flags <> 0 then Write      (Rec.Attributes,   SizeOf (Rec.Attributes));
  if gfSize         and Rec.Flags <> 0 then Write      (Rec.Size,         SizeOf (Rec.Size));
  if gfLinkName     and Rec.Flags <> 0 then WriteString(Rec.LinkName);
  if gfUID          and Rec.Flags <> 0 then Write      (Rec.UID,          SizeOf (Rec.UID));
  if gfUserName     and Rec.Flags <> 0 then WriteString(Rec.UserName);
  if gfGID          and Rec.Flags <> 0 then Write      (Rec.GID,          SizeOf (Rec.GID));
  if gfGroupName    and Rec.Flags <> 0 then WriteString(Rec.GroupName);
  if gfSeek         and Rec.Flags <> 0 then Write      (Rec.Seek,         SizeOf (Rec.Seek));

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
  Rec.Checksum := SHA1Print(Digest);
  if gfChecksum and Rec.Flags <> 0 then
    FStream.WriteAnsiString(Rec.Checksum);
end;

procedure TGulpWriter.Delete (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
begin
  Rec := TGulpRec.Create;
  // ---
  ClearRec(Rec);
  IncludeFlag(Rec.Flags, gfDelete);
  IncludeFlag(Rec.Flags, gfChecksum);
  if LastFlag then
    IncludeFlag(Rec.Flags, gfLast);

  Rec.Name       := FileName;
  Rec.StoredTime := FStoredTime;

  WriteStream (Rec, nil);
  Inc(FDeletionCount);
  // ---
  FreeAndNil(Rec);
end;

procedure TGulpWriter.AddFile (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
  Stream: TFileStream;
begin
  Rec := TGulpRec.Create;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    ClearRec(Rec);
    IncludeFlag(Rec.Flags, gfAdd);
    IncludeFlag(Rec.Flags, gfFile);
    IncludeFlag(Rec.Flags, gfModifiedTime);
    IncludeFlag(Rec.Flags, gfAttributes);
    IncludeFlag(Rec.Flags, gfSize);
    IncludeFlag(Rec.Flags, gfUID);
    IncludeFlag(Rec.Flags, gfGID);
    IncludeFlag(Rec.Flags, gfSeek);
    IncludeFlag(Rec.Flags, gfChecksum);
    if LastFlag then
      IncludeFlag(Rec.Flags, gfLast);

    Rec.Name         := FileName;
    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.Size         := SR.Size;
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);
    Rec.Seek         := FStream.Seek(0, soCurrent);

    try
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    except
      ExcludeFlag(Rec.Flags, gfSize);
      Stream := nil;
    end;
    WriteStream(Rec, Stream);
    if Assigned(Stream) then
      FreeAndNil(Stream);
    Inc(FFileCount);
  end;
  FindClose(SR);
  FreeAndNil(Rec);
end;

procedure TGulpWriter.AddLink (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
begin
  Rec := TGulpRec.Create;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    ClearRec(Rec);
    IncludeFlag(Rec.Flags, gfAdd);
    IncludeFlag(Rec.Flags, gfLink);
    IncludeFlag(Rec.Flags, gfModifiedTime);
    IncludeFlag(Rec.Flags, gfAttributes);
    IncludeFlag(Rec.Flags, gfLinkName);
    IncludeFlag(Rec.Flags, gfUID);
    IncludeFlag(Rec.Flags, gfGID);
    IncludeFlag(Rec.Flags, gfChecksum);
    if LastFlag then
      IncludeFlag(Rec.Flags, gfLast);

    Rec.Name         := FileName;
    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.LinkName     := fpReadLink(FileName);
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);

    WriteStream(Rec, nil);
    Inc(FLinkCount);
  end;
  FindClose(SR);
  FreeAndNil(Rec);
end;

procedure TGulpWriter.AddSymLink (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
begin
  Rec := TGulpRec.Create;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    ClearRec(Rec);
    IncludeFlag(Rec.Flags, gfAdd);
    IncludeFlag(Rec.Flags, gfSymbolicLink);
    IncludeFlag(Rec.Flags, gfModifiedTime);
    IncludeFlag(Rec.Flags, gfAttributes);
    IncludeFlag(Rec.Flags, gfLinkName);
    IncludeFlag(Rec.Flags, gfUID);
    IncludeFlag(Rec.Flags, gfGID);
    IncludeFlag(Rec.Flags, gfChecksum);
    if LastFlag then
      IncludeFlag(Rec.Flags, gfLast);

    Rec.Name         := FileName;
    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.LinkName     := fpReadLink(FileName);
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);

    WriteStream(Rec, nil);
    Inc(FSymLinkCount);
  end;
  FindClose(SR);
  FreeAndNil(Rec);
end;

procedure TGulpWriter.AddDirectory (const FileName: string; LastFlag: boolean);
var
  Rec: TGulpRec;
  SR: TSearchRec;
begin
  Rec := TGulpRec.Create;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
  begin
    ClearRec(Rec);
    IncludeFlag(Rec.Flags, gfAdd);
    IncludeFlag(Rec.Flags, gfDirectory);
    IncludeFlag(Rec.Flags, gfModifiedTime);
    IncludeFlag(Rec.Flags, gfAttributes);
    IncludeFlag(Rec.Flags, gfUID);
    IncludeFlag(Rec.Flags, gfGID);
    IncludeFlag(Rec.Flags, gfChecksum);
    if LastFlag then
      IncludeFlag(Rec.Flags, gfLast);

    Rec.Name         := FileName;
    Rec.StoredTime   := FStoredTime;
    Rec.ModifiedTime := FileDateToDateTime(SR.Time);
    Rec.Attributes   := GetFileAttributes(FileName);
    Rec.UID          := GetFileUID(FileName);
    Rec.GID          := GetFileGID(FileName);

    WriteStream(Rec, nil);
    Inc(FDirectoryCount);
  end;
  FindClose(SR);
  FreeAndNil(Rec);
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
    Items[I].Destroy;
  FList.Clear;
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
      if Rec.Flags and gfDelete = 0 then
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

function TGulpList.Get(Index: longint): TGulpRec;
begin
  Result := TGulpRec(FList[Index]);
end;

function TGulpList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TGulpList.GetDeletionCount: longint;
begin
  Result := FReader.DeletionCount;
end;

function TGulpList.GetFileCount: longint;
begin
  Result := FReader.FileCount;
end;

function TGulpList.GetLinkCount: longint;
begin
  Result := FReader.LinkCount;
end;

function TGulpList.GetSymLinkCount: longint;
begin
  Result := FReader.FSymLinkCount;
end;

function TGulpList.GetDirectoryCount: longint;
begin
  Result := FReader.DirectoryCount;
end;

end.
