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
  {$IFDEF UNIX} BaseUnix, {$ENDIF} Classes,
  GulpCommon,
  GulpList,
  GulpFixes,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} Sha1,
  SysUtils;

type
  // --- Gulp Item Flags

  TGulpFlag = (gfAdd, gfDelete, gfClose, gfFullName, gfTimeUTC,
    gfAttributes, gfMode, gfSize, gfLinkName, gfUserID,
    gfGroupID, gfUserName, gfGroupName, gfComment);

  TGulpFlags = set of TGulpFlag;

  // --- Gulp item ---

  PGulpItem = ^TGulpItem;

  TGulpItem = record
    Flags:      TGulpFlags;       // Flags
    FullName:   rawbytestring;    // Full Filename
    TimeUTC:    TDateTime;        // Last modification date and time (UTC)
    Attributes: longint;          // Attributes (MSWindows/Unix)
    Mode:       longint;          // Mode (Unix)
    Size:       int64;            // Size in bytes
    LinkName:   rawbytestring;    // Name of link
    UserID:     longword;         // User ID
    GroupID:    longword;         // Group ID
    UserName:   rawbytestring;    // User Name
    GroupName:  rawbytestring;    // Group Name
    Comment:    rawbytestring;    // Comment
    Beginning:  int64;            // Stream begin position (reserved)
    Ending:     int64;            // Stream end position   (reserved)
    Version:    longword;         // Version               (reserved)
  end;

  // --- The Gulp Application events ---
  TGulpShowItem    = procedure(P: PGulpItem) of object;
  TGulpShowMessage = procedure(const Message: rawbytestring) of object;
  TGulpTerminate   = function: boolean of object;

  // --- The Gulp Application class ---
  TGulpApplication = class(TObject)
  private
    FExclude:       TRawByteStringList;
    FInclude:       TRawByteStringList;
    FNodelete:      boolean;
    FUntilVersion:  longword;
    FOnShowItem:    TGulpShowItem;
    FOnShowMessage: TGulpShowMessage;
    procedure ShowItem(const Item: PGulpItem);
    procedure ShowMessage(const Message: rawbytestring);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Sync(const FileName: rawbytestring);
    procedure Restore(const FileName: rawbytestring);
    procedure Purge(const FileName: rawbytestring);
    procedure List(const FileName: rawbytestring);
    procedure Fix(const FileName: rawbytestring);
    procedure Check(const FileName: rawbytestring);
    procedure Reset;
  public
    property Exclude: TRawByteStringList Read FExclude;
    property Include: TRawByteStringList Read FInclude;
    property NoDelete: boolean Read FNoDelete Write FNoDelete;
    property UntilVersion: longword Read FUntilVersion Write FUntilVersion;
    property OnShowItem: TGulpShowItem Read FOnShowItem Write FOnShowItem;
    property OnShowMessage: TGulpShowMessage Read FOnShowMessage Write FOnShowMessage;
  end;

// --- Some useful routines ---

function VersionToString(const Version: longword): rawbytestring;
function AttrToString(const Attr: longint): rawbytestring;
function SizeToString(const Size: int64): rawbytestring;
function TimeToString(const T: TDateTime): rawbytestring;
function ModeToString(const Mode: longint): rawbytestring;

function StringToAttr(const S: rawbytestring): longint;
function StringToMode(const S: rawbytestring): longint;
function FlagsToString(const F: TGulpFlags): rawbytestring;

 // =============================================================================
 // IMPLEMENTATION
 // =============================================================================

implementation

uses
  DateUtils,
  GulpScanner,
  GulpStream,
  Math;

const
  GulpMarker003: TSHA1Digest = (255, 210, 119, 9, 180, 210, 231, 123,
    25, 91, 110, 159, 243, 53, 246, 80, 215, 248, 114, 172);

  GulpDescription =
    'GULP v0.0.3 journaling archiver, copyright (c) 2014-2016 Melchiorre Caruso.' +
    LineEnding + 'GULP archiver for user-level incremental backups with rollback capability.'
    + LineEnding;

type
  TGulpList = specialize TGenericList<PGulpItem>;

  // --- The Gulp Reader class ---
  TGulpReader = class(TObject)
  private
    FList: TGulpList;
    FStream: TStream;
    procedure Read(P: PGulpItem);
    function GetItem(Index: longint): PGulpItem;
    function GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure Load(UntilVersion: longword);
    procedure Extract(Index: longint; Stream: TStream); overload;
    procedure Extract(Index: longint); overload;
    function Find(const FileName: rawbytestring): longint;
    procedure Clear;
  public
    property Items[Index: longint]: PGulpItem Read GetItem; default;
    property Count: longint Read GetCount;
  end;

  // --- The Gulp Writer class ---
  TGulpWriter = class(TObject)
  private
    FList: TGulpList;
    FStream: TStream;
    procedure Write(P: PGulpItem); overload;
    procedure Write(Stream: TStream; Size: int64); overload;
    function GetItem(Index: longint): PGulpItem;
    function GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure Delete(const FileName: rawbytestring);
    procedure Add(const FileName: rawbytestring);
    procedure Clear;
  public
    property Items[Index: longint]: PGulpItem Read GetItem; default;
    property Count: longint Read GetCount;
  end;

 // =============================================================================
 // Internal rutines
 // =============================================================================

procedure ItemClear(P: PGulpItem);
begin
  with P^ do
  begin
    Flags := [];
    FullName := '';
    TimeUTC := 0.0;
    Attributes := 0;
    Mode  := 0;
    Size  := 0;
    LinkName := '';
    UserID := 0;
    GroupID := 0;
    UserName := '';
    GroupName := '';
    Comment := '';
    Beginning := 0;
    Ending := 0;
    Version := 0;
  end;
end;

function ItemGetDigest(P: PGulpItem): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  with P^ do
  begin

    SHA1Update(Context, Flags, SizeOf(Flags));

    if gfFullName in Flags then
      SHA1Update(Context, Pointer(FullName)^, Length(FullName));
    if gfTimeUTC in Flags then
      SHA1Update(Context, TimeUTC, SizeOf(TimeUTC));
    if gfAttributes in Flags then
      SHA1Update(Context, Attributes, SizeOf(Attributes));
    if gfMode in Flags then
      SHA1Update(Context, Mode, SizeOf(Mode));
    if gfSize in Flags then
      SHA1Update(Context, Size, SizeOf(Size));


    if gfLinkName in Flags then
      SHA1Update(Context, Pointer(LinkName)^, Length(LinkName));
    if gfUserID in Flags then
      SHA1Update(Context, UserID, SizeOf(UserID));
    if gfGroupID in Flags then
      SHA1Update(Context, GroupID, SizeOf(GroupID));

    if gfUserName in Flags then
      SHA1Update(Context, Pointer(UserName)^, Length(UserName));
    if gfGroupName in Flags then
      SHA1Update(Context, Pointer(GroupName)^, Length(GroupName));
    if gfComment in Flags then
      SHA1Update(Context, Pointer(Comment)^, Length(Comment));
    if gfSize in Flags then
      SHA1Update(Context, Beginning, SizeOf(Beginning));
    if gfSize in Flags then
      SHA1Update(Context, Ending, SizeOf(Ending));

  end;
  SHA1Final(Context, Result);
end;

 // =============================================================================
 // Library routines
 // =============================================================================

function VersionToString(const Version: longword): rawbytestring;
begin
  Result := IntToStr(Version);
end;

function FlagsToString(const F: TGulpFlags): rawbytestring;
begin
  if (gfAdd in F) and (gfDelete in F) then
    Result := 'UPD'
  else if (gfAdd in F) then
    Result := 'ADD'
  else if (gfDelete in F) then
    Result := 'DEL'
  else
    raise Exception.Create('Invalid marker value (ex0001)');
end;

function TimeToString(const T: TDateTime): rawbytestring;
begin
  Result := FormatDateTime(DefaultFormatSettings.LongDateFormat +
    ' ' + DefaultFormatSettings.LongTimeFormat, T);
end;

function SizeToString(const Size: int64): rawbytestring;
begin
  Result := Format('%u', [Size]);
end;

function AttrToString(const Attr: longint): rawbytestring;
begin
  Result := '       ';
  if Attr and faReadOnly <> 0 then
    Result[1] := 'R';
  if Attr and faHidden <> 0 then
    Result[2] := 'H';
  if Attr and faSysFile <> 0 then
    Result[3] := 'S';
  if Attr and faVolumeId <> 0 then
    Result[4] := 'V';
  if Attr and faDirectory <> 0 then
    Result[5] := 'D';
  if Attr and faArchive <> 0 then
    Result[6] := 'A';
  if Attr and faSymLink <> 0 then
    Result[7] := 'L';
end;

function StringToAttr(const S: rawbytestring): longint;
begin
  Result := 0;
  if Length(S) = 7 then
  begin
    if Upcase(S[1]) = 'R' then
      Result := Result or faReadOnly;
    if Upcase(S[2]) = 'H' then
      Result := Result or faHidden;
    if Upcase(S[3]) = 'S' then
      Result := Result or faSysFile;
    if Upcase(S[4]) = 'V' then
      Result := Result or faVolumeId;
    if Upcase(S[5]) = 'D' then
      Result := Result or faDirectory;
    if Upcase(S[6]) = 'A' then
      Result := Result or faArchive;
    if Upcase(S[7]) = 'L' then
      Result := Result or faSymLink;
  end;
end;

function ModeToString(const Mode: longint): rawbytestring;
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

function StringToMode(const S: rawbytestring): longint;
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

function Compare41(P1, P2: PGulpItem): longint; inline;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(P1^.FullName, P2^.FullName);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result := AnsiCompareText(P1^.FullName, P2^.FullName);
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function Compare40(P1, P2: PGulpItem): longint; inline;
begin
  Result := Compare41(P1, P2);

  if Result = 0 then
    if P1^.Version < P2^.Version then
      Result := -1
    else if P1^.Version > P2^.Version then
      Result := 1;

  if Result = 0 then
    if (gfDelete in P1^.Flags) and (gfAdd in P2^.Flags) then
      Result := -1
    else if (gfDelete in P2^.Flags) and (gfAdd in P1^.Flags) then
      Result := 1;
end;

constructor TGulpReader.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FList := TGulpList.Create(@Compare41);
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
    Dispose(FList[0]);
    FList.Delete(0);
  end;
end;

procedure TGulpReader.Read(P: PGulpItem);
var
  Digest: TSHA1Digest;
begin
  ItemClear(P);
  if FStream.Read(Digest, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0002)');
  if SHA1Match(Digest, GulpMarker003) = False then
    raise Exception.Create('Invalid marker value (ex0003)');

  with P^ do
  begin
    FStream.Read(Flags, SizeOf(Flags));

    if gfFullName in Flags then
      FullName := FStream.ReadAnsiString;
    if gfTimeUTC in Flags then
      FStream.Read(TimeUTC, SizeOf(TimeUTC));
    if gfAttributes in Flags then
      FStream.Read(Attributes, SizeOf(Attributes));
    if gfMode in Flags then
      FStream.Read(Mode, SizeOf(Mode));


    if gfSize in Flags then
      FStream.Read(Size, SizeOf(Size));
    if gfLinkName in Flags then
      LinkName := FStream.ReadAnsiString;
    if gfUserID in Flags then
      FStream.Read(UserID, SizeOf(UserID));
    if gfGroupID in Flags then
      FStream.Read(GroupID, SizeOf(GroupID));

    if gfUserName in Flags then
      UserName := FStream.ReadAnsiString;
    if gfGroupName in Flags then
      GroupName := FStream.ReadAnsiString;

    if gfComment in Flags then
      Comment := FStream.ReadAnsiString;
    if gfSize in Flags then
      FStream.Read(Beginning, SizeOf(Beginning));
    if gfSize in Flags then
      FStream.Read(Ending, SizeOf(Ending));

    if ((gfDelete in Flags) = False) and ((gfADD in Flags) = False) then
      raise Exception.Create('Archive is damaged, try with the "fix" command (ex0004)');
    if ((Ending = 0) or (Beginning = 0)) and (Size <> 0) then
      raise Exception.Create('Archive is damaged, try with the "fix" command (ex0005)');
    if FStream.Read(Digest, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
      raise Exception.Create('Archive is damaged, try with the "fix" command (ex0006)');
    if SHA1Match(Digest, ItemGetDigest(P)) = False then
      raise Exception.Create('Archive is damaged, try with the "fix" command (ex0007)');

    DoDirSeparators(LinkName);

    DoDirSeparators(FullName);
  end;
end;

procedure TGulpReader.Load(UntilVersion: longword);
var
  I: longint;
  Item: PGulpItem;
  OffSet: int64 = 0;
  Size: int64 = 0;
  Version: longword = 1;
begin
  Clear;
  FreeAndNil(FList);
  if 0 = UntilVersion then
    FList := TGulpList.Create(@Compare40)
  else
    FList := TGulpList.Create(@Compare41);

  Item := New(PGulpItem);
  try
    while True do
    begin
      Read(Item);
      Item^.Version := Version;

      OffSet := Max(OffSet, Item^.Ending);
      if gfClose in Item^.Flags then
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
        Item := New(PGulpItem);
      end
      else if Version <= UntilVersion then
      begin
        if gfDelete in Item^.Flags then
        begin
          I := FList.Find(Item);
          if I <> -1 then
          begin
            Dispose(FList[I]);
            FList.Delete(I);
          end;
        end;

        if gfAdd in Item^.Flags then
        begin
          if FList.Add(Item) = -1 then
            raise Exception.Create('Duplicates non allowed (ex0010)');
          Item := New(PGulpItem);
        end;
      end;
    end;

  except
  end;
  Dispose(Item);

  if Size <> FStream.Seek(0, soEnd) then
    raise Exception.Create('Archive is broken, try with fix command (ex0011)');
end;

procedure TGulpReader.Extract(Index: longint; Stream: TStream);
var
  Buffer: array[0..$FFFF] of byte;
  Context: TSHA1Context;
  Digest1: TSHA1Digest;
  Digest2: TSHA1Digest;
  Readed: longint;
  Size: int64;
begin
  FStream.Seek(Items[Index]^.Beginning, soBeginning);
  Size := Items[Index]^.Size;
  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := FStream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0012)');
    SHA1Update(Context, Buffer, Readed);
    Stream.Write(Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest1);
  if FStream.Read(Digest2, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0013)');
  if SHA1Match(Digest1, Digest2) = False then
    raise Exception.CreateFmt('Mismatched checksum for "%s"', [Items[Index]^.FullName]);
end;

procedure TGulpReader.Extract(Index: longint);
var
  Check: boolean;
  Item:  PGulpItem;
  ItemPath: rawbytestring;
  Stream: TFileStream;
begin
  Item := Items[Index];
  if Item^.Attributes and (faSysFile or faVolumeId) = 0 then
  begin
    ItemPath := ExtractFileDir(Item^.FullName);
    if (ItemPath <> '') and (ForceDirectories(ItemPath) = False) then
      raise Exception.CreateFmt('Unable to create path "%s"', [ItemPath]);

    Check := False;
    if (Item^.Attributes and faSymLink) = faSymLink then
    {$IFDEF UNIX}
{$ELSE}
{$IFDEF MSWINDOWS}
{$ELSE}
{$ENDIF}
{$ENDIF}
    else
    if (Item^.Attributes and faDirectory) = faDirectory then
    begin
      Check := DirectoryExists(Item^.FullName);
      if Check = False then
        Check := CreateDir(Item^.FullName);
    end
    else
    if (gfSize in Item^.Flags) then
    begin
      if FileExists(Item^.FullName) = False then
        Stream := TFileStream.Create(Item^.FullName, fmCreate)
      else
        Stream := TFileStream.Create(Item^.FullName, fmOpenWrite);
      Extract(Index, Stream);
      FreeAndNil(Stream);
      Check := True;
    end;

    if Check = True then
    begin
      {$IFDEF UNIX}
      if (gfUserID in Item^.Flags) or (gfGroupID in Item^.Flags) then
        if FpChown(Item^.FullName, Item^.UserID, Item^.GroupID) <> 0 then
          raise Exception.CreateFmt('Unable to set user/group id for "%s"', [Item^.FullName]);

      if gfMODE in Item^.Flags then
        if FpChmod(Item^.FullName, Item^.Mode) <> 0 then
          raise Exception.CreateFmt('Unable to set mode for "%s"', [Item^.FullName]);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      if FileSetAttr(Item^.FullName, Item^.Attributes) <> 0 then
        raise Exception.CreateFmt('Unable to set attributes for "%s"',
          [Item^.FullName]);
      {$ELSE}
        Unsupported platform...
      {$ENDIF}
      {$ENDIF}
      if FileSetDate(Item^.FullName, DateTimeToFileDate(
        GulpFixes.UniversalTime2Local(Item^.TimeUTC))) <> 0 then
        raise Exception.CreateFmt('Unable to set date for "%s"', [Item^.FullName]);
    end
    else
      raise Exception.CreateFmt('Unable to restore item "%s"', [Item^.FullName]);

  end;
end;

function TGulpReader.Find(const FileName: rawbytestring): longint;
var
  Item: PGulpItem;
begin
  Item := New(PGulpItem);
  ItemClear(Item);
  Item^.Flags := [gfAdd, gfFullName];
  Item^.FullName := FileName;
  begin
    Result := FList.Find(Item);
  end;
  Dispose(Item);
end;

function TGulpReader.GetItem(Index: longint): PGulpItem;
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

function Compare42(Item1, Item2: PGulpItem): longint;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(Item1^.FullName, Item2^.FullName);
  {$ELSE}
    {$IFDEF MSWINDOWS}
  Result := AnsiCompareText(Item1^.FullName, Item2^.FullName);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  if Result = 0 then
    if (gfDelete in Item1^.Flags) and (gfAdd in Item2^.Flags) then
      Result := -1
    else if (gfDelete in Item2^.Flags) and (gfAdd in Item1^.Flags) then
      Result := 1;
end;

constructor TGulpWriter.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FList := TGulpList.Create(@Compare42);
end;

destructor TGulpWriter.Destroy;
var
  I: longint;
  Size: int64;
  Source: TStream;
begin
  if FList.Count > 0 then
  begin
    Size := FStream.Seek(0, soEnd);
    Include(FList[FList.Count - 1]^.Flags, gfClose);
    for I := 0 to FList.Count - 1 do
      Write(FList[I]);
    for I := 0 to FList.Count - 1 do
      if FList[I]^.Attributes and (faSysFile or faVolumeId) = 0 then
        if FList[I]^.Attributes and (faSymLink or faDirectory) = 0 then
          if gfSIZE in FList[I]^.Flags then
          begin
            FList[I]^.Beginning := FStream.Seek(0, soCurrent);
            Source :=
              TFileStream.Create(FList[I]^.FullName, fmOpenRead or fmShareDenyNone);
            Write(Source, FList[I]^.Size);
            FreeAndNil(Source);
            FList[I]^.Ending := FStream.Seek(0, soCurrent);
          end;
    FStream.Seek(Size, soBeginning);
    for I := 0 to FList.Count - 1 do
      Write(FList[I]);
    FStream.Seek(0, soEnd);
    Clear;
  end;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpWriter.Clear;
begin
  while FList.Count > 0 do
  begin
    Dispose(FList[0]);
    FList.Delete(0);
  end;
end;

procedure TGulpWriter.Add(const FileName: rawbytestring);
var
  Item: PGulpItem;
  SR: TSearchRec;
begin
  if SysUtils.FindFirst(FileName, faReadOnly or faHidden or faSysFile or
    faVolumeId or faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
    if GulpCommon.FileGetAttr(SR) and (faSysFile or faVolumeId) = 0 then
    begin
      Item := New(PGulpItem);
      ItemClear(Item);


      Item^.FullName := FileName;
      Item^.TimeUTC := FileGetTimeUTC(SR);
      Item^.Size := FileGetSize(SR);
      Item^.Attributes := GulpCommon.FileGetAttr(SR);
      Include(Item^.Flags, gfFullName);
      Include(Item^.Flags, gfTimeUTC);
      if Item^.Attributes and faSymLink = 0 then
        if Item^.Attributes and faDirectory = 0 then
          Include(Item^.Flags, gfSize);
      Include(Item^.Flags, gfAttributes);
      {$IFDEF UNIX}
      Item^.Mode    := FileGetMode    (FileName);
      Item^.LinkName    := FileGetLinkName(FileName);
      Item^.UserID  := FileGetUserID  (FileName);
      Item^.GroupID := FileGetGroupID (FileName);
      Include(Item^.FLags, gfMode);
      Include(Item^.FLags, gfLinkName);
      Include(Item^.FLags, gfUserID );
      Include(Item^.FLags, gfGroupID );
      {$ELSE}
      {$IFDEF MSWINDOWS}
      {$ELSE}
        Unsupported platform...
      {$ENDIF}
      {$ENDIF}
      Include(Item^.Flags, gfAdd);
      if FList.Add(Item) = -1 then
        raise Exception.Create('Duplicates non allowed (ex0014)');
    end;
  SysUtils.FindClose(SR);
end;

procedure TGulpWriter.Delete(const FileName: rawbytestring);
var
  Item: PGulpItem;
begin
  Item := New(PGulpItem);

  ItemClear(Item);
  Item^.Flags := [gfDelete, gfFullName];
  Item^.FullName := FileName;
  if FList.Add(Item) = -1 then
    raise Exception.Create('Duplicates non allowed (ex0015)');
end;

procedure TGulpWriter.Write(Stream: TStream; Size: int64);
var
  Buffer:  array[0..$FFFF] of byte;
  Context: TSHA1Context;
  Digest:  TSHA1Digest;
  Readed:  longint;
begin
  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := Stream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0016)');
    SHA1Update(Context, Buffer, Readed);
    FStream.Write(Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest);
  FStream.Write(Digest, SizeOf(TSHA1Digest));
end;

procedure TGulpWriter.Write(P: PGulpItem);
begin
  FStream.Write(GulpMarker003, SizeOf(GulpMarker003));
  with P^ do
  begin

    FStream.Write(Flags, SizeOf(Flags));
    if gfFullName in Flags then
      FStream.WriteAnsiString(FullName);
    if gfTimeUTC in Flags then
      FStream.Write(TimeUTC, SizeOf(TimeUTC));
    if gfAttributes in Flags then
      FStream.Write(Attributes, SizeOf(Attributes));
    if gfMode in Flags then
      FStream.Write(Mode, SizeOf(Mode));

    if gfSize in Flags then
      FStream.Write(Size, SizeOf(Size));
    if gfLinkName in Flags then
      FStream.WriteAnsiString(LinkName);
    if gfUserID in Flags then
      FStream.Write(UserID, SizeOf(UserID));
    if gfGroupID in Flags then
      FStream.Write(GroupID, SizeOf(GroupID));

    if gfUserName in Flags then
      FStream.WriteAnsiString(UserName);
    if gfGroupName in Flags then
      FStream.WriteAnsiString(GroupName);
    if gfComment in Flags then
      FStream.WriteAnsiString(Comment);
    if gfSIZE in Flags then
      FStream.Write(Beginning, SizeOf(Beginning));
    if gfSIZE in Flags then
      FStream.Write(Ending, SizeOf(Ending));

  end;
  FStream.Write(ItemGetDigest(P), SizeOf(TSHA1Digest));
end;

function TGulpWriter.GetItem(Index: longint): PGulpItem;
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
  FOnShowItem := nil;
  FOnShowMessage := nil;
  FExclude  := TRawByteStringList.Create;
  FInclude  := TRawByteStringList.Create;
  FNodelete := False;
  FUntilVersion := $FFFFFFFF;
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
  FNodelete := False;
  FUntilVersion := $FFFFFFFF;
end;

procedure TGulpApplication.ShowItem(const Item: PGulpItem);
begin
  if Assigned(FOnShowItem) then
    FOnShowItem(Item);
end;

procedure TGulpApplication.ShowMessage(const Message: rawbytestring);
begin
  if Assigned(FOnShowMessage) then
    FOnShowMessage(Message);
end;

procedure TGulpApplication.Sync(const FileName: rawbytestring);
var
  LibReader: TGulpReader;
  LibWriter: TGulpWriter;
  I, J: longint;
  Scan: TScanner;
  Size: int64;
  Stream: TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Sync the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...      ', [#13]));
  if FileExists(FileName) then
    Stream := TFileStream.Create(FileName, fmOpenReadWrite)
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  Size := Stream.Seek(0, soEnd);

  ShowMessage(Format('%sScanning filesystem...   ', [#13]));
  Scan := TScanner.Create;
  for I := FInclude.Count - 1 downto 0 do
    if DirectoryExists(FInclude[I]) = True then
      FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*');

  if FInclude.Count = 0 then
    FInclude.Add('*');
  for I := FInclude.Count - 1 downto 0 do
    Scan.Add(FInclude[I]);

  for I := FExclude.Count - 1 downto 0 do
    if DirectoryExists(FExclude[I]) = True then
      FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*');

  FExclude.Add(FileName);
  for I := Scan.Count - 1 downto 0 do
    if FileNameMatch(Scan[I], FExclude) = True then
      Scan.Delete(I);

  ShowMessage(Format('%sSyncing items...         ', [#13]));
  LibWriter := TGulpWriter.Create(Stream);
  if FNoDelete = False then
    for I := 0 to LibReader.Count - 1 do
      if Scan.Find(LibReader[I]^.FullName) = -1 then
        LibWriter.Delete(LibReader[I]^.FullName);

  for I := 0 to Scan.Count - 1 do
  begin
    J := LibReader.Find(Scan[I]);
    if J = -1 then
      LibWriter.Add(Scan[I])
    else if FileGetTimeUTC(Scan[I]) <> LibReader[J]^.TimeUTC then
    begin
      LibWriter.Delete(Scan[I]);
      LibWriter.Add(Scan[I]);
    end;
  end;
  FreeAndNil(LibWriter);
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Scan);

  ShowMessage(Format('%sFinished (%u added bytes) %s',
    [#13, FileGetSize(FileName) - Size, LineEnding]));
end;

procedure TGulpApplication.Restore(const FileName: rawbytestring);
var
  LibReader: TGulpReader;
  LibRec: PGulpItem;
  I, J: longint;
  Scan: TScanner;
  Size: int64 = 0;
  Stream: TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Restore the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...         ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  ShowMessage(Format('%sScanning filesystem...      ', [#13]));
  Scan := TScanner.Create;
  Scan.Add('*');
  for I := FInclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FInclude[I]);
    if J <> -1 then
      if LibReader[J]^.Attributes and faDirectory = faDirectory then
        FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*');
  end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FExclude[I]);
    if J <> -1 then
      if LibReader[J]^.Attributes and faDirectory = faDirectory then
        FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*');
  end;
  FExclude.Add(FileName);

  ShowMessage(Format('%sRestoring items...          ', [#13]));
  if FNoDelete = False then
    for I := Scan.Count - 1 downto 0 do
    begin
      J := LibReader.Find(Scan[I]);
      if (J = -1) or (FileNameMatch(LibReader[J]^.FullName, FInclude) = False) or
        (FileNameMatch(LibReader[J]^.FullName, FExclude) = True) then
        if DirectoryExists(Scan[I]) = True then
          RemoveDir(Scan[I])
        else
          DeleteFile(Scan[I]);
    end;

  for I := LibReader.Count - 1 downto 0 do
  begin
    LibRec := LibReader[I];
    if FileNameMatch(LibRec^.FullName, FInclude) = True then
      if FileNameMatch(LibRec^.FullName, FExclude) = False then
      begin
        J := Scan.Find(LibRec^.FullName);
        if J = -1 then
        begin
          LibReader.Extract(I);
          Inc(Size, LibRec^.Size);
        end
        else if FileGetTimeUTC(Scan[J]) <> LibRec^.TimeUTC then
        begin
          LibReader.Extract(I);
          Inc(Size, LibRec^.Size);
        end;
      end;
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Scan);

  ShowMessage(Format('%sFinished (%u extracted bytes) %s', [#13, Size, LineEnding]));
end;

procedure TGulpApplication.Check(const FileName: rawbytestring);
var
  LibReader: TGulpReader;
  I: longint;
  Nul: TStream;
  Stream: TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Check the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...       ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(0);

  ShowMessage(Format('%sChecking items...         ', [#13]));
  Nul := TNulStream.Create;
  for I := 0 to LibReader.Count - 1 do
    if LibReader[I]^.Size > 0 then
      LibReader.Extract(I, Nul);
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Nul);

  ShowMessage(Format('%sFinished (%u checked bytes) %s',
    [#13, FileGetSize(FileName), LineEnding]));
end;

procedure TGulpApplication.Fix(const FileName: rawbytestring);
var
  LibReader: TGulpReader;
  LibRec: PGulpItem;
  OffSet: int64 = 0;
  Size: int64 = 0;
  Stream: TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Fix the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sFixing items...         ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  LibReader := TGulpReader.Create(Stream);
  LibRec := New(PGulpItem);

  try
    while True do
    begin
      LibReader.Read(LibRec);
      OffSet := Max(OffSet, LibRec^.Ending);
      if gfClose in LibRec^.Flags then
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
  Dispose(LibRec);
  FreeAndNil(Stream);

  ShowMessage(Format('%sFinished (%u removed bytes) %s', [#13, OffSet, LineEnding]));
end;

procedure TGulpApplication.Purge(const FileName: rawbytestring);
var
  LibItem: PGulpItem;
  LibReader: TGulpReader;
  LibWriter: TGulpWriter;
  I: longint;
  Size: int64 = 0;
  Stream: TStream;
  Tmp: TStream;
  TmpName: rawbytestring;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Purge the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...       ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  ShowMessage(Format('%sMoving items...           ', [#13]));
  TmpName := GetTempFileName(ExtractFileDir(FileName), '');
  Tmp := TFileStream.Create(TmpName, fmCreate);
  LibWriter := TGulpWriter.Create(Tmp);

  if LibReader.Count > 0 then
  begin
    for I := 0 to LibReader.Count - 1 do
      LibWriter.Write(LibReader[I]);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibItem := LibReader[I];
      if LibItem^.Size > 0 then
      begin
        Stream.Seek(LibItem^.Beginning, soBeginning);
        Size := LibItem^.Ending - LibItem^.Beginning;
        LibItem^.Beginning := Tmp.Seek(0, soCurrent);
        Tmp.CopyFrom(Stream, Size);
        LibItem^.Ending := Tmp.Seek(0, soCurrent);
      end;
    end;

    Tmp.Seek(0, soBeginning);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibItem := LibReader[I];
      if I = LibReader.Count - 1 then
        System.Include(LibItem^.Flags, gfClose)
      else
        System.Exclude(LibItem^.Flags, gfClose);
      LibWriter.Write(LibItem);
    end;
    Tmp.Seek(0, soEnd);
  end;

  Size := Stream.Size - Tmp.Size;
  FreeAndNil(LibReader);
  FreeAndNil(LibWriter);
  FreeAndNil(Stream);
  FreeAndNil(Tmp);

  if DeleteFile(FileName) = False then
    raise Exception.CreateFmt('Unable to delete file "%s"', [FileName])
  else if RenameFile(TmpName, FileName) = False then
    raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);

  ShowMessage(Format('%sFinished (%u removed bytes) %s', [#13, Size, LineEnding]));
end;

procedure TGulpApplication.List(const FileName: rawbytestring);
var
  Count: longint = 0;
  LibReader: TGulpReader;
  LibRec: PGulpItem;
  I, J: longint;
  Stream: TStream;
  Version: longword = 0;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('List the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...      ', [#13]));
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  for I := FInclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FInclude[I]);
    if J <> -1 then
      if LibReader[J]^.Attributes and faDirectory = faDirectory then
        FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*');
  end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FExclude[I]);
    if J <> -1 then
      if LibReader[J]^.Attributes and faDirectory = faDirectory then
        FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*');
  end;

  ShowMessage(Format('%sListing items...       %s', [#13, LineEnding]));
  for I := 0 to LibReader.Count - 1 do
  begin
    LibRec  := LibReader[I];
    Version := Max(Version, LibRec^.Version);
    if FileNameMatch(LibRec^.FullName, FInclude) = True then
      if FileNameMatch(LibRec^.FullName, FExclude) = False then
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
