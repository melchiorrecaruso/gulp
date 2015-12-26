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

    v0.0.3 - 2015.12.26 by Melchiorre Caruso.
}

unit GulpLibrary;

{$mode objfpc}

interface

uses
  {$IFDEF UNIX} BaseUnix, {$ENDIF} Classes, GulpCommon,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} Sha1, SysUtils;

type 
  // --- Gulp flags
  TGulpFlag = (gfADD,  gfDEL, gfLAST,  gfNAME, gfTIME,  gfSIZE, gfATTR, gfMODE,
               gfLINK, gfUID, gfUNAME, gfGID,  gfGNAME, gfPLAT, gfCOMM, gfPOS);

  TGulpFlags = set of TGulpFlag;

  // --- Gulp platform ---
  TGulpPlatform = (gpNONE, gpUNIX, gpMSWINDOWS, gpMAC);

  // --- Gulp item ---
  TGulpItem = class(TObject)
  private
    FFlags     : TGulpFlags;    // Flags
    FName      : ansistring;    // Path and name
    FTime      : TDateTime;     // Last modification date and time (UTC)
    FSize      : int64;         // Size in bytes
    FAttr      : longint;       // Attributes (MSWindows)
    FMode      : longint;       // Mode (Unix)
    FLinkName  : ansistring;    // Name of link
    FUserID    : longword;      // User ID
    FUserName  : ansistring;    // User Name
    FGroupID   : longword;      // Group ID
    FGroupName : ansistring;    // Group Name
    FComment   : ansistring;    // Comment
    FPlatform  : TGulpPlatform; // Platform
    FStartPos  : int64;         // Stream start position (reserved)
    FEndPos    : int64;         // Stream end position   (reserved)
    FVersion   : longword;      // Version               (reserved)
  public
    property Flags     : TGulpFlags    read FFlags;
    property Name      : ansistring    read FName;
    property Time      : TDateTime     read FTime;
    property Size      : int64         read FSize;
    property Attr      : longint       read FAttr;
    property Mode      : longint       read FMode;
    property LinkName  : ansistring    read FLinkName;
    property UserID    : longword      read FUserID;
    property UserName  : ansistring    read FUserName;
    property GroupID   : longword      read FGroupID;
    property GroupName : ansistring    read FGroupName;
    property Comment   : ansistring    read FComment;
    property Platform  : TGulpPlatform read FPlatform;
    property Version   : longword      read FVersion;
  end;

  // --- The Gulp Application events ---
  TGulpShowItem    = procedure(const Item: TGulpItem) of object;
  TGulpShowMessage = procedure(const Message: ansistring) of object;
  TGulpTerminate   = function : boolean of object;

  // --- The Gulp Application class ---
  TGulpApplication = class(TObject)
  private
    FExclude       : TStrList;
    FInclude       : TStrList;
    FNodelete      : boolean;
    FUntilVersion  : longword;
    FOnShowItem    : TGulpShowItem;
    FOnShowMessage : TGulpShowMessage;
    procedure ShowItem(Item: TGulpItem);
    procedure ShowMessage(const Message: ansistring);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Sync    (const FileName: ansistring);
    procedure   Restore (const FileName: ansistring);
    procedure   Purge   (const FileName: ansistring);
    procedure   List    (const FileName: ansistring);
    procedure   Fix     (const FileName: ansistring);
    procedure   Check   (const FileName: ansistring);
    procedure   Reset;
  public
    property Exclude       : TStrList          read FExclude;
    property Include       : TStrList          read FInclude;
    property NoDelete      : boolean           read FNoDelete      write FNoDelete;
    property UntilVersion  : longword          read FUntilVersion  write FUntilVersion;
    property OnShowItem    : TGulpShowItem     read FOnShowItem    write FOnShowItem;
    property OnShowMessage : TGulpShowMessage  read FOnShowMessage write FOnShowMessage;
  end;

// --- Some useful routines ---

function GetTime(const Time: TDateTime     ): TDateTime; overload;
function GetTime(const FileName: ansistring): TDateTime; overload;
function GetTime(var   SR: TSearchRec      ): TDateTime; overload;
function GetSize(const FileName: ansistring): int64;     overload;
function GetSize(var   SR: TSearchRec      ): int64;     overload;
function GetAttr(const FileName: ansistring): longint;   overload;
function GetAttr(var   SR: TSearchRec      ): longint;   overload;

{$IFDEF UNIX}
function GetMode(const FileName: ansistring): longint;   overload;
function GetMode(var   Info: stat          ): longint;   overload;
function GetUID (const FileName: ansistring): longword;  overload;
function GetUID (var   Info: stat          ): longword;  overload;
function GetGID (const FileName: ansistring): longword;  overload;
function GetGID (var   Info: stat          ): longword;  overload;
{$ENDIF}

function  VerToString(const Version: longword): ansistring;
function AttrToString(const Attr: longint    ): ansistring;
function SizeToString(const Size: int64      ): ansistring;
function TimeToString(const T: TDateTime     ): ansistring;
function ModeToString(const Mode: longint    ): ansistring;

function StringToAttr(const S: ansistring    ): longint;
function StringToMode(const S: ansistring    ): longint;
function PlatToString(const P: TGulpPlatform ): ansistring;
function FlagToString(const F: TGulpFlags    ): ansistring;

// =============================================================================
// IMPLEMENTATION
// =============================================================================

implementation

uses
  DateUtils, Math;

const
  GulpMarker : TSHA1Digest = (255, 210, 119, 9, 180, 210, 231,
    123, 25, 91, 110, 159, 243,  53, 246, 80, 215, 248, 114, 172);

  GulpDescription =
      'GULP v0.0.3 journaling archiver, copyright (c) 2014-2015 Melchiorre Caruso.' + LineEnding +
      'GULP archiver for user-level incremental backups with rollback capability.'  + LineEnding ;

type
  TGulpList = specialize TGenericList<TGulpItem>;

  // --- The Gulp Reader class ---
  TGulpReader = class(TObject)
  private
    FList       : TGulpList;
    FStream     : TStream;
    procedure   Read(Item: TGulpItem);
    function    GetItem(Index: longint): TGulpItem;
    function    GetCount: longint;
  public
    constructor Create(Stream: TStream);
    destructor  Destroy; override;
    procedure   Load(UntilVersion: longword);
    procedure   Extract(Index: longint; Stream: TStream); overload;
    procedure   Extract(Index: longint); overload;
    function    Find(const FileName: ansistring): longint;
    procedure   Clear;
  public
    property Items[Index: longint]: TGulpItem read GetItem; default;
    property Count: longint read GetCount;
  end;

  // --- The Gulp Writer class ---
  TGulpWriter = class(TObject)
  private
    FList       : TGulpList;
    FStream     : TStream;
    procedure   Write(Item: TGulpItem); overload;
    procedure   Write(Stream: TStream; Size: int64); overload;
    function    GetItem(Index: longint): TGulpItem;
    function    GetCount: longint;
  public
    constructor Create   (Stream: TStream);
    destructor  Destroy; override;
    procedure   Delete   (const FileName: ansistring);
    procedure   Add      (const FileName: ansistring);
    procedure   Clear;
  public
    property Items[Index: longint]: TGulpItem read GetItem; default;
    property Count: longint read GetCount;
  end;

// =============================================================================
// Internal rutines
// =============================================================================

function Clear(Item: TGulpItem): TGulpItem; inline;
begin
  Item.FFlags     := [];
  Item.FName      := '';
  Item.FTime      := 0.0;
  Item.FSize      := 0;
  Item.FAttr      := 0;
  Item.FMode      := 0;
  Item.FLinkName  := '';
  Item.FUserID    := 0;
  Item.FUserName  := '';
  Item.FGroupID   := 0;
  Item.FGroupName := '';
  Item.FComment   := '';
  Item.FPlatform  := gpNONE;
  Item.FStartPos  := 0;
  Item.FEndPos    := 0;
  Item.FVersion   := 0;
  Result := Item;
end;

function GetDigest(Item: TGulpItem): TSHA1Digest; inline;
var
  Context : TSHA1Context;
begin
  SHA1Init(Context);
                                 SHA1Update(Context,         Item.FFlags,       SizeOf(Item.FFlags    ));
  if gfNAME  in Item.FFlags then SHA1Update(Context, Pointer(Item.FName)^,      Length(Item.FName     ));
  if gfTIME  in Item.FFlags then SHA1Update(Context,         Item.FTime,        SizeOf(Item.FTime     ));
  if gfSIZE  in Item.FFlags then SHA1Update(Context,         Item.FSize,        SizeOf(Item.FSize     ));
  if gfATTR  in Item.FFlags then SHA1Update(Context,         Item.FAttr,        SizeOf(Item.FAttr     ));
  if gfMODE  in Item.FFlags then SHA1Update(Context,         Item.FMode,        SizeOf(Item.FMode     ));
  if gfLINK  in Item.FFlags then SHA1Update(Context, Pointer(Item.FLinkName)^,  Length(Item.FLinkName ));
  if gfUID   in Item.FFlags then SHA1Update(Context,         Item.FUserID,      SizeOf(Item.FUserID   ));
  if gfUNAME in Item.FFlags then SHA1Update(Context, Pointer(Item.FUserName)^,  Length(Item.FUserName ));
  if gfGID   in Item.FFlags then SHA1Update(Context,         Item.FGroupID,     SizeOf(Item.FGroupID  ));
  if gfGNAME in Item.FFlags then SHA1Update(Context, Pointer(Item.FGroupName)^, Length(Item.FGroupName));
  if gfCOMM  in Item.FFlags then SHA1Update(Context, Pointer(Item.FComment)^,   Length(Item.FComment  ));
  if gfPLAT  in Item.FFlags then SHA1Update(Context,         Item.FPlatform,    SizeOf(Item.FPlatform ));
  if gfPOS   in Item.FFlags then SHA1Update(Context,         Item.FStartPos,    SizeOf(Item.FStartPos ));
  if gfPOS   in Item.FFlags then SHA1Update(Context,         Item.FEndPos,      SizeOf(Item.FEndPos   ));
  SHA1Final(Context, Result);
end;

// =============================================================================
// Library routines
// =============================================================================

function GetTime(const Time: TDateTime): TDateTime;
var
  OffSet : longint;
begin
  Result := Time;
  OffSet := GetLocalTimeOffSet;
  if (OffSet > 0) then
    Result := Result + EncodeTime(Offset div 60, Offset mod 60, 0, 0)
  else
    if (Offset < 0) then
      Result := Result - EncodeTime(Abs(Offset) div 60, Abs(Offset) mod 60, 0, 0);
end;

function GetTime(var SR: TSearchRec): TDateTime;
begin
  Result := GetTime(FileDateToDateTime(SR.Time));
end;

function GetTime(const FileName: ansistring): TDateTime;
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

function GetSize(const FileName: ansistring): int64;
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

function GetAttr(const FileName: ansistring): longint;
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
function GetMode(const FileName: ansistring): longint;
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

function GetUID(const FileName: ansistring): longword;
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

function GetGID(const FileName: ansistring): longword;
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

function VerToString(const Version: longword): ansistring;
begin
  Result := IntTostr(Version);
end;

function FlagToString(const F: TGulpFlags): ansistring;
begin
  if (gfAdd in F) and (gfDEL in F) then
    Result := 'UPD'
  else
    if (gfAdd in F) then
      Result := 'ADD'
    else
      if (gfDEL in F) then
        Result := 'DEL'
      else
        raise Exception.Create('Invalid marker value (ex0001)');
end;

function TimeToString(const T: TDateTime): ansistring;
begin
  Result := FormatDateTime(
    DefaultFormatSettings.LongDateFormat + ' ' +
    DefaultFormatSettings.LongTimeFormat, T);
end;

function SizeToString(const Size: int64): ansistring;
begin
  Result := Format('%u', [Size]);
end;

function AttrToString(const Attr: longint): ansistring;
begin
  Result := '       ';
  if Attr and faReadOnly  <> 0 then Result[1] := 'R';
  if Attr and faHidden    <> 0 then Result[2] := 'H';
  if Attr and faSysFile   <> 0 then Result[3] := 'S';
  if Attr and faVolumeId  <> 0 then Result[4] := 'V';
  if Attr and faDirectory <> 0 then Result[5] := 'D';
  if Attr and faArchive   <> 0 then Result[6] := 'A';
  if Attr and faSymLink   <> 0 then Result[7] := 'L';
end;

function StringToAttr(const S: ansistring): longint;
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

function ModeToString(const Mode: longint): ansistring;
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

function StringToMode(const S: ansistring): longint;
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

function PlatToString(const P: TGulpPlatform): ansistring;
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

function Compare41(Item1, Item2: TGulpItem): longint; inline;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(Item1.FName, Item2.FName);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := AnsiCompareText(Item1.FName, Item2.FName);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
end;

function Compare40(Item1, Item2: TGulpItem): longint; inline;
begin
  Result := Compare41(Item1, Item2);

  if Result = 0 then
  begin
    if Item1.Version < Item2.Version then
      Result := - 1
    else
      if Item1.Version > Item2.Version then
        Result := 1;
  end;

  if Result = 0 then
  begin
    if (gfDEL in Item1.FFlags) and
       (gfADD in Item2.FFlags) then
      Result := - 1
    else
      if (gfDEL in Item2.FFlags) and
         (gfADD in Item1.FFlags) then
        Result := 1;
  end;
end;

constructor TGulpReader.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FList   := TGulpList.Create(@Compare41);
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
    FList.Items [0].Destroy;
    FList.Delete(0);
  end;
end;

procedure TGulpReader.Read(Item: TGulpItem);
var
  Digest : TSHA1Digest;
begin
  Item   := GulpLibrary.Clear(Item);
  if FStream.Read(Digest, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0002)');
  if SHA1Match(Digest, GulpMarker) = FALSE then
    raise Exception.Create('Invalid marker value (ex0003)');

  FStream.Read(Item.FFlags, SizeOf(Item.FFlags));
  if gfNAME  in Item.Flags then Item.FName      := FStream.ReadAnsiString;
  if gfTIME  in Item.Flags then FStream.Read(Item.FTime,     SizeOf(Item.FTime    ));
  if gfSIZE  in Item.Flags then FStream.Read(Item.FSize,     SizeOf(Item.FSize    ));
  if gfATTR  in Item.Flags then FStream.Read(Item.FAttr,     SizeOf(Item.FAttr    ));
  if gfMODE  in Item.Flags then FStream.Read(Item.FMode,     SizeOf(Item.FMode    ));
  if gfLINK  in Item.Flags then Item.FLinkName  := FStream.ReadAnsiString;
  if gfUID   in Item.Flags then FStream.Read(Item.FUserID,   SizeOf(Item.FUserID  ));
  if gfUNAME in Item.Flags then Item.FUserName  := FStream.ReadAnsiString;
  if gfGID   in Item.Flags then FStream.Read(Item.FGroupID,  SizeOf(Item.FGroupID ));
  if gfGNAME in Item.Flags then Item.FGroupName := FStream.ReadAnsiString;
  if gfCOMM  in Item.Flags then Item.FComment   := FStream.ReadAnsiString;
  if gfPLAT  in Item.Flags then FStream.Read(Item.FPlatform, SizeOf(Item.FPlatform));
  if gfPOS   in Item.Flags then FStream.Read(Item.FStartPos, SizeOf(Item.FStartPos));
  if gfPOS   in Item.Flags then FStream.Read(Item.FEndPos,   SizeOf(Item.FEndPos  ));

  if ((Item.FEndPos = 0) or (Item.FStartPos = 0)) and (Item.FSize <> 0) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0004)');
  if FStream.Read(Digest, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0005)');
  if SHA1Match(Digest, GetDigest(Item)) = FALSE then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0006)');

  DoDirSeparators(Item.FLinkName);
  DoDirSeparators(Item.FName);
end;

procedure TGulpReader.Load(UntilVersion: longword);
var
  I       : longint;
  Item    : TGulpItem;
  OffSet  : int64 = 0;
  Size    : int64 = 0;
  Version : longword = 1;
begin
  Clear;
  if 0 = UntilVersion then
    FList.Compare := @Compare40
  else
    FList.Compare := @Compare41;

  Item := TGulpItem.Create;
  Size := FStream.Seek(0, soEnd);
          FStream.Seek(0, soBeginning);
  while   FStream.Seek(0, soCurrent) < Size do
  begin
    Read(Item);
    Item.FVersion := Version;
    OffSet := Max(OffSet, Item.FEndPos);
    if gfLAST in Item.FFlags then
    begin
      OffSet := Max(OffSet, FStream.Seek(0, soCurrent));
      FStream.Seek(OffSet, soBeginning);
      Inc(Version);
    end;

    if 0 = UntilVersion then
    begin
      if FList.Add(Item) = - 1 then
        raise Exception.Create('Duplicates non allowed (ex0007)');
      Item := TGulpItem.Create;
    end else
      if Version <= UntilVersion then
      begin
        if (gfDEL in Item.FFlags) = FALSE then
          if (gfADD in Item.FFlags) = FALSE then
            raise Exception.Create('Invalid signature value (ex0008)');

        if gfDEL in Item.FFlags then
        begin
          I := FList.Find(Item);
          if I <> -1 then
            FList.Delete(I);
        end;

        if gfADD in Item.FFlags then
        begin
          if FList.Add(Item) = -1 then
            raise Exception.Create('Duplicates non allowed (ex0009)');
          Item := TGulpItem.Create;
        end;
      end;
  end;

  if FList.Count = 0 then
  begin
    if FStream.Seek(0, soEnd) <> 0 then
      raise Exception.Create('Invalid signature value (ex0010)');
  end else
    if (gfLAST in FList[FList.Count -1].FFLags) = FALSE then
      raise Exception.Create('Archive is broken, try with fix command (ex0011)');

    if FStream.Seek(0, soCurrent) <> FStream.Seek(0, soEnd) then
      raise Exception.Create('Archive is broken, try with fix command (ex0012)');
  FreeAndNil(Item);
end;

procedure TGulpReader.Extract(Index: longint; Stream: TStream);
var
  Buffer  : array[0..$FFFF] of byte;
  Context : TSHA1Context;
  Digest1 : TSHA1Digest;
  Digest2 : TSHA1Digest;
  Readed  : longint;
  Size    : int64;
begin
  FStream.Seek(Items[Index].FStartPos, soBeginning);
  Size := Items[Index].Size;
  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := FStream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0013)');
    SHA1Update  (Context, Buffer, Readed);
    Stream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest1);

  if FStream.Read(Digest2, SizeOf(TSHA1Digest)) <> SizeOf(TSHA1Digest) then
    raise Exception.Create('Archive is damaged, try with the "fix" command (ex0014)');

  if SHA1Match(Digest1, Digest2) = FALSE then
    raise Exception.CreateFmt('Mismatched checksum for "%s"', [Items[Index].Name]);
end;

procedure TGulpReader.Extract(Index: longint);
var
  Item   : TGulpItem;
  Stream : TFileStream;
begin
  Item := Items[Index];
  if ForceDirectories(ExtractFileDir(Item.Name)) = FALSE then
    raise Exception.CreateFmt('Unable to create path "%s"',
      [ExtractFileDir(Item.Name)]);
  {$IFDEF UNIX}
    if Item.Attr and faSymLink = faSymLink then
    begin
      //if FpLink(RecLinkName, RecName) <> 0 then
      //  raise Exception.CreateFmt('Unable to create hardlink %s', [RecName]);
      if FpSymLink(pchar(Item.LinkName), pchar(Item.Name)) <> 0 then
        raise Exception.CreateFmt('Unable to create symlink "%s"', [Item.Name]);
    end else
  {$ELSE}
    {$IFDEF MSWINDOWS}
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
      if Item.Attr and faDirectory = faDirectory then
      begin
        if DirectoryExists(Item.Name) = FALSE then
          if CreateDir(Item.Name) = FALSE then
            raise Exception.CreateFmt('Unable to create directory "%s"', [Item.Name]);
      end else
        if Item.Size > 0 then
        begin
          Stream := TFileStream.Create(Item.Name, fmCreate);
          Extract(Index, Stream);
          FreeAndNil(Stream);
        end;
  {$IFDEF UNIX}
    if Item.Platform = gpUNIX then
      if FpChmod(Item.Name, Item.Mode) <> 0 then
        raise Exception.CreateFmt('Unable to set mode for "%s"', [Item.Name]);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      if FileSetAttr(Item.Name, Item.Attr) <> 0 then
        raise Exception.CreateFmt('Unable to set attrbutes for "%s"', [Item.Name]);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  if FileSetDate(Item.Name, DateTimeToFileDate(UniversalTimeToLocal(Item.Time))) <> 0 then
    raise Exception.CreateFmt('Unable to set date for "%s"', [Item.Name]);
end;

function  TGulpReader.Find(const FileName: ansistring): longint;
var
  Item : TGulpItem;
begin
  Item := GulpLibrary.Clear(TGulpItem.Create);
  Item.FFlags := [gfADD, gfNAME];
  Item.FName  := FileName;
  begin
    Result := FList.Find(Item);
  end;
  FreeAndNil(Item);
end;

function TGulpReader.GetItem(Index: longint): TGulpItem;
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

function Compare42(Item1, Item2: TGulpItem): longint; inline;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(Item1.FName, Item2.FName);
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Result := AnsiCompareText(Item1.FName, Item2.FName);
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  if Result = 0 then
  begin
    if (gfDEL in Item1.FFlags) and
       (gfADD in Item2.FFlags) then
      Result := - 1
    else
      if (gfDEL in Item2.FFlags) and
         (gfADD in Item1.FFlags) then
        Result := 1;
  end;
end;

constructor TGulpWriter.Create(Stream: TStream);
begin
  inherited Create;
  FList   := TGulpList.Create(@Compare42);
  FStream := Stream;
end;

destructor TGulpWriter.Destroy;
var
  I      : longint;
  Size   : int64;
  Source : TStream;
begin
  if FList.Count > 0 then
  begin
    Size  := FStream.Seek(0, soEnd);
    Include(FList[FList.Count - 1].FFlags, gfLAST);
    for I := 0 to FList.Count - 1 do Write(FList[I]);
    for I := 0 to FList.Count - 1 do
      if FList[I].FSize > 0 then
      begin
        Source := TFileStream.Create(FList[I].FName, fmOpenRead or fmShareDenyNone);
        FList[I].FStartPos := FStream.Seek(0, soCurrent);
        Write(Source, FList[I].FSize);
        FList[I].FEndPos   := FStream.Seek(0, soCurrent);
        FreeAndNil(Source);
      end;
    FStream.Seek(Size , soBeginning);
    for I := 0 to FList.Count - 1 do Write(FList[I]);
    FStream.Seek(0 , soEnd);
    Clear;
  end;
  FList.Destroy;
  inherited Destroy;
end;

procedure TGulpWriter.Clear;
begin
  while FList.Count > 0 do
  begin
    FList.Items [0].Destroy;
    FList.Delete(0);
  end;
end;

procedure TGulpWriter.Add(const FileName: ansistring);
var
  Item : TGulpItem;
  SR   : TSearchRec;
begin
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
    if GetAttr(SR) and (faSysFile or faVolumeId) = 0 then
    begin
      Item               := GulpLibrary.Clear(TGulpItem.Create);
      Item.FName         := FileName;
      Item.FTime         := GetTime(SR);
      Item.FSize         := GetSize(SR);
      Item.FAttr         := GetAttr(SR);
      {$IFDEF UNIX}
        Item.FMode       := GetMode(FileName);
        if Item.Attr and faSymLink = faSymLink then
          Item.FLinkName := fpReadLink(FileName);
        Item.FUserID     := GetUID (FileName);
        Item.FGroupID    := GetGID (FileName);
        Item.FPlatform   := gpUNIX;
        Item.FFLags      := [gfNAME, gfTIME, gfSIZE, gfATTR, gfPOS, gfPLAT,
                             gfMODE, gfLINK];
      {$ELSE}
        {$IFDEF MSWINDOWS}
          Item.FPlatform := gpMSWINDOWS;
          Item.FFLags    := [gfNAME, gfTIME, gfSIZE, gfATTR, gfPOS, gfPLAT];
        {$ELSE}
          Unsupported platform...
        {$ENDIF}
      {$ENDIF}
      Include(Item.FFlags, gfADD);
      if FList.Add(Item) = - 1 then
        raise Exception.Create('Duplicates non allowed (ex0015)');
    end;
  SysUtils.FindClose(SR);
end;

procedure TGulpWriter.Delete(const FileName: ansistring);
var
  Item : TGulpItem;
begin
  Item        := GulpLibrary.Clear(TGulpItem.Create);
  Item.FFlags := [gfDEL, gfNAME];
  Item.FName  := FileName;
  if FList.Add(Item) = -1 then
    raise Exception.Create('Duplicates non allowed (ex0016)');
end;

procedure TGulpWriter.Write(Stream: TStream; Size: int64);
var
  Buffer  : array[0..$FFFF] of byte;
  Context : TSHA1Context;
  Digest  : TSHA1Digest;
  Readed  : longint;
begin
  SHA1Init(Context);
  while Size > 0 do
  begin
    Readed := Stream.Read(Buffer, Min(SizeOf(Buffer), Size));
    if Readed = 0 then
      raise Exception.Create('Unable to read stream (ex0017)');
    SHA1Update   (Context, Buffer, Readed);
    FStream.Write(         Buffer, Readed);
    Dec(Size, Readed);
  end;
  SHA1Final(Context, Digest);
  FStream.Write(Digest, SizeOf(TSHA1Digest));
end;

procedure TGulpWriter.Write(Item: TGulpItem);
begin
  FStream.Write(GulpMarker,  SizeOf(GulpMarker));
  FStream.Write(Item.FFlags, SizeOf(Item.FFlags));
  if gfNAME  in Item.FFlags then FStream.WriteAnsiString(Item.FName);
  if gfTIME  in Item.FFlags then FStream.Write(Item.FTime, SizeOf(Item.FTime));
  if gfSIZE  in Item.FFlags then FStream.Write(Item.FSize, SizeOf(Item.FSize));
  if gfATTR  in Item.FFlags then FStream.Write(Item.FAttr, SizeOf(Item.FAttr));
  if gfMODE  in Item.FFlags then FStream.Write(Item.FMode, SizeOf(Item.FMode));
  if gfLINK  in Item.FFlags then FStream.WriteAnsiString(Item.FLinkName);
  if gfUID   in Item.FFlags then FStream.Write(Item.FUserID, SizeOf(Item.FUserID));
  if gfUNAME in Item.FFlags then FStream.WriteAnsiString(Item.FUserName);
  if gfGID   in Item.FFlags then FStream.Write(Item.FGroupID, SizeOf(Item.FGroupID));
  if gfGNAME in Item.FFlags then FStream.WriteAnsiString(Item.FGroupName);
  if gfCOMM  in Item.FFlags then FStream.WriteAnsiString(Item.FComment);
  if gfPLAT  in Item.FFlags then FStream.Write(Item.FPlatform, SizeOf(Item.FPlatform));
  if gfPOS   in Item.FFlags then FStream.Write(Item.FStartPos, SizeOf(Item.FStartPos));
  if gfPOS   in Item.FFlags then FStream.Write(Item.FEndPos,   SizeOf(Item.FEndPos));
  FStream.Write(GetDigest(Item), SizeOf(TSHA1Digest));
end;

function TGulpWriter.GetItem(Index: longint): TGulpItem;
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
  FOnShowItem    := nil;
  FOnShowMessage := nil;
  FExclude       := TStrList.Create;
  FInclude       := TStrList.Create;
  FNodelete      := FALSE;
  FUntilVersion  := $FFFFFFFF;
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
  FNodelete     := FALSE;
  FUntilVersion := $FFFFFFFF;
end;

procedure TGulpApplication.ShowItem(Item: TGulpItem);
begin
  if Assigned(FOnShowItem) then
    FOnShowItem(Item);
end;

procedure TGulpApplication.ShowMessage(const Message: ansistring);
begin
  if Assigned(FOnShowMessage) then
    FOnShowMessage(Message);
end;

procedure TGulpApplication.Sync(const FileName: ansistring);
var
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Sync the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...      ', [#13]));
  if FileExists(FileName) then
    Stream := TFileStream.Create(FileName, fmOpenReadWrite)
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  LibReader:= TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  Size := Stream.Seek(0, soEnd);

  ShowMessage(Format('%sScanning filesystem...   ', [#13]));
  Scan  := TSysScanner.Create;
  for I := FInclude.Count - 1 downto 0 do
    if DirectoryExists(FInclude[I]) = TRUE then
      FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*');

  if FInclude.Count = 0 then
    FInclude.Add('*');
  for I := FInclude.Count - 1 downto 0 do
    Scan.Add(FInclude[I]);

  for I := FExclude.Count - 1 downto 0 do
    if DirectoryExists(FExclude[I]) = TRUE then
      FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*');

  FExclude.Add(FileName);
  for I := Scan.Count - 1 downto 0 do
    if FileNameMatch(Scan[I], FExclude) = TRUE then
      Scan.Delete(I);

  ShowMessage(Format('%sSyncing items...         ', [#13]));
  LibWriter := TGulpWriter.Create(Stream);
  if FNoDelete = FALSE then
    for I := 0 to LibReader.Count - 1 do
      if Scan.Find(LibReader[I].Name) = -1 then
        LibWriter.Delete(LibReader[I].Name);

  for I := 0 to Scan.Count - 1 do
  begin
    J := LibReader.Find(Scan[I]);
    if J = -1 then
      LibWriter.Add(Scan[I])
    else
      if GetTime(Scan[I]) <> LibReader[J].Time then
      begin
        LibWriter.Delete(Scan[I]);
        LibWriter.Add   (Scan[I]);
      end;
  end;
  FreeAndNil(LibWriter);
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeandNil(Scan);

  ShowMessage(Format('%sFinished (%u added bytes) %s',
    [#13, GetSize(FileName) - Size, LineEnding]));
end;

procedure TGulpApplication.Restore(const FileName: ansistring);
var
  LibReader : TGulpReader;
     LibRec : TGulpItem;
       I, J : longint;
       Scan : TSysScanner;
       Size : int64 = 0;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Restore the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...         ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  ShowMessage(Format('%sScanning filesystem...      ', [#13]));
  Scan := TSysScanner.Create;
  Scan.Add('*');
  for I := FInclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FInclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
  end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FExclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
  end;
  FExclude.Add(FileName);

  ShowMessage(Format('%sRestoring items...          ', [#13]));
  if FNoDelete = FALSE then
    for I := Scan.Count - 1 downto 0 do
    begin
      J := LibReader.Find(Scan[I]);
      if (J = -1) or
         (FileNameMatch(LibReader[J].Name, FInclude) = FALSE) or
         (FileNameMatch(LibReader[J].Name, FExclude) = TRUE ) then
      begin
        if DirectoryExists(Scan[I]) = TRUE then
          RemoveDir(Scan[I])
        else
          DeleteFile(Scan[I]);
      end;
    end;

  for I := LibReader.Count - 1 downto 0 do
  begin
    LibRec := LibReader[I];
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
      begin
        J := Scan.Find(LibRec.Name);
        if J = -1 then
        begin
          LibReader.Extract(I);
          Inc(Size, LibRec.Size);
        end else
          if GetTime(Scan[J]) <> LibRec.Time then
          begin
            LibReader.Extract(I);
            Inc(Size, LibRec.Size);
          end;
      end;
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Scan);

  ShowMessage(Format('%s Finished (%u extracted bytes) %s',
    [#13, Size, LineEnding]));
end;

procedure TGulpApplication.Check(const FileName: ansistring);
var
  LibReader : TGulpReader;
          I : longint;
        Nul : TStream;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Check the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...       ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(0);

  ShowMessage(Format('%sChecking items...         ', [#13]));
  Nul   := TNulStream.Create;
  for I := 0 to LibReader.Count - 1 do
  begin
    if LibReader[I].Size > 0 then
      LibReader.Extract(I, Nul);
  end;
  FreeAndNil(LibReader);
  FreeAndNil(Stream);
  FreeAndNil(Nul);

  ShowMessage(Format('%sFinished (%u checked bytes) %s',
    [#13, GetSize(FileName), LineEnding]));
end;

procedure TGulpApplication.Fix(const FileName: ansistring);
var
  LibReader : TGulpReader;
     LibRec : TGulpItem;
     OffSet : int64 = 0;
       Size : int64 = 0;
     Stream : TStream;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Fix the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sFixing items...         ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenReadWrite);
  LibReader := TGulpReader.Create(Stream);
  LibRec    := TGulpItem.Create;

  try
    while TRUE do
    begin
      LibReader.Read(LibRec);
      OffSet := Max(OffSet, LibRec.FEndPos);
      if gfLAST in LibRec.Flags then
      begin
        OffSet := Max(OffSet, Stream.Seek(0, soCurrent));
        if Stream.Seek(OffSet, soBeginning) <> OffSet then
          raise Exception.Create('Stream is not a valid archive (ex0018)');
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
    raise Exception.Create('Stream is not a valid archive (ex0019)');
  FreeAndNil(LibReader);
  FreeAndNil(LibRec);
  FreeAndNil(Stream);

  ShowMessage(Format('%sFinished (%u removed bytes) %s',
    [#13, OffSet, LineEnding]));
end;

procedure TGulpApplication.Purge(const FileName: ansistring);
var
    LibItem : TGulpItem;
  LibReader : TGulpReader;
  LibWriter : TGulpWriter;
          I : longint;
       Size : int64 = 0;
     Stream : TStream;
        Tmp : TStream;
    TmpName : ansistring;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('Purge the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...       ', [#13]));
  Stream    := TFileStream.Create(FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load($FFFFFFFF);
  ShowMessage(Format('%sMoving items...           ', [#13]));
  TmpName   := GetTempFileName(ExtractFileDir(FileName), '');
  Tmp       := TFileStream.Create(TmpName, fmCreate);
  LibWriter := TGulpWriter.Create(Tmp);

  if LibReader.Count > 0 then
  begin
    for I := 0 to LibReader.Count - 1 do LibWriter.Write(LibReader[I]);
    for I := 0 to LibReader.Count - 1 do
    begin
      LibItem := LibReader[I];
      if LibItem.FSize > 0 then
      begin
        Stream.Seek(LibItem.FStartPos, soBeginning);
        Size := LibItem.FEndPos - LibItem.FStartPos;
        LibItem.FStartPos := Tmp.Seek(0, soCurrent);
        Tmp.CopyFrom(Stream, Size);
        LibItem.FEndPos   := Tmp.Seek(0, soCurrent);
      end;
    end;

    Tmp.Seek(0, soBeginning);
    for I := 0 to LibReader.Count -1 do
    begin
      LibItem := LibReader[I];
      if I = LibReader.Count - 1 then
        System.Include(LibItem.FFlags, gfLAST)
      else
        System.Exclude(LibItem.FFlags, gfLAST);
      LibWriter.Write(LibItem);
    end;
    Tmp.Seek(0, soEnd);
  end;

  Size := Stream.Size - Tmp.Size;
  FreeAndNil(LibReader);
  FreeAndNil(LibWriter);
  FreeAndNil(Stream);
  FreeAndNil(Tmp);

  if DeleteFile(FileName) = FALSE then
    raise Exception.CreateFmt('Unable to delete file "%s"', [FileName])
  else
    if RenameFile(TmpName, FileName)= FALSE then
      raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);

  ShowMessage(Format('%sFinished (%u removed bytes) %s',
    [#13, Size, LineEnding]));
end;

procedure TGulpApplication.List(const FileName: ansistring);
var
      Count : longint = 0;
  LibReader : TGulpReader;
     LibRec : TGulpItem;
       I, J : longint;
     Stream : TStream;
    Version : longword = 0;
begin
  ShowMessage(GulpDescription);
  ShowMessage(Format('List the content of "%s" %s', [FileName, LineEnding]));
  ShowMessage(Format('%sScanning archive...      ', [#13]));
  Stream    := TFileStream.Create (FileName, fmOpenRead);
  LibReader := TGulpReader.Create(Stream);
  LibReader.Load(FUntilVersion);

  for I := FInclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FInclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
  end;
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
  begin
    J := LibReader.Find(FExclude[I]);
    if J <> -1 then
      if LibReader[J].Attr and faDirectory = faDirectory then
        FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
  end;

  ShowMessage(Format('%sListing items...       %s', [#13, LineEnding]));
  for I := 0 to LibReader.Count - 1 do
  begin
    LibRec  := LibReader[I];
    Version := Max(Version, LibRec.Version);
    if FileNameMatch(LibRec.Name, FInclude) = TRUE then
      if FileNameMatch(LibRec.Name, FExclude) = FALSE then
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
