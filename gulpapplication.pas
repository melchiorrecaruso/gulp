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

    The journaling archiver shell.

  Modified:

    v0.0.2 - 2015.05.10 by Melchiorre Caruso.
}

unit GulpApplication;

interface

uses
  Classes,
  GulpLibrary;

type
  { TGulpApplication class }

  TGulpApplication = class(TObject)
  private
    FFileName     : string;
    FExclude      : TStringList;
    FInclude      : TStringList;
    FMethod       : string;
    FNodelete     : boolean;
    FUntilVersion : string;
  protected
    procedure ShowMessage(const Message: string); virtual; abstract;
    procedure ShowRec    (Rec: TGulpRec        ); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Synchronize;
    procedure Restore;
    procedure Purge;
    procedure List;
    procedure Fix;
    procedure Check;
    property FileName     : string      read FFileName     write FFileName;
    property Exclude      : TStringList read FExclude;
    property Include      : TStringList read FInclude;
    property Method       : string      read FMethod       write FMethod;
    property NoDelete     : boolean     read FNoDelete     write FNoDelete;
    property UntilVersion : string      read FUntilVersion write FUntilVersion;
  end;

  { TGulpShellApplication class }

  TGulpShellApplication = class(TGulpApplication)
  protected
    procedure ShowMessage(const Message: string); override;
    procedure ShowRec    (Rec: TGulpRec        ); override;
  end;

implementation

uses
  GulpCommon,
  SysUtils;

const
  Description = 'GULP v0.0.2 journaling archiver, copyright (c) 2014-2015 Melchiorre Caruso.' + LineEnding +
                'GULP archiver for user-level incremental backups with rollback capability.';

// =============================================================================
// TGulpApplication
// =============================================================================

constructor TGulpApplication.Create;
begin
  inherited Create;
  FFileName := '';
  FExclude  := TStringList.Create;
  FInclude  := TStringList.Create;
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
  FMethod              := '';
  FNodelete            := FALSE;
  FUntilVersion        := '';
end;

destructor TGulpApplication.Destroy;
begin
  FExclude.Destroy;
  FInclude.Destroy;
  inherited Destroy;
end;

procedure TGulpApplication.Synchronize;
var
  GulpLib : TGulpLib;
     I, J : longint;
     Scan : TSysScanner;
     Size : int64 = 0;
   Stream : TStream;
begin
  ShowMessage(Description);
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('Synch the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage('Scanning archive...      ');
  if FileExists(FileName) then
    Stream := TFileStream.Create (FileName, fmOpenReadWrite)
  else
    Stream := TFileStream.Create (FileName, fmCreate);
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(longword(-2)) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  Size := Stream.Seek(0, soEnd);
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Scanning filesystem...   ');
  Scan := TSysScanner.Create;
  for I := FInclude.Count - 1 downto 0 do
    if DirectoryExists(FInclude[I]) = TRUE then
      FInclude.Add(IncludeTrailingPathDelimiter(FInclude[I]) + '*')
    else
      if FInclude[I][Length(FInclude[I])] = PathDelim then
        FInclude[I] := FInclude[I] + '*';
  if FInclude.Count = 0 then
    FInclude.Add('*');

  for I := FExclude.Count - 1 downto 0 do
    if DirectoryExists(FExclude[I]) = TRUE then
      FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
    else
      if FExclude[I][Length(FExclude[I])] = PathDelim then
        FExclude[I] := FExclude[I] + '*';
  FExclude.Add(FileName);
  for I := FInclude.Count - 1 downto 0 do
    Scan.Add(FInclude[I]);
  for I := Scan.Count - 1 downto 0 do
    if FileNameMatch(Scan.Items[I], FExclude) = TRUE then
      Scan.Delete(I);
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Deleting records...      ');
  if FNoDelete = FALSE then
  begin
    for I := 0 to GulpLib.Count - 1 do
      if Scan.Find(GulpLib.Items[I].Name) = -1 then
        GulpLib.Delete(I);
  end;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Adding records...        ');
  if FMethod <> '' then
  begin
    if FMethod = 'gzfast' then GulpLib.Method := gmGZFast   else
    if FMethod = 'gz'     then GulpLib.Method := gmGZNormal else
    if FMethod = 'gzmax'  then GulpLib.Method := gmGZMax    else
      raise Exception.Create('Wrong method value');
  end;
  for I := 0 to Scan.Count - 1 do
  begin
    J := GulpLib.Find(Scan.Items[I]);
    if J = -1 then
      GulpLib.Add(Scan.Items[I])
    else
      if GetTime(Scan.Items[I]) <> GulpLib.Items[J].Time then
      begin
        GulpLib.Delete(J);
        GulpLib.Add(Scan.Items[I]);
      end;
  end;
  GulpLib.CloseArchive;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage(Format('Finished (%u added bytes)', [Stream.Seek(0, soEnd) - Size]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  FreeandNil(Scan);
end;

procedure TGulpApplication.Restore;
var
  GulpLib : TGulpLib;
  GulpRec : TGulpRec;
     I, J : longint;
     Scan : TSysScanner;
     Size : int64 = 0;
   Stream : TStream;
  Version : longword;
begin
  ShowMessage(Description);
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('Restore the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage('Scanning archive...      ');
  Stream := TFileStream.Create(FileName, fmOpenRead);
  Version := longword(-2);
  if FUntilVersion <> '' then
  begin
    if FUntilVersion = 'last' then
      Version := longword(-2)
    else
      Version := StrToInt(FUntilVersion);
  end;
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(Version) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Scanning filesystem...   ');
  Scan := TSysScanner.Create;
  Scan.Add('*');
  for I := FInclude.Count - 1 downto 0 do
    if FInclude[I][Length(FInclude[I])] = PathDelim then
    begin
      FInclude[I] := FInclude[I] + '*'
    end else
    begin
      J := GulpLib.Find(FInclude[I]);
      if J <> -1 then
        if GulpLib.Items[J].Attributes and faDirectory = faDirectory then
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
      J := GulpLib.Find(FExclude[I]);
      if J <> -1 then
        if GulpLib.Items[J].Attributes and faDirectory = faDirectory then
          FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
    end;
  FExclude.Add(FileName);
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Deleting records...      ');
  if FNoDelete = FALSE then
    for I := Scan.Count - 1 downto 0 do
    begin
      J := GulpLib.Find(Scan.Items[I]);
      if (J = -1) or
         (FileNameMatch(GulpLib.Items[J].Name, FInclude) = FALSE) or
         (FileNameMatch(GulpLib.Items[J].Name, FExclude) = TRUE ) then
        begin
          if DirectoryExists(Scan.Items[I]) = TRUE then
            RemoveDir(Scan.Items[I])
          else
            DeleteFile(Scan.Items[I]);
        end;
    end;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Extracting records...    ');
  for I := GulpLib.Count - 1 downto 0 do
  begin
    GulpRec := GulpLib.Items[I];
    if FileNameMatch(GulpRec.Name, FInclude) = TRUE then
      if FileNameMatch(GulpRec.Name, FExclude) = FALSE then
      begin
        J := Scan.Find(GulpRec.Name);
        if J = -1 then
        begin
          GulpLib.Extract(I);
          Inc(Size, GulpRec.Size);
        end else
          if GetTime(Scan.Items[J]) <> GulpRec.Time then
          begin
            GulpLib.Extract(I);
            Inc(Size, GulpRec.Size);
          end;
      end;
  end;
  GulpLib.CloseArchive;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage(Format('Finished (%u extracted bytes)', [Size]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  FreeAndNil(Scan);
end;

procedure TGulpApplication.Fix;
var
    Size : int64;
  Stream : TStream;
begin
  ShowMessage(Description);
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('Fix the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage('Fixing archive...        ');
  Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  Size   := Stream.Seek(0, soEnd);
  FixArchive(Stream);
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage(Format('Finished (%u removed bytes)', [Size - Stream.Size]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  FreeAndNil(Stream);
end;

procedure TGulpApplication.Check;
var
  GulpLib : TGulpLib;
        I : longint;
      Nul : TStream;
   Stream : TStream;
begin
  ShowMessage(Description);
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('Check the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage('Scanning archive...      ');
  Stream := TFileStream.Create (FileName, fmOpenRead);
  Nul    := TNulStream.Create;
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(longword(-1)) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Checking records...      ');
  for I := 0 to GulpLib.Count - 1 do
    GulpLib.ExtractTo(I, Nul);
  GulpLib.CloseArchive;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage(Format('Finished (%u checked bytes)', [Stream.Size]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
  FreeAndNil(Nul);
end;

procedure TGulpApplication.Purge;
var
   Stream : TStream;
      Tmp : TStream;
  TmpName : string;
begin
  ShowMessage(Description);
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('Purge the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage('Moving records...        ');
  TmpName := GetTempFileName(ExtractFileDir(FileName), '');
  Tmp     := TFileStream.Create(TmpName,  fmCreate  );
  Stream  := TFileStream.Create(FileName, fmOpenRead);
  PurgeArchive(Stream, Tmp);
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage(Format('Finished (%u removed bytes)', [Stream.Size - Tmp.Size]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  FreeAndNil(Stream);
  FreeAndNil(Tmp);
  if DeleteFile(FileName) = FALSE then
    raise Exception.CreateFmt('Unable to delete file "%s"', [FileName])
  else
    if RenameFile(TmpName, FileName)= FALSE then
      raise Exception.CreateFmt('Unable to rename file "%s"', [TmpName]);
end;

procedure TGulpApplication.List;
var
    Count : longint;
  GulpLib : TGulpLib;
  GulpRec : TGulpRec;
     I, J : longint;
   Stream : TStream;
  Version : longword;
begin
  ShowMessage(Description);
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('List the content of %s', [FileName]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage('Scanning archive...      ');
  Stream := TFileStream.Create (FileName, fmOpenRead);
  Version := longword(-1);
  if FUntilVersion <> '' then
  begin
    if FUntilVersion = 'last' then
      Version := longword(-2)
    else
      Version := StrToInt(FUntilVersion);
  end;
  GulpLib := TGulpLib.Create(Stream);
  if GulpLib.OpenArchive(Version) = FALSE then
  begin
    if Stream.Size <> 0 then
      raise Exception.Create('Invalid signature value');
  end;
  {$IFDEF CONSOLEAPP} ShowMessage(#13); {$ENDIF}
  ShowMessage('Listing records...       ');
  for I := FInclude.Count - 1 downto 0 do
    if FInclude[I][Length(FInclude[I])] = PathDelim then
    begin
      FInclude[I] := FInclude[I] + '*'
    end else
    begin
      J := GulpLib.Find(FInclude[I]);
      if J <> -1 then
        if GulpLib.Items[J].Attributes and faDirectory = faDirectory then
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
      J := GulpLib.Find(FExclude[I]);
      if J <> -1 then
        if GulpLib.Items[J].Attributes and faDirectory = faDirectory then
          FExclude.Add(IncludeTrailingPathDelimiter(FExclude[I]) + '*')
    end;

  Count := 0;
  for I := 0 to GulpLib.Count - 1 do
  begin
    GulpRec := GulpLib.Items[I];
    if FileNameMatch(GulpRec.Name, FInclude) = TRUE then
      if FileNameMatch(GulpRec.Name, FExclude) = FALSE then
      begin
        ShowRec(GulpRec);
        Inc(Count);
      end;
  end;
  GulpLib.CloseArchive;
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('Finished (%u listed records)', [Count]));
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  FreeAndNil(GulpLib);
  FreeAndNil(Stream);
end;

// =============================================================================
// TGulpShellApplication
// =============================================================================

procedure TGulpShellApplication.ShowMessage(const Message: string);
begin
  write(Message);
end;

procedure TGulpShellApplication.ShowRec(Rec: TGulpRec);
begin
  {$IFDEF CONSOLEAPP} ShowMessage(LineEnding); {$ENDIF}
  ShowMessage(Format('%4s %3s %3s %7s %19s %12s %s', [
     VerTostring(Rec),
    FlagToString(Rec),
    ModeToString(Rec),
    AttrToString(Rec),
    TimeToString(Rec),
    SizeToString(Rec),
    Rec.Name]));
end;

end.
