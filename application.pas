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

unit Application;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  {$ENDIF}
  SysUtils,
  Classes,
  CustApp,
  Libgulp,
  Common;

type
  { TGulpApplication class }

  TGulpApplication = class (TCustomApplication)
  private
    FileNames : TStringList;
    Switches  : TStringList;
    Scanner   : TSysScanner;
    Stream    : TFileStream;
    Start     : TDateTime;
    procedure Synch;
    procedure Restore;
    procedure Purge;
    procedure List;
    procedure Fix;
    procedure Check;
    procedure Help;
  protected
    procedure DoRun; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Kill;
  end;

implementation

{$IFDEF MSWINDOWS}
function SetIdlePriority: boolean;
begin
  Result := SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
end;
{$ENDIF}

{$IFDEF UNIX}
function SetIdlePriority: boolean;
begin
  Result := FpNice (10) = 0;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function SetNormalPriority: boolean;
begin
  Result := SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
end;
{$ENDIF}

{$IFDEF UNIX}
function SetNormalPriority: boolean;
begin
  Result := FpNice (10) = 0;
end;
{$ENDIF}

// =============================================================================
// TGulpApplication
// =============================================================================

constructor TGulpApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CaseSensitiveOptions := TRUE;
  StopOnException      := TRUE;
  FileNames            := TStringList.Create;
  Switches             := TStringList.Create;
  Scanner              := TSysScanner.Create;
  Start                := Now;
end;

destructor TGulpApplication.Destroy;
begin
  FileNames.Destroy;
  Switches.Destroy;
  Scanner.Destroy;
  inherited Destroy;
end;

procedure TGulpApplication.Synch;
var
  I, J       : longint;
  GulpWriter : TGulpWriter;
  GulpReader : TGulpReader;
  GulpList   : TGulpList;
  GulpRec    : TGulpRec;
begin
  writeln(#13, #13: 80, 'Synch the contents of ' + GetOptionValue('s', 'synch'));
  write  (#13, #13: 80, 'Scanning filesystem... ');
  for I := 0 to FileNames.Count - 1 do
    Scanner.Add(FileNames[I], rmAUTO);

  write(#13, #13: 80, 'Opening archive... ');
  if FileExists(GetOptionValue('s', 'synch')) then
    Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmOpenReadWrite)
  else
    Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmCreate);;

  write(#13, #13: 80, 'Reading records... ');
  GulpRec    := TGulpRec.Create;
  GulpList   := TGulpList.Create(Start);
  GulpReader := TGulpReader.Create(Stream);
  while GulpReader.ReadNext(GulpRec, nil) do
  begin
    GulpList.Add(GulpRec);
    GulpRec := TGulpRec.Create;
  end;
  FreeAndNil(GulpRec);

  write(#13, #13: 80, 'Deleting recors... ');
  GulpWriter := TGulpWriter.Create(Stream);
  for I := 0 to GulpList.Count - 1 do
  begin
    GulpRec := GulpList.Items[I];
    J := Scanner.Find(GulpRec.Name);
    if J = -1 then
    begin
      GulpWriter.Delete(GulpRec.Name);
    end else
      if (GetTime(Scanner.Items[J]) <> GulpRec.MTime) or
         (GetAttr(Scanner.Items[J]) <> GulpRec.Attr) or
         (GetMode(Scanner.Items[J]) <> GulpRec.Mode) or
         (GetSize(Scanner.Items[J]) <> GulpRec.Size) then
      begin
        GulpWriter.Delete(GulpRec.Name);
      end else
        Scanner.Delete(J);
  end;

  write(#13, #13: 80, 'Adding recors... ');
  for I := 0 to Scanner.Count - 1 do
    GulpWriter.Add(Scanner.Items[I]);

  write(#13, #13: 80, 'Finished (', GulpWriter.Count, ' records writed)');
  FreeAndNil(GulpWriter);
  FreeAndNil(GulpReader);
  FreeAndNil(GulpList);
  FreeAndNil(Stream);
  writeln;
end;

procedure TGulpApplication.Restore;
begin

end;

procedure TGulpApplication.Fix;
var
  OldSize    : int64 = 0;
  NewSize    : int64 = 0;
  GulpReader : TGulpReader;
  GulpRec    : TGulpRec;
begin
  writeln(#13, #13: 80, 'Fix the contents of ' + GetOptionValue('f', 'fix'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create (GetOptionValue('f', 'fix'), fmOpenReadWrite);

  write(#13, #13: 80, 'Reading records... ');
  GulpRec    := TGulpRec.Create;
  GulpReader := TGulpReader.Create(Stream);
  try
    while GulpReader.ReadNext(GulpRec ,nil) do
      if GulpRec.Flags and $FF = gfFix then
      begin
        NewSize := Stream.Position;
        writeln(#13, #13: 80, 'Founded fix point at ',
          FormatDateTime(
            DefaultFormatSettings.LongDateFormat + ' ' +
            DefaultFormatSettings.LongTimeFormat, GulpRec.STime));
        write(#13, #13: 80, 'Reading records... ');
      end;
  except
    // noting to do
  end;

  write(#13, #13: 80, 'Resizing archive... ');
  OldSize := Stream.Size;
  if NewSize < OldSize then
    Stream.Size := NewSize;

  write(#13, #13: 80, 'Finished (', OldSize - NewSize, ' bytes removed)');
  FreeAndNil(GulpReader);
  FreeAndNil(GulpRec);
  FreeAndNil(Stream);
  writeln;
end;

procedure TGulpApplication.Check;
var
  GulpReader : TGulpReader;
  GulpRec    : TGulpRec;
  Temp       : TStream;
begin
  writeln(#13, #13: 80, 'Check the contents of ' + GetOptionValue('c', 'check'));
  write  (#13, #13: 80, 'Opening archive... ');
  Temp   := TNulStream.Create;
  Stream := TFileStream.Create (GetOptionValue('c', 'check'), fmOpenRead);

  write(#13, #13: 80, 'Reading records... ');
  GulpRec    := TGulpRec.Create;
  GulpReader := TGulpReader.Create(Stream);
  while GulpReader.ReadNext(GulpRec, Temp) do
  begin
    if (GulpRec.Flags and $FF) = gfFix then
    begin
      writeln(#13, #13: 80, 'Founded fix point at ',
        FormatDateTime(
          DefaultFormatSettings.LongDateFormat + ' ' +
          DefaultFormatSettings.LongTimeFormat, GulpRec.STime));
      write  (#13, #13: 80, 'Reading records... ');
    end;

    if GulpRec.ChecksumOK = FALSE then
    begin
      if GulpRec.Flags and $FF = gfFIX then
        raise Exception.Create('Fix point checksum mismatched')
      else
        raise Exception.CreateFmt('Checksum mismatched for %s', [GulpRec.Name]);
    end;
  end;

  write(#13, #13: 80, 'Finished (No error detected)');
  FreeAndNil(GulpReader);
  FreeAndNil(GulpRec);
  FreeAndNil(Stream);
  FreeAndNil(Temp);
  writeln;
end;

procedure TGulpApplication.Purge;
var
  I          : longint;
  Count      : longword;
  GulpWriter : TGulpWriter;
  GulpReader : TGulpReader;
  GulpList   : TGulpList;
  GulpRec    : TGulpRec;
  TempName   : string;
  Temp       : TStream;
begin
  writeln(#13, #13: 80, 'Purge the contents of ' + GetOptionValue('p', 'purge'));
  write  (#13, #13: 80, 'Opening archive... ');
  TempName := GetTempFileName(GetCurrentDir, '');
  Temp     := TFileStream.Create(TempName, fmCreate);
  Stream   := TFileStream.Create(GetOptionValue('p', 'purge'), fmOpenRead);

  write(#13, #13: 80, 'Reading records... ');
  GulpRec    := TGulpRec.Create;
  GulpList   := TGulpList.Create(Start);
  GulpReader := TGulpReader.Create(Stream);
  while GulpReader.ReadNext(GulpRec, nil) do
  begin
    GulpList.Add(GulpRec);
    GulpRec := TGulpRec.Create;
  end;
  FreeAndNil(GulpRec);
  FreeAndNil(GulpReader);

  write(#13, #13: 80, 'Moving records... ');
  Count   := 0;
  GulpRec := TGulpRec.Create;
  GulpReader := TGulpReader.Create(Stream);
  GulpWriter := TGulpWriter.Create(Temp);
  repeat
    I := GulpList.Find(Count);
    if I = - 1 then
    begin
      if GulpReader.ReadNext(GulpRec, nil) = FALSE then Break;
    end else
      GulpWriter.Copy(GulpList.Items[I], Stream);
    Inc(Count);
  until FALSE;
  FreeAndNil(GulpRec);
  FreeAndNil(GulpReader);

  write(#13, #13: 80, 'Finished (', GulpWriter.Count, ' records writed)');
  FreeAndNil(GulpWriter);
  FreeAndNil(GulpList);
  FreeAndNil(Stream);
  FreeAndNil(Temp);

  if DeleteFile(GetOptionValue('p', 'purge')) = FALSE then
    raise Exception.CreateFmt('Error deleting file %s', [GetOptionValue('p', 'purge')])
  else
    if RenameFile(TempName, GetOptionValue('p', 'purge'))= FALSE then
      raise Exception.CreateFmt('Error renaming tmpfile %s', [TempName]);
  writeln;
end;

procedure TGulpApplication.List;
var
  I, J        : longint;
  GulpReader  : TGulpReader;
  GulpList    : TGulpList;
  GulpRec     : TGulpRec;
  StoredTime  : TDateTime;
begin
  writeln(#13, #13: 80, 'List the contents of ' + GetOptionValue('l', 'list'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create (GetOptionValue('l', 'list'), fmOpenRead);

  if GetOptionValue('t', 'time') <> '' then
  begin
    if lowercase(GetOptionValue('t', 'time')) <> 'now' then
      StoredTime := StrToDateTime(GetOptionValue('t', 'time')) + 0.00002
    else
      StoredTime := Start;
  end else
    StoredTime := 0.0;

  if FileNames.Count = 0 then
    FileNames.Add('*');

  write (#13, #13: 80, 'Reading records... ');
  GulpRec     := TGulpRec.Create;
  GulpList    := TGulpList.Create(StoredTime);
  GulpReader  := TGulpReader.Create(Stream);
  while GulpReader.ReadNext(GulpRec, nil) do
  begin
    GulpList.Add(GulpRec);
    GulpRec := TGulpRec.Create;
  end;
  FreeAndNil(GulpRec);

  for I := 0 to GulpList.Count - 1 do
  begin
    GulpRec := GulpList.Items[I];
    if GulpRec.Flags and $FF = gfFIX then
    begin
      writeln(#13, #13: 80, 'FIX point at ',
        FormatDateTime(
          DefaultFormatSettings.LongDateFormat + ' ' +
          DefaultFormatSettings.LongTimeFormat, GulpRec.STime));
    end else
      for J := 0 to FileNames.Count - 1 do
      begin
        if FileNameMatch(GulpRec.Name, FileNames[J])  then
          writeln(#13, #13: 80, Format('%3s %7s %9s %19s %12s %s', [
             TagToString(GulpRec),
            AttrToString(GulpRec),
            ModeToString(GulpRec),
            TimeToString(GulpRec),
            SizeToString(GulpRec),
            GulpRec.Name]));
        Break;
      end;
  end;

  FreeAndNil(GulpReader);
  FreeAndNil(GulpList);
  FreeAndNil(Stream);
  writeln(#13, #13: 80, 'Finished');
end;

procedure TGulpApplication.Help;
begin
  writeln('HELP-FILE');
end;

procedure TGulpApplication.Kill;
begin
  raise Exception.Create('User abort');
end;

procedure TGulpApplication.DoRun;
var
  Error         : string;
  ShortSwitches : string;
  LongSwitches  : TStringList;
begin
  inherited DoRun;
  writeln('GULP 0.0.2 archiver utility, copyright (c) 2014-2015 Melchiorre Caruso.');

  DefaultFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
  DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  ShortSwitches := 's:r:p:l:c:f:h:t:';
  LongSwitches  :=  TStringList.Create;
  LongSwitches.Add('synch:');
  LongSwitches.Add('restore:');
  LongSwitches.Add('purge:');
  LongSwitches.Add('list:');
  LongSwitches.Add('check:');
  LongSwitches.Add('fix:');
  LongSwitches.Add('help');
  LongSwitches.Add('time:');

  try
    Error := CheckOptions(ShortSwitches, LongSwitches, Switches, FileNames);
    if Error = '' then
    begin
      if HasOption('s', 'synch'  ) then Synch   else
      if HasOption('r', 'restore') then Restore else
      if HasOption('p', 'purge'  ) then Purge   else
      if HasOption('l', 'list'   ) then List    else
      if HasOption('c', 'check'  ) then Check   else
      if HasOption('f', 'fix'    ) then Fix     else
      if HasOption('h', 'help'   ) then Help    else  Help;
    end else
      writeln(#13, #13: 80, Error);
  except
    on E: Exception do
      writeln(#13, #13: 80, 'An exception was raised: ' + E.Message);
  end;
  FreeAndNil(LongSwitches);
  writeln(#13, #13: 80, 'Elapsed ',
    Format('%0.2f', [(Now - Start) * (24 * 60 * 60)]) , ' sec');
  Terminate;
end;

end.
