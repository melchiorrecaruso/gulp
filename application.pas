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

unit Application;

{$I gulp.inc}

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
  LibGulp,
  Scanner;

type
  TGulpApplication = class(TCustomApplication)
  private
    FileNames: TStringList;
    Switches:  TStringList;
    Scanner:   TSysScanner;
    Stream:    TFileStream;
    Start:     TDateTime;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Streams;

function SetIdlePriority: boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := FpNice (10) = 0;
  {$ENDIF}
end;

function SetNormalPriority: boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
  {$ENDIF}
  {$IFDEF UNIX}
    Result := FpNice (10) = 0;
  {$ENDIF}
end;

// =============================================================================
// TGulpApplication
// =============================================================================

constructor TGulpApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := TRUE;
  FileNames       := TStringList.Create;
  Switches        := TStringList.Create;
  Scanner         := TSysScanner.Create;
  Start           := Now;
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
  I, J: longint;
  GulpWriter: TGulpWriter = nil;
  GulpList: TGulpList = nil;
  Rec: TGulpRec = nil;
begin
  writeln(#13, #13: 80, 'Synchronize ' + GetOptionValue('s', 'synch'));
  write  (#13, #13: 80, 'Scanning... ');
  for I := 0 to FileNames.Count - 1 do
    Scanner.Add(FileNames[I]);

  write(#13, #13: 80, 'Opening archive... ');
  if FileExists(GetOptionValue('s', 'synch')) then
    Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmOpenReadWrite)
  else
    Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmCreate);;

  write(#13, #13: 80, 'Read records... ');
  GulpList := TGulpList.Create(Stream);
  GulpList.LoadAt(Start);

  write(#13, #13: 80, 'Deleting recors... ');
  GulpWriter := TGulpWriter.Create(Stream);
  for I := 0 to GulpList.Count - 1 do
  begin
    Rec := GulpList.Items[I];
    J := Scanner.Find(Rec.Name);
    if J = -1 then
      GulpWriter.Delete(Rec.Name)
    else
      if (CompareFileAttr(Scanner.Items[J], Rec) <> 0) or
         (CompareFileTime(Scanner.Items[J], Rec) <> 0) or
         (CompareFileSize(Scanner.Items[J], Rec) <> 0) then
        GulpWriter.Delete(Rec.Name)
      else
        Scanner.Delete(J);
  end;

  write(#13, #13: 80, 'Adding recors... ');
  for I := 0 to Scanner.Count - 1 do
    GulpWriter.Add(Scanner.Items[I]);

  FreeAndNil(GulpWriter);
  FreeAndNil(GulpList);
  FreeAndNil(Stream);
  writeln(#13, #13: 80, 'Finished.');
end;

procedure TGulpApplication.Restore;
begin

end;

procedure TGulpApplication.Fix;
var
  GulpReader: TGulpReader = nil;
  Rec: TGulpRec = nil;
  FixSize: int64 = 0;
begin
  writeln(#13, #13: 80, 'Fix the contents of ' + GetOptionValue('f', 'fix'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create (GetOptionValue('f', 'fix'), fmOpenReadWrite);

  write(#13, #13: 80, 'Read records... ');
  GulpReader := TGulpReader.Create(Stream);
  GulpReader.Reset;

  Rec := TGulpRec.Create;
  try
    while GulpReader.FindNext(Rec ,nil) do
      if Rec.Flags and $FF = gfLast then
      begin
        FixSize := Stream.Position;
        writeln(#13, #13: 80, 'Founded a job at ',
          FormatDateTime(
            DefaultFormatSettings.LongDateFormat + ' ' +
            DefaultFormatSettings.LongTimeFormat, Rec.StoredTime));
        write(#13, #13: 80, 'Read records... ');
      end;
  except
    // noting to do
  end;

  if FixSize < Stream.Size then
  begin
    writeln(#13, #13: 80, 'Fixed size at ', FixSize, '/', Stream.Size);
    Stream.Size := FixSize;
  end;
  FreeAndNil(Rec);
  FreeAndNil(GulpReader);
  FreeAndNil(Stream);
  writeln(#13, #13: 80, 'Finished.');
end;

procedure TGulpApplication.Check;
var
  Nul: TNulStream;
  GulpReader: TGulpReader;
  Rec: TGulpRec;
  StoredTime: TDateTime = 0.0;
begin
  writeln(#13, #13: 80, 'Check the contents of ' + GetOptionValue('c', 'check'));
  write  (#13, #13: 80, 'Opening archive...');
  Stream := TFileStream.Create (GetOptionValue('c', 'check'), fmOpenRead);

  GulpReader := TGulpReader.Create(Stream);
  GulpReader.Reset;

  Rec := TGulpRec.Create;
  Nul := TNulStream.Create;
  while GulpReader.FindNext(Rec, Nul) do
  begin
    if StoredTime <> Rec.StoredTime then
    begin
      StoredTime := Rec.StoredTime;
      writeln(#13, #13: 80, 'Founded a job at ' + DateTimeToStr(StoredTime));
    end;

    if Rec.ChecksumOK = FALSE then
      raise Exception.Create(Rec.Name + ' checksum mismatched');
  end;
  FreeAndNil(Nul);
  FreeAndNil(Rec);
  FreeAndNil(GulpReader);
  FreeAndNil(Stream);
  writeln(#13, #13: 80, 'Finished.');
end;

procedure TGulpApplication.Purge;
var
  I: longint;
  GulpReader: TGulpReader;
  GulpWriter: TGulpWriter;
  GulpList: TGulpList;
  Rec: TGulpRec;
  New: TStream;
begin
  writeln(#13, #13: 80, 'Purge ' + GetOptionValue('p', 'purge'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create(GetOptionValue('p', 'purge'), fmOpenRead);

  write(#13, #13: 80, 'Read records... ');
  GulpList := TGulpList.Create(Stream);
  GulpList.LoadAt(Start);

  write(#13, #13: 80, 'Move records... ');
  New := TFileStream.Create('pippo', fmCreate);
  GulpWriter := TGulpWriter.Create(New);
  for I := 0 to GulpList.Count - 1 do
  begin
    Rec := GulpList.Items[I];
    GulpWriter.CopyFrom(Rec, Stream);
  end;

  FreeAndNil(New);
  FreeAndNil(GulpWriter);
  FreeAndNil(GulpList);
  FreeAndNil(Stream);
  writeln(#13, #13: 80, 'Finished.');
end;

procedure TGulpApplication.List;
var
  I, J: longint;
  Rec: TGulpRec;
  GulpList: TGulpList = nil;
  StoredTime: TDateTime = 0.0;
  HistoryMode: boolean;
begin
  writeln(#13, #13: 80, 'List the contents of ' + GetOptionValue('l', 'list'));
  write  (#13, #13: 80, 'Opening archive... ');
  Stream := TFileStream.Create (GetOptionValue('l', 'list'), fmOpenRead);

  write  (#13, #13: 80, 'Read records... ');
  GulpList := TGulpList.Create(Stream);
  if FileNames.Count = 0 then
    FileNames.Add('*');

  HistoryMode := GetOptionValue('t', 'time') = '';
  if HistoryMode = FALSE then
  begin
    if lowercase(GetOptionValue('t', 'time')) = 'now' then
      GulpList.LoadAt(Start)
    else
      GulpList.LoadAt(StrToDateTime(GetOptionValue('t', 'time')))
  end else
    GulpList.Load;

  for J := 0 to GulpList.Count - 1 do
  begin
    Rec   := GulpList.Items[J];
    for I := 0 to FileNames.Count - 1 do
      if FileNameMatch(Rec.Name, FileNames[I])  then
      begin
        if HistoryMode = TRUE then
          if StoredTime <> Rec.StoredTime then
          begin
            StoredTime := Rec.StoredTime;
            writeln(#13, #13: 80, 'Founded a job at ' + DateTimeToStr(StoredTime));
          end;

        writeln(#13, #13: 80, Format('%3s %17s %19s %12s %s', [
          CommandToString(Rec),
             AttrToString(Rec),
             TimeToString(Rec),
             SizeToString(Rec),
          Rec.Name]));
        Break;
      end;
  end;


  FreeAndNil(GulpList);
  FreeAndNil(Stream);
  writeln(#13, #13: 80, 'Finished.');
end;

procedure TGulpApplication.Help;
begin
  writeln('HELP-FILE');
end;

procedure TGulpApplication.DoRun;
var
  Error: string;
  ShortSwitches: string;
  LongSwitches: TStrings;
begin
  writeln('GULP 0.0.2 archiver utility, Copyright (c) 2014 Melchiorre Caruso.');

  DefaultFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
  DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  ShortSwitches := 's:r:p:l:c:f:t:h';
  LongSwitches  :=  TStringList.Create;
  LongSwitches.Add('synch:');
  LongSwitches.Add('restore:');
  LongSwitches.Add('purge:');
  LongSwitches.Add('list:');
  LongSwitches.Add('check:');
  LongSwitches.Add('fix:');
  LongSwitches.Add('time:');
  LongSwitches.Add('help');

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
  writeln(#13, #13: 80, 'Elapsed ',
    Format('%0.2f', [(Now - Start) * (24 * 60 * 60)]) , ' sec');
  FreeAndNil(LongSwitches);
  Terminate;
end;

end.
