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
  {$IFDEF UNIX}
  BaseUnix,
  Unix,
  UnixType,
  {$ENDIF}
  Classes,
  CustApp,
  LibGulp,
  Scanner;

const
  ShortSwitches = 's:r:p:l:t:f:c:';
  LongSwitches: array[1..7] of string = (
    'synch:', 'restore:', 'purge:', 'list:', 'time:', 'fix:', 'check:');

type
  TGulpProgress =class(TObject)
  private
    FIndex: cardinal;
  public
    procedure Start(const Message: string);
    procedure Stop (const Message: string);
    procedure Show;
  end;

  TGulpApplication = class(TCustomApplication)
  protected
    Message:   string;
    FileNames: TStringList;
    Progress:  TGulpProgress;
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
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Streams,
  Common;

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

function TimeDifference(X: TDateTime): string;
begin
  Result := Format('%0.2f', [(Now - X) * (24 * 60 * 60)]);
end;

// =============================================================================
// TGulpProgress
// =============================================================================

procedure TGulpProgress.Start(const Message: string);
begin
  write(Message);
  FIndex := 0;
end;

procedure TGulpProgress.Stop(const Message: string);
begin
  writeln(Message);
end;

procedure TGulpProgress.Show;
const
  Spin: array[0..3] of char = ('-','\','|','/');
begin
  if FIndex = SizeOf(Spin) then FIndex := 0;
  write(Spin[FIndex], #8);
  Inc(FIndex);
end;

// =============================================================================
// TGulpApplication
// =============================================================================

constructor TGulpApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileNames := TStringList.Create;
  Progress  := TGulpProgress.Create;
  Switches  := TStringList.Create;
  Scanner   := TSysScanner.Create;
  Start     := Now;
end;

destructor TGulpApplication.Destroy;
begin
  FileNames.Destroy;
  Progress.Destroy;
  Switches.Destroy;
  Scanner.Destroy;
  inherited Destroy;
end;

procedure TGulpApplication.Synch;
var
  I, J: longint;
  LastFlag: boolean;
  GulpWriter: TGulpWriter = nil;
  GulpList: TGulpList = nil;
  Rec: TGulpRec;
begin
  writeln('Synchronize ' + GetOptionValue('s', 'synch'));
  try
    Message := 'Fatal error on scanning.';
    write('Scanning...');
    for I := 0 to FileNames.Count - 1 do
      Scanner.Add(FileNames[I]);
    writeln(' done');

    Message := 'Fatal error on opening archive.';
    write('Opening archive...');
    if FileExists(GetOptionValue('s', 'synch')) then
      Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmOpenRead)
    else
      Stream := nil;
    writeln(' done');

    Message := 'Fatal error on reading records.';
    write('Read records...');
    GulpList := TGulpList.Create(Stream);
    if Assigned(Stream) then
    begin
      GulpList.LoadAt(Start);
      FreeAndNil(Stream);
    end;
    writeln(' done');

    Message := 'Fatal error on opening/creating archive.';
    write('Opening archive...');
    if FileExists(GetOptionValue('s', 'synch')) then
      Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmOpenWrite)
    else
      Stream := TFileStream.Create (GetOptionValue('s', 'synch'), fmCreate);
    writeln(' done');

    Message := 'Fatal error on delete records.';
    GulpWriter := TGulpWriter.Create(Stream);
    Progress.Start('Deleting recors...');
    for I := 0 to GulpList.Count - 1 do
    begin
      Rec   := GulpList.Items[I];
      Message := 'Fatal error on delete ' + Rec.Name + '.';

      LastFlag := FALSE;
      if Scanner.Count = 0 then
        LastFlag := GulpList.Count -1 = I;

      J := Scanner.Find(Rec.Name);
      if J = -1 then
        GulpWriter.WriteClean(Rec.Name, LastFlag)
      else
        if (CompareFileAttr(Scanner.Items[J], Rec) <> 0) or
           (CompareFileTime(Scanner.Items[J], Rec) <> 0) or
           (CompareFileSize(Scanner.Items[J], Rec) <> 0) then
          GulpWriter.WriteClean(Rec.Name, LastFlag)
        else
          Scanner.Delete(J);
      Progress.Show;
    end;
    Progress.Stop(' done');

    Message := 'Fatal error on adding records.';
    Progress.Start('Adding recors...');
    for I := 0 to Scanner.Count - 1 do
    begin
      Message := 'Fatal error on adding ' + Scanner.Items[I] + '.';

      LastFlag := Scanner.Count -1 = I;
      case GetFileFlag(Scanner.Items[I]) of
        gfFile:         GulpWriter.WriteFile      (Scanner.Items[I], LastFlag);
        gfLink:         GulpWriter.WriteLink      (Scanner.Items[I], LastFlag);
        gfSymbolicLink: GulpWriter.WriteSymLink   (Scanner.Items[I], LastFlag);
        gfDirectory:    GulpWriter.WriteDirectory (Scanner.Items[I], LastFlag);
      end;
      Progress.Show;
    end;
    Progress.Stop(' done');

    Message := 'Fatal error on showing changes.';
    writeln('Added ', GulpWriter.CleanCount,     ' Cleans');
    writeln('Added ', GulpWriter.FileCount,      ' Files');
    writeln('Added ', GulpWriter.LinkCount,      ' Links');
    writeln('Added ', GulpWriter.SymLinkCount,   ' Symbolic Links');
    writeln('Added ', GulpWriter.DirectoryCount, ' Directories');

    writeln('Finished.');
  except
    writeln;
    writeln(Message);
  end;
  if Assigned(GulpWriter) then FreeAndNil(GulpWriter);
  if Assigned(GulpList)   then FreeAndNil(GulpList);
  if Assigned(Stream)     then FreeAndNil(Stream);
end;

procedure TGulpApplication.Restore;
begin
end;

procedure TGulpApplication.Fix;
var
  Rec: TGulpRec = nil;
  GulpReader: TGulpReader = nil;
  FixSize: int64 = 0;
begin
  writeln('Fix the contents of ' + GetOptionValue('f', 'fix'));
  try
    Message := 'Fatal error on opening archive.';
    write('Opening archive...');
    Stream := TFileStream.Create (GetOptionValue('f', 'fix'), fmOpenRead);
    writeln(' done');

    Message := 'Fatal error on reading records.';
    GulpReader := TGulpReader.Create(Stream);
    GulpReader.Reset;

    Rec := TGulpRec.Create;
    try
      while GulpReader.FindNext(Rec) do
        if Rec.Flags and gfLast <> 0 then
        begin
          FixSize := Stream.Position;
          writeln('Found job at ',
            FormatDateTime(
              DefaultFormatSettings.LongDateFormat + ' ' +
              DefaultFormatSettings.LongTimeFormat, Rec.StoredTime));
        end;
    except
      // noting to do
    end;

    if FixSize <> Stream.Size then
    begin
      writeln('Fix at ', FixSize, '/', Stream.Size);

      FreeAndNil(Stream);
      Stream := TFileStream.Create (GetOptionValue('f', 'fix'), fmOpenWrite);
      Stream.Size := FixSize;
    end;
    writeln('Finished.');
  except
    writeln;
    writeln(Message);
  end;
  if Assigned(GulpReader) then FreeAndNil(GulpReader);
  if Assigned(Stream)     then FreeAndNil(Stream);
  if Assigned(Rec)        then FreeAndNil(Rec);
end;

procedure TGulpApplication.Check;
var
  Nul: TNulStream;
  Rec: TGulpRec = nil;
  GulpReader: TGulpReader = nil;
  StoredTime: TDateTime = 0.0;
begin
  writeln('Check the contents of ' + GetOptionValue('c', 'check'));
   try
     Message := 'Fatal error on opening archive.';
     write('Opening archive...');
     Stream := TFileStream.Create (GetOptionValue('c', 'check'), fmOpenRead);
     writeln(' done');

     Message := 'Fatal error on reading records.';
     GulpReader := TGulpReader.Create(Stream);
     GulpReader.Reset;


     Rec := TGulpRec.Create;
     Nul := TNulStream.Create;
     while GulpReader.ReadStream(Rec, Nul) do
     begin

       if StoredTime <> Rec.StoredTime then
       begin
         StoredTime := Rec.StoredTime;
         writeln('Found job at ' + DateTimeToStr(StoredTime));
       end;

       if Rec.Flags and gfChecksum <> 0 then
         if Rec.Checksum <> Rec.ChecksumAux then
         begin
           Writeln (Rec.Name + ' CHECKSUM MISMATCHED');
         end
     end;

     writeln('Finished.');
   except
     writeln;
     writeln(Message);
   end;
   if Assigned(GulpReader) then FreeAndNil(GulpReader);
   if Assigned(Stream)     then FreeAndNil(Stream);
   if Assigned(Nul)        then FreeAndNil(Nul);
   if Assigned(Rec)        then FreeAndNil(Rec);
end;

procedure TGulpApplication.Purge;
var
  GulpReader: TGulpReader;
  GulpWriter: TGulpWriter;
  GulpList: TGulpList;
  New: TStream;
begin
  writeln('Purge ' + GetOptionValue('p', 'purge'));
  // open and read archive ...
  write('Opening archive...');
  if FileExists(GetOptionValue('s', 'synch')) then
  begin
    Stream := TFileStream.Create(GetOptionValue('s', 'synch'), fmOpenRead);
  end else
    Stream := nil;

  GulpList := TGulpList.Create(Stream);
    GulpList.LoadAt(Start);
    FreeAndNil(Stream);




    GulpList := TGulpList.Create(nil);
  writeln(' done');










  // re-open/create and update archive ...
  if FileExists(GetOptionValue('s', 'synch')) then
    Stream := TFileStream.Create(GetOptionValue('s', 'synch'), fmOpenWrite)
  else
    Stream := TFileStream.Create(GetOptionValue('s', 'synch'), fmCreate);
  Stream.Seek(0, soFromEnd);
  GulpWriter := TGulpWriter.Create(Stream);
  // delete old records ...
  Progress.Start('Deleting old recors...');







end;

procedure TGulpApplication.List;
var
  I, J: longint;
  Rec: TGulpRec;
  GulpList: TGulpList = nil;
  HistoryMode: boolean;
  StoredTime: TDateTime = 0.0;
begin
  writeln('List the contents of ' + GetOptionValue('l', 'list'));
  try
    Message := 'Fatal error on opening archive.';
    write('Opening archive...');
    Stream := TFileStream.Create (GetOptionValue('l', 'list'), fmOpenRead);
    writeln(' done');

    Message := 'Fatal error on reading records.';
    GulpList := TGulpList.Create(Stream);
    if FileNames.Count = 0 then
      FileNames.Add('*');

    HistoryMode := GetOptionValue('t', 'time') = '';
    if HistoryMode = FALSE then
      GulpList.LoadAt(StrToDateTime(GetOptionValue('t', 'time')))
    else
      GulpList.Load;

    Message := 'Fatal error on showing records.';
    for J := 0 to GulpList.Count - 1 do
    begin
      Rec   := GulpList.Items[J];
      Message := 'Fatal error on showing ' + Rec.Name + '.';
      for I := 0 to FileNames.Count - 1 do
        if FileNameMatch(Rec.Name, FileNames[I])  then
        begin

          if HistoryMode = TRUE then
            if StoredTime <> Rec.StoredTime then
            begin
              StoredTime := Rec.StoredTime;
              writeln('Found job at ' + DateTimeToStr(StoredTime));
            end;

          writeln(Format('%9s %19s %12s %s', [
            AttrToString(Rec),
            TimeToString(Rec),
            SizeToString(Rec),
            Rec.Name]));
          Break;
        end;
    end;

    Message := 'Fatal error on showing changes.';
    writeln('Listed ', GulpList.CleanCount,     ' Cleans');
    writeln('Listed ', GulpList.FileCount,      ' Files');
    writeln('Listed ', GulpList.LinkCount,      ' Links');
    writeln('Listed ', GulpList.SymLinkCount,   ' Symbolic Links');
    writeln('Listed ', GulpList.DirectoryCount, ' Directories');

    writeln('Finished.');
  except
    writeln;
    writeln(Message);
  end;
  if Assigned(GulpList) then FreeAndNil(GulpList);
  if Assigned(Stream)   then FreeAndNil(Stream);
end;

procedure TGulpApplication.DoRun;
var
  Error: string;
begin
  writeln('GULP 0.0.2 archiver utility, Copyright (c) 2014 Melchiorre Caruso.');

  DefaultFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
  DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  Error := CheckOptions(ShortSwitches, LongSwitches, Switches, FileNames);
  if Error = '' then
  begin
    if HasOption('s', 'synch'  ) then Synch   else
    if HasOption('r', 'restore') then Restore else
    if HasOption('p', 'purge'  ) then Purge   else
    if HasOption('f', 'fix'    ) then Fix     else
    if HasOption('c', 'check'  ) then Check   else
    if HasOption('l', 'list'   ) then List;

    writeln('Elapsed ', TimeDifference(Start), ' sec');
  end else
    writeln(Error);

  Terminate;
end;

end.
