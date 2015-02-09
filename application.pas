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

  TGulpApplication = class(TCustomApplication)
  private
    FFileNames : TStringList;
    FSwitches  : TStringList;
    FScanner   : TSysScanner;
    FStream    : TFileStream;
    FStart     : TDateTime;
    FVersion   : longword;
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
    destructor  Destroy; override;
    procedure   Kill;
  end;

implementation

uses
  DateUtils;

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
  FFileNames           := TStringList.Create;
  FSwitches            := TStringList.Create;
  FScanner             := TSysScanner.Create;
  FStart               := Now;
  FVersion             := longword(-2);
end;

destructor TGulpApplication.Destroy;
begin
  FScanner.Destroy;
  FSwitches.Destroy;
  FFileNames.Destroy;
  inherited Destroy;
end;

procedure TGulpApplication.Synch;
var
     I, J : longint;
    Count : longint;
  GulpLib : TGulpLib;
begin
  writeln(#13, #13: 80, 'Synch the content of ' + GetOptionValue('s', 'synch'));
  write  (#13, #13: 80, 'Scanning filesystem... ');
  if FFileNames.Count = 0 then
    FFileNames.Add('*');
  for I := 0 to FFileNames.Count - 1 do
    FScanner.Add(FFileNames[I]);

  write(#13, #13: 80, 'Opening archive... ');
  if FileExists(GetOptionValue('s', 'synch')) then
    FStream := TFileStream.Create (GetOptionValue('s', 'synch'), fmOpenReadWrite)
  else
    FStream := TFileStream.Create (GetOptionValue('s', 'synch'), fmCreate);;

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(FStream);
  if GulpLib.OpenArchive(longword(-2)) = FALSE then
  begin
    if FStream.Size <> 0 then
      raise Exception.Create('FIX point error');
  end;

  write(#13, #13: 80, 'Deleting records... ');
  Count := 0;
  for I := 0 to GulpLib.Count - 1 do
  begin
    J := FScanner.Find(GulpLib.Items[I].Name);
    if J = -1 then
    begin
      Inc(Count);
      GulpLib.Delete(I);
    end else
      if GetTime(FScanner.Items[J]) <> GulpLib.Items[I].Time then
      begin
        Inc(Count);
        GulpLib.Delete(I);
      end else
        FScanner.Delete(J);
  end;

  write(#13, #13: 80, 'Adding records... ');
  for I := 0 to FScanner.Count - 1 do
  begin
    Inc(Count);
    GulpLib.Add(FScanner.Items[I]);
  end;
  GulpLib.CloseArchive;

  write(#13, #13: 80, 'Finished (', Count, ' added records)');
  FreeAndNil(GulpLib);
  FreeAndNil(FStream);
  writeln;
end;

procedure TGulpApplication.Restore;
var
     I, J : longint;
    Count : longint;
  GulpLib : TGulpLib;
begin
  writeln(#13, #13: 80, 'Restore the content of ' + GetOptionValue('r', 'restore'));
  write  (#13, #13: 80, 'Scanning filesystem... ');
  if FFileNames.Count = 0 then
    FFileNames.Add('*');
  for I := 0 to FFileNames.Count - 1 do
    FScanner.Add(FFileNames[I]);

  write(#13, #13: 80, 'Opening archive... ');
  FStream := TFileStream.Create(GetOptionValue('r', 'restore'), fmOpenRead);

  if GetOptionValue('u', 'until') <> '' then
  begin
    if lowercase(GetOptionValue('u', 'until')) <> 'now' then
      FVersion := StrToQWordDef(GetOptionValue('u', 'until'), longword(-1));
  end;

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(FStream);
  if GulpLib.OpenArchive(FVersion) = FALSE then
  begin
    if FStream.Size <> 0 then
      raise Exception.Create('FIX point error');
  end;

  write(#13, #13: 80, 'Deleting records... ');
  for I := 0 to FScanner.Count - 1 do
    if GulpLib.Find(FScanner.Items[I]) = - 1 then
      if DeleteFile(FScanner.Items[I]) then
        FScanner.Delete(I);

  for I := 0 to FScanner.Count - 1 do
    if GulpLib.Find(FScanner.Items[I]) = - 1 then
      if RemoveDir(FScanner.Items[I]) then
        FScanner.Delete(I);

  write(#13, #13: 80, 'Extracting records... ');
  Count := 0;
  for I := 0 to GulpLib.Count - 1 do
  begin
    J := FScanner.Find(GulpLib.Items[I].Name);
    if J = - 1 then
    begin
      Inc(Count);
      GulpLib.Extract(I);
    end else
      if GetTime(FScanner.Items[J]) <> GulpLib.Items[I].Time then
      begin
        Inc(Count);
        GulpLib.Extract(I);
      end;
  end;
  FreeAndNil(GulpLib);

  write(#13, #13: 80, 'Finished (', Count, ' records extracted)');
  FreeAndNil(FStream);
  writeln;
end;

procedure TGulpApplication.Fix;
var
  Size : int64;
begin
  writeln(#13, #13: 80, 'Fix the content of ' + GetOptionValue('f', 'fix'));
  write  (#13, #13: 80, 'Opening archive... ');
  FStream := TFileStream.Create (GetOptionValue('f', 'fix'), fmOpenReadWrite);
  Size    := FStream.Size;

  write(#13, #13: 80, 'Fixing archive... ');
  FixArchive(FStream);

  write(#13, #13: 80, 'Finished (', Size - FStream.Size, ' removed bytes)');
  FreeAndNil(FStream);
  writeln;
end;

procedure TGulpApplication.Check;
var
        I : longint;
    Count : longint;
  GulpLib : TGulpLib;
begin
  writeln(#13, #13: 80, 'Check the content of ' + GetOptionValue('c', 'check'));
  write  (#13, #13: 80, 'Opening archive... ');
  FStream := TFileStream.Create (GetOptionValue('c', 'check'), fmOpenRead);

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(FStream);
  if GulpLib.OpenArchive(longword(-1)) = FALSE then
  begin
    if FStream.Size <> 0 then
      raise Exception.Create('FIX point error');
  end;

  write(#13, #13: 80, 'Checking records... ');
  Count := 0;
  for I := 0 to GulpLib.Count - 1 do
  begin
    Inc(Count);
    GulpLib.Check(I);
  end;
  GulpLib.CloseArchive;

  write(#13, #13: 80, 'Finished (', Count, ' checked records)');
  FreeAndNil(GulpLib);
  FreeAndNil(FStream);
  writeln;
end;

procedure TGulpApplication.Purge;
var
  TmpName : string;
      Tmp : TStream;
begin
  writeln(#13, #13: 80, 'Purge the content of ' + GetOptionValue('p', 'purge'));
  write  (#13, #13: 80, 'Opening archive... ');
  TmpName := GetTempFileName(GetCurrentDir, '');
  Tmp     := TFileStream.Create(TmpName, fmCreate);
  FStream := TFileStream.Create(GetOptionValue('p', 'purge'), fmOpenRead);

  write(#13, #13: 80, 'Moving records... ');
  PurgeArchive(FStream, Tmp);

  write(#13, #13: 80, 'Finished (', FStream.Size - Tmp.Size, ' bytes removed)');
  FreeAndNil(FStream);
  FreeAndNil(Tmp);

  if DeleteFile(GetOptionValue('p', 'purge')) = FALSE then
    raise Exception.CreateFmt('Error deleting file %s', [GetOptionValue('p', 'purge')])
  else
    if RenameFile(TmpName, GetOptionValue('p', 'purge'))= FALSE then
      raise Exception.CreateFmt('Error renaming tmpfile %s', [TmpName]);
  writeln;
end;

procedure TGulpApplication.List;
var
     I, J : longint;
    Count : longint;
  GulpLib : TGulpLib;
  GulpRec : TGulpRec;
begin
  writeln(#13, #13: 80, 'List the content of ' + GetOptionValue('l', 'list'));
  write  (#13, #13: 80, 'Opening archive... ');
  FStream := TFileStream.Create (GetOptionValue('l', 'list'), fmOpenRead);

  FVersion := longword(-1);
  if GetOptionValue('u', 'until') <> '' then
  begin
    if lowercase(GetOptionValue('u', 'until')) = 'now' then
      FVersion := longword(-2)
    else
      FVersion := StrToInt(GetOptionValue('u', 'until'));
  end;

  if FFileNames.Count = 0 then
    FFileNames.Add('*');

  write(#13, #13: 80, 'Reading records... ');
  GulpLib := TGulpLib.Create(FStream);
  if GulpLib.OpenArchive(FVersion) = FALSE then
  begin
    if FStream.Size <> 0 then
      raise Exception.Create('FIX point error');
  end;

  write (#13, #13: 80, 'Reading records... ');
  Count := 0;
  for I := 0 to GulpLib.Count - 1 do
  begin
    GulpRec := GulpLib.Items[I];
    for J := 0 to FFileNames.Count - 1 do
       if FileNameMatch(GulpRec.Name, FFileNames[J])  then
       begin
         writeln(#13, #13: 80, Format('%3s %3s %7s %9s %19s %12s %s', [
            VerTostring(Gulprec),
           FlagToString(GulpRec),
           AttrToString(GulpRec),
           ModeToString(GulpRec),
           TimeToString(GulpRec),
           SizeToString(GulpRec),
           GulpRec.Name]));
         Inc(Count);
         Break;
       end;
  end;

  writeln(#13, #13: 80, 'Finished (', Count, ' listed records)');
  FreeAndNil(GulpLib);
  FreeAndNil(FStream);
  writeln;
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
  writeln('GULP v0.0.2 journaling archiver, copyright (c) 2014-2015 Melchiorre Caruso.');
  writeln('GULP archiver for incremental backups with rollback capability.');

  DefaultFormatSettings.LongDateFormat  := 'yyyy-mm-dd';
  DefaultFormatSettings.ShortDateFormat := 'yyyy-mm-dd';

  ShortSwitches := 's:r:p:l:c:f:h:u:';
  LongSwitches  :=  TStringList.Create;
  LongSwitches.Add('synch:');
  LongSwitches.Add('restore:');
  LongSwitches.Add('purge:');
  LongSwitches.Add('list:');
  LongSwitches.Add('check:');
  LongSwitches.Add('fix:');
  LongSwitches.Add('help');
  LongSwitches.Add('until:');

  try
    Error := CheckOptions(ShortSwitches, LongSwitches, FSwitches, FFileNames);
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
    Format('%0.2f', [(Now - FStart) * (24 * 60 * 60)]) , ' sec');
  Terminate;
end;

end.
