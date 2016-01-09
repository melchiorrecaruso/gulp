{
  Copyright (c) 2016 Melchiorre Caruso.

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

    Various helper routines.

  Modified:

    v0.0.3 - 2016.01.09 by Melchiorre Caruso.
}

unit GulpCommon;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

  { File info routines }

  function FileGetTimeUTC  (var SR: TSearchRec): TDateTime; overload;
  function FileGetSize     (var SR: TSearchRec): int64;     overload;
  function FileGetAttr     (var SR: TSearchRec): longint;   overload;

  function FileGetTimeUTC  (const FileName: rawbytestring): TDateTime; overload;
  function FileGetSize     (const FileName: rawbytestring): int64;     overload;
  function FileGetAttr     (const FileName: rawbytestring): longint;   overload;

  function FileGetLinkName (const FileName: rawbytestring): rawbytestring;
  function FileGetMode     (const FileName: rawbytestring): longint;
  function FileGetUserID   (const FileName: rawbytestring): longword;
  function FileGetUserName (const FileName: rawbytestring): rawbytestring;
  function FileGetGroupID  (const FileName: rawbytestring): longword;
  function FileGetGroupName(const FileName: rawbytestring): rawbytestring;

  { Priority routines }

  function SetPriorityNormal: boolean;
  function SetPriorityIdle: boolean;

implementation

uses
  {$IFDEF UNIX} GulpFixes, BaseUnix; {$ENDIF}
  {$IFDEF MSWINDOWS} GulpFixes, Windows; {$ENDIF}

{ File info routine }

function FileGetTimeUTC(var SR: TSearchRec): TDateTime;
begin
  Result := GulpFixes.LocalTime2Universal(FileDateToDateTime(SR.Time));
end;

function FileGetTimeUTC(const FileName: rawbytestring): TDateTime;
var
  SR : TSearchRec;
begin
  Result := 0.0;
  if SysUtils. FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := FileGetTimeUTC(SR);
  end;
  SysUtils.FindClose(SR);
end;

function FileGetSize(var SR: TSearchRec): int64;
begin
  Result := 0;
  if SR.Attr and (faDirectory or faVolumeId or faSymLink) = 0 then
  begin
    Result := SR.Size;
  end;
end;

function FileGetSize(const FileName: rawbytestring): int64;
var
  SR : TSearchRec;
begin
  Result := 0;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := FileGetSize(SR);
  end;
  SysUtils.FindClose(SR);
end;

function FileGetAttr(var SR: TSearchRec): longint;
begin
  Result := SR.Attr;
end;

function FileGetAttr(const FileName: rawbytestring): longint;
var
  SR : TSearchRec;
begin
  Result := 0;
  if SysUtils.FindFirst(FileName,
    faReadOnly  or faHidden  or faSysFile or faVolumeId or
    faDirectory or faArchive or faSymLink or faAnyFile, SR) = 0 then
  begin
    Result := GulpCommon.FileGetAttr(SR);
  end;
  SysUtils.FindClose(SR);
end;

function FileGetLinkName(const FileName: rawbytestring): rawbytestring;
begin
  Result := '';
  {$IFDEF UNIX}
  if FileGetAttr(FileName) and faSymLink = faSymLink then
    Result := fpReadLink(FileName);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function FileGetMode(const FileName: rawbytestring): longint;
{$IFDEF UNIX}
var
  Info : stat;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF UNIX}
  if fpLstat(FileName, Info) = 0 then
    Result := Info.st_mode
  else
    if fpstat(FileName, Info) = 0 then
      Result := Info.st_mode;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function FileGetUserID(const FileName: rawbytestring): longword;
{$IFDEF UNIX}
var
  Info : stat;
{$ENDIF}
begin
  Result := $FFFFFFFF;
  {$IFDEF UNIX}
  if fpLstat(FileName, Info) = 0 then
    Result := Info.st_uid
  else
    if fpstat(FileName, Info) = 0 then
      Result := Info.st_uid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function FileGetUserName(const FileName: rawbytestring): rawbytestring;
begin
  Result := '';
end;

function FileGetGroupID(const FileName: rawbytestring): longword;
{$IFDEF UNIX}
var
  Info : stat;
{$ENDIF}
begin
  Result := $FFFFFFFF;
  {$IFDEF UNIX}
  if fpLstat(FileName, Info) = 0 then
    Result := Info.st_gid
  else
    if fpstat(FileName, Info) = 0 then
      Result := Info.st_gid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function FileGetGroupName(const FileName: rawbytestring): rawbytestring;
begin
  Result := '';
end;

{ Priority routines }

function SetPriorityIdle: boolean;
begin
  {$IFDEF UNIX}
    Result := FpNice(5) = 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
    Result := SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

function SetPriorityNormal: boolean;
begin
  {$IFDEF UNIX}
    Result := FpNice(10) = 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
    Result := SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

end.

