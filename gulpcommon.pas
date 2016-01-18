{ Description: Common routines unit.

  Copyright (C) 2014-2016 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit gulpcommon;

{$mode objfpc}
{$H+}

interface

uses sysutils;

function filegettimeutc(var sr: tsearchrec): tdatetime; overload;
function filegetsize(var sr: tsearchrec): int64; overload;
function filegetattr(var sr: tsearchrec): longint; overload;
function filegettimeutc(const filename: rawbytestring): tdatetime; overload;
function filegetsize(const filename: rawbytestring): int64; overload;
function filegetattr(const filename: rawbytestring): longint; overload;
function filegetlinkname(const filename: rawbytestring): rawbytestring;
function filegetmode(const filename: rawbytestring): longint;
function filegetuserid(const filename: rawbytestring): longword;
function filegetusername(const filename: rawbytestring): rawbytestring;
function filegetgroupid(const filename: rawbytestring): longword;
function filegetgroupname(const filename: rawbytestring): rawbytestring;
function isabsolutepath(const pathname: rawbytestring): boolean;
function setprioritynormal: boolean;
function setpriorityidle: boolean;

implementation

uses
{$IFDEF UNIX}
  baseunix,
{$ENDIF}
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  gulpfixes;

function filegettimeutc(var sr: tsearchrec): tdatetime;
begin
  result := localtime2universal(filedatetodatetime(sr.time));
end;

function filegettimeutc(const filename: rawbytestring): tdatetime;
var
  sr: tsearchrec;
begin
  result := 0.0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile,sr) = 0 then
      result := filegettimeutc(sr);
  sysutils.findclose(sr);
end;

function filegetsize(var sr: tsearchrec): int64;
begin
  result := 0;
  if sr.attr and (fadirectory or favolumeid or fasymlink) = 0 then
    result := sr.size;
end;

function filegetsize(const filename: rawbytestring): int64;
var
  sr: tsearchrec;
begin
  result := 0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      result := filegetsize(sr);
  sysutils.findclose(sr);
end;

function filegetattr(var sr: tsearchrec): longint;
begin
  result := sr.attr;
end;

function filegetattr(const filename: rawbytestring): longint;
var
  sr: tsearchrec;
begin
  result := 0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      result := gulpcommon.filegetattr(sr);
  sysutils.findclose(sr);
end;

function filegetlinkname(const filename: rawbytestring): rawbytestring;
begin
  result := '';
{$IFDEF UNIX}
  if filegetattr(filename) and fasymlink = fasymlink then
    result := fpreadlink(filename);
{$ELSE}
{$IFDEF MSWINDOWS}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function filegetmode(const filename: rawbytestring): longint;
{$IFDEF UNIX}
var
  info: stat;
{$ENDIF}
begin
  result := 0;
{$IFDEF UNIX}
  if fplstat(filename, info) = 0 then
    result := info.st_mode
  else
  if fpstat(filename, info) = 0 then
    result := info.st_mode;
{$ELSE}
{$IFDEF MSWINDOWS}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function filegetuserid(const filename: rawbytestring): longword;
{$IFDEF UNIX}
var
  info: stat;
{$ENDIF}
begin
  result := $FFFFFFFF;
{$IFDEF UNIX}
  if fplstat(filename, info) = 0 then
    result := info.st_uid
  else
  if fpstat(filename, info) = 0 then
    result := info.st_uid;
{$ELSE}
{$IFDEF MSWINDOWS}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function filegetusername(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

function filegetgroupid(const filename: rawbytestring): longword;
{$IFDEF UNIX}
var
  info: stat;
{$ENDIF}
begin
  result := $FFFFFFFF;
{$IFDEF UNIX}
  if fplstat(filename, info) = 0 then
    result := info.st_gid
  else
  if fpstat(filename, info) = 0 then
    result := info.st_gid;
{$ELSE}
{$IFDEF MSWINDOWS}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function filegetgroupname(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

function isabsolutepath(const pathname: rawbytestring): boolean;
begin
{$IFDEF UNIX}
  result := ((length(pathname) > 0) and (pathname[1] in ['/','\']));
{$ELSE}
{$IFDEF MSWINDOWS}
  result := ((length(pathname) > 0) and (pathname[1] in ['/','\'])) or
            ((length(pathname) > 1) and (pathname[2] = ':'));
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function setpriorityidle: boolean;
begin
{$IFDEF UNIX}
  result := fpnice(5) = 0;
{$ELSE}
{$IFDEF MSWINDOWS}
  result := setpriorityclass(getcurrentprocess, idle_priority_class);
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function setprioritynormal: boolean;
begin
{$IFDEF UNIX}
  result := fpnice(10) = 0;
{$ELSE}
{$IFDEF MSWINDOWS}
  result := setpriorityclass(getcurrentprocess, normal_priority_class);
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

end.
