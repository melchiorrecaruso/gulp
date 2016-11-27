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

uses
  sysutils;

type
  { gulp interface events }

   tgulpshowmessage = procedure(const message: rawbytestring) of object;
   tgulpshowitem    = procedure(p: pointer) of object;
   tgulpterminate   = function: boolean of object;

  { gulp app interface class }

  tgulpinterface = class
  private
    fonshowitem:     tgulpshowitem;
    fonshowmessage1: tgulpshowmessage;
    fonshowmessage2: tgulpshowmessage;
    fonshowwarning:  tgulpshowmessage;
  protected
    procedure showitem    (const item: pointer);
    procedure showmessage1(const message: rawbytestring);
    procedure showmessage2(const message: rawbytestring);
    procedure showwarning (const message: rawbytestring);
  public
    constructor create;
    destructor destroy; override;
    property onshowitem:     tgulpshowitem    read fonshowitem     write fonshowitem;
    property onshowmessage1: tgulpshowmessage read fonshowmessage1 write fonshowmessage1;
    property onshowmessage2: tgulpshowmessage read fonshowmessage2 write fonshowmessage2;
    property onshowwarning:  tgulpshowmessage read fonshowwarning  write fonshowwarning;
  end;

  { common routines }

  function _getfiletimeutc(var sr: tsearchrec): tdatetime; overload;
  function _getfiletimeutc(const filename: rawbytestring): tdatetime; overload;
  function _setfiletimeutc(const filename: rawbytestring; timeutc: tdatetime): longint;

  function _getfileattr(const filename: rawbytestring): longint; overload;
  function _getfileattr(var sr: tsearchrec): longint; overload;
  function _setfileattr(const filename: rawbytestring; attr: longint): longint;

  function _getfilesize(var sr: tsearchrec): int64; overload;
  function _getfilesize(const filename: rawbytestring): int64; overload;

  function _getsymlink(const filename: rawbytestring): rawbytestring;
  function _setsymlink(const filename, linkname: rawbytestring): longint;

  function _getfilemode(const filename: rawbytestring): longword;
  function _setfilemode(const filename: rawbytestring; mode: longword): longint;

  function _getfileuserid(const filename: rawbytestring): longword;
  function _getfilegroupid(const filename: rawbytestring): longword;

  function _setfileuserid(const filename: rawbytestring; userid: longword): longint;
  function _setfilegroupid(const filename: rawbytestring; groupid: longword): longint;

  function _getfileusername (const filename: rawbytestring): rawbytestring;
  function _getfilegroupname(const filename: rawbytestring): rawbytestring;

  function isabsolutepath(const pathname: rawbytestring): boolean;

  function hex(const data; count: longint): rawbytestring;
  function hextodata(const s: rawbytestring; var data; count: longint): boolean;

  function setprioritynormal: boolean;
  function setpriorityidle: boolean;

  function max(a, b: longint): longint; inline; overload;
  function min(a, b: longint): longint; inline; overload;
  function max(const a, b: int64): int64; inline; overload;
  function min(const a, b: int64): int64; inline; overload;

implementation

uses
  {$IFDEF LINUX}
  baseunix,
  syscall,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  gulpfixes;

const
  hexadecimals: array [0..15] of char = '0123456789ABCDEF';
  hexvalues: array ['0'..'F'] of byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    0, 0, 0, 0, 0, 0, 0, 10, 11, 12, 13, 14, 15);

{ interface class }

constructor tgulpinterface.create;
begin
  inherited create;
  fonshowitem     := nil;
  fonshowmessage1 := nil;
  fonshowmessage2 := nil;
  fonshowwarning  := nil;
end;

destructor tgulpinterface.destroy;
begin
  inherited destroy;
end;

procedure tgulpinterface.showitem(const item: pointer);
begin
  if assigned(fonshowitem) then
    fonshowitem(item);
end;

procedure tgulpinterface.showmessage1(const message: rawbytestring);
begin
  if assigned(fonshowmessage1) then
    fonshowmessage1(message);
end;

procedure tgulpinterface.showmessage2(const message: rawbytestring);
begin
  if assigned(fonshowmessage2) then
    fonshowmessage2(message);
end;

procedure tgulpinterface.showwarning(const message: rawbytestring);
begin
  if assigned(fonshowwarning) then
    fonshowwarning(message);
end;

{ common routines }

function _getfiletimeutc(var sr: tsearchrec): tdatetime;
begin
  result := localtime2universal(filedatetodatetime(sr.time));
end;

function _getfiletimeutc(const filename: rawbytestring): tdatetime;
var
  sr: tsearchrec;
begin
  result := 0.0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile,sr) = 0 then
      result := _getfiletimeutc(sr);
  sysutils.findclose(sr);
end;

function _setfiletimeutc(const filename: rawbytestring; timeutc: tdatetime): longint;
begin
  {$IFDEF LINUX}
  if _getfileattr(filename) and fasymlink = 0 then
    result := filesetdate(filename, datetimetofiledate(universaltime2local(timeutc)))
  else
    result := 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := filesetdate(filename, datetimetofiledate(universaltime2local(timeutc)))
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _getfileattr(var sr: tsearchrec): longint;
begin
  result := sr.attr;
end;

function _getfileattr(const filename: rawbytestring): longint;
var
  sr: tsearchrec;
begin
  result := -1;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      result := _getfileattr(sr);
  sysutils.findclose(sr);
end;

function _setfileattr(const filename: rawbytestring; attr: longint): longint;
begin
  {$IFDEF LINUX}
  result := -1;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := filesetattr(filename, attr);
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _getfilesize(var sr: tsearchrec): int64;
begin
  if sr.attr and (fadirectory or favolumeid or fasymlink) = 0 then
    result := sr.size
  else
    result := 0;
end;

function _getfilesize(const filename: rawbytestring): int64;
var
  sr: tsearchrec;
begin
  result := 0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      result := _getfilesize(sr);
  sysutils.findclose(sr);
end;

function _getsymlink(const filename: rawbytestring): rawbytestring;
begin
  {$IFDEF LINUX}
  result := fpreadlink(filename);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := '';
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _setsymlink(const filename, linkname: rawbytestring): longint;
begin
  {$IFDEF LINUX}
  result := fpsymlink(pchar(linkname), pchar(filename));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := 0;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _getfilemode(const filename: rawbytestring): longword;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if _getfileattr(filename) and fasymlink = fasymlink then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_mode;
  end else
    if fpstat(filename, info) = 0 then
      result := info.st_mode;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := 0;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _setfilemode(const filename: rawbytestring; mode: longword): longint;
begin
  {$IFDEF LINUX}
  if _getfileattr(filename) and fasymlink = 0 then
    result := fpchmod(filename, mode)
  else
    result := 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := 0;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _getfileuserid(const filename: rawbytestring): longword;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if _getfileattr(filename) and fasymlink = fasymlink then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_uid;
  end else
    if fpstat(filename, info) = 0 then
      result := info.st_uid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := -1;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _getfilegroupid(const filename: rawbytestring): longword;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if _getfileattr(filename) and fasymlink = fasymlink then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_gid;
  end else
    if fpstat(filename, info) = 0 then
      result := info.st_gid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := 0;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _setfileuserid(const filename: rawbytestring; userid: longword): longint;
{$IFDEF LINUX}
var
  systempath: rawbytestring;
{$ENDIF}
begin
  {$IFDEF LINUX}
  systempath := tosinglebytefilesystemencodedfilename(filename);
  if _getfileattr(filename) and fasymlink = 0 then
    result := do_syscall(syscall_nr_chown,
      tsysparam(pchar(systempath)), tsysparam(userid), tsysparam(_getfilegroupid(filename)))
  else
    result := do_syscall(syscall_nr_lchown,
      tsysparam(pchar(systempath)), tsysparam(userid), tsysparam(_getfilegroupid(filename)));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := 0;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _setfilegroupid(const filename: rawbytestring; groupid: longword): longint;
{$IFDEF LINUX}
var
  systempath: rawbytestring;
{$ENDIF}
begin
  {$IFDEF LINUX}
  systempath := tosinglebytefilesystemencodedfilename(filename);
  if _getfileattr(filename) and fasymlink = 0 then
    result := do_syscall(syscall_nr_chown,
      tsysparam(pchar(systempath)), tsysparam(_getfileuserid(filename)), tsysparam(groupid))
  else
    result := do_syscall(syscall_nr_lchown,
      tsysparam(pchar(systempath)), tsysparam(_getfileuserid(filename)), tsysparam(groupid));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := 0;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function _getfileusername(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

function _getfilegroupname(const filename: rawbytestring): rawbytestring;
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

function hex(const data; count: longint): rawbytestring;
var
  i, j: longint;
  k:    longword;
begin
  setlength(result, count shl 1);
  j := 1;
  for i := 0 to count - 1 do
  begin
    k := tbytearray(data)[i];
    result[j] := hexadecimals[k shr 4];
    inc(j);
    result[j] := hexadecimals[k and $f];
    inc(j);
  end;
end;

function hextodata(const s: rawbytestring; var data; count: longint): boolean;
var
  i: longint;
begin
  result := false;
  if length(s) < count * 2 then exit;

  for i := 0 to count - 1 do
  begin
    if (s[i * 2 + 1] in ['0'..'9', 'A'..'F']) and (s[i * 2 + 2] in ['0'..'9', 'A'..'F']) then
    begin
      tbytearray(data)[i] := hexvalues[s[i * 2 + 1]] shl 4 + hexvalues[s[i * 2 + 2]]
    end else
      exit;
  end;
  result := true;
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

function max(a, b: longint): longint; inline;
begin
  if a > b then
    result := a
  else
    result := b;
end;

function min(a, b: longint): longint; inline;
begin
  if a < b then
    result := a
  else
    result := b;
end;

function max(const a, b: int64): int64; inline;
begin
  if a > b then
    result := a
  else
    result := b;
end;

function min(const a, b: int64): int64; inline;
begin
  if a < b then
    result := a
  else
    result := b;
end;

end.
