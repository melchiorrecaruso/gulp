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

  function filegettimeutc(var sr: tsearchrec): tdatetime; overload;
  function filegetsize   (var sr: tsearchrec): int64; overload;
  function filegetattr   (var sr: tsearchrec): longint; overload;
  function filegettimeutc  (const filename: rawbytestring): tdatetime; overload;
  function filegetsize     (const filename: rawbytestring): int64; overload;
  function filegetattr     (const filename: rawbytestring): longint; overload;
  function filegetlinkname (const filename: rawbytestring): rawbytestring;
  function filegetmode     (const filename: rawbytestring): longint;
  function filegetuserid   (const filename: rawbytestring): longword;
  function filegetusername (const filename: rawbytestring): rawbytestring;
  function filegetgroupid  (const filename: rawbytestring): longword;
  function filegetgroupname(const filename: rawbytestring): rawbytestring;

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
{$IFDEF UNIX}
  baseunix,
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
  if (filegetattr(filename) and fasymlink) = fasymlink then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_mode;
  end else
  begin
    if fpstat(filename, info) = 0 then
      result := info.st_mode;
  end;
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
  result := $ffffffff;
{$IFDEF UNIX}
  if (filegetattr(filename) and fasymlink) = fasymlink then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_uid;
  end else
  begin
    if fpstat(filename, info) = 0 then
      result := info.st_uid;
  end;
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
  result := $ffffffff;
{$IFDEF UNIX}
  if (filegetattr(filename) and fasymlink) = fasymlink then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_gid;
  end else
  begin
    if fpstat(filename, info) = 0 then
      result := info.st_gid;
  end;
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
