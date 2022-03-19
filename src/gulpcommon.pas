{ Description: Common routines unit.

  Copyright (C) 2014-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  sha1,
  sysutils;

type
  { gulp flags }

  tgulpflag  = (gfadd, gfdelete, gfclose);

  tgulpflags = set of tgulpflag;

  { gulp mode }

  tgulppermission  = (gpreadbyowner, gpwritebyowner, gpexecutebyowner,
                      gpreadbygroup, gpwritebygroup, gpexecutebygroup,
                      gpreadbyother, gpwritebyother, gpexecutebyother);

  tgulppermissions = set of tgulppermission;

  { gulp attr }

  tgulpattribute   = (gareadonly,  gahidden,  gasysfile, gavolumeid,
                      gadirectory, gaarchive, gasymlink, galink);

  tgulpattributes  = set of tgulpattribute;

  { gulp item }

  tgulpitem = packed record
    name:      rawbytestring;
    flags:     tgulpflags;
    mtime:     tdatetime;
    stime:     tdatetime;
    attr:      tgulpattributes;
    mode:      tgulppermissions;
    size:      int64;
    linkname:  rawbytestring;
    userid:    longint;
    groupid:   longint;
    username:  rawbytestring;
    groupname: rawbytestring;
    comment:   rawbytestring;
    offset1:   int64;
    offset2:   int64;
    checksum1: tsha1digest;
    checksum2: tsha1digest;
    version:   longint;
  end;

  pgulpitem  = ^tgulpitem;

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

  function version2str(const version: longint): rawbytestring;
  function flags2str  (const flags: tgulpflags): rawbytestring;

  { common attributes routines }

  function l2sattr (attr: tgulpattributes): longint;
  function s2lattr (attr: longint): tgulpattributes;
  function getattr (var sr: tsearchrec): longint;
  function getattr (const filename: rawbytestring): longint;
  function setattr (const filename: rawbytestring; attr: longint): longint;
  function attr2str(attr: longint): rawbytestring;
  function str2attr(attr: rawbytestring): longint;

  { common mode routines }

  function l2smode (mode: tgulppermissions): longint;
  function s2lmode (mode: longint): tgulppermissions;
  function getmode (const filename: rawbytestring): longint;
  function setmode (const filename: rawbytestring; mode: longint): longint;
  function mode2str(const mode: longint): rawbytestring;
  function str2mode(const mode: rawbytestring): longint;

  { common size routines }

  function getsize (var sr: tsearchrec): int64;
  function getsize (const filename: rawbytestring): int64;
  function size2str(const size: int64): rawbytestring;

   { common time routines }

  function gettimeutc(var sr: tsearchrec): tdatetime;
  function gettimeutc(const filename: rawbytestring): tdatetime;
  function settimeutc(const filename: rawbytestring; timeutc: tdatetime): longint;
  function time2str  (const t: tdatetime): rawbytestring;

  { common time routines }

  function getsymlink(const filename: rawbytestring): rawbytestring;
  function setsymlink(const filename, linkname: rawbytestring): longint;

  { common user/group routines }

  function getuserid   (const filename: rawbytestring): longint;
  function setuserid   (const filename: rawbytestring; userid: longint): longint;

  function getgroupid  (const filename: rawbytestring): longint;
  function setgroupid  (const filename: rawbytestring; groupid: longint): longint;

  function getusername (const filename: rawbytestring): rawbytestring;
  function getgroupname(const filename: rawbytestring): rawbytestring;

  { common files routines }

  function issymlink  (attr: longint): boolean; overload;
  function issymlink  (attr: tgulpattributes): boolean; overload;
  function issymlink  (const filename: rawbytestring): boolean; overload;

  function isdirectory(attr: longint): boolean; overload;
  function isdirectory(attr: tgulpattributes): boolean; overload;
  function isdirectory(const filename: rawbytestring): boolean; overload;

  function isregular  (attr: longint): boolean; overload;
  function isregular  (attr: tgulpattributes): boolean; overload;
  function isregular  (const filename: rawbytestring): boolean; overload;

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
  gulpfixes,
  gulpmessages;

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

function version2str(const version: longint): rawbytestring;
begin
  result := inttostr(version);
end;

function flags2str(const flags: tgulpflags): rawbytestring;
begin
  if (gfadd in flags) then
    result := 'ADD'
  else
  if (gfdelete in flags) then
    result := 'DEL'
  else
    result := '???';
end;

{ common attributes routines }

function l2sattr(attr: tgulpattributes): longint;
begin
  result := 0;
  if gareadonly  in attr then result := result or fareadonly;
  if gahidden    in attr then result := result or fahidden;
  if gasysfile   in attr then result := result or fasysfile;
  if gavolumeid  in attr then result := result or favolumeid;
  if gadirectory in attr then result := result or fadirectory;
  if gaarchive   in attr then result := result or faarchive;
  if gasymlink   in attr then result := result or fasymlink;
end;

function s2lattr(attr: longint): tgulpattributes;
begin
  result :=[];
  if fareadonly  and attr <> 0 then include(result, gareadonly);
  if fahidden    and attr <> 0 then include(result, gahidden);
  if fasysfile   and attr <> 0 then include(result, gasysfile);
  if favolumeid  and attr <> 0 then include(result, gavolumeid);
  if fadirectory and attr <> 0 then include(result, gadirectory);
  if faarchive   and attr <> 0 then include(result, gaarchive);
  if fasymlink   and attr <> 0 then include(result, gasymlink);
end;

function getattr(var sr: tsearchrec): longint;
begin
  result := sr.attr;
end;

function getattr(const filename: rawbytestring): longint;
var
  sr: tsearchrec;
begin
  if sysutils.findfirst(filename,
    fareadonly  or fahidden  or fasysfile or favolumeid or
    fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
  begin
    result := sr.attr;
  end else
    result := 0;
  sysutils.findclose(sr);
end;

function setattr(const filename: rawbytestring; attr: longint): longint;
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

function attr2str(attr: longint): rawbytestring;
begin
  result := '-------';
  if fareadonly  and attr <> 0 then result[1] := 'R';
  if fahidden    and attr <> 0 then result[2] := 'H';
  if fasysfile   and attr <> 0 then result[3] := 'S';
  if favolumeid  and attr <> 0 then result[4] := 'V';
  if fadirectory and attr <> 0 then result[5] := 'D';
  if faarchive   and attr <> 0 then result[6] := 'A';
  if fasymlink   and attr <> 0 then result[7] := 'L';
end;

function str2attr(attr: rawbytestring): longint;
const
  a: array[0..6] of char = ('R','H','S','V','D','A','L');
var
  i: longword;
begin
  result := 0;
  for i  := 0 to 6 do
    if pos(a[i], uppercase(attr)) > 0 then
    begin
      if a[i] = 'R' then result := result or fareadonly;
      if a[i] = 'H' then result := result or fahidden;
      if a[i] = 'S' then result := result or fasysfile;
      if a[i] = 'V' then result := result or favolumeid;
      if a[i] = 'D' then result := result or fadirectory;
      if a[i] = 'A' then result := result or faarchive;
      if a[i] = 'L' then result := result or fasymlink;
      delete(attr, pos(a[i], uppercase(attr)),  1);
    end;

  if attr <> '' then raiseexception(gereadstream, '006001');
end;

{ common modes routines }

function l2smode(mode: tgulppermissions): longint;
begin
  result := 0;
  {$IFDEF LINUX}
  if gpreadbyowner    in mode then result := result or $0100;
  if gpwritebyowner   in mode then result := result or $0080;
  if gpexecutebyowner in mode then result := result or $0040;
  if gpreadbygroup    in mode then result := result or $0020;
  if gpwritebygroup   in mode then result := result or $0010;
  if gpexecutebygroup in mode then result := result or $0008;
  if gpreadbyother    in mode then result := result or $0004;
  if gpwritebyother   in mode then result := result or $0002;
  if gpexecutebyother in mode then result := result or $0001;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function s2lmode(mode: longint): tgulppermissions;
begin
  result := [];
  {$IFDEF LINUX}
  if mode and $0100 <> 0 then include(result, gpreadbyowner);
  if mode and $0080 <> 0 then include(result, gpwritebyowner);
  if mode and $0040 <> 0 then include(result, gpexecutebyowner);
  if mode and $0020 <> 0 then include(result, gpreadbygroup);
  if mode and $0010 <> 0 then include(result, gpwritebygroup);
  if mode and $0008 <> 0 then include(result, gpexecutebygroup);
  if mode and $0004 <> 0 then include(result, gpreadbyother);
  if mode and $0002 <> 0 then include(result, gpwritebyother);
  if mode and $0001 <> 0 then include(result, gpexecutebyother);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function getmode(const filename: rawbytestring): longint;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  result := 0;
  {$IFDEF LINUX}
  if issymlink(filename) then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_mode;
  end else
    if fpstat(filename, info) = 0 then
      result := info.st_mode;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function setmode(const filename: rawbytestring; mode: longint): longint;
begin
  result := -1;
  {$IFDEF LINUX}
  if issymlink(filename) = false then
    result := fpchmod(filename, mode);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function mode2str(const mode: longint): rawbytestring;
begin
  result := '---';
  {$IFDEF UNIX}
  if mode <> 0 then
    result := octstr(mode, 3);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  {$ENDIF}
  {$ENDIF}
end;

function str2mode(const mode: rawbytestring): longint;
{$IFDEF LINUX}
var
  i: longint;
{$ENDIF}
begin
  result := 0;
  {$IFDEF LINUX}
  for i := 1 to length(mode) do
    result := result * 8 + strtoint(copy(mode, i, 1));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

{ common size routines }

function getsize(var sr: tsearchrec): int64;
begin
  if isregular(sr.attr) then
    result := sr.size
  else
    result := 0;
end;

function getsize(const filename: rawbytestring): int64;
var
  sr: tsearchrec;
begin
  if sysutils.findfirst(filename,
    fareadonly  or fahidden  or fasysfile or favolumeid or
    fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
  begin
    result := getsize(sr);
  end else
    result := 0;
  sysutils.findclose(sr);
end;

function size2str(const size: int64): rawbytestring;
begin
  result := format('%u', [size]);
end;

{ common time routines }

function gettimeutc(var sr: tsearchrec): tdatetime;
begin
  result := localtime2universal(filedatetodatetime(sr.time));
end;

function gettimeutc(const filename: rawbytestring): tdatetime;
var
  sr: tsearchrec;
begin
  if sysutils.findfirst(filename,
    fareadonly  or fahidden  or fasysfile or favolumeid or
    fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
  begin
    result := gettimeutc(sr);
  end else
    result := 0;
  sysutils.findclose(sr);
end;

function settimeutc(const filename: rawbytestring; timeutc: tdatetime): longint;
begin
  {$IFDEF LINUX}
  if issymlink(filename) = false then
    result := filesetdate(filename, datetimetofiledate(universaltime2local(timeutc)))
  else
    result := -1;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := filesetdate(filename, datetimetofiledate(universaltime2local(timeutc)));
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function time2str(const t: tdatetime): rawbytestring;
begin
  result := formatdatetime(
    defaultformatsettings.longdateformat + ' ' +
    defaultformatsettings.longtimeformat, t);
end;

{ common link routines }

function getsymlink(const filename: rawbytestring): rawbytestring;
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

function setsymlink(const filename, linkname: rawbytestring): longint;
begin
  {$IFDEF LINUX}
  result := fpsymlink(pchar(linkname), pchar(filename));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := -1;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

{ common user/group routines }

function getuserid(const filename: rawbytestring): longint;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  result := -1;
  {$IFDEF LINUX}
  if issymlink(filename) then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_uid;
  end else
    if fpstat(filename, info) = 0 then
      result := info.st_uid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function getgroupid(const filename: rawbytestring): longint;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  result := -1;
  {$IFDEF LINUX}
  if issymlink(filename) then
  begin
    if fplstat(filename, info) = 0 then
      result := info.st_gid;
  end else
    if fpstat(filename, info) = 0 then
      result := info.st_gid;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function setuserid(const filename: rawbytestring; userid: longint): longint;
{$IFDEF LINUX}
var
  systempath: rawbytestring;
{$ENDIF}
begin
  result := -1;
  {$IFDEF LINUX}
  systempath := tosinglebytefilesystemencodedfilename(filename);
  if issymlink(filename) = false then
    result := do_syscall(syscall_nr_chown,
      tsysparam(pchar(systempath)), tsysparam(userid), tsysparam(getgroupid(filename)))
  else
    result := do_syscall(syscall_nr_lchown,
      tsysparam(pchar(systempath)), tsysparam(userid), tsysparam(getgroupid(filename)));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function setgroupid(const filename: rawbytestring; groupid: longint): longint;
{$IFDEF LINUX}
var
  systempath: rawbytestring;
{$ENDIF}
begin
  result := -1;
  {$IFDEF LINUX}
  systempath := tosinglebytefilesystemencodedfilename(filename);
  if issymlink(filename) = false then
    result := do_syscall(syscall_nr_chown,
      tsysparam(pchar(systempath)), tsysparam(getuserid(filename)), tsysparam(groupid))
  else
    result := do_syscall(syscall_nr_lchown,
      tsysparam(pchar(systempath)), tsysparam(getuserid(filename)), tsysparam(groupid));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function getusername(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

function getgroupname(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

{ common file routines }

function issymlink(attr: longint): boolean;
begin
  result := fasymlink and attr > 0;
end;

function issymlink(attr: tgulpattributes): boolean;
begin
  result := gasymlink in attr;
end;

function issymlink(const filename: rawbytestring): boolean;
begin
  result := issymlink(getattr(filename));
end;

function isdirectory(attr: longint): boolean;
begin
  result := issymlink(attr) = false;
  if result then
  begin
    result := fadirectory and attr > 0;
  end;
end;

function isdirectory(attr: tgulpattributes): boolean;
begin
  result := issymlink(attr) = false;
  if result then
  begin
    result := gadirectory in attr;
  end;
end;

function isdirectory(const filename: rawbytestring): boolean;
begin
  result := isdirectory(getattr(filename));
end;

function isregular(attr: longint): boolean;
begin
  result := issymlink(attr) = false;
  if result then
  begin
    result := isdirectory(attr) = false;
  end;
end;

function isregular(attr: tgulpattributes): boolean;
begin
  result := issymlink(attr) = false;
  if result then
  begin
    result := isdirectory(attr) = false;
  end;
end;

function isregular(const filename: rawbytestring): boolean;
begin
  result := isregular(getattr(filename));
end;

function isabsolutepath(const pathname: rawbytestring): boolean;
begin
  {$IFDEF LINUX}
  result := ((length(pathname) > 0) and (pathname[1] in ['/','\']));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := ((length(pathname) > 0) and (pathname[1] in ['/','\'])) or
            ((length(pathname) > 1) and (pathname[2] = ':'));
  {$ELSE}
  {$ENDIF}
  {$ENDIF}
end;

{ common hex routines }

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

{ common process priority routines }

function setpriorityidle: boolean;
begin
  {$IFDEF LINUX}
  result := fpnice(5) = 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := setpriorityclass(getcurrentprocess, idle_priority_class);
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function setprioritynormal: boolean;
begin
  {$IFDEF LINUX}
  result := fpnice(10) = 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := setpriorityclass(getcurrentprocess, normal_priority_class);
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

{ common min7max routines }

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
