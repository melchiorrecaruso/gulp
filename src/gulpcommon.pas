{ Description: Common routines unit.

  Copyright (C) 2014-2017 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

  { gulp permissions }

  tgulppermission  = (gpreadbyowner, gpwritebyowner, gpexecutebyowner,
                      gpreadbygroup, gpwritebygroup, gpexecutebygroup,
                      gpreadbyother, gpwritebyother, gpexecutebyother);

  tgulppermissions = set of tgulppermission;

  { gulp attributes }

  tgulpattribute   = (gareadonly,  gahidden,  gasysfile, gavolumeid,
                      gadirectory, gaarchive, gasymlink, galink);

  tgulpattributes  = set of tgulpattribute;

  { gulp item }

  tgulpitem = record
    name:       rawbytestring;
    flags:      tgulpflags;
    mtime:      tdatetime;
    stime:      tdatetime;
    attr:       tgulpattributes;
    mode:       tgulppermissions;
    size:       int64;
    linkname:   rawbytestring;
    userid:     longint;
    groupid:    longint;
    username:   rawbytestring;
    groupname:  rawbytestring;
    comment:    rawbytestring;
    offset1:    int64;
    offset2:    int64;
    checksum1:  tsha1digest;
    checksum2:  tsha1digest;
    version:    longint;
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

  function getversion(const version: longint): rawbytestring;

  function gettime(const t: tdatetime): rawbytestring;
  function getflags(const flags: tgulpflags): rawbytestring;

  { common attributes routines }

  function getattributes1(attr: tgulpattributes): longint;
  function getattributes1(attr: longint): tgulpattributes;
  function getattributes2(var sr: tsearchrec): longint;
  function getattributes2(var sr: tsearchrec): tgulpattributes;
  function getattributes3(const filename: rawbytestring): longint;
  function getattributes3(const filename: rawbytestring): tgulpattributes;
  function getattributes4(attr: rawbytestring): longint;
  function getattributes4(attr: longint): rawbytestring;

  function setattributes(const filename: rawbytestring; attr: tgulpattributes): tgulpattributes;
  function setattributes(const filename: rawbytestring; attr: longint): longint;

  { common mode routines }

  function getmode1(mode: tgulppermissions): longint;
  function getmode1(mode: longint): tgulppermissions;
  function getmode3(const filename: rawbytestring): longint;
  function getmode3(const filename: rawbytestring): tgulppermissions;
  function getmode4(const mode: rawbytestring): longint;
  function getmode4(const mode: tgulppermissions): rawbytestring;

  function setmode(const filename: rawbytestring; mode: longint): longint;
  function setmode(const filename: rawbytestring; mode: tgulppermissions): tgulppermissions;

  { common size routines }

  function getsize2(var sr: tsearchrec): int64;
  function getsize3(const filename: rawbytestring): int64;
  function getsize4(const size: int64): rawbytestring;




  function  osattrtoattributes(attr: longint): tgulpattributes;
  function  osattrtostring    (attr: longint): rawbytestring;

  function  permissionstoosmode(p: tgulppermissions): longint;
  function  osmodetopermissions(p: longint): tgulppermissions;



  function _getfiletimeutc(var sr: tsearchrec): tdatetime; overload;
  function _getfiletimeutc(const filename: rawbytestring): tdatetime; overload;
  function _setfiletimeutc(const filename: rawbytestring; timeutc: tdatetime): longint;





  function _getsymlink(const filename: rawbytestring): rawbytestring;
  function _setsymlink(const filename, linkname: rawbytestring): longint;

  function _getfilemode(const filename: rawbytestring): tgulppermissions;
  function _getfilemode(const filename: rawbytestring): longint;
  function _setfilemode(const filename: rawbytestring; mode: longint): longint;

  function _getfileuserid(const filename: rawbytestring): longint;
  function _getfilegroupid(const filename: rawbytestring): longint;

  function _setfileuserid(const filename: rawbytestring; userid: longint): longint;
  function _setfilegroupid(const filename: rawbytestring; groupid: longint): longint;

  function _getfileusername (const filename: rawbytestring): rawbytestring;
  function _getfilegroupname(const filename: rawbytestring): rawbytestring;

  function fileissymlink  (attr: longint): boolean; overload;
  function fileisdirectory(attr: longint): boolean; overload;
  function fileisregular  (attr: longint): boolean; overload;

  function fileissymlink  (attr: tgulpattributes): boolean; overload;
  function fileisdirectory(attr: tgulpattributes): boolean; overload;
  function fileisregular  (attr: tgulpattributes): boolean; overload;

  function fileissymlink  (const filename: rawbytestring): boolean; overload;
  function fileisdirectory(const filename: rawbytestring): boolean; overload;
  function fileisregular  (const filename: rawbytestring): boolean; overload;

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

function getversion(const version: longint): rawbytestring;
begin
  if version <> -1 then
    result := inttostr(version)
  else
    result := '???';
end;



function gettime(const t: tdatetime): rawbytestring;
begin
  if t <> -1 then
    result := formatdatetime(
      defaultformatsettings.longdateformat + ' ' +
      defaultformatsettings.longtimeformat, t)
  else
    result := '???';
end;

function getflags(const flags: tgulpflags): rawbytestring;
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

function getattributes1(attr: tgulpattributes): longint;
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

function getattributes1(attr: longint): tgulpattributes;
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

function getattributes2(var sr: tsearchrec): longint;
begin
  result := sr.attr;
end;

function getattributes2(var sr: tsearchrec): tgulpattributes;
begin
  result := getattributes1(sr.attr);
end;

function getattributes3(const filename: rawbytestring): longint;
var
  sr: tsearchrec;
begin
  result := 0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      result := sr.attr;
  sysutils.findclose(sr);
end;

function getattributes3(const filename: rawbytestring): tgulpattributes;
begin
  result := getattributes1(getattributes3(filename));
end;

function getattributes4(attr: longint): rawbytestring;
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

function getattributes4(attr: rawbytestring): longint;
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

  if attr <> '' then
    raise exception.createfmt(gereadstream, ['006001']);
end;

function setattributes(const filename: rawbytestring; attr: longint): longint;
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

function setattributes(const filename: rawbytestring; attr: tgulpattributes): tgulpattributes;
begin
  result := getattributes1(setattributes(filename, getattributes1(attr)));
end;

{ common modes routines }

function getmode1(mode: tgulppermissions): longint;
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

function getmode1(mode: longint): tgulppermissions;
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

function getmode3(const filename: rawbytestring): longint;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  result := 0;
  {$IFDEF LINUX}
  if fileissymlink(filename) then
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

function getmode3(const filename: rawbytestring): tgulppermissions;
begin
  result := getmode1(getmode3(filename));
end;

function getmode4(const mode: rawbytestring): longint;
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

function getmode4(const mode: rawbytestring): tgulppermissions;
begin
  result := getmode1(getmode4(mode));
end;

function setmode(const filename: rawbytestring; mode: longint): longint;
begin
  result := 0;
  {$IFDEF LINUX}
  if fileissymlink(filename) = false then
    result := fpchmod(filename, mode);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function setmode(const filename: rawbytestring; mode: tgulppermissions): tgulppermissions;
begin
  result := getmode1(setmode(filename, getmode1(mode)));;
end;

{ common size routines }

function getsize2(var sr: tsearchrec): int64;
begin
  if fileisregular(sr.attr) then
    result := sr.size
  else
    result := 0;
end;

function getsize3(const filename: rawbytestring): int64;
var
  sr: tsearchrec;
begin
  result := 0;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      result := _getfilesize(sr);
  sysutils.findclose(sr);
end;

function getsize4(const size: int64): rawbytestring;
begin
  if size <> -1 then
    result := format('%u', [size])
  else
    result := '???';
end;
















function _getfiletimeutc(var sr: tsearchrec): tdatetime;
begin
  result := localtime2universal(filedatetodatetime(sr.time));
end;

function _getfiletimeutc(const filename: rawbytestring): tdatetime;
var
  sr: tsearchrec;
begin
  result := -1;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile,sr) = 0 then
      result := _getfiletimeutc(sr);
  sysutils.findclose(sr);
end;

function _setfiletimeutc(const filename: rawbytestring; timeutc: tdatetime): longint;
begin
  result := -1;
  if timeutc <> -1 then
  begin
    {$IFDEF LINUX}
    if fileissymlink(_getfileattr(filename)) = false then
      result := filesetdate(filename, datetimetofiledate(universaltime2local(timeutc)));
    {$ELSE}
    {$IFDEF MSWINDOWS}
    result := filesetdate(filename, datetimetofiledate(universaltime2local(timeutc)))
    {$ELSE}
    ...
    {$ENDIF}
    {$ENDIF}
  end;
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
  result := -1;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;





function _getfileuserid(const filename: rawbytestring): longint;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  result := -1;
  {$IFDEF LINUX}
  if fileissymlink(filename) then
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

function _getfilegroupid(const filename: rawbytestring): longint;
{$IFDEF LINUX}
var
  info: stat;
{$ENDIF}
begin
  result := -1;
  {$IFDEF LINUX}
  if fileissymlink(filename) then
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

function _setfileuserid(const filename: rawbytestring; userid: longint): longint;
{$IFDEF LINUX}
var
  systempath: rawbytestring;
{$ENDIF}
begin
  result := -1;
  if userid <> -1 then
  begin
    {$IFDEF LINUX}
    systempath := tosinglebytefilesystemencodedfilename(filename);
    if fileissymlink(filename) = false then
      result := do_syscall(syscall_nr_chown,
        tsysparam(pchar(systempath)), tsysparam(userid), tsysparam(_getfilegroupid(filename)))
    else
      result := do_syscall(syscall_nr_lchown,
        tsysparam(pchar(systempath)), tsysparam(userid), tsysparam(_getfilegroupid(filename)));
    {$ELSE}
    {$IFDEF MSWINDOWS}
    {$ELSE}
    ...
    {$ENDIF}
    {$ENDIF}
  end;
end;

function _setfilegroupid(const filename: rawbytestring; groupid: longint): longint;
{$IFDEF LINUX}
var
  systempath: rawbytestring;
{$ENDIF}
begin
  result := -1;
  if groupid <> -1 then
  begin
    {$IFDEF LINUX}
    systempath := tosinglebytefilesystemencodedfilename(filename);
    if fileissymlink(filename) = false then
      result := do_syscall(syscall_nr_chown,
        tsysparam(pchar(systempath)), tsysparam(_getfileuserid(filename)), tsysparam(groupid))
    else
      result := do_syscall(syscall_nr_lchown,
        tsysparam(pchar(systempath)), tsysparam(_getfileuserid(filename)), tsysparam(groupid));
    {$ELSE}
    {$IFDEF MSWINDOWS}
    {$ELSE}
    ...
    {$ENDIF}
    {$ENDIF}
  end;
end;

function _getfileusername(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

function _getfilegroupname(const filename: rawbytestring): rawbytestring;
begin
  result := '';
end;

function fileissymlink(attr: longint): boolean;
begin
  result := attr <> -1;
  if result then
    result := attr and fasymlink > 0;
end;

function fileisdirectory(attr: longint): boolean;
begin
  result := attr <> -1;
  if result then
  begin
    result := fileissymlink(attr) = false;
    if result then
      result := attr and fadirectory > 0
  end;
end;

function fileisregular(attr: longint): boolean;
begin
  result := attr <> -1;
  if result then
  begin
    result := fileissymlink(attr) = false;
    if result then
      result := fileisdirectory(attr) = false;
  end;
end;

function fileissymlink(const filename: rawbytestring): boolean;
begin
  result := fileissymlink(_getfileattr(filename));
end;

function fileisdirectory(const filename: rawbytestring): boolean;
begin
  result := fileisdirectory(_getfileattr(filename));
end;

function fileisregular(const filename: rawbytestring): boolean;
begin
  result := fileisregular(_getfileattr(filename));
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
  {$IFDEF LINUX}
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
  {$IFDEF LINUX}
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
