{ Description: main library.

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

unit gulplibrary;

{$mode objfpc}
{$H+}

interface

uses
{$IFDEF UNIX}
  baseunix,
{$ENDIF}
  classes,
  gulpcommon,
  gulpfixes,
  gulplist,
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  sha1,
  sysutils;

type
  { gulp item flags }

  tgulpflag  = (gfadd, gfdelete, gfclose, gffullname, gftimeutc,
    gfattributes, gfmode, gfsize, gflinkname, gfuserid, gfgroupid,
    gfusername, gfgroupname, gfcomment);

  tgulpflags = set of tgulpflag;

  { gulp item }

  pgulpitem  = ^tgulpitem;

  tgulpitem = record
    flags:      tgulpflags;
    fullname:   rawbytestring;
    timeutc:    tdatetime;
    attributes: longint;
    mode:       longint;
    size:       int64;
    linkname:   rawbytestring;
    userid:     longword;
    groupid:    longword;
    username:   rawbytestring;
    groupname:  rawbytestring;
    comment:    rawbytestring;
    beginning:  int64;
    ending:     int64;
    version:    longword;
  end;

  { gulp item list }

  tgulplist = specialize tgenericlist<pgulpitem>;

  { gulp application events }

  tgulpshowitem    = procedure(p: pgulpitem) of object;
  tgulpshowmessage = procedure(const message: rawbytestring) of object;
  tgulpterminate   = function: boolean of object;

   { gulp application }

  tgulpapplication = class
  private
    fexclude: trawbytestringlist;
    finclude: trawbytestringlist;
    fnodelete: boolean;
    funtilversion: longword;
    fonshowitem: tgulpshowitem;
    fonshowmessage: tgulpshowmessage;
    procedure showitem(const item: pgulpitem);
    procedure showmessage(const message: rawbytestring);
  public
    constructor create;
    destructor destroy; override;
    procedure sync(const filename: rawbytestring);
    procedure restore(const filename: rawbytestring);
    procedure purge(const filename: rawbytestring);
    procedure list(const filename: rawbytestring);
    procedure fix(const filename: rawbytestring);
    procedure check(const filename: rawbytestring);
    procedure reset;
  public
    property exclude: trawbytestringlist read fexclude;
    property include: trawbytestringlist read finclude;
    property nodelete: boolean read fnodelete write fnodelete;
    property untilversion: longword read funtilversion write funtilversion;
    property onshowitem: tgulpshowitem read fonshowitem write fonshowitem;
    property onshowmessage: tgulpshowmessage
      read fonshowmessage write fonshowmessage;
  end;

{ usefull routines }

function versiontostring(const version: longword): rawbytestring;
function attrtostring(const attr: longint): rawbytestring;
function sizetostring(const size: int64): rawbytestring;
function timetostring(const t: tdatetime): rawbytestring;
function modetostring(const mode: longint): rawbytestring;
function stringtoattr(const s: rawbytestring): longint;
function stringtomode(const s: rawbytestring): longint;
function flagstostring(const f: tgulpflags): rawbytestring;

implementation

uses
  dateutils,
  gulpmessages,
  gulpscanner,
  gulpstream,
  math;

const
  gulpmarker: tsha1digest =
    (255, 210, 119,  9, 180, 210, 231, 123,  25,  91,
     110, 159, 243, 53, 246,  80, 215, 248, 114, 172);

  gulpdescription =
    'GULP v0.0.3 journaling archiver, copyright (c) 2014-2016 Melchiorre Caruso.'
    + lineending +
    'GULP archiver for user-level incremental backups with rollback capability.'
    + lineending;

{ usefull routines }

function versiontostring(const version: longword): rawbytestring;
begin
  result := inttostr(version);
end;

function flagstostring(const f: tgulpflags): rawbytestring;
begin
  if (gfadd in f) and (gfdelete in f) then
    result := 'UPD'
  else
  if (gfadd in f) then
    result := 'ADD'
  else
  if (gfdelete in f) then
    result := 'DEL'
  else
    raise exception.createfmt(gewrongflag, [003099]);
end;

function timetostring(const t: tdatetime): rawbytestring;
begin
  result := formatdatetime(
    defaultformatsettings.longdateformat + ' ' +
    defaultformatsettings.longtimeformat, t);
end;

function sizetostring(const size: int64): rawbytestring;
begin
  result := format('%u', [size]);
end;

function attrtostring(const attr: longint): rawbytestring;
begin
  result := '       ';
  if attr and fareadonly  <> 0 then result[1] := 'R';
  if attr and fahidden    <> 0 then result[2] := 'H';
  if attr and fasysfile   <> 0 then result[3] := 'S';
  if attr and favolumeid  <> 0 then result[4] := 'V';
  if attr and fadirectory <> 0 then result[5] := 'D';
  if attr and faarchive   <> 0 then result[6] := 'A';
  if attr and fasymlink   <> 0 then result[7] := 'L';
end;

function stringtoattr(const s: rawbytestring): longint;
begin
  result := 0;
  if length(s) = 7 then
  begin
    if upcase(s[1]) = 'R' then result := result or fareadonly;
    if upcase(s[2]) = 'H' then result := result or fahidden;
    if upcase(s[3]) = 'S' then result := result or fasysfile;
    if upcase(s[4]) = 'V' then result := result or favolumeid;
    if upcase(s[5]) = 'D' then result := result or fadirectory;
    if upcase(s[6]) = 'A' then result := result or faarchive;
    if upcase(s[7]) = 'L' then result := result or fasymlink;
  end;
end;

function modetostring(const mode: longint): rawbytestring;
begin
{$IFDEF UNIX}
  result := octstr(mode, 3);
{$ELSE}
{$IFDEF MSWINDOWS}
  result := '...';
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function stringtomode(const s: rawbytestring): longint;
{$IFDEF UNIX}
var
  i: longint;
{$ENDIF}
begin
  result := 0;
{$IFDEF UNIX}
  for i := 1 to length(s) do
    result := result * 8 + strtoint(copy(s, i, 1));
{$ELSE}
{$IFDEF MSWINDOWS}
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

{ internal routines }

function itemclear(p: pgulpitem): pgulpitem;
begin
  p^.flags      := [];
  p^.fullname   := '';
  p^.timeutc    := 0.0;
  p^.attributes := 0;
  p^.mode       := 0;
  p^.size       := 0;
  p^.linkname   := '';
  p^.userid     := 0;
  p^.groupid    := 0;
  p^.username   := '';
  p^.groupname  := '';
  p^.comment    := '';
  p^.beginning  := 0;
  p^.ending     := 0;
  p^.version    := 0;
  result := p;
end;

function itemgetdigest(p: pgulpitem): tsha1digest;
var
  context: tsha1context;
begin
  sha1init(context);
  sha1update(context, p^.flags, sizeof(p^.flags));
  if gffullname in p^.flags then
    sha1update(context, pointer(p^.fullname)^, length(p^.fullname));
  if gftimeutc in p^.flags then
    sha1update(context, p^.timeutc, sizeof(p^.timeutc));
  if gfattributes in p^.flags then
    sha1update(context, p^.attributes, sizeof(p^.attributes));
  if gfmode in p^.flags then
    sha1update(context, p^.mode, sizeof(p^.mode));
  if gfsize in p^.flags then
    sha1update(context, p^.size, sizeof(p^.size));
  if gflinkname in p^.flags then
    sha1update(context, pointer(p^.linkname)^, length(p^.linkname));
  if gfuserid in p^.flags then
    sha1update(context, p^.userid, sizeof(p^.userid));
  if gfgroupid in p^.flags then
    sha1update(context, p^.groupid, sizeof(p^.groupid));
  if gfusername in p^.flags then
    sha1update(context, pointer(p^.username)^, length(p^.username));
  if gfgroupname in p^.flags then
    sha1update(context, pointer(p^.groupname)^, length(p^.groupname));
  if gfcomment in p^.flags then
    sha1update(context, pointer(p^.comment)^, length(p^.comment));
  if gfsize in p^.flags then
    sha1update(context, p^.beginning, sizeof(p^.beginning));
  if gfsize in p^.flags then
    sha1update(context, p^.ending, sizeof(p^.ending));
  sha1final(context, result);
end;

function itemgetsize(p: pgulpitem): int64;
begin
  result := sizeof(tsha1digest);
  inc(result, sizeof(p^.flags));
  if gffullname in p^.flags then
  begin
    inc(result, sizeof(longint));
    inc(result, length(p^.fullname));
  end;
  if gftimeutc    in p^.flags then inc(result, sizeof(p^.timeutc));
  if gfattributes in p^.flags then inc(result, sizeof(p^.attributes));
  if gfmode       in p^.flags then inc(result, sizeof(p^.mode));
  if gfsize       in p^.flags then inc(result, sizeof(p^.size));

  if gflinkname in p^.flags then
  begin
    inc(result, sizeof(longint));
    inc(result, length(p^.linkname));
  end;
  if gfuserid  in p^.flags then inc(result, sizeof(p^.userid));
  if gfgroupid in p^.flags then inc(result, sizeof(p^.groupid));

  if gfusername in p^.flags then
  begin
    inc(result, sizeof(longint));
    inc(result, length(p^.username));
  end;
  if gfgroupname in p^.flags then
  begin
    inc(result, sizeof(longint));
    inc(result, length(p^.groupname));
  end;
  if gfcomment in p^.flags then
  begin
    inc(result, sizeof(longint));
    inc(result, length(p^.comment));
  end;

  if gfsize in p^.flags then inc(result, sizeof(p^.beginning));
  if gfsize in p^.flags then inc(result, sizeof(p^.ending));
  inc(result, sizeof(tsha1digest));
end;

{ gulp library routines }

function compare40(p1, p2: pgulpitem): longint;
begin
{$IFDEF UNIX}
  result := ansicomparestr(p1^.fullname, p2^.fullname);
{$ELSE}
{$IFDEF MSWINDOWS}
  result := ansicomparetext(p1^.fullname, p2^.fullname);
{$ELSE}
{$ENDIF}
{$ENDIF}
end;

function compare41(p1, p2: pgulpitem): longint;
begin
  result := compare40(p1, p2);

  if result = 0 then
    if p1^.version < p2^.version then
      result := -1
    else
    if p1^.version > p2^.version then
      result := 1;

  if result = 0 then
    if (gfdelete in p1^.flags) and (gfadd in p2^.flags) then
      result := -1
    else
    if (gfdelete in p2^.flags) and (gfadd in p1^.flags) then
      result := 1;
end;

function compare42(p1, p2: pgulpitem): longint;
begin
{$IFDEF UNIX}
  result := ansicomparestr(p1^.fullname, p2^.fullname);
{$ELSE}
{$IFDEF MSWINDOWS}
  result := ansicomparetext(p1^.fullname, p2^.fullname);
{$ELSE}
{$ENDIF}
{$ENDIF}
  if result = 0 then
    if (gfdelete in p1^.flags) and (gfadd in p2^.flags) then
      result := -1
    else
    if (gfdelete in p2^.flags) and (gfadd in p1^.flags) then
      result := 1;
end;

procedure librestore(instream, outstream: tstream; size: int64);
var
  buffer:  array[0..$FFFF] of byte;
  context: tsha1context;
  digest1: tsha1digest;
  digest2: tsha1digest;
  readed:  longint;
begin
  sha1init(context);
  while size > 0 do
  begin
    readed := instream.read(buffer, min(sizeof(buffer), size));
    if readed = 0 then
      raise exception.createfmt(gereadstream, [003012]);
    sha1update(context, buffer, readed);
    outstream.write(buffer, readed);
    dec(size, readed);
  end;
  sha1final(context, digest1);
  if instream.read(digest2, sizeof(tsha1digest)) <> sizeof(tsha1digest) then
    raise exception.createfmt(gebrokenarchive, [003013]);
  if sha1match(digest1, digest2) = false then
    raise exception.createfmt(gechecksum, [003093]);
end;

procedure librestore(instream: tstream; p: pgulpitem);
var
  check:     boolean;
  outpath:   rawbytestring;
  outstream: tfilestream;
begin
  if p^.attributes and (fasysfile or favolumeid) = 0 then
  begin
    outpath := extractfiledir(p^.fullname);
    if (outpath <> '') and (forcedirectories(outpath) = false) then
      raise exception.createfmt(gecreatepath, [outpath]);

    check := false;
    if (p^.attributes and fasymlink) = fasymlink then
    begin
    {$IFDEF UNIX}
      check := fpsymlink(pchar(p^.linkname), pchar(p^.fullname)) = 0;
    {$ELSE}
    {$IFDEF MSWINDOWS}
    {$ELSE}
    {$ENDIF}
    {$ENDIF}
    end else
    if (p^.attributes and fadirectory) = fadirectory then
    begin
      check := directoryexists(p^.fullname);
      if check = false then
        check := createdir(p^.fullname);
    end else
    if (gfsize in p^.flags) then
    begin
      instream.seek(p^.beginning, sobeginning);
      if fileexists(p^.fullname) = false then
        outstream := tfilestream.create(p^.fullname, fmcreate)
      else
        outstream := tfilestream.create(p^.fullname, fmopenwrite);
      librestore(instream, outstream, p^.size);
      freeandnil(outstream);
      check := true;
    end;

    if check = true then
    begin
      {$IFDEF UNIX}
      if (gfuserid in p^.flags) or (gfgroupid in p^.flags) then
        if fpchown(p^.fullname, p^.userid, p^.groupid) <> 0 then
          raise exception.createfmt(gesetid, [p^.fullname]);

      if gfmode in p^.flags then
        if fpchmod(p^.fullname, p^.mode) <> 0 then
          raise exception.createfmt(gesetmode, [p^.fullname]);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      if filesetattr(p^.fullname, p^.attributes) <> 0 then
        raise exception.createfmt(gesetattributes, [p^.fullname]);
      {$ELSE}
      {$ENDIF}
      {$ENDIF}
      if filesetdate(p^.fullname,
        datetimetofiledate(universaltime2local(p^.timeutc))) <> 0 then
          raise exception.createfmt(gesetdatetime, [p^.fullname]);
    end else
      raise exception.createfmt(gerestoreitem, [p^.fullname]);
  end;
end;

procedure libwrite(instream, outstream: tstream; size: int64);
var
  buffer:  array[0..$FFFF] of byte;
  context: tsha1context;
  digest:  tsha1digest;
  readed:  longint;
begin
  sha1init(context);
  while size > 0 do
  begin
    readed := instream.read(buffer, min(sizeof(buffer), size));
    if readed = 0 then
      raise exception.createfmt(gereadstream, [003097]);
    sha1update(context, buffer, readed);
    outstream.write(buffer, readed);
    dec(size, readed);
  end;
  sha1final(context, digest);
  outstream.write(digest, sizeof(tsha1digest));
end;

procedure libwrite(outstream: tstream; p: pgulpitem);
begin
  outstream.write(gulpmarker, sizeof(gulpmarker));
  outstream.write(p^.flags, sizeof(p^.flags));
  if gffullname   in p^.flags then outstream.writeansistring(p^.fullname);
  if gftimeutc    in p^.flags then outstream.write(p^.timeutc, sizeof(p^.timeutc));
  if gfattributes in p^.flags then outstream.write(p^.attributes, sizeof(p^.attributes));
  if gfmode       in p^.flags then outstream.write(p^.mode, sizeof(p^.mode));
  if gfsize       in p^.flags then outstream.write(p^.size, sizeof(p^.size));
  if gflinkname   in p^.flags then outstream.writeansistring(p^.linkname);
  if gfuserid     in p^.flags then outstream.write(p^.userid, sizeof(p^.userid));
  if gfgroupid    in p^.flags then outstream.write(p^.groupid, sizeof(p^.groupid));
  if gfusername   in p^.flags then outstream.writeansistring(p^.username);
  if gfgroupname  in p^.flags then outstream.writeansistring(p^.groupname);
  if gfcomment    in p^.flags then outstream.writeansistring(p^.comment);

  if gfsize in p^.flags then outstream.write(p^.beginning, sizeof(p^.beginning));
  if gfsize in p^.flags then outstream.write(p^.ending, sizeof(p^.ending));
  outstream.write(itemgetdigest(p), sizeof(tsha1digest));
end;

procedure libwrite(outstream: tstream; list: tgulplist);
var
  i:      longint;
  size:   int64;
  source: tstream;
begin
  if list.count > 0 then
  begin
    size := outstream.seek(0, soend);
    include(list[list.count - 1]^.flags, gfclose);
    for i := 0 to list.count - 1 do
      libwrite(outstream, list[i]);

    for i := 0 to list.count - 1 do
      if list[i]^.attributes and (fasysfile or favolumeid) = 0 then
        if list[i]^.attributes and (fasymlink or fadirectory) = 0 then
          if gfsize in list[i]^.flags then
          begin
            list[i]^.beginning := outstream.seek(0, socurrent);
            source := tfilestream.create(list[i]^.fullname,
              fmopenread or fmsharedenynone);
            libwrite(outstream, source, list[i]^.size);
            freeandnil(source);
            list[i]^.ending := outstream.seek(0, socurrent);
          end;
    outstream.seek(size, sobeginning);
    for i := 0 to list.count - 1 do
      libwrite(outstream, list[i]);
    outstream.seek(0, soend);
  end;
end;

function libload(stream: tstream; p: pgulpitem): pgulpitem;
var
  digest: tsha1digest;
begin
  result := itemclear(p);
  if stream.read(digest, sizeof(tsha1digest)) <> sizeof(tsha1digest) then
    raise exception.createfmt(gebrokenarchive, [003001]);
  if sha1match(digest, gulpmarker) = false then
    raise exception.createfmt(gewrongmarker, [003002]);

  stream.read(p^.flags, sizeof(p^.flags));
  if gffullname   in p^.flags then p^.fullname := stream.readansistring;
  if gftimeutc    in p^.flags then stream.read(p^.timeutc, sizeof(p^.timeutc));
  if gfattributes in p^.flags then stream.read(p^.attributes, sizeof(p^.attributes));
  if gfmode       in p^.flags then stream.read(p^.mode, sizeof(p^.mode));
  if gfsize       in p^.flags then stream.read(p^.size, sizeof(p^.size));
  if gflinkname   in p^.flags then p^.linkname := stream.readansistring;
  if gfuserid     in p^.flags then stream.read(p^.userid, sizeof(p^.userid));
  if gfgroupid    in p^.flags then stream.read(p^.groupid, sizeof(p^.groupid));
  if gfusername   in p^.flags then p^.username  := stream.readansistring;
  if gfgroupname  in p^.flags then p^.groupname := stream.readansistring;
  if gfcomment    in p^.flags then p^.comment   := stream.readansistring;

  if gfsize in p^.flags then stream.read(p^.beginning, sizeof(p^.beginning));
  if gfsize in p^.flags then stream.read(p^.ending, sizeof(p^.ending));

  if ((gfdelete in p^.flags) = false) and ((gfadd in p^.flags) = false) then
    raise exception.createfmt(gebrokenarchive, [003003]);
  if ((p^.ending = 0) or (p^.beginning = 0)) and (p^.size <> 0) then
    raise exception.createfmt(gebrokenarchive, [003004]);
  if stream.read(digest, sizeof(tsha1digest)) <> sizeof(tsha1digest) then
    raise exception.createfmt(gebrokenarchive, [003005]);
  if sha1match(digest, itemgetdigest(p)) = false then
    raise exception.createfmt(gebrokenarchive, [003007]);

  dodirseparators(p^.linkname);
  dodirseparators(p^.fullname);
end;

procedure libload(stream: tstream; list: tgulplist);
var
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  size := stream.seek(0, soend);
          stream.seek(0, sobeginning);
  p    := new(pgulpitem);
  while offset < size do
  begin
    libload(stream, p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.ending - p^.beginning);
    if gfclose in p^.flags then
    begin
      if stream.seek(offset, sobeginning) <> offset then
        raise exception.createfmt(genotarchive, [003008]);
      inc(version);
    end;

    if list.add(p) = -1 then
      raise exception.createfmt(geduplicates, [003009]);
    p := new(pgulpitem);
  end;
  dispose(p);
  if offset <> size then
    raise exception.createfmt(gebrokenarchive, [003011]);
end;

procedure libload(stream: tstream; list: tgulplist; untilversion: longword);
var
  i:       longint;
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  size := stream.seek(0, soend);
          stream.seek(0, sobeginning);
  p    := new(pgulpitem);
  while offset < size do
  begin
    libload(stream, p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.ending - p^.beginning);
    if gfclose in p^.flags then
    begin
      if stream.seek(offset, sobeginning) <> offset then
        raise exception.createfmt(genotarchive, [003096]);
      inc(version);
    end;

    if version <= untilversion then
    begin
      if gfdelete in p^.flags then
      begin
        i := list.find(p);
        if i <> -1 then
        begin
          dispose(list[i]);
          list.delete(i);
        end;
      end;
      if gfadd in p^.flags then
      begin
        if list.add(p) = -1 then
          raise exception.createfmt(geduplicates, [003010]);
        p := new(pgulpitem);
      end;
    end;
  end;
  dispose(p);
  if size <> stream.seek(0, soend) then
    raise exception.createfmt(gebrokenarchive, [003011]);
end;

function libnew1(const filename: rawbytestring): pgulpitem;
begin
  result           := itemclear(new(pgulpitem));
  result^.flags    := [gfdelete, gffullname];
  result^.fullname := filename;
end;

function libnew2(const filename: rawbytestring): pgulpitem;
var
  sr: tsearchrec;
begin
  result := nil;
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
      if gulpcommon.filegetattr(sr) and (fasysfile or favolumeid) = 0 then
      begin
        result             := itemclear(new(pgulpitem));
        result^.fullname   := filename;
        result^.timeutc    := filegettimeutc(sr);
        result^.size       := filegetsize(sr);
        result^.attributes := gulpcommon.filegetattr(sr);
        include(result^.flags, gfadd);
        include(result^.flags, gffullname);
        include(result^.flags, gftimeutc);
        if result^.attributes and fasymlink = 0 then
         if result^.attributes and fadirectory = 0 then
           include(result^.flags, gfsize);
        include(result^.flags, gfattributes);
      {$IFDEF UNIX}
        result^.mode     := filegetmode(filename);
        result^.linkname := filegetlinkname(filename);
        result^.userid   := filegetuserid(filename);
        result^.groupid  := filegetgroupid(filename);
        include(result^.flags, gfmode);
        include(result^.flags, gflinkname);
        include(result^.flags, gfuserid);
        include(result^.flags, gfgroupid);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      {$ELSE}
      {$ENDIF}
      {$ENDIF}
      end;
  sysutils.findclose(sr);
end;

procedure libappend(list: tgulplist; p: pgulpitem);
begin
  if list.add(p) = -1 then
    raise exception.createfmt(geduplicates, [003098]);
end;

function libfind(list: tgulplist; const filename: rawbytestring): longint;
var
  p: pgulpitem;
begin
  p := itemclear(new(pgulpitem));
  p^.flags    := [gfadd, gffullname];
  p^.fullname := filename;
  result := list.find(p);
  dispose(p);
end;

procedure libclear(list: tgulplist);
begin
  while list.count > 0 do
  begin
    dispose(list[0]);
    list.delete(0);
  end;
end;

{ tgulpapplication class }

constructor tgulpapplication.create;
begin
  inherited create;
  fonshowitem    := nil;
  fonshowmessage := nil;
  fexclude       := trawbytestringlist.create;
  finclude       := trawbytestringlist.create;
  fnodelete      := false;
  funtilversion  := $FFFFFFFF;
end;

destructor tgulpapplication.destroy;
begin
  fexclude.destroy;
  finclude.destroy;
  inherited destroy;
end;

procedure tgulpapplication.reset;
begin
  fexclude.clear;
  finclude.clear;
  fnodelete     := false;
  funtilversion := $FFFFFFFF;
end;

procedure tgulpapplication.showitem(const item: pgulpitem);
begin
  if assigned(fonshowitem) then
    fonshowitem(item);
end;

procedure tgulpapplication.showmessage(const message: rawbytestring);
begin
  if assigned(fonshowmessage) then
    fonshowmessage(message);
end;

procedure tgulpapplication.sync(const filename: rawbytestring);
var
  i, j:   longint;
  list1:  tgulplist;
  list2:  tgulplist;
  scan:   tscanner;
  size:   int64;
  stream: tstream;
begin
  showmessage(gulpdescription);
  showmessage(format(gmsync, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  if fileexists(filename) = TRUE then
    stream := tfilestream.create(filename, fmopenreadwrite)
  else
    stream := tfilestream.create(filename, fmcreate);
  list1    := tgulplist.create(@compare40);
  libload(stream, list1, $FFFFFFFF);
  size := stream.seek(0, soend);

  showmessage(format(gmscanningfs, [#13]));
  scan := tscanner.create;
  for i := finclude.count - 1 downto 0 do
    if directoryexists(finclude[i]) = true then
      finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  if finclude.count = 0 then
    finclude.add('*');
  for i := finclude.count - 1 downto 0 do
    scan.add(finclude[i]);

  for i := fexclude.count - 1 downto 0 do
    if directoryexists(fexclude[i]) = true then
      fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  fexclude.add(filename);
  for i := scan.count - 1 downto 0 do
    if filenamematch(scan[i], fexclude) = true then
      scan.delete(i);

  showmessage(format(gmsyncitems, [#13]));
  list2 := tgulplist.create(@compare42);
  if fnodelete = false then
    for i := 0 to list1.count - 1 do
      if scan.find(list1[i]^.fullname) = -1 then
        libappend(list2, libnew1(list1[i]^.fullname));

  for i := 0 to scan.count - 1 do
  begin
    j := libfind(list1, scan[i]);
    if j = -1 then
      libappend(list2, libnew2(scan[i]))
    else
    if filegettimeutc(scan[i]) <> list1[j]^.timeutc then
    begin
      libappend(list2, libnew1(scan[i]));
      libappend(list2, libnew2(scan[i]));
    end;
  end;
  libwrite(stream, list2);

  libclear(list1);
  libclear(list2);
  freeandnil(list1);
  freeandnil(list2);
  freeandnil(stream);
  freeandnil(scan);
  showmessage(format(gmsyncfinish, [#13,
    filegetsize(filename) - size, lineending]));
end;

procedure tgulpapplication.restore(const filename: rawbytestring);
var
  i, j:   longint;
  list1:  tgulplist;
  p:      pgulpitem;
  scan:   tscanner;
  size:   int64 = 0;
  stream: tstream;
begin
  showmessage(gulpdescription);
  showmessage(format(gmrestore, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream := tfilestream.create(filename, fmopenread);
  list1  := tgulplist.create(@compare40);
  libload(stream, list1, funtilversion);

  showmessage(format(gmscanningfs, [#13]));
  scan := tscanner.create;
  scan.add('*');
  for i := finclude.count - 1 downto 0 do
  begin
    j := libfind(list1, finclude[i]);
    if j <> -1 then
      if list1[j]^.attributes and fadirectory = fadirectory then
        finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  end;
  if finclude.count = 0 then
    finclude.add('*');

  for i := fexclude.count - 1 downto 0 do
  begin
    j := libfind(list1, fexclude[i]);
    if j <> -1 then
      if list1[j]^.attributes and fadirectory = fadirectory then
        fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  end;
  fexclude.add(filename);

  showmessage(format(gmrestoreitems, [#13]));
  if fnodelete = false then
    for i := scan.count - 1 downto 0 do
    begin
      j := libfind(list1, scan[i]);
      if (j = -1) or
         (filenamematch(list1[j]^.fullname, finclude) = false) or
         (filenamematch(list1[j]^.fullname, fexclude) = true) then
        if directoryexists(scan[i]) = true then
          removedir(scan[i])
        else
          deletefile(scan[i]);
    end;

  for i := list1.count - 1 downto 0 do
  begin
    p := list1[i];
    if filenamematch(p^.fullname, finclude) = true then
      if filenamematch(p^.fullname, fexclude) = false then
      begin
        j := scan.find(p^.fullname);
        if j = -1 then
        begin
          librestore(stream, p);
          inc(size, p^.size);
        end else
        if filegettimeutc(scan[j]) <> p^.timeutc then
        begin
          librestore(stream, p);
          inc(size, p^.size);
        end;
      end;
  end;
  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  freeandnil(scan);
  showmessage(format(gmrestorefinish, [#13, size, lineending]));
end;

procedure tgulpapplication.check(const filename: rawbytestring);
var
  i:      longint;
  list1:  tgulplist;
  nul:    tstream;
  stream: tstream;
begin
  showmessage(gulpdescription);
  showmessage(format(gmcheck, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream := tfilestream.create(filename, fmopenread);
  list1  := tgulplist.create(@compare41);
  libload(stream, list1);

  showmessage(format(gmcheckitems, [#13]));
  nul := tnulstream.create;
  for i := 0 to list1.count - 1 do
    if gfsize in list1[i]^.flags then
    begin
      stream.seek(list1[i]^.beginning, sobeginning);
      librestore(stream, nul, list1[i]^.size);
    end;
  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  freeandnil(nul);
  showmessage(format(gmcheckfinish, [#13, filegetsize(filename), lineending]));
end;

procedure tgulpapplication.fix(const filename: rawbytestring);
var
  p:       pgulpitem;
  offset:  int64 = 0;
  size:    int64 = 0;
  stream:  tstream;
begin
  showmessage(gulpdescription);
  showmessage(format(gmfix, [filename, lineending]));
  showmessage(format(gmfixitems, [#13]));
  stream := tfilestream.create(filename, fmopenreadwrite);

  p := new(pgulpitem);
  try
    while true do
    begin
      libload(stream, p);
      inc(offset, itemgetsize(p));
      inc(offset, p^.ending - p^.beginning);
      if gfclose in p^.flags then
      begin
        if stream.seek(offset, sobeginning) <> offset then
          raise exception.createfmt(genotarchive, [003095]);
        size := offset;
      end;
    end;
  except
  end;
  dispose(p);

  offset := stream.size - size;
  if size > 0 then
    stream.size := size
  else
    raise exception.createfmt(genotarchive, [003094]);
  freeandnil(stream);
  showmessage(format(gmfixfinish, [#13, offset, lineending]));
end;

procedure tgulpapplication.purge(const filename: rawbytestring);
var
  i:       longint;
  list1:   tgulplist;
  p:       pgulpitem;
  size:    int64 = 0;
  stream:  tstream;
  tmp:     tstream;
  tmpname: rawbytestring;

begin
  showmessage(gulpdescription);
  showmessage(format(gmpurge, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream := tfilestream.create(filename, fmopenread);
  list1  := tgulplist.create(@compare40);
  libload(stream, list1, $FFFFFFFF);

  showmessage(format(gmmoveitems, [#13]));
  tmpname := gettempfilename(extractfiledir(filename), '');
  tmp     := tfilestream.create(tmpname, fmcreate);
  if list1.count > 0 then
  begin
    for i := 0 to list1.count - 1 do
      libwrite(tmp, list1[i]);

    for i := 0 to list1.count - 1 do
    begin
      p := list1[i];
      if gfsize in p^.flags then
      begin
        stream.seek(p^.beginning, sobeginning);
        size := p^.ending - p^.beginning;
        p^.beginning := tmp.seek(0, socurrent);
        tmp.copyfrom(stream, size);
        p^.ending := tmp.seek(0, socurrent);
      end;
    end;

    tmp.seek(0, sobeginning);
    for i := 0 to list1.count - 1 do
    begin
      p := list1[i];
      if i = list1.count - 1 then
        system.include(p^.flags, gfclose)
      else
        system.exclude(p^.flags,gfclose);
      libwrite(tmp, p);
    end;
    tmp.seek(0, soend);
  end;
  size := stream.size - tmp.size;

  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  freeandnil(tmp);
  if deletefile(filename) = false then
    raise exception.createfmt(gedeletefile, [filename])
  else
  if renamefile(tmpname, filename) = false then
    raise exception.createfmt(gerenamefile, [tmpname]);
  showmessage(format(gmpurgefinish, [#13, size, lineending]));
end;

procedure tgulpapplication.list(const filename: rawbytestring);
var
  c:      longint = 0;
  i, j:   longint;
  list1:  tgulplist;
  p:      pgulpitem;
  stream: tstream;
begin
  showmessage(gulpdescription);
  showmessage(format(gmlist, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream := tfilestream.create(filename, fmopenread);
  if funtilversion = 0 then
  begin
    list1 := tgulplist.create(@compare41);
    libload(stream, list1);
  end else
  begin
    list1 := tgulplist.create(@compare40);
    libload(stream, list1, funtilversion);
  end;

  for i := finclude.count - 1 downto 0 do
  begin
    j := libfind(list1, finclude[i]);
    if j <> -1 then
      if list1[j]^.attributes and fadirectory = fadirectory then
        finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  end;
  if finclude.count = 0 then
    finclude.add('*');

  for i := fexclude.count - 1 downto 0 do
  begin
    j := libfind(list1, fexclude[i]);
    if j <> -1 then
      if list1[j]^.attributes and fadirectory = fadirectory then
        fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  end;

  showmessage(format(gmlistitems, [#13, lineending]));
  for i := 0 to list1.count - 1 do
  begin
    p := list1[i];
    j := max(j, p^.version);
    if filenamematch(p^.fullname, finclude) = true then
      if filenamematch(p^.fullname, fexclude) = false then
      begin
        showitem(p);
        inc(c);
      end;
  end;
  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  showmessage(format(gmlistfinish, [c, lineending]));
  showmessage(format(gmlistlastversion, [j, lineending]));
end;

end.
