{ Description: library main unit.

  Copyright (C) 2016 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

{ some usefull routines }

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

type
  { gulp item list }

  tgulplist = specialize tgenericlist<pgulpitem>;

  { gulp archive reader }

  tgulpreader = class
  private
    flist:   tgulplist;
    fstream: tstream;
    function read(p: pgulpitem): pgulpitem;
    function get(index: longint): pgulpitem;
    function getcount: longint;
  public
    constructor create(stream: tstream);
    destructor destroy; override;
    procedure load; overload;
    procedure load(untilversion: longword); overload;
    procedure extract(index: longint; stream: tstream); overload;
    procedure extract(index: longint); overload;
    function find(const filename: rawbytestring): longint;
    procedure clear;
  public
    property items[index: longint]: pgulpitem read get; default;
    property count: longint read getcount;
  end;

  { gulp archive writer }

  tgulpwriter = class
  private
    flist:   tgulplist;
    fstream: tstream;
    procedure write(p: pgulpitem); overload;
    procedure write(stream: tstream; size: int64); overload;
    function get(index: longint): pgulpitem;
    function getcount: longint;
  public
    constructor create(stream: tstream);
    destructor destroy; override;
    procedure delete(const filename: rawbytestring);
    procedure add(const filename: rawbytestring);
    procedure clear;
  public
    property items[index: longint]: pgulpitem read get; default;
    property count: longint read getcount;
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

{ compare items routines }

function compare40(p1, p2: pgulpitem): longint; inline;
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

function compare41(p1, p2: pgulpitem): longint; inline;
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

{ tgulpreader class }

constructor tgulpreader.create(stream: tstream);
begin
  inherited create;
  fstream := stream;
  flist   := tgulplist.create(@compare40);
end;

destructor tgulpreader.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tgulpreader.clear;
begin
  while flist.count > 0 do
  begin
    dispose(flist[0]);
    flist.delete(0);
  end;
end;

function tgulpreader.read(p: pgulpitem): pgulpitem;
var
  digest: tsha1digest;
begin
  result := itemclear(p);
  if fstream.read(digest, sizeof(tsha1digest)) <> sizeof(tsha1digest) then
    raise exception.createfmt(gebrokenarchive, [003001]);
  if sha1match(digest, gulpmarker) = false then
    raise exception.createfmt(gewrongmarker, [003002]);

  fstream.read(p^.flags, sizeof(p^.flags));
  if gffullname   in p^.flags then p^.fullname := fstream.readansistring;
  if gftimeutc    in p^.flags then fstream.read(p^.timeutc, sizeof(p^.timeutc));
  if gfattributes in p^.flags then fstream.read(p^.attributes, sizeof(p^.attributes));
  if gfmode       in p^.flags then fstream.read(p^.mode, sizeof(p^.mode));
  if gfsize       in p^.flags then fstream.read(p^.size, sizeof(p^.size));
  if gflinkname   in p^.flags then p^.linkname := fstream.readansistring;
  if gfuserid     in p^.flags then fstream.read(p^.userid, sizeof(p^.userid));
  if gfgroupid    in p^.flags then fstream.read(p^.groupid, sizeof(p^.groupid));
  if gfusername   in p^.flags then p^.username  := fstream.readansistring;
  if gfgroupname  in p^.flags then p^.groupname := fstream.readansistring;
  if gfcomment    in p^.flags then p^.comment   := fstream.readansistring;

  if gfsize in p^.flags then fstream.read(p^.beginning, sizeof(p^.beginning));
  if gfsize in p^.flags then fstream.read(p^.ending, sizeof(p^.ending));

  if ((gfdelete in p^.flags) = false) and ((gfadd in p^.flags) = false) then
    raise exception.createfmt(gebrokenarchive, [003003]);
  if ((p^.ending = 0) or (p^.beginning = 0)) and (p^.size <> 0) then
    raise exception.createfmt(gebrokenarchive, [003004]);
  if fstream.read(digest, sizeof(tsha1digest)) <> sizeof(tsha1digest) then
    raise exception.createfmt(gebrokenarchive, [003005]);
  if sha1match(digest, itemgetdigest(p)) = false then
    raise exception.createfmt(gebrokenarchive, [003007]);

  dodirseparators(p^.linkname);
  dodirseparators(p^.fullname);
end;

procedure tgulpreader.load;
var
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  clear;
  freeandnil(flist);
  flist := tgulplist.create(@compare41);
  size  := fstream.seek(0, soend);
           fstream.seek(0, sobeginning);
  p     := new(pgulpitem);
  while offset < size do
  begin
    read(p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.ending - p^.beginning);
    if gfclose in p^.flags then
    begin
      if fstream.seek(offset, sobeginning) <> offset then
        raise exception.createfmt(genotarchive, [003008]);
      inc(version);
    end;

    if flist.add(p) = -1 then
      raise exception.createfmt(geduplicates, [003009]);
    p := new(pgulpitem);
  end;
  dispose(p);
  if offset <> size then
    raise exception.createfmt(gebrokenarchive, [003011]);
end;

procedure tgulpreader.load(untilversion: longword);
var
  i:       longint;
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  clear;
  freeandnil(flist);
  flist := tgulplist.create(@compare40);
  size  := fstream.seek(0, soend);
           fstream.seek(0, sobeginning);
  p     := new(pgulpitem);
  while offset < size do
  begin
    read(p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.ending - p^.beginning);
    if gfclose in p^.flags then
    begin
      if fstream.seek(offset, sobeginning) <> offset then
        raise exception.createfmt(genotarchive, [003096]);
      inc(version);
    end;

    if version <= untilversion then
    begin
      if gfdelete in p^.flags then
      begin
        i := flist.find(p);
        if i <> -1 then
        begin
          dispose(flist[i]);
          flist.delete(i);
        end;
      end;
      if gfadd in p^.flags then
      begin
        if flist.add(p) = -1 then
          raise exception.createfmt(geduplicates, [003010]);
        p := new(pgulpitem);
      end;
    end;
  end;
  dispose(p);
  if size <> fstream.seek(0, soend) then
    raise exception.createfmt(gebrokenarchive, [003011]);
end;

procedure tgulpreader.extract(index: longint; stream: tstream);
var
  buffer:  array[0..$FFFF] of byte;
  context: tsha1context;
  digest1: tsha1digest;
  digest2: tsha1digest;
  readed:  longint;
  size:    int64;
begin
  fstream.seek(items[index]^.beginning, sobeginning);
  size := items[index]^.size;
  sha1init(context);
  while size > 0 do
  begin
    readed := fstream.read(buffer, min(sizeof(buffer), size));
    if readed = 0 then
      raise exception.createfmt(gereadstream, [003012]);
    sha1update(context, buffer, readed);
    stream.write(buffer, readed);
    dec(size, readed);
  end;
  sha1final(context, digest1);
  if fstream.read(digest2, sizeof(tsha1digest)) <> sizeof(tsha1digest) then
    raise exception.createfmt(gebrokenarchive, [003013]);
  if sha1match(digest1, digest2) = false then
    raise exception.createfmt(gechecksum, [items[index]^.fullname]);
end;

procedure tgulpreader.extract(index: longint);
var
  check:  boolean;
  p:      pgulpitem;
  path:   rawbytestring;
  stream: tfilestream;
begin
  p := items[index];
  if p^.attributes and (fasysfile or favolumeid) = 0 then
  begin
    path := extractfiledir(p^.fullname);
    if (path <> '') and (forcedirectories(path) = false) then
      raise exception.createfmt(gecreatepath, [path]);

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
      if fileexists(p^.fullname) = false then
        stream := tfilestream.create(p^.fullname, fmcreate)
      else
        stream := tfilestream.create(p^.fullname, fmopenwrite);
      extract(index, stream);
      freeandnil(stream);
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

function tgulpreader.find(const filename: rawbytestring): longint;
var
  item: pgulpitem;
begin
  item := itemclear(new(pgulpitem));
  item^.flags    := [gfadd, gffullname];
  item^.fullname := filename;
  result := flist.find(item);
  dispose(item);
end;

function tgulpreader.get(index: longint): pgulpitem;
begin
  result := flist[index];
end;

function tgulpreader.getcount: longint;
begin
  result := flist.count;
end;

{ tgulpwriter class }

constructor tgulpwriter.create(stream: tstream);
begin
  inherited create;
  fstream := stream;
  flist   := tgulplist.create(@compare42);
end;

destructor tgulpwriter.destroy;
var
  i:      longint;
  size:   int64;
  source: tstream;
begin
  if flist.count > 0 then
  begin
    size := fstream.seek(0, soend);
    include(flist[flist.count - 1]^.flags, gfclose);
    for i := 0 to flist.count - 1 do
      write(flist[i]);

    for i := 0 to flist.count - 1 do
      if flist[i]^.attributes and (fasysfile or favolumeid) = 0 then
        if flist[i]^.attributes and (fasymlink or fadirectory) = 0 then
          if gfsize in flist[i]^.flags then
          begin
            flist[i]^.beginning := fstream.seek(0, socurrent);
            source := tfilestream.create(flist[i]^.fullname,
              fmopenread or fmsharedenynone);
            write(source, flist[i]^.size);
            freeandnil(source);
            flist[i]^.ending := fstream.seek(0, socurrent);
          end;
    fstream.seek(size, sobeginning);
    for i := 0 to flist.count - 1 do
      write(flist[i]);
    fstream.seek(0, soend);
    clear;
  end;
  flist.destroy;
  inherited destroy;
end;

procedure tgulpwriter.clear;
begin
  while flist.count > 0 do
  begin
    dispose(flist[0]);
    flist.delete(0);
  end;
end;

procedure tgulpwriter.add(const filename: rawbytestring);
var
  p:  pgulpitem;
  sr: tsearchrec;
begin
  if sysutils.findfirst(filename, fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr) = 0 then
    if gulpcommon.filegetattr(sr) and (fasysfile or favolumeid) = 0 then
    begin
      p := itemclear(new(pgulpitem));
      p^.fullname   := filename;
      p^.timeutc    := filegettimeutc(sr);
      p^.size       := filegetsize(sr);
      p^.attributes := gulpcommon.filegetattr(sr);
      include(p^.flags, gffullname);
      include(p^.flags, gftimeutc);
      if p^.attributes and fasymlink = 0 then
        if p^.attributes and fadirectory = 0 then
          include(p^.flags, gfsize);
      include(p^.flags, gfattributes);
      {$IFDEF UNIX}
      p^.mode     := filegetmode(filename);
      p^.linkname := filegetlinkname(filename);
      p^.userid   := filegetuserid(filename);
      p^.groupid  := filegetgroupid(filename);
      include(p^.flags, gfmode);
      include(p^.flags, gflinkname);
      include(p^.flags, gfuserid);
      include(p^.flags, gfgroupid);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      {$ELSE}
      {$ENDIF}
      {$ENDIF}
      include(p^.flags, gfadd);
      if flist.add(p) = -1 then
        raise exception.createfmt(geduplicates, [003014]);
    end;
  sysutils.findclose(sr);
end;

procedure tgulpwriter.delete(const filename: rawbytestring);
var
  p: pgulpitem;
begin
  p := itemclear(new(pgulpitem));
  p^.flags    := [gfdelete, gffullname];
  p^.fullname := filename;
  if flist.add(p) = -1 then
    raise exception.createfmt(geduplicates, [003098]);
end;

procedure tgulpwriter.write(stream: tstream; size: int64);
var
  buffer:  array[0..$FFFF] of byte;
  context: tsha1context;
  digest:  tsha1digest;
  readed:  longint;
begin
  sha1init(context);
  while size > 0 do
  begin
    readed := stream.read(buffer, min(sizeof(buffer), size));
    if readed = 0 then
      raise exception.createfmt(gereadstream, [003097]);
    sha1update(context, buffer, readed);
    fstream.write(buffer, readed);
    dec(size, readed);
  end;
  sha1final(context, digest);
  fstream.write(digest, sizeof(tsha1digest));
end;

procedure tgulpwriter.write(p: pgulpitem);
begin
  fstream.write(gulpmarker, sizeof(gulpmarker));
  fstream.write(p^.flags, sizeof(p^.flags));
  if gffullname   in p^.flags then fstream.writeansistring(p^.fullname);
  if gftimeutc    in p^.flags then fstream.write(p^.timeutc, sizeof(p^.timeutc));
  if gfattributes in p^.flags then fstream.write(p^.attributes, sizeof(p^.attributes));
  if gfmode       in p^.flags then fstream.write(p^.mode, sizeof(p^.mode));
  if gfsize       in p^.flags then fstream.write(p^.size, sizeof(p^.size));
  if gflinkname   in p^.flags then fstream.writeansistring(p^.linkname);
  if gfuserid     in p^.flags then fstream.write(p^.userid, sizeof(p^.userid));
  if gfgroupid    in p^.flags then fstream.write(p^.groupid, sizeof(p^.groupid));
  if gfusername   in p^.flags then fstream.writeansistring(p^.username);
  if gfgroupname  in p^.flags then fstream.writeansistring(p^.groupname);
  if gfcomment    in p^.flags then fstream.writeansistring(p^.comment);

  if gfsize in p^.flags then fstream.write(p^.beginning, sizeof(p^.beginning));
  if gfsize in p^.flags then fstream.write(p^.ending, sizeof(p^.ending));
  fstream.write(itemgetdigest(p), sizeof(tsha1digest));
end;

function tgulpwriter.get(index: longint): pgulpitem;
begin
  result := flist[index];
end;

function tgulpwriter.getcount: longint;
begin
  result := flist.count;
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
  size:   int64;
  scan:   tscanner;
  stream: tstream;
  reader: tgulpreader;
  writer: tgulpwriter;
begin
  showmessage(gulpdescription);
  showmessage(format(gmsync, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  if fileexists(filename) then
    stream := tfilestream.create(filename, fmopenreadwrite)
  else
    stream := tfilestream.create(filename, fmcreate);
  reader := tgulpreader.create(stream);
  reader.load($FFFFFFFF);
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
  writer := tgulpwriter.create(stream);
  if fnodelete = false then
    for i := 0 to reader.count - 1 do
      if scan.find(reader[i]^.fullname) = -1 then
        writer.delete(reader[i]^.fullname);

  for i := 0 to scan.count - 1 do
  begin
    j := reader.find(scan[i]);
    if j = -1 then
      writer.add(scan[i])
    else
    if filegettimeutc(scan[i]) <> reader[j]^.timeutc then
    begin
      writer.delete(scan[i]);
      writer.add(scan[i]);
    end;
  end;
  freeandnil(writer);
  freeandnil(reader);
  freeandnil(stream);
  freeandnil(scan);
  showmessage(format(gmsyncfinish, [#13,
    filegetsize(filename) - size, lineending]));
end;

procedure tgulpapplication.restore(const filename: rawbytestring);
var
  i, j:   longint;
  p:      pgulpitem;
  size:   int64 = 0;
  scan:   tscanner;
  stream: tstream;
  reader: tgulpreader;
begin
  showmessage(gulpdescription);
  showmessage(format(gmrestore, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream    := tfilestream.create(filename, fmopenread);
  reader := tgulpreader.create(stream);
  reader.load(funtilversion);

  showmessage(format(gmscanningfs, [#13]));
  scan := tscanner.create;
  scan.add('*');
  for i := finclude.count - 1 downto 0 do
  begin
    j := reader.find(finclude[i]);
    if j <> -1 then
      if reader[j]^.attributes and fadirectory = fadirectory then
        finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  end;
  if finclude.count = 0 then
    finclude.add('*');

  for i := fexclude.count - 1 downto 0 do
  begin
    j := reader.find(fexclude[i]);
    if j <> -1 then
      if reader[j]^.attributes and fadirectory = fadirectory then
        fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  end;
  fexclude.add(filename);

  showmessage(format(gmrestoreitems, [#13]));
  if fnodelete = false then
    for i := scan.count - 1 downto 0 do
    begin
      j := reader.find(scan[i]);
      if (j = -1) or
         (filenamematch(reader[j]^.fullname, finclude) = false) or
         (filenamematch(reader[j]^.fullname, fexclude) = true) then
        if directoryexists(scan[i]) = true then
          removedir(scan[i])
        else
          deletefile(scan[i]);
    end;

  for i := reader.count - 1 downto 0 do
  begin
    p := reader[i];
    if filenamematch(p^.fullname, finclude) = true then
      if filenamematch(p^.fullname, fexclude) = false then
      begin
        j := scan.find(p^.fullname);
        if j = -1 then
        begin
          reader.extract(i);
          inc(size, p^.size);
        end else
        if filegettimeutc(scan[j]) <> p^.timeutc then
        begin
          reader.extract(i);
          inc(size, p^.size);
        end;
      end;
  end;
  freeandnil(reader);
  freeandnil(stream);
  freeandnil(scan);
  showmessage(format(gmrestorefinish, [#13, size, lineending]));
end;

procedure tgulpapplication.check(const filename: rawbytestring);
var
  i:      longint;
  nul:    tstream;
  stream: tstream;
  reader: tgulpreader;
begin
  showmessage(gulpdescription);
  showmessage(format(gmcheck, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream    := tfilestream.create(filename, fmopenread);
  reader := tgulpreader.create(stream);
  reader.load;

  showmessage(format(gmcheckitems, [#13]));
  nul := tnulstream.create;
  for i := 0 to reader.count - 1 do
    if gfsize in reader[i]^.flags then
      reader.extract(i, nul);

  freeandnil(reader);
  freeandnil(stream);
  freeandnil(nul);
  showmessage(format(gmcheckfinish, [#13, filegetsize(filename), lineending]));
end;

procedure tgulpapplication.fix(const filename: rawbytestring);
var
  p:      pgulpitem;
  offset: int64 = 0;
  size:   int64 = 0;
  stream: tstream;
  reader: tgulpreader;
begin
  showmessage(gulpdescription);
  showmessage(format(gmfix, [filename, lineending]));
  showmessage(format(gmfixitems, [#13]));
  stream := tfilestream.create(filename, fmopenreadwrite);
  reader := tgulpreader.create(stream);

  p := new(pgulpitem);
  try
    while true do
    begin
      reader.read(p);
      offset := max(offset, p^.ending);
      if gfclose in p^.flags then
      begin
        offset := max(offset, stream.seek(0, socurrent));
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
  freeandnil(reader);
  freeandnil(stream);
  showmessage(format(gmfixfinish, [#13, offset, lineending]));
end;

procedure tgulpapplication.purge(const filename: rawbytestring);
var
  i:       longint;
  p:       pgulpitem;
  size:    int64 = 0;
  stream:  tstream;
  tmp:     tstream;
  tmpname: rawbytestring;
  reader:  tgulpreader;
  writer:  tgulpwriter;
begin
  showmessage(gulpdescription);
  showmessage(format(gmpurge, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream := tfilestream.create(filename, fmopenread);
  reader := tgulpreader.create(stream);
  reader.load($FFFFFFFF);

  showmessage(format(gmmoveitems, [#13]));
  tmpname := gettempfilename(extractfiledir(filename), '');
  tmp     := tfilestream.create(tmpname, fmcreate);
  writer  := tgulpwriter.create(tmp);
  if reader.count > 0 then
  begin
    for i := 0 to reader.count - 1 do
      writer.write(reader[i]);

    for i := 0 to reader.count - 1 do
    begin
      p := reader[i];
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
    for i := 0 to reader.count - 1 do
    begin
      p := reader[i];
      if i = reader.count - 1 then
        system.include(p^.flags, gfclose)
      else
        system.exclude(p^.flags,gfclose);
      writer.write(p);
    end;
    tmp.seek(0, soend);
  end;
  size := stream.size - tmp.size;

  freeandnil(reader);
  freeandnil(writer);
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
  count:  longint = 0;
  i, j:   longint;
  p:      pgulpitem;
  stream: tstream;
  reader: tgulpreader;
begin
  showmessage(gulpdescription);
  showmessage(format(gmlist, [filename, lineending]));
  showmessage(format(gmscanningarchive, [#13]));
  stream := tfilestream.create(filename, fmopenread);
  reader := tgulpreader.create(stream);
  if funtilversion > 0 then
    reader.load(funtilversion)
  else
    reader.load;

  for i := finclude.count - 1 downto 0 do
  begin
    j := reader.find(finclude[i]);
    if j <> -1 then
      if reader[j]^.attributes and fadirectory = fadirectory then
        finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  end;
  if finclude.count = 0 then
    finclude.add('*');

  for i := fexclude.count - 1 downto 0 do
  begin
    j := reader.find(fexclude[i]);
    if j <> -1 then
      if reader[j]^.attributes and fadirectory = fadirectory then
        fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  end;

  showmessage(format(gmlistitems, [#13, lineending]));
  for i := 0 to reader.count - 1 do
  begin
    p := reader[i];
    j := max(j, p^.version);
    if filenamematch(p^.fullname, finclude) = true then
      if filenamematch(p^.fullname, fexclude) = false then
      begin
        showitem(p);
        inc(count);
      end;
  end;
  freeandnil(reader);
  freeandnil(stream);
  showmessage(format(gmlistfinish, [count, lineending]));
  showmessage(format(gmlistlastversion, [j, lineending]));
end;

end.
