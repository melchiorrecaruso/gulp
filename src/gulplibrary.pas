{ Description: Main library unit.

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

unit gulplibrary;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF LINUX}
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
  { gulp list }

  tgulplist = specialize tgenericlist<pgulpitem>;

  { gulp library }

  tgulplibrary = class(tgulpinterface)
  protected
    function  libmove   (instream,  outstream: tstream; size: int64): tsha1digest;
    procedure librestore(instream:  tstream; p:    pgulpitem);
    procedure librestore(                    p:    pgulpitem);
    procedure libwrite  (outstream: tstream; p:    pgulpitem);
    procedure libwrite  (outstream: tstream; list: tgulplist);
    function  libread   (instream:  tstream; p:    pgulpitem): pgulpitem;
    procedure libread   (instream:  tstream; list: tgulplist);
    procedure libread   (instream:  tstream; list: tgulplist; untilversion: longword);
    function  libnew1   (const filename: rawbytestring; stimeutc: tdatetime): pgulpitem;
    function  libnew2   (const filename: rawbytestring; stimeutc: tdatetime): pgulpitem;
    procedure libappend (list: tgulplist; p: pgulpitem);
    function  libfind   (list: tgulplist; const filename: rawbytestring): longint;
    procedure libclear  (list: tgulplist);
  end;

  { gulp application }

  tgulpapplication = class(tgulplibrary)
  private
    fexclude: trawbytestringlist;
    fexcludeattr: longword;
    fexcludemode: longword;
    finclude: trawbytestringlist;
    fforcepath: boolean;
    fnodelete: boolean;
    fonlyversion: longword;
    fstimeutc: tdatetime;
    fterminated: boolean;
    funtilversion: longword;
    function isexcluded(const item: pgulpitem): boolean; overload;
    function isincluded(const item: pgulpitem): boolean; overload;
    function isexcluded(const filename: rawbytestring): boolean; overload;
    function isincluded(const filename: rawbytestring): boolean; overload;
  public
    constructor create;
    destructor destroy; override;
    procedure sync(const filename: rawbytestring);
    procedure restore(const filename: rawbytestring);
    procedure check(const filename: rawbytestring);
    procedure fix(const filename: rawbytestring);
    procedure purge(const filename: rawbytestring);
    procedure list(const filename: rawbytestring);
    procedure reset;
  public
    property exclude: trawbytestringlist read fexclude;
    property excludeattr: longword read fexcludeattr write fexcludeattr;
    property excludemode: longword read fexcludemode write fexcludemode;
    property include: trawbytestringlist read finclude;
    property forcepath: boolean read fforcepath write fforcepath;
    property nodelete: boolean read fnodelete write fnodelete;
    property onlyversion: longword read fonlyversion write fonlyversion;
    property terminated: boolean read fterminated;
    property untilversion: longword read funtilversion write funtilversion;
  end;


implementation

uses
  dateutils,
  gulpmessages,
  gulpscanner,
  gulpstream;

const
  gulpmarker : tsha1digest = (106,144,157,18,207,10,
    68,233,72,6,60,107,74,16,223,55,134,75,20,207);

  gulpdescription =
    'GULP v0.4 journaling archiver, copyright (c) 2014-2017 Melchiorre Caruso.'
    + lineending +
    'GULP archiver for user-level incremental backups with rollback capability.';

{ internal routines }

function itemclear(p: pgulpitem): pgulpitem;
begin
  p^.name      := '';
  p^.flags     := [];
  p^.mtime     := 0.0;
  p^.stime     := 0.0;
  p^.attr      := [];
  p^.mode      := [];
  p^.size      := 0;
  p^.linkname  := '';
  p^.userid    := 0;
  p^.groupid   := 0;
  p^.username  := '';
  p^.groupname := '';
  p^.comment   := '';
  p^.offset1   := 0;
  p^.offset2   := 0;
  fillchar(p^.checksum1, sizeof(tsha1digest), 0);
  fillchar(p^.checksum2, sizeof(tsha1digest), 0);
  p^.version   := 0;
  result       := p;
end;

function itemgetdigest(p: pgulpitem): tsha1digest;
var
  context: tsha1context;
begin
  sha1init  (context);
  sha1update(context, pointer(p^.name)^, length(p^.name));

  sha1update(context, p^.flags, sizeof(p^.flags));
  sha1update(context, p^.mtime, sizeof(p^.mtime));
  sha1update(context, p^.stime, sizeof(p^.stime));
  sha1update(context, p^.attr,  sizeof(p^.attr));
  sha1update(context, p^.mode,  sizeof(p^.mode));
  sha1update(context, p^.size,  sizeof(p^.size));

  sha1update(context, pointer(p^.linkname)^, length(p^.linkname));

  sha1update(context, p^.userid,  sizeof(p^.userid));
  sha1update(context, p^.groupid, sizeof(p^.groupid));

  sha1update(context, pointer(p^.username)^,  length(p^.username));
  sha1update(context, pointer(p^.groupname)^, length(p^.groupname));
  sha1update(context, pointer(p^.comment)^,   length(p^.comment));

  sha1update(context, p^.offset1, sizeof(p^.offset1));
  sha1update(context, p^.offset2, sizeof(p^.offset2));

  sha1final (context, result);
end;

function itemgetsize(p: pgulpitem): int64;
begin
  result :=   sizeof(tsha1digest);

  inc(result, sizeof(longint) + length(p^.name));

  inc(result, sizeof(p^.flags));
  inc(result, sizeof(p^.mtime));
  inc(result, sizeof(p^.stime));
  inc(result, sizeof(p^.attr));
  inc(result, sizeof(p^.mode));
  inc(result, sizeof(p^.size));

  inc(result, sizeof(longint) + length(p^.linkname));

  inc(result, sizeof(p^.userid));
  inc(result, sizeof(p^.groupid));

  inc(result, sizeof(longint) + length(p^.username));
  inc(result, sizeof(longint) + length(p^.groupname));
  inc(result, sizeof(longint) + length(p^.comment));

  inc(result, sizeof(p^.offset1));
  inc(result, sizeof(p^.offset2));
  inc(result, sizeof(tsha1digest));
  inc(result, sizeof(tsha1digest));
end;

function compare40(p1, p2: pgulpitem): longint;
begin
  {$IFDEF LINUX}
  result := ansicomparestr(p1^.name, p2^.name);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := ansicomparetext(p1^.name, p2^.name);
  {$ELSE}
  ...
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
  result := compare40(p1, p2);

  if result = 0 then
    if (gfdelete in p1^.flags) and (gfadd in p2^.flags) then
      result := -1
    else
    if (gfdelete in p2^.flags) and (gfadd in p1^.flags) then
      result := 1;
end;

{ gulp library class }

function tgulplibrary.libmove(instream, outstream: tstream; size: int64): tsha1digest;
var
  buffer:  array[0..$FFFF] of byte;
  context: tsha1context;
  readed:  longint;
begin
  sha1init(context);
  while size > 0 do
  begin
    readed := instream.read(buffer, min(sizeof(buffer), size));
    if readed = 0 then
      raiseexception(gereadstream, '003012');
    sha1update(context, buffer, readed);
    outstream.write(buffer, readed);
    dec(size, readed);
  end;
  sha1final(context, result);
end;

procedure tgulplibrary.librestore(instream: tstream; p: pgulpitem);
var
  outpath:   rawbytestring;
  outstream: tstream;

  procedure librestorefile(instream: tstream; p: pgulpitem); inline;
  begin
    instream.seek(p^.offset1, sobeginning);
    try
      outstream := tfilestream.create(p^.name, fmcreate);
    except
      showwarning(format(geopenstream, [p^.name]));
      outstream := nil;
    end;

    if assigned(outstream) then
    begin
      if sha1match(libmove(instream, outstream,
        p^.offset2 - p^.offset1), p^.checksum2) = false then
        raiseexception(gechecksum, '003016');
      outstream.destroy;
    end;
  end;

begin
  outpath := extractfiledir(p^.name);
  if (outpath <> '') and (forcedirectories(outpath) = false) then
    raiseexception(gecreatepath, outpath);

  if issymlink(p^.attr) then
  begin
    {$IFDEF LINUX}
    gulpscanner.deleteany(p^.name);
    if setsymlink(p^.name, p^.linkname) <> 0 then
      showwarning(format(gerestorelink, [p^.name]));
    {$ELSE}
    {$IFDEF MSWINDOWS}
    gulpscanner.deleteany(p^.name);
    librestorefile(instream, p);
    {$ELSE}
    ...
    {$ENDIF}
    {$ENDIF}
  end else
  if isdirectory(p^.attr) then
  begin
    if issymlink(p^.name) then
      gulpscanner.deleteany(p^.name);

    if isdirectory(p^.name) = false then
    begin
      gulpscanner.deleteany(p^.name);
      if createdir(p^.name) = false then
        showwarning(format(gerestoredir, [p^.name]));
    end else
    begin
      {$IFDEF LINUX}
      if setmode(p^.name, s_irwxo or s_irwxg or s_irwxu) <> 0 then
        showwarning(format(gesetmode, [p^.name]));
      {$ELSE}
      {$IFDEF MSWINDOWS}
      if _setfileattr(p^.name, result and (fahidden or fasysfile or
        favolumeid or fadirectory or faarchive or fasymlink)) = -1 then
        showwarning(format(gesetmode, [p^.name]));
      {$ELSE}
      ...
      {$ENDIF}
      {$ENDIF}
    end;

  end else
  begin
    gulpscanner.deleteany(p^.name);
    librestorefile(instream, p);
  end;
end;

procedure tgulplibrary.librestore(p: pgulpitem);
begin
  {$IFDEF LINUX}
  if setuserid(p^.name, p^.userid) <> 0 then
    showwarning(format(gesetuserid, [p^.name]));

  if setgroupid(p^.name, p^.groupid) <> 0 then
    showwarning(format(gesetgroupid, [p^.name]));

  if settimeutc(p^.name, p^.mtime) <> 0 then
    showwarning(format(gesetdatetime, [p^.name]));

  if setmode(p^.name, l2smode(p^.mode)) <> 0 then
    showwarning(format(gesetmode, [p^.name]));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  if setattributes(p^.name, p^.attrs) <> 0 then
    showwarning(format(gesetattributes, [p^.name]));

  if settimeutc03(p^.name, p^.mtime) <> 0 then
    showwarning(format(gesetdatetime, [p^.name]));
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

procedure tgulplibrary.libwrite(outstream: tstream; p: pgulpitem);
begin
  outstream.write(gulpmarker, sizeof(tsha1digest));

  outstream.writeansistring(p^.name);

  outstream.write(p^.flags, sizeof(p^.flags));
  outstream.write(p^.mtime, sizeof(p^.mtime));
  outstream.write(p^.stime, sizeof(p^.stime));
  outstream.write(p^.attr,  sizeof(p^.attr));
  outstream.write(p^.mode,  sizeof(p^.mode));
  outstream.write(p^.size,  sizeof(p^.size));

  outstream.writeansistring(p^.linkname);

  outstream.write(p^.userid,  sizeof(p^.userid));
  outstream.write(p^.groupid, sizeof(p^.groupid));

  outstream.writeansistring(p^.username);
  outstream.writeansistring(p^.groupname);
  outstream.writeansistring(p^.comment);

  outstream.write(p^.offset1,   sizeof(p^.offset1));
  outstream.write(p^.offset2,   sizeof(p^.offset2));
  outstream.write(p^.checksum1, sizeof(tsha1digest));
  outstream.write(p^.checksum2, sizeof(tsha1digest));
end;

procedure tgulplibrary.libwrite(outstream: tstream; list: tgulplist);
var
  i:        longint;
  instream: tstream;
  size:     int64;
begin
  if list.count > 0 then
  begin
    size := outstream.seek(outstream.size, sobeginning);
    for i := 0 to list.count - 1 do
      libwrite(outstream, list[i]);

    for i := 0 to list.count - 1 do
      if gfadd in list[i]^.flags then
      begin
        {$IFDEF LINUX}
        if issymlink(list[i]^.attr) then
        begin
          list[i]^.linkname := getsymlink(list[i]^.name);
        end else
        {$ELSE}
        {$IFDEF MSWINDOWS}
        {$ELSE}
        ...
        {$ENDIF}
        {$ENDIF}
        if isdirectory(list[i]^.attr) then
        begin
           // nothing to do
        end else
        begin
          list[i]^.offset1 := outstream.position;
          try
            instream := tfilestream.create(
              list[i]^.name, fmopenread or fmsharedenywrite);
          except
            showwarning(format(geopenstream, [list[i]^.name]));
            instream := nil;
          end;

          if assigned(instream) then
          begin
            list[i]^.size      := instream.size;
            list[i]^.checksum2 := libmove(instream, outstream, list[i]^.size);
            instream.destroy;
          end;
          list[i]^.offset2 := outstream.position;
        end;
      end;

    outstream.seek(size, sobeginning);
    include(list[list.count - 1]^.flags, gfclose);
    for i := 0 to list.count - 1 do
    begin
      list[i]^.checksum1 := itemgetdigest(list[i]);
      libwrite(outstream, list[i]);
    end;
    outstream.seek(outstream.size, sobeginning);
  end;
end;

function tgulplibrary.libread(instream: tstream; p: pgulpitem): pgulpitem;
var
  marker: tsha1digest;
begin
  result := itemclear(p);

  fillbyte(marker, sizeof(tsha1digest), 0);
  instream.read(marker, sizeof(tsha1digest));
  if sha1match(marker, gulpmarker) = false then
    raiseexception(gewrongmarker, '003002');

  p^.name := instream.readansistring;

  instream.read(p^.flags, sizeof(p^.flags));
  instream.read(p^.mtime, sizeof(p^.mtime));
  instream.read(p^.stime, sizeof(p^.stime));
  instream.read(p^.attr,  sizeof(p^.attr));
  instream.read(p^.mode,  sizeof(p^.mode));
  instream.read(p^.size,  sizeof(p^.size));

  p^.linkname := instream.readansistring;

  instream.read(p^.userid,  sizeof(p^.userid));
  instream.read(p^.groupid, sizeof(p^.groupid));

  p^.username  := instream.readansistring;
  p^.groupname := instream.readansistring;
  p^.comment   := instream.readansistring;

  instream.read(p^.offset1,   sizeof(p^.offset1));
  instream.read(p^.offset2,   sizeof(p^.offset2));
  instream.read(p^.checksum1, sizeof(tsha1digest));
  instream.read(p^.checksum2, sizeof(tsha1digest));

  if (([] = p^.flags)) or (([gfclose] = p^.flags)) then
    raiseexception(gewrongflag, '004001');
  if ((gfdelete in p^.flags)) and ((gfadd in p^.flags)) then
    raiseexception(gewrongflag, '003003');

  //if (p^.offset2 - p^.offset1 <> p^.size) then
  //  raiseexception(gebrokenarchive, '003004');
  if sha1match(p^.checksum1, itemgetdigest(p)) = false then
    raiseexception(gebrokenarchive, '003007');

  dodirseparators(p^.linkname);
  dodirseparators(p^.name);
end;

procedure tgulplibrary.libread(instream: tstream; list: tgulplist);
var
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  size := instream.size;
          instream.seek(0, sobeginning);
  p    := new(pgulpitem);
  while offset < size do
  begin
    libread(instream, p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.offset2 - p^.offset1);

    if gfclose in p^.flags then
    begin
      if instream.seek(offset, sobeginning) <> offset then
        raiseexception(gereadarchive, '003008');
      inc(version);
    end;

    if list.add(p) = -1 then
      raiseexception(geduplicates, '003009');
    p := new(pgulpitem);
  end;
  dispose(p);
  if offset <> size then
    raiseexception(gebrokenarchive, '003011');
end;

procedure tgulplibrary.libread(instream: tstream; list: tgulplist; untilversion: longword);
var
  i:       longint;
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  size := instream.size;
          instream.seek(0, sobeginning);
  p    := new(pgulpitem);
  while offset < size do
  begin
    libread(instream, p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.offset2 - p^.offset1);

    if gfclose in p^.flags then
    begin
      if instream.seek(offset, sobeginning) <> offset then
        raiseexception(gereadarchive, '003018');
      inc(version);
    end;

    if p^.version <= untilversion then
    begin

      if (gfdelete in p^.flags) or
         (gfadd    in p^.flags) then
      begin
        i := list.find(p);
        if i <> -1 then
        begin
          dispose(list[i]);
          list.delete(i);
        end;
      end;

      if (gfadd in p^.flags) then
      begin
        if list.add(p) = -1 then
          raiseexception(geduplicates, '003010');
        p := new(pgulpitem);
      end;

    end;
  end;
  dispose(p);

  if offset <> size then
    raiseexception(gebrokenarchive, '003014');
end;

function tgulplibrary.libnew1(const filename: rawbytestring; stimeutc: tdatetime): pgulpitem;
begin
  result        := itemclear(new(pgulpitem));
  result^.name  := filename;
  result^.flags := [gfdelete];
  result^.stime := stimeutc;
end;

function tgulplibrary.libnew2(const filename: rawbytestring; stimeutc: tdatetime): pgulpitem;
begin
  result           := itemclear(new(pgulpitem));
  result^.name     := filename;
  result^.flags    := [gfadd];
  result^.stime    := stimeutc;
  result^.mtime    := gettimeutc(filename);
  result^.attr     := s2lattr(getattr(filename));
  result^.mode     := s2lmode(getmode(filename));
//result^.size     := getsize(filename);
//result^.linkname := getsymlink(filename);
  result^.userid   := getuserid(filename);
  result^.groupid  := getgroupid(filename);
end;

procedure tgulplibrary.libappend(list: tgulplist; p: pgulpitem);
begin
  if list.add(p) = -1 then
    raiseexception(geduplicates, '003019');
end;

function tgulplibrary.libfind(list: tgulplist; const filename: rawbytestring): longint;
var
  p: pgulpitem;
begin
  p        := itemclear(new(pgulpitem));
  p^.name  := filename;
  p^.flags := [gfdelete];
  result   := list.find(p);
  dispose(p);
end;

procedure tgulplibrary.libclear(list: tgulplist);
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
  fexclude := trawbytestringlist.create;
  finclude := trawbytestringlist.create;
  reset;
end;

destructor tgulpapplication.destroy;
begin
  reset;
  fexclude.destroy;
  finclude.destroy;
  inherited destroy;
end;

procedure tgulpapplication.reset;
begin
  fexclude.clear;
  fexcludeattr  := 0;
  fexcludemode  := 0;
  finclude.clear;
  fforcepath    := false;
  fnodelete     := false;
  fonlyversion  := 0;
  fstimeutc     := localtime2universal(now);
  fterminated   := true;
  funtilversion := $ffffffff;
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
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmsync, [filename]));
  if fileexists(filename) = TRUE then
    stream := tfilestream.create(filename, fmopenreadwrite or fmsharedenywrite)
  else
    stream := tfilestream.create(filename, fmcreate);
  list1    := tgulplist.create(@compare40);
  libread(stream, list1, $ffffffff);
  size := stream.seek(stream.size, sobeginning);

  scan := tscanner.create;
  for i := finclude.count - 1 downto 0 do
    if directoryexists(finclude[i]) = true then
      finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  if finclude.count = 0 then
    finclude.add('*');
  for i := finclude.count - 1 downto 0 do
  begin
    if (fforcepath = FALSE) and (isabsolutepath(finclude[i]) = TRUE) then
      raiseexception(geabsolutepath, '003021');
    scan.add(finclude[i]);
  end;

  for i := fexclude.count - 1 downto 0 do
    if directoryexists(fexclude[i]) = true then
      fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  fexclude.add(filename);
  for i := scan.count - 1 downto 0 do
    if isexcluded(scan[i]) then
      scan.delete(i);

  list2 := tgulplist.create(@compare42);
  if fnodelete = false then
    for i := 0 to list1.count - 1 do
      if scan.find(list1[i]^.name) = -1 then
      begin
        showmessage2(format(gmdeleteitem, [list1[i]^.name]));
        libappend(list2, libnew1(list1[i]^.name, fstimeutc));
      end;

  for i := 0 to scan.count - 1 do
  begin
    j := libfind(list1, scan[i]);
    if j = -1 then
    begin;
      showmessage2(format(gmsyncitem, [scan[i]]));
      libappend(list2, libnew2(scan[i], fstimeutc));
    end else
    if gettimeutc(scan[i]) <> list1[j]^.mtime then
    begin
      showmessage2(format(gmsyncitem, [scan[i]]));
      libappend(list2, libnew1(scan[i], fstimeutc));
      libappend(list2, libnew2(scan[i], fstimeutc));
    end;
  end;
  libwrite(stream, list2);

  libclear(list1);
  libclear(list2);
  freeandnil(list1);
  freeandnil(list2);
  freeandnil(stream);
  freeandnil(scan);

  showmessage1(format(gmsyncfinish, [getsize(filename) - size]));
  fterminated := true;
end;

procedure tgulpapplication.restore(const filename: rawbytestring);
var
  i, j:   longint;
  list1:  tgulplist;
  list2:  tgulplist;
  p:      pgulpitem;
  scan:   tscanner;
  size:   int64 = 0;
  stream: tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmrestore, [filename]));
  stream := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  list1  := tgulplist.create(@compare40);
  list2  := tgulplist.create(@compare40);
  if funtilversion = 0 then
    funtilversion := $ffffffff;
  libread(stream, list1, funtilversion);

  scan := tscanner.create;
  scan.add('*');
  for i := finclude.count - 1 downto 0 do
  begin
    j := libfind(list1, finclude[i]);
    if j <> -1 then
      if isdirectory(list1[j]^.attr) then
        finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  end;
  if finclude.count = 0 then
    finclude.add('*');

  for i := fexclude.count - 1 downto 0 do
  begin
    j := libfind(list1, fexclude[i]);
    if j <> -1 then
      if isdirectory(list1[j]^.attr) then
        fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  end;
  fexclude.add(filename);

  if fnodelete = false then
    for i := scan.count - 1 downto 0 do
    begin
      j := libfind(list1, scan[i]);
      if (j = -1) or (isincluded(list1[j]) = false) or isexcluded(list1[j]) then
      begin
        showmessage2(format(gmdeleteitem, [scan[i]]));
        deleteany(scan[i]);
      end;
    end;

  for i := 0 to list1.count - 1 do
  begin
    p := list1[i];
    if isincluded(p) and (isexcluded(p) = false) then
    begin
      j := scan.find(p^.name);
      if (j = -1) or (gettimeutc(scan[j]) <> p^.mtime) then
      begin
        showmessage2(format(gmrestoreitem, [p^.name]));
        librestore(stream, p);
        inc(size, p^.offset2 - p^.offset1);
        list2.add(p);
      end;
    end;
  end;

  while list2.count > 0 do
  begin
    librestore(list2[0]);
    list2.delete(0);
  end;
  freeandnil(list2);

  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  freeandnil(scan);

  showmessage1(format(gmrestorefinish, [size]));
  fterminated := true;
end;

procedure tgulpapplication.check(const filename: rawbytestring);
var
  i:      longint;
  list1:  tgulplist;
  nul:    tstream;
  stream: tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmcheck, [filename]));
  stream := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  list1  := tgulplist.create(@compare41);
  libread(stream, list1);

  nul := tnulstream.create;
  for i := 0 to list1.count - 1 do
  begin
    showmessage2(format(gmcheckitem, [list1[i]^.name]));
    if (list1[i]^.offset2 > list1[i]^.offset1) then
    begin
      stream.seek(list1[i]^.offset1, sobeginning);
      libmove(stream, nul, list1[i]^.offset2 - list1[i]^.offset1);
    end;
  end;

  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  freeandnil(nul);

  showmessage1(format(gmcheckfinish, [getsize(filename)]));
  fterminated := true;
end;

procedure tgulpapplication.fix(const filename: rawbytestring);
var
  p:      pgulpitem;
  offset: int64 = 0;
  size:   int64 = 0;
  stream: tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmfix, [filename]));
  stream := tfilestream.create(filename, fmopenreadwrite or fmsharedenywrite);

  p := new(pgulpitem);
  try
    while true do
    begin
      libread(stream, p);
      inc(offset, itemgetsize(p));
      inc(offset, p^.offset2 - p^.offset1);

      showmessage2(format(gmfixitem, [p^.name]));
      if gfclose in p^.flags then
      begin
        if stream.seek(offset, sobeginning) <> offset then
          raiseexception(gereadarchive, '003020');
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
    raise exception.createfmt(gereadarchive, ['003015']);
  freeandnil(stream);

  showmessage1(format(gmfixfinish, [offset]));
  fterminated := true;
end;

procedure tgulpapplication.purge(const filename: rawbytestring);
var
  i:       longint;
  list1:   tgulplist;
  p:       pgulpitem;
  size:    int64;
  stream:  tstream;
  tmp:     tstream;
  tmpname: rawbytestring;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmpurge, [filename]));
  stream := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  list1  := tgulplist.create(@compare40);
  libread(stream, list1, $ffffffff);

  tmpname := gettempfilename(extractfiledir(filename), '');
  tmp     := tfilestream.create(tmpname, fmcreate);
  if list1.count > 0 then
  begin
    for i := 0 to list1.count - 1 do
      libwrite(tmp, list1[i]);

    for i := 0 to list1.count - 1 do
    begin
      p := list1[i];
      system.exclude(p^.flags, gfclose);
      if (p^.offset2 > p^.offset1) then
      begin
        showmessage2(format(gmmoveitem, [p^.name]));
        stream.seek(p^.offset1, sobeginning);
        size := p^.offset2 - p^.offset1;

        p^.offset1 := tmp.position;
        tmp.copyfrom(stream, size);
        p^.offset2 := tmp.position;
      end;
    end;

    tmp.seek(0, sobeginning);
    system.include(list1[list1.count - 1]^.flags, gfclose);
    for i := 0 to list1.count - 1 do
    begin
      list1[i]^.checksum1 := itemgetdigest(list1[i]);
      libwrite(tmp, list1[i]);
    end;
    tmp.seek(tmp.size, sobeginning);
  end;
  size := stream.size - tmp.size;

  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);
  freeandnil(tmp);
  if deletefile(filename) = false then
    raiseexception(gedeletefile, filename)
  else
  if renamefile(tmpname, filename) = false then
    raiseexception(gerenamefile, tmpname);

  showmessage1(format(gmpurgefinish, [size]));
  fterminated := true;
end;

procedure tgulpapplication.list(const filename: rawbytestring);
var
  c:      longint = 0;
  i, j:   longint;
  list1:  tgulplist;
  p:      pgulpitem;
  stream: tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmlist, [filename]));
  stream := tfilestream.create(filename, fmopenread or fmsharedenywrite);

  if fonlyversion > 0 then
  begin
    list1 := tgulplist.create(@compare41);
    libread(stream, list1);
  end else
    if funtilversion > 0 then
    begin
      list1 := tgulplist.create(@compare40);
      libread(stream, list1, funtilversion);
    end else
    begin
      list1 := tgulplist.create(@compare41);
      libread(stream, list1);
    end;

  for i := finclude.count - 1 downto 0 do
  begin
    j := libfind(list1, finclude[i]);
    if j <> -1 then
      if isdirectory(list1[j]^.attr) then
        finclude.add(includetrailingpathdelimiter(finclude[i]) + '*');
  end;
  if finclude.count = 0 then
    finclude.add('*');

  for i := fexclude.count - 1 downto 0 do
  begin
    j := libfind(list1, fexclude[i]);
    if j <> -1 then
      if isdirectory(list1[j]^.attr) then
        fexclude.add(includetrailingpathdelimiter(fexclude[i]) + '*');
  end;

  c := 0;
  j := 0;
  for i := 0 to list1.count - 1 do
  begin
    p := list1[i];
    j := max(j, p^.version);
    if isincluded(p) and (isexcluded(p) = false) then
      if fonlyversion = 0 then
      begin
        showitem(tobject(p));
        inc(c);
      end else
        if p^.version = fonlyversion then
        begin
          showitem(p);
          inc(c);
        end;
  end;

  libclear(list1);
  freeandnil(list1);
  freeandnil(stream);

  showmessage1(format(gmlistfinish, [c]));
  showmessage1(format(gmlistlastversion, [j]));
  fterminated := true;
end;

function tgulpapplication.isincluded(const filename: rawbytestring): boolean;
begin
  result := filenamematch(filename, finclude);
end;

function tgulpapplication.isincluded(const item: pgulpitem): boolean;
begin
  result := filenamematch(item^.name, finclude);
end;

function tgulpapplication.isexcluded(const item: pgulpitem): boolean;
begin
  result := filenamematch(item^.name, fexclude)                 or
            (l2sattr(item^.attr) and fexcludeattr <> 0) or
            (l2smode(item^.mode) and fexcludemode <> 0);
end;

function tgulpapplication.isexcluded(const filename: rawbytestring): boolean;
begin
  result := filenamematch(filename, fexclude)                 or
            (getattr(filename) and fexcludeattr <> 0) or
            (getmode(filename) and fexcludemode <> 0);
end;

end.
