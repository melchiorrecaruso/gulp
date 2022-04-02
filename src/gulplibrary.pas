{ Description: Main library unit.

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
  gulpstream,
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
    function  libmove   (instrm,  outstrm: tstream; size: int64): tsha1digest;
    procedure librestore(instrm:  tstream; p:    pgulpitem);
    procedure librestore(                  p:    pgulpitem);
    procedure libwrite  (outstrm: tstream; p:    pgulpitem);
    procedure libwrite  (outstrm: tstream; list: tgulplist);
    function  libread   (instrm:  tstream; p:    pgulpitem): pgulpitem;
    procedure libread   (instrm:  tstream; list: tgulplist);
    procedure libread   (instrm:  tstream; list: tgulplist; untilversion: longword);
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
    fpipe: tmemorystream;
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
    procedure view(const filename: rawbytestring);
    procedure reset;
  public
    property exclude: trawbytestringlist read fexclude;
    property excludeattr: longword read fexcludeattr write fexcludeattr;
    property excludemode: longword read fexcludemode write fexcludemode;
    property include: trawbytestringlist read finclude;
    property forcepath: boolean read fforcepath write fforcepath;
    property nodelete: boolean read fnodelete write fnodelete;
    property onlyversion: longword read fonlyversion write fonlyversion;
    property pipe: tmemorystream read fpipe write fpipe;
    property terminated: boolean read fterminated;
    property untilversion: longword read funtilversion write funtilversion;
  end;


implementation

uses
  dateutils,
  gulpmessages,
  gulpscanner;

const
  gulpmarker : tsha1digest = (106,144,157,18,207,10,
    68,233,72,6,60,107,74,16,223,55,134,75,20,207);

  nullmarker : tsha1digest = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

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
  p^.checksum1 := nullmarker;
  p^.checksum2 := nullmarker;
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

  sha1update(context, p^.offset1,   sizeof(p^.offset1));
  sha1update(context, p^.offset2,   sizeof(p^.offset2));
//sha1update(context, p^.checksum1, sizeof(p^.checksum1));
//sha1update(context, p^.checksum2, sizeof(p^.checksum2));

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
  inc(result, sizeof(p^.checksum1));
  inc(result, sizeof(p^.checksum2));
end;

function compare40(p1, p2: pgulpitem): longint;
begin
  {$IFDEF LINUX}
  result := comparestr(p1^.name, p2^.name);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := comparetext(p1^.name, p2^.name);
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

function tgulplibrary.libmove(instrm, outstrm: tstream; size: int64): tsha1digest;
var
  buffer:  array[0..$FFFF] of byte;
  context: tsha1context;
  readed:  longint;
begin
  sha1init(context);
  while size > 0 do
  begin
    readed := instrm.read(buffer, min(sizeof(buffer), size));
    if readed = 0 then
      raiseexception(gereadstream, '003012');
    sha1update(context, buffer, readed);
    outstrm.write(buffer, readed);
    dec(size, readed);
  end;
  sha1final(context, result);
end;

procedure tgulplibrary.librestore(instrm: tstream; p: pgulpitem);
var
  outpath: rawbytestring;
  outsize: int64;

  procedure librestorefile(instrm: tstream; p: pgulpitem); inline;
  var
    outstrm: tstream;
  begin
    instrm.seek(p^.offset1, sobeginning);
    try
      outstrm := tfilestream.create(p^.name, fmcreate);
    except
      showwarning(format(geopenstream, [p^.name]));
      outstrm := nil;
    end;

    if assigned(outstrm) then
    begin
      outsize := p^.offset2 - p^.offset1;
      if not sha1match(p^.checksum2, libmove(instrm, outstrm, outsize)) then
        raiseexception(gechecksum, '003016');
      outstrm.destroy;
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
    librestorefile(instrm, p);
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
      if setattr(p^.name, l2sattr(p^.attr)) = -1 then
        showwarning(format(gesetmode, [p^.name]));
      {$ELSE}
      ...
      {$ENDIF}
      {$ENDIF}
    end;

  end else
  begin
    gulpscanner.deleteany(p^.name);
    librestorefile(instrm, p);
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
  if setattr(p^.name, l2sattr(p^.attr)) <> 0 then
    showwarning(format(gesetattributes, [p^.name]));

  if settimeutc(p^.name, p^.mtime) <> 0 then
    showwarning(format(gesetdatetime, [p^.name]));
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

procedure tgulplibrary.libwrite(outstrm: tstream; p: pgulpitem);
begin
  outstrm.write(gulpmarker, sizeof(gulpmarker));

  outstrm.writeansistring(p^.name);

  outstrm.write(p^.flags, sizeof(p^.flags));
  outstrm.write(p^.mtime, sizeof(p^.mtime));
  outstrm.write(p^.stime, sizeof(p^.stime));
  outstrm.write(p^.attr,  sizeof(p^.attr));
  outstrm.write(p^.mode,  sizeof(p^.mode));
  outstrm.write(p^.size,  sizeof(p^.size));

  outstrm.writeansistring(p^.linkname);

  outstrm.write(p^.userid,  sizeof(p^.userid));
  outstrm.write(p^.groupid, sizeof(p^.groupid));

  outstrm.writeansistring(p^.username);
  outstrm.writeansistring(p^.groupname);
  outstrm.writeansistring(p^.comment);

  outstrm.write(p^.offset1,   sizeof(p^.offset1));
  outstrm.write(p^.offset2,   sizeof(p^.offset2));
  outstrm.write(p^.checksum1, sizeof(p^.checksum1));
  outstrm.write(p^.checksum2, sizeof(p^.checksum2));
end;

procedure tgulplibrary.libwrite(outstrm: tstream; list: tgulplist);
var
  i:      longint;
  instrm: tstream;
  size:   int64;
begin
  if list.count > 0 then
  begin
    size := outstrm.seek(0, soend);
    for i := 0 to list.count - 1 do
      libwrite(outstrm, list[i]);

    for i := 0 to list.count - 1 do
      if gfadd in list[i]^.flags then
      begin
        {$IFDEF LINUX}
        if issymlink(list[i]^.attr) = false then
          if isdirectory(list[i]^.attr) = false then
        {$ELSE}
        {$IFDEF MSWINDOWS}
          if isdirectory(list[i]^.attr) = false then
        {$ELSE}
        ...
        {$ENDIF}
        {$ENDIF}
        begin
          list[i]^.offset1 := outstrm.position;
          try
            instrm := tfilestream.create(list[i]^.name, fmopenread or fmsharedenywrite);
          except
            showwarning(format(geopenstream, [list[i]^.name]));
            instrm := nil;
          end;

          if assigned(instrm) then
          begin
            list[i]^.checksum2 := libmove(instrm, outstrm, list[i]^.size);
            instrm.destroy;
          end;
          list[i]^.offset2 := outstrm.position;
        end;
      end;

    outstrm.seek(size, sobeginning);
    include(list[list.count - 1]^.flags, gfclose);
    for i := 0 to list.count - 1 do
    begin
      list[i]^.checksum1 := itemgetdigest(list[i]);
      libwrite(outstrm, list[i]);
    end;
    outstrm.seek(0, soend);
  end;
end;

function tgulplibrary.libread(instrm: tstream; p: pgulpitem): pgulpitem;
var
  marker: tsha1digest;
begin
  result := itemclear(p);
  marker := nullmarker;
  instrm.read(marker, sizeof(marker));
  if not sha1match(marker, gulpmarker)then
    raiseexception(gewrongmarker, '003002');

  p^.name := instrm.readansistring;

  instrm.read(p^.flags, sizeof(p^.flags));
  instrm.read(p^.mtime, sizeof(p^.mtime));
  instrm.read(p^.stime, sizeof(p^.stime));
  instrm.read(p^.attr,  sizeof(p^.attr));
  instrm.read(p^.mode,  sizeof(p^.mode));
  instrm.read(p^.size,  sizeof(p^.size));

  p^.linkname := instrm.readansistring;

  instrm.read(p^.userid,  sizeof(p^.userid));
  instrm.read(p^.groupid, sizeof(p^.groupid));

  p^.username  := instrm.readansistring;
  p^.groupname := instrm.readansistring;
  p^.comment   := instrm.readansistring;

  instrm.read(p^.offset1,   sizeof(p^.offset1));
  instrm.read(p^.offset2,   sizeof(p^.offset2));
  instrm.read(p^.checksum1, sizeof(p^.checksum1));
  instrm.read(p^.checksum2, sizeof(p^.checksum2));

  if not sha1match(p^.checksum1, itemgetdigest(p)) then
    raiseexception(gebrokenarchive, '003007');

  if (([] = p^.flags)) or (([gfclose] = p^.flags)) then
    raiseexception(gewrongflag, '004001');
  if ((gfdelete in p^.flags)) and ((gfadd in p^.flags)) then
    raiseexception(gewrongflag, '003003');
  //if (p^.offset2 - p^.offset1 <> p^.size) then
  //  raiseexception(gebrokenarchive, '003004');

  dodirseparators(p^.linkname);
  dodirseparators(p^.name);
end;

procedure tgulplibrary.libread(instrm: tstream; list: tgulplist);
var
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  size := instrm.size;
          instrm.seek(0, sobeginning);
  p    := new(pgulpitem);
  while offset < size do
  begin
    libread(instrm, p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.offset2 - p^.offset1);

    if gfclose in p^.flags then
    begin
      if instrm.seek(offset, sobeginning) <> offset then
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

procedure tgulplibrary.libread(instrm: tstream; list: tgulplist; untilversion: longword);
var
  i:       longint;
  p:       pgulpitem;
  offset:  int64    = 0;
  size:    int64    = 0;
  version: longword = 1;
begin
  size := instrm.size;
          instrm.seek(0, sobeginning);
  p    := new(pgulpitem);
  while offset < size do
  begin
    libread(instrm, p)^.version := version;
    inc(offset, itemgetsize(p));
    inc(offset, p^.offset2 - p^.offset1);

    if gfclose in p^.flags then
    begin
      if instrm.seek(offset, sobeginning) <> offset then
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
  {$IFDEF LINUX}
  if issymlink(result^.attr) then
    result^.linkname := getsymlink(result^.name)
  else
    if isdirectory(result^.attr) then
      result^.size := 0
    else
      result^.size := getsize(filename);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  if isdirectory(result^.attr) then
    result^.size   := 0
  else
    result^.size   := getsize(filename);
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
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
  if assigned(fpipe) then
    fpipe.clear;
  fstimeutc     := localtime2universal(now);
  fterminated   := true;
  funtilversion := $ffffffff;
end;

procedure tgulpapplication.sync(const filename: rawbytestring);
var
  i, j:  longint;
  list1: tgulplist;
  list2: tgulplist;
  scan:  tscanner;
  size:  int64 = 0;
  strm:  tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmsync, [filename]));

  if fileexists(filename) = TRUE then
    strm := tfilestream.create(filename, fmopenreadwrite or fmsharedenywrite)
  else
    strm := tfilestream.create(filename, fmcreate);
  list1 := tgulplist.create(@compare40);
  libread(strm, list1, $ffffffff);
  size := strm.seek(0, soend);

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
    if comparedatetime(gettimeutc(scan[i]), list1[j]^.mtime) <> 0 then
    begin
      showmessage2(format(gmsyncitem, [scan[i]]));
      libappend(list2, libnew1(scan[i], fstimeutc));
      libappend(list2, libnew2(scan[i], fstimeutc));
    end;
  end;
  libwrite(strm, list2);

  libclear  (list1);
  libclear  (list2);
  freeandnil(list1);
  freeandnil(list2);
  freeandnil(scan);
  freeandnil(strm);

  showmessage1(format(gmsyncfinish, [getsize(filename) - size]));
  fterminated := true;
end;

procedure tgulpapplication.restore(const filename: rawbytestring);
var
  i, j:  longint;
  list1: tgulplist;
  list2: tgulplist;
  p:     pgulpitem;
  scan:  tscanner;
  size:  int64 = 0;
  strm:  tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmrestore, [filename]));

  strm  := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  list1 := tgulplist.create(@compare40);
  list2 := tgulplist.create(@compare40);
  if funtilversion = 0 then
    funtilversion := $ffffffff;
  libread(strm, list1, funtilversion);

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
      if (j = -1) or (comparedatetime(gettimeutc(scan[j]), p^.mtime) <> 0) then
      begin
        showmessage2(format(gmrestoreitem, [p^.name]));
        librestore(strm, p);
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

  libclear  (list1);
  freeandnil(list1);
  freeandnil(list2);
  freeandnil(scan);
  freeandnil(strm);

  showmessage1(format(gmrestorefinish, [size]));
  fterminated := true;
end;

procedure tgulpapplication.check(const filename: rawbytestring);
var
  i:        longint;
  list1:    tgulplist;
  null:     tstream;
  strm:     tstream;
  strmsize: int64;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmcheck, [filename]));

  strm  := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  list1 := tgulplist.create(@compare41);
  libread(strm, list1);

  null := tnullstream.create;
  for i := 0 to list1.count - 1 do
  begin
    showmessage2(format(gmcheckitem, [list1[i]^.name]));
    if (list1[i]^.offset2 > list1[i]^.offset1) then
    begin
      strm.seek  (list1[i]^.offset1, sobeginning);
      strmsize := list1[i]^.offset2 - list1[i]^.offset1;

      if not sha1match(list1[i]^.checksum2, libmove(strm, null, strmsize)) then
        raiseexception(gechecksum, '004050');
    end;
  end;

  libclear  (list1);
  freeandnil(list1);
  freeandnil(null);
  freeandnil(strm);

  showmessage1(format(gmcheckfinish, [getsize(filename)]));
  fterminated := true;
end;

procedure tgulpapplication.fix(const filename: rawbytestring);
var
  p:      pgulpitem;
  offset: int64 = 0;
  size:   int64 = 0;
  strm:   tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmfix, [filename]));

  strm := tfilestream.create(filename, fmopenreadwrite or fmsharedenywrite);
  p := new(pgulpitem);
  try
    while true do
    begin
      libread(strm, p);
      inc(offset, itemgetsize(p));
      inc(offset, p^.offset2 - p^.offset1);

      showmessage2(format(gmfixitem, [p^.name]));
      if gfclose in p^.flags then
      begin
        if strm.seek(offset, sobeginning) <> offset then
          raiseexception(gereadarchive, '003020');
        size := offset;
      end;
    end;
  except
  end;
  dispose(p);
  strm.seek(0, soend);
  offset := strm.size - size;

  if size > 0 then
    strm.size := size
  else
    raise exception.createfmt(gereadarchive, ['003015']);
  freeandnil(strm);

  showmessage1(format(gmfixfinish, [offset]));
  fterminated := true;
end;

procedure tgulpapplication.purge(const filename: rawbytestring);
var
  i:       longint;
  list1:   tgulplist;
  p:       pgulpitem;
  size:    int64;
  instrm:  tstream;
  outstrm: tstream;
  outname: rawbytestring;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmpurge, [filename]));

  instrm := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  list1  := tgulplist.create(@compare40);
  libread(instrm, list1, $ffffffff);

  outname := gettempfilename(extractfiledir(filename), '');
  outstrm := tfilestream.create(outname, fmcreate);
  if list1.count > 0 then
  begin
    for i := 0 to list1.count - 1 do
      libwrite(outstrm, list1[i]);

    for i := 0 to list1.count - 1 do
    begin
      p := list1[i];
      system.exclude(p^.flags, gfclose);
      if (p^.offset2 > p^.offset1) then
      begin
        showmessage2(format(gmmoveitem, [p^.name]));
        instrm.seek(p^.offset1, sobeginning);
        size := p^.offset2 - p^.offset1;

        p^.offset1 := outstrm.position;
        outstrm.copyfrom(instrm, size);
        p^.offset2 := outstrm.position;
      end;
    end;

    outstrm.seek(0, sobeginning);
    system.include(list1[list1.count - 1]^.flags, gfclose);
    for i := 0 to list1.count - 1 do
    begin
      list1[i]^.checksum1 := itemgetdigest(list1[i]);
      libwrite(outstrm, list1[i]);
    end;
    outstrm.seek(0, soend);
  end;
  size := instrm.size - outstrm.size;

  libclear  (list1);
  freeandnil(list1);
  freeandnil(instrm);
  freeandnil(outstrm);
  if deletefile(filename) = false then
    raiseexception(gedeletefile, filename)
  else
  if renamefile(outname, filename) = false then
    raiseexception(gerenamefile, outname);

  showmessage1(format(gmpurgefinish, [size]));
  fterminated := true;
end;

procedure tgulpapplication.list(const filename: rawbytestring);
var
  c:     longint = 0;
  i, j:  longint;
  list1: tgulplist;
  p:     pgulpitem;
  strm:  tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmlist, [filename]));

  strm := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  if fonlyversion > 0 then
  begin
    list1 := tgulplist.create(@compare41);
    libread(strm, list1);
  end else
    if funtilversion > 0 then
    begin
      list1 := tgulplist.create(@compare40);
      libread(strm, list1, funtilversion);
    end else
    begin
      list1 := tgulplist.create(@compare41);
      libread(strm, list1);
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

  libclear  (list1);
  freeandnil(list1);
  freeandnil(strm);

  showmessage1(format(gmlistfinish, [c]));
  showmessage1(format(gmlistlastversion, [j]));
  fterminated := true;
end;

procedure tgulpapplication.view(const filename: rawbytestring);
var
  c:     longint = 0;
  i, j:  longint;
  list1: tgulplist;
  p:     pgulpitem;
  strm:  tstream;
begin
  fterminated := false;
  showmessage1(gulpdescription);
  showmessage1(format(gmview, [filename]));

  strm := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  if fonlyversion > 0 then
  begin
    list1 := tgulplist.create(@compare41);
    libread(strm, list1);
  end else
    if funtilversion > 0 then
    begin
      list1 := tgulplist.create(@compare40);
      libread(strm, list1, funtilversion);
    end else
    begin
      list1 := tgulplist.create(@compare41);
      libread(strm, list1);
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

        strm.seek(p^.offset1, sobeginning);
        if assigned(fpipe) then
          fpipe.copyfrom(strm, p^.offset2 - p^.offset1);

        inc(c);
      end else
        if p^.version = fonlyversion then
        begin

         strm.seek(p^.offset1, sobeginning);
         if assigned(fpipe) then
           fpipe.copyfrom(strm, p^.offset2 - p^.offset1);

          inc(c);
        end;
  end;

  libclear  (list1);
  freeandnil(list1);
  freeandnil(strm);

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
  result := filenamematch(item^.name, fexclude);
  if result = false then
  begin
    result := (l2sattr(item^.attr) and fexcludeattr <> 0) or
              (l2smode(item^.mode) and fexcludemode <> 0);
  end;
end;

function tgulpapplication.isexcluded(const filename: rawbytestring): boolean;
begin
  result := filenamematch(filename, fexclude);
  if result = false then
  begin
   result := (getattr(filename) and fexcludeattr <> 0) or
             (getmode(filename) and fexcludemode <> 0);
  end;

end;

end.
