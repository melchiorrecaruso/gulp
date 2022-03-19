{ Description: Directories scanner unit.

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

unit gulpscanner;

{$mode objfpc}
{$H+}

interface

uses
  {$IFDEF LINUX}
  baseunix,
  {$ENDIF}
  gulpcommon,
  gulplist,
  sysutils;

const
  gulpnotsupported = fasysfile or favolumeid;

type
  tscanner = class(TObject)
  private
    flist: trawbytestringlist;
    function getcount: integer;
    function getitem(index: longint): rawbytestring;
    procedure scan(const filemask: rawbytestring; recursive: boolean);
  public
    constructor create;
    destructor destroy; override;
    procedure add(const filemask: rawbytestring);
    function find(const filename: rawbytestring): longint;
    procedure delete(index: longint);
    procedure clear;
  public
    property count: integer read getcount;
    property items[index: longint]: rawbytestring read getitem; default;
  end;

function filenamematch(const filename: rawbytestring;
  const filemask: rawbytestring): boolean; overload;

function filenamematch(const filename: rawbytestring;
  filemasks: trawbytestringlist): boolean; overload;

function deletelink(const linkname: rawbytestring): boolean;
function deletefile(const filename: rawbytestring): boolean;
function deletedir (const dirname:  rawbytestring): boolean;
function deleteany (const name:     rawbytestring): boolean;

implementation

constructor tscanner.create;
begin
  inherited create;
  flist := trawbytestringlist.create;
end;

destructor tscanner.destroy;
begin
  flist.destroy;
  inherited destroy;
end;

procedure tscanner.clear;
begin
  flist.clear;
end;

procedure tscanner.scan(const filemask: rawbytestring; recursive: boolean);
var
  err:  longint;
  mask: rawbytestring;
  path: rawbytestring;
  sr:   tsearchrec;
begin
  path := extractfilepath(filemask);
  mask := extractfilename(filemask);
  err  := sysutils.findfirst(path + '*',
    fareadonly  or fahidden  or fasysfile or favolumeid or
    fadirectory or faarchive or fasymlink or faanyfile, sr);
  while err = 0 do
  begin
    if sr.attr and gulpnotsupported = 0 then
    begin

      if sr.attr and fadirectory <> 0 then
      begin
        if (sr.name <> '..') and (sr.name <> '.') then
        begin
          flist.add(path + sr.name);

          if (sr.attr and fasymlink = 0) and recursive then
            scan(path + includetrailingpathdelimiter(sr.name) + mask, true);
        end;

      end else
        if filenamematch(path + sr.name, filemask) then
          flist.add(path + sr.name);

    end;
    err := findnext(sr);
  end;
  sysutils.findclose(sr);
end;

procedure tscanner.add(const filemask: rawbytestring);
begin
  if filemask = '' then exit;

  if directoryexists(filemask) then
    flist.add(filemask)
  else
  if fileexists(filemask) then
    flist.add(filemask)
  else
    scan(filemask, true);
end;

procedure tscanner.delete(index: longint);
begin
  flist.delete(index);
end;

function tscanner.find(const filename: rawbytestring): longint;
begin
  result := flist.find(filename);
end;

function tscanner.getitem(index: longint): rawbytestring;
begin
  result := flist[index];
end;

function tscanner.getcount: longint;
begin
  result := flist.count;
end;

function matchpattern(element, pattern: pansichar): boolean;
begin
  if 0 = strcomp(pattern, '*') then
    result := true
  else
  if (element^ = chr(0)) and (pattern^ <> chr(0)) then
    result := false
  else
  if element^ = chr(0) then
    result := true
  else
    case pattern^ of
      '*': if matchpattern(element, @pattern[1]) then
          result := true
        else
          result := matchpattern(@element[1], pattern);
      '?': result := matchpattern(@element[1], @pattern[1]);
    else
      if element^ = pattern^ then
        result := matchpattern(@element[1], @pattern[1])
      else
        result := false;
    end;
end;

function filenamematch(const filename: rawbytestring;
  const filemask: rawbytestring): boolean;
begin
  result := matchpattern(pansichar(filename), pansichar(filemask));
end;

function filenamematch(const filename: rawbytestring;
  filemasks: trawbytestringlist): boolean;
var
  i: longint;
begin
  result := false;
  for i := 0 to filemasks.count - 1 do
    if filenamematch(filename, filemasks[i]) = true then
    begin
      result := true;
      break;
    end;
end;

function deletelink(const linkname: rawbytestring): boolean;
begin
  {$IFDEF LINUX}
  result := fpunlink(linkname) = 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := sysutils.deletefile(linkname);
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}
end;

function deletefile(const filename: rawbytestring): boolean;
begin
  result := sysutils.deletefile(filename);
end;

function deletedir(const dirname: rawbytestring): boolean;
var
  attr: longint;
  i:    longint;
  s:    tscanner;
begin
  s := tscanner.create;
  s.add(dirname);

  for i := s.count -1 downto 0 do
  begin
    attr := getattr(s[i]);
    if issymlink(attr) then
      deletelink(s[i])
    else
    if isdirectory(attr) then
      removedir(s[i])
    else
    if isregular(attr) then
      deletefile(s[i]);
  end;
  s.destroy;

  result := removedir(dirname);
end;

function deleteany(const name: rawbytestring) : boolean;
var
  attr: longint;
begin
  attr := getattr(name);
  if issymlink(attr) then
    result := deletelink(name)
  else
  if isdirectory(attr) then
    result := deletedir(name)
  else
  if isregular(attr) then
    result := deletefile(name)
  else
    result := false;
end;

end.
