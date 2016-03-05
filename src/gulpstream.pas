{ Description: Streams unit.

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

unit gulpstream;

{$mode objfpc}
{$H+}

interface

type

  tstream = class
  private
    ffile: file of byte;
    function  getsize: int64; virtual;
    function  getposition: int64; virtual;
    procedure setsize(const newsize: int64); virtual;
  public
    constructor create(const filename: rawbytestring; mode: longint);
    destructor destroy; override;
    function read(var buffer; count: longint): longint; virtual; overload;
    function read(var s: rawbytestring): longint; overload;
    function readbyte: byte;
    function write(const buffer; count: longint): longint; virtual; overload;
    function write(const s: rawbytestring): longint; overload;
    function writebyte(b: byte): longint;
    function seek(const offset: int64): int64; virtual;
    procedure copyfrom(source: tstream; count: int64);
  public
    property position: int64 read getposition;
    property size: int64 read getsize write setsize;
  end;

  tnulstream = class(tstream)
  private
    function  getsize: int64; override;
    function  getposition: int64; override;
    procedure setsize(const newsize: int64); override;
  public
    constructor create;
    destructor destroy; override;
    function read(var buffer; count: longint): longint; override;
    function write(const buffer; count: longint): longint; override;
    function seek(const offset: int64): int64; override;
  end;

  function createstream(const filename: rawbytestring; mode: longint): tstream;

implementation

uses
  gulpcommon,
  sysutils;

function createstream(const filename: rawbytestring; mode: longint): tstream;
begin
  result := tstream.create(filename, mode);
  if assigned(result) = false then
    raise exception.createfmt('Unable to open stream "%s"', [filename]);
end;

{ tstream class }

constructor tstream.create(const filename: rawbytestring; mode: longint);
begin
  {$i-}
  system.assign(ffile, filename);
  if mode = fmoutput then
    system.rewrite(ffile)
  else
    system.reset(ffile);
  {$i+}
  if ioresult <> 0 then
    fail;
end;

destructor tstream.destroy;
begin
  system.close(ffile);
  inherited destroy;
end;

function tstream.read(var buffer; count: longint): longint;
begin
  system.blockread(ffile, buffer, count, result);
end;

function tstream.write(const buffer; count: longint): longint;
begin
  system.blockwrite(ffile, buffer, count, result);
end;

function tstream.read(var s: rawbytestring): longint;
begin
  system.blockread(ffile, result, sizeof(longint));
  setlength(s, result);
  if result > 0 then
    system.blockread(ffile, s[1], result);
  inc(result, sizeof(longint));
end;

function tstream.write(const s: rawbytestring): longint;
begin
  result := length(s);
  system.blockwrite(ffile, result, sizeof(longint));
  if result > 0 then
    system.blockwrite(ffile, s[1], result);
  inc(result, sizeof(longint));
end;

function tstream.readbyte: byte;
begin
  read(result, 1);
end;

function tstream.writebyte(b: byte): longint;
begin
  result := write(b, 1);
end;

procedure tstream.copyfrom(source: tstream; count: int64);
var
  buffer: array[0..$FFFF] of byte;
begin
  while count > 0 do
    dec(count, write(buffer, source.read(buffer, min(count, sizeof(buffer)))));
end;

function tstream.seek(const offset: int64): int64;
begin
  system.seek(ffile, offset);
  result := offset;
end;

procedure tstream.setsize(const newsize: int64);
begin
  system.seek(ffile, newsize);
  system.truncate(ffile);
end;

function tstream.getsize: int64;
begin
  result := system.filesize(ffile);
end;

function tstream.getposition: int64;
begin
  result := system.filepos(ffile);
end;

{ tnulstream class }

constructor tnulstream.create;
begin
  // nothing to do
end;

destructor tnulstream.destroy;
begin
  // nothing to do
end;

function tnulstream.read(var buffer; count: longint): longint;
begin
  result := count;
end;

function tnulstream.write(const buffer; count: longint): longint;
begin
  result := count;
end;

function tnulstream.seek(const offset: int64): int64;
begin
  result := offset;
end;

function  tnulstream.getsize: int64;
begin
  result := 0;
end;

function  tnulstream.getposition: int64;
begin
  result := 0;
end;

procedure tnulstream.setsize(const newsize: int64);
begin
  //noting to do
end;

end.
