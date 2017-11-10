{ Description: Streams unit.

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

unit gulpstream;

{$mode objfpc}
{$H+}

interface

uses
  classes;

type
  { tbuffstream }

  tbuffstream = class
  protected
    fbuffer: array[0..4095] of byte;
    fbufferindex: longint;
    fbuffersize: longint;
    fstream: tstream;
    function getsize: int64; virtual abstract;
    function getposition: int64; virtual abstract;
    procedure setsize(value: int64); virtual abstract;
    procedure setposition(value: int64); virtual abstract;
  public
    constructor create;
    function readbyte: byte; virtual abstract;
    function readstring: rawbytestring; virtual abstract;
    function read(var buff; count: longint): longint; virtual abstract;

    procedure writebyte(const b: byte); virtual abstract;
    procedure writestring(const s: rawbytestring); virtual abstract;
    function  write(const buffer; count: longint): longint; virtual abstract;

    function seek(const offset: int64; origin: tseekorigin): int64; virtual abstract;
    procedure fillbuffer; virtual abstract;
    procedure flushbuffer; virtual abstract;
  published
    property position: int64 read getposition write setposition;
    property size: int64 read getsize write setsize;
  end;

  { tfilereader }

  tfilereader = class(tbuffstream)
  protected
    function getsize: int64; override;
    function getposition: int64; override;
    procedure setposition(value: int64); override;
  public
    constructor create(const filename: rawbytestring);
    destructor destroy; override;
    function readbyte: byte; override;
    function readstring: rawbytestring; override;
    function read(var buffer; count: longint): longint; override;
    function seek(const offset: int64; origin: tseekorigin): int64; override;
    procedure fillbuffer; override;
  end;

  { tfilewriter }

  tfilewriter = class(tbuffstream)
  protected
    function getsize: int64; override;
    function getposition: int64; override;
    procedure setsize(value: int64); override;
  public
    constructor create(const filename: rawbytestring; overwrite: boolean);
    destructor destroy; override;
    procedure writebyte(const b: byte); override;
    procedure writestring(const s: rawbytestring); override;
    function write(const buffer; count: longint): longint; override;
    function seek(const offset: int64; origin: tseekorigin): int64; override;
    procedure copyfrom(source: tfilereader; const count: int64);
    procedure flushbuffer; override;
  end;

type
  tnullwriter = class(tbuffstream)
  public
    function read(var buffer; count: longint): longint; override;
    function write(const buffer; count: longint): longint; override;
  end;

implementation

uses
  sysutils, gulpcommon;

{ tbuffstream }

constructor tbuffstream.create;
begin
  inherited create;
  fbufferindex := 0;
  fbuffersize  := 0;
end;

{ tfilereader }

constructor tfilereader.create(const filename: rawbytestring);
begin
  inherited create;
  fstream := tfilestream.create(filename, fmopenread or fmsharedenywrite);
end;

destructor tfilereader.destroy;
begin
  fstream.destroy;
  inherited destroy;
end;

procedure tfilereader.fillbuffer;
begin
  fbufferindex := 0;
  fbuffersize  := fstream.read(fbuffer, sizeof(fbuffer));
end;

function tfilereader.readbyte: byte;
begin
  if fbufferindex = fbuffersize then fillbuffer;
  result := fbuffer[fbufferindex];
  inc(fbufferindex);
end;

function tfilereader.readstring: rawbytestring;
var
  len: longint = 0;
begin
  read(len, sizeof(len));
  if len > 0 then
  begin
    setlength(result, len);
    read(result[1], len);
  end;
end;

function tfilereader.read(var buffer; count: longint): longint;
var
  data: array[0.. maxint] of byte absolute buffer;
  i: longint;
begin
  result := 0;
  repeat
    if fbufferindex = fbuffersize then
    begin
      fillbuffer;
      if fbuffersize = 0 then break;
    end;
    i := min(count - result, fbuffersize - fbufferindex);

    move(fbuffer[fbufferindex], data[result], i);
    inc(fbufferindex, i);
    inc(result, i);
  until result = count;
end;

function tfilereader.seek(const offset: int64; origin: tseekorigin): int64;
begin
  fbufferindex := 0;
  fbuffersize  := 0;
  result       := fstream.seek(offset, origin);
end;

function tfilereader.getsize: int64;
begin
  // fbufferindex := 0;
  // fbuffersize  := 0;
  result := fstream.size;
end;

function tfilereader.getposition: int64;
begin
  result := fstream.seek(0, socurrent) - fbufferindex;
end;

procedure tfilereader.setposition(value: int64);
begin
  seek(value, sobeginning);
end;

{ tfilewriter }

constructor tfilewriter.create(const filename: rawbytestring; overwrite: boolean);
begin
  inherited create;
  if overwrite = TRUE then
    fstream := tfilestream.create(filename, fmcreate)
  else
    fstream := tfilestream.create(filename, fmopenreadwrite or fmsharedenywrite);
end;

destructor tfilewriter.destroy;
begin
  flushbuffer;
  fstream.destroy;
  inherited destroy;
end;

procedure tfilewriter.flushbuffer;
begin
  if fbufferindex > 0 then
  begin
    fstream.write(fbuffer, fbufferindex);
    fbufferindex := 0;
    fbuffersize  := 0;
  end;
end;

procedure tfilewriter.writebyte(const b: byte);
begin
  if fbufferindex = sizeof(fbuffer) then flushbuffer;
  fbuffer[fbufferindex] := b;
  inc(fbufferindex);
end;

procedure tfilewriter.writestring(const s: rawbytestring);
var
  len: longint;
begin
  len := length(s);
  write(len, sizeof(len));
  if len > 0 then
  begin
    write(s[1], len);
  end;
end;

function tfilewriter.write(const buffer; count: longint): longint;
var
  data: array[0.. maxint] of byte absolute buffer;
  i: longint;
begin
  result := 0;
  repeat
    if fbufferindex = sizeof(fbuffer) then
    begin
      flushbuffer;
    end;
    i := min(count - result, sizeof(fbuffer) - fbufferindex);

    move(data[result], fbuffer[fbufferindex], i);
    inc(fbufferindex, i);
    inc(result, i);
  until result = count;
end;

function tfilewriter.seek(const offset: int64; origin: tseekorigin): int64;
begin
  flushbuffer;
  result := fstream.seek(offset, origin);
end;

procedure tfilewriter.copyfrom(source: tfilereader; const count: int64);
var
  i: int64;
begin
  for i := 0 to count -1 do
    writebyte(source.readbyte);
end;

function tfilewriter.getsize: int64;
begin
  flushbuffer;
  result := fstream.size;
end;

function tfilewriter.getposition: int64;
begin
  result := fstream.seek(0, socurrent) + fbufferindex;
end;

procedure tfilewriter.setsize(value: int64);
begin
  flushbuffer;
  fstream.size := value;
end;

{ tnullwriter class }

function tnullwriter.read(var buffer; count: longint): longint;
begin
  result := count;
end;

function tnullwriter.write(const buffer; count: longint): longint;
begin
  result := count;
end;

end.
