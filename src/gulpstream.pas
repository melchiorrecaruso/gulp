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
    f: file;
    function  getsize: int64;
    procedure setsize(const newsize: int64);
  public
    constructor create(const filename: rawbytestring; mode: longint);
    destructor destroy; override;
    function read(var buffer; count: longint): longint; virtual;
    function write(const buffer; count: longint): longint; virtual;
    function readrawbytestring(var s: rawbytestring): longint;
    function writerawbytestring(const s: rawbytestring): longint;

    procedure copyfrom(source: tstream; count: int64);
    function seek(const offset: int64): int64; virtual;
    function position: int64; virtual;
  public
    property size: int64 read getsize write setsize;
  end;

  tnulstream = class(tstream)
  public
    function read(var buffer; count: longint): longint; override;
    function write(const buffer; count: longint): longint; override;
    function seek(const offset: int64): int64; override;
    function position: int64; override;
  end;

implementation

constructor tstream.create(const filename: rawbytestring; mode: longint);
begin
  system.assign(f, filename);
  case mode of
    fminput:  system.reset(f);
    fmoutput: system.rewrite(f);
    fminout:  begin
                system.reset(f);
                system.rewrite(f)
              end;
  end;
end;

destructor tstream.destroy;
begin
  system.close(f);
  inherited destroy;
end;

function tstream.read(var buffer; count: longint): longint;
begin
  system.blockread(f, buffer, count, result);
end;

function tstream.write(const buffer; count: longint): longint;
begin
  system.blockwrite(f, buffer, count, result);
end;

function tstream.readrawbytestring(var s: rawbytestring): longint;
begin
  system.blockread(f, result, sizeof(longint));
  setlength(s, result);
  if result > 0 then
    system.blockread(f, s[1], result);
  inc(result, sizeof(longint));
end;

function tstream.writerawbytestring(const s: rawbytestring): longint;
begin
  result := length(s);
  system.blockwrite(f, result, sizeof(longint));
  if result > 0 then
    system.blockwrite(f, s[1], result);
  inc(result, sizeof(longint));
end;

procedure tstream.copyfrom(source: tstream; count: int64);
begin



end;

function tstream.seek(const offset: int64): int64;
begin
  system.seek(f, offset);
  result := offset;
end;

procedure tstream.setsize(const newsize: int64);
begin
  system.seek(f, newsize);
  system.truncate(f);
end;

function tstream.getsize: int64;
begin
  result := system.filesize(f);
end;

function tstream.position: int64;
begin
  result := system.filepos(f);
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

function tnulstream.position: int64;
begin
  result := 0;
end;

end.
