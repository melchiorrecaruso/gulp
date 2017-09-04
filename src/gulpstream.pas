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
    function seek(offset: longint; origin: word): longint; override;
    function seek(const offset: int64; origin: tseekorigin): int64; override;
  end;

implementation

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

function tnulstream.seek(offset: longint; origin: word): longint;
begin
  result := offset;
end;

function tnulstream.seek(const offset: int64; origin: tseekorigin): int64;
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
  // noting to do
end;

end.
