{ Description: Lists unit.

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

unit gulplist;

{$mode objfpc}
{$H+}

interface

uses
  classes,
  sysutils;

type
  generic tgenericlist<tgenericitem> = class(TObject)
  public
    type tgenericcompare = function(item1, item2: tgenericitem): longint;
  private
    flist:    tlist;
    fcompare: tgenericcompare;
    function getcount: longint;
    function get(index: longint): tgenericitem;
    function search(item: tgenericitem; var m: longint): longint;
  public
    constructor create(compare: tgenericcompare);
    destructor destroy; override;
    function add(item: tgenericitem): longint; virtual;
    function find(item: tgenericitem): longint; virtual;
    procedure delete(index: longint); virtual;
    procedure clear;
  public
    property items[index: longint]: tgenericitem read get; default;
    property count: longint read getcount;
  end;

  prawbytestringitem = ^trawbytestringitem;

  trawbytestringitem = record
    fstring: rawbytestring;
    fobject: tobject;
  end;
  prawbytestringlist = specialize tgenericlist<prawbytestringitem>;

  trawbytestringlist = class(TObject)
  private
    flist: prawbytestringlist;
    function getcount: longint;
    function get(index: longint): rawbytestring;
  public
    constructor create;
    destructor destroy; override;
    procedure add(const s: rawbytestring);
    function find(const s: rawbytestring): longint;
    procedure delete(index: longint);
    procedure clear;
  public
    property items[index: longint]: rawbytestring read get; default;
    property count: longint read getcount;
  end;

implementation

{ tgenericlist }

constructor tgenericlist.create(compare: tgenericcompare);
begin
  inherited create;
  fcompare := compare;
  flist    := tlist.create;
end;

destructor tgenericlist.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tgenericlist.delete(index: longint);
begin
  flist.delete(index);
end;

procedure tgenericlist.clear;
begin
  while getcount > 0 do
    delete(0);
end;

function tgenericlist.search(item: tgenericitem; var m: longint): longint;
var
  l, h, i: longint;
begin
  i := 1;
  l := 0;
  m := 0;
  h := flist.count - 1;
  while h >= l do
  begin
    m := (l + h) div 2;
    i := fcompare(get(m), item);
    if i < 0 then
      l := m + 1
    else
    if i > 0 then
      h := m - 1
    else
      break;
  end;

  if i <> 0 then
  begin
    if i < 0 then
      m := m + 1;
    result := -1;
  end else
    result := m;
end;

function tgenericlist.add(item: tgenericitem): longint;
begin
  if search(item, result) = -1 then
    flist.insert(result, item)
  else
    result := -1;
end;

function tgenericlist.find(item: tgenericitem): longint;
var
  m: longint;
begin
  result := search(item, m);
end;

function tgenericlist.get(index: longint): tgenericitem;
begin
  result := tgenericitem(flist[index]);
end;

function tgenericlist.getcount: longint;
begin
  result := flist.count;
end;

{ trawbytestringlist }

function compare(item1, item2: prawbytestringitem): longint;
begin
  {$IFDEF UNIX}
  result := comparestr(item1^.fstring, item2^.fstring);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result := comparetext(item1^.fstring, item2^.fstring);
  {$ELSE}
  {$ENDIF}
  {$ENDIF}
end;

constructor trawbytestringlist.create;
begin
  flist := prawbytestringlist.create(@compare);
end;

destructor trawbytestringlist.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure trawbytestringlist.delete(index: longint);
begin
  dispose(flist[index]);
  flist.delete(index);
end;

procedure trawbytestringlist.clear;
begin
  while getcount > 0 do
    delete(0);
end;

procedure trawbytestringlist.add(const s: rawbytestring);
var
  p: prawbytestringitem;
begin
  p := new(prawbytestringitem);
  p^.fstring := s;
  p^.fobject := nil;
  if flist.add(p) = -1 then
    dispose(p);
end;

function trawbytestringlist.find(const s: rawbytestring): longint;
var
  p: prawbytestringitem;
begin
  p := new(prawbytestringitem);
  p^.fstring := s;
  p^.fobject := nil;
  result := flist.find(p);
  dispose(p);
end;

function trawbytestringlist.getcount: longint;
begin
  result := flist.count;
end;

function trawbytestringlist.get(index: longint): rawbytestring;
begin
  result := flist[index]^.fstring;
end;

end.
