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

unit gulpdmc;

{$mode objfpc}
{$H+}

interface

uses
  classes, math;

const
  cc = $FF;

//////////////////////////////// dmc coder /////////////////////////////////////
//  an encoder does arithmetic encoding.  methods:
//  encoder(compress, f) creates encoder for compression to archive f, which
//  must be open past any header for writing in binary mode
//  encoder(decompress, f) creates encoder for decompression from archive f,
//  which must be open past any header for reading in binary mode
//  encode(bit) in compress mode compresses bit to file f.
//  decode() in decompress mode returns the next decompressed bit from file f.
//  flush() should be called when there is no more to compress.

type
  tdmccoder = class
  private
    stream      : tstream;
    x1, x2      : longword; // range, initially [0, 1), scaled by 2^32
    x           : longword; // last 4 input bytes of archive.
  public
    constructor create(astream: tstream);
    destructor  destroy; override;
    procedure   flush;
    procedure   init;
    // update(bit) -- encode bit by splitting the range [x1, x2] in proportion
    // to p(1) and p(0) as given by the predictor and narrowing to the appropriate
    // subrange.  output leading bytes of the range as they become known.
    procedure   encode(p: longword; const bit: byte);
    procedure   decode(p: longword; var   bit: byte);
  end;

//////////////////////////////// dmc predictor /////////////////////////////////
// a predictor estimates the probability that the next bit of uncompressed
// data is 1. method update(bit) returns p(1) as a 16 bit number (0..65535) and
// trains the predictor with the actual bit (0 or 1).

type
  pdmcnode = ^tdmcnode;
  tdmcnode = packed record
    count : array[0..1] of word;
    next  : array[0..1] of longword;
  end;

type
  tdmcmodeller = class
  private
    clonec1     : longword;
    clonec2     : longword;
    curr        : pdmcnode;
    nodes       : array of tdmcnode;
    nodescount  : longword;
    nodeslimit  : longword;
    function    clone(bit: longword): longword;
    function    new(next0, next1: longword): longword;
  public
    constructor create(aclone1, aclone2, anodeslimit: longword);
    destructor  destroy; override;
    procedure   update(bit: longword);
    function    predict: longword;
    procedure   init;
    procedure   reset;
  end;

type
  tdmccompressionstream = class
  protected
    fcoder      : tdmccoder;
    fmodeller   : tdmcmodeller;
  public
    constructor create(astream: tstream; c1, c2, nodeslimit: longword);
    destructor  destroy; override;
    function    write(const abuffer; acount : longint) : integer;
  end;

type
  tdmcdecompressionstream = class
  protected
    fcoder      : tdmccoder;
    fmodeller   : tdmcmodeller;
  public
    constructor create(astream: tstream; c1, c2, nodeslimit: longword);
    destructor  destroy; override;
    function    read(var abuffer; acount : longint) : integer;
  end;

implementation

//////////////////////////////// dmc coder /////////////////////////////////////

constructor tdmccoder.create(astream: tstream);
begin
  inherited create;
  stream := astream;
  x  := 0;
  x1 := 0;
  x2 := $ffffff;
end;

destructor  tdmccoder.destroy;
begin
  inherited destroy;
end;

procedure   tdmccoder.init;
var
  i : longint;
begin
  for i := 2 downto 0 do
    x := x + (stream.readbyte shl (i shl 3));
end;

procedure   tdmccoder.flush;
var
  i : longint;
begin
  for i := 2 downto 0 do
    stream.writebyte((x2 shr (i shl 3)) and $ff);
end;

procedure   tdmccoder.encode(p: longword; const bit: byte);
var
  xmid: longword;
begin
  assert(p <= $ffff);
  assert((bit = 0) or (bit = 1));
  xmid := x1 + ((x2 - x1) shr 16) * p + (((x2 - x1) and $ffff) * p shr 16);
  assert((xmid >= x1) and (xmid < x2));

  if boolean(bit) then
    x2 := xmid
  else
    x1 := xmid + 1;

  // shift equal msb's out
  while (((x1 xor x2) and $ff0000) = 0) do
  begin
    stream.writebyte((x2 shr 16) and $ff);
    x1 := (x1 shl 8);
    x2 := (x2 shl 8) + $ff;
  end;
end;

procedure   tdmccoder.decode(p: longword; var   bit: byte);
var
  xmid: longword;
begin
  assert(p <= $ffff);
  assert((bit = 0) or (bit = 1));
  xmid := x1 + ((x2 - x1) shr 16) * p + (((x2 - x1) and $ffff) * p shr 16);
  assert((xmid >= x1) and (xmid < x2));

  bit := 0;
  if (x <= xmid) then
  begin
    bit := 1;
    x2 := xmid;
  end else
    x1 := xmid + 1;

  // shift equal msb's out
  while (((x1 xor x2) and $ff0000) = 0) do
  begin
    x1 := (x1 shl 8);
    x2 := (x2 shl 8) + $ff;
    x :=  (x  shl 8) + (stream.readbyte and $ff);
  end;
end;

//////////////////////////////// dmc modeller///////////////////////////////////

constructor tdmcmodeller.create(aclone1, aclone2, anodeslimit: longword);
begin
  inherited create;
  nodeslimit := anodeslimit;
  clonec1    := cc * aclone1;
  clonec2    := cc * aclone2;

  setlength(nodes, nodeslimit);

  writeln('MEM-SIZE = ', (nodeslimit * sizeof(tdmcnode)) div (1024*1024), 'MB');
  init;
end;

destructor tdmcmodeller.destroy;
begin
  setlength(nodes, 0);
  inherited destroy;
end;

function tdmcmodeller.new(next0, next1: longword): longword;
begin
  result := nodescount;
  nodes[result].count[0] := $1;
  nodes[result].count[1] := $1;
  nodes[result].next [0] := next0;
  nodes[result].next [1] := next1;
  inc(nodescount);
end;

procedure tdmcmodeller.init;
var
  i : longint;
begin
  nodescount := 0;
  for i := 0 to $7e do new(i * 2 + 1, i * 2 + 2);
  for i := 0 to $7f do new(0, 0);
  reset;
end;

procedure tdmcmodeller.reset;
begin
  curr := @nodes[0];
end;

function tdmcmodeller.clone(bit: longword): longword;
var
  next: pdmcnode;
begin
  next := @nodes[curr^.next[bit]];

  result := new(next^.next[0], next^.next[1]);

  nodes[result].count[0] := max($1, (curr^.count[bit] * next^.count[0]) div (next^.count[0] + next^.count[1]));
  nodes[result].count[1] := max($1, (curr^.count[bit] * next^.count[1]) div (next^.count[0] + next^.count[1]));
end;

function tdmcmodeller.predict: longword;
begin
  result := ($ffff * (curr^.count[1]) div (curr^.count[0] + curr^.count[1]));
end;

procedure tdmcmodeller.update(bit: longword);
begin

  if nodescount < nodeslimit then
  begin
    if curr^.count[bit] > (clonec1) then
      if nodes[curr^.next[bit]].count[0] +
         nodes[curr^.next[bit]].count[1] >
               curr^.count[bit] + (clonec2) then
               curr^.next[bit] := clone(bit);
  end;
  curr^.count[bit] := min($ffff, curr^.count[bit] + cc);

  if curr^.count[bit] = $ffff then
  begin
    curr^.count[0] := max($1, curr^.count[0] shr 1);
    curr^.count[1] := max($1, curr^.count[1] shr 1);
  end;
  curr := @nodes[curr^.next[bit]];
end;

//////////////////////////////// dmc stream ////////////////////////////////////

constructor tdmccompressionstream.create(astream: tstream; c1, c2, nodeslimit: longword);
begin
  inherited create;
  fcoder    := tdmccoder.create(astream);
  fmodeller := tdmcmodeller.create(c1, c2, nodeslimit);
end;

destructor tdmccompressionstream.destroy;
begin
  fcoder.flush;
  inherited destroy;
end;

function tdmccompressionstream.write(const abuffer; acount : longint) : integer;
var
  data : array[0.. $FFFFFFF] of byte absolute abuffer;
  i    : longint;
  bit  : byte;
begin
  result := 0;
  while result < acount do
  begin
    for i := 7 downto 0 do
    begin
      bit := (data[result] shr i) and 1;
      fcoder.encode(fmodeller.predict, bit);
      fmodeller.update(bit);
    end;
    inc(result);
  end;
end;

constructor tdmcdecompressionstream.create(astream: tstream; c1, c2, nodeslimit: longword);
begin
  inherited create;
  fcoder    := tdmccoder.create(astream);
  fmodeller := tdmcmodeller.create(1, 2, 4*1024*1024);
end;

destructor tdmcdecompressionstream.destroy;
begin
  inherited destroy;
end;

function tdmcdecompressionstream.read(var abuffer; acount : longint) : integer;
var
  data : array[0.. $FFFFFFF] of byte absolute abuffer;
  i    : longint;
  bit  : byte;
begin
  result := 0;
  while result < acount do
  begin
    data[result] := 0;
    for i  := 0 to 7 do
    begin
      fmodeller.update(bit);
      inc(data[result], data[result] + bit);
    end;
    inc(result);
  end;
end;

end.

