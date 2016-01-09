{
  Copyright (c) 2014-2016 Melchiorre Caruso.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  Contains:

    Stream classes.

  Modified:

    v0.0.3 - 2016.01.08 by Melchiorre Caruso.
}

unit GulpStream;

{$mode objfpc}

interface

uses
  Classes;

type
  { TNulStream class }

  TNulStream = class(TStream)
  public
    function Read (var   Buffer; Count: Longint): longint; override;
    function Write(const Buffer; Count: Longint): longint; override;
  end;

implementation

{ TNulStream class }

function TNulStream.Read(var Buffer; Count: longint): longint;
begin
  Result := Count;
end;

function TNulStream.Write(const Buffer; Count: longint): longint;
begin
  Result := Count;
end;

end.

