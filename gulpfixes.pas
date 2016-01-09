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

    Compiler RTL fixes.

  Modified:

    v0.0.3 - 2016.01.09 by Melchiorre Caruso.
}

unit GulpFixes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

{ Conversion of UTC to local time and vice versa }

function UniversalTime2Local(UT: TDateTime): TDateTime;
function UniversalTime2Local(UT: TDateTime;  TZOffset : longint): TDateTime;
function LocalTime2Universal(LT: TDateTime): TDateTime;
function LocalTime2Universal(LT: TDateTime;  TZOffset:  longint): TDateTime;

implementation

{ Conversion of UTC to local time and vice versa }

function UniversalTime2Local(UT: TDateTime): TDateTime;
begin
  Result := UniversalTime2Local(UT, -GetLocalTimeOffset);
end;

function UniversalTime2Local(UT: TDateTime;  TZOffset : longint): TDateTime;
begin
  if (TZOffset > 0) then
    Result := UT + EncodeTime(TZOffset div 60, TZOffset mod 60, 0, 0)
  else
    if (TZOffset < 0) then
      Result := UT - EncodeTime(Abs(TZOffset) div 60, Abs(TZOffset) mod 60, 0, 0)
    else
      Result := UT;
end;

function LocalTime2Universal(LT: TDateTime): TDateTime;
begin
  Result := LocalTime2Universal(LT, -GetLocalTimeOffset);
end;

function LocalTime2Universal(LT: TDateTime;  TZOffset:  longint): TDateTime;
begin
  if (TZOffset > 0) then
    Result := LT - EncodeTime(TZOffset div 60, TZOffset mod 60, 0, 0)
  else
    if (TZOffset < 0) then
      Result := LT + EncodeTime(Abs(TZOffset) div 60, Abs(TZOffset) mod 60, 0, 0)
    else
      Result := LT;
end;

end.

