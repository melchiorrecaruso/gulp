{ Description: Fixes unit.

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

unit gulpfixes;

{$mode objfpc}
{$H+}

interface

uses sysutils;

function universaltime2local(ut: tdatetime): tdatetime;
function universaltime2local(ut: tdatetime; tzoffset: longint): tdatetime;
function localtime2universal(lt: tdatetime): tdatetime;
function localtime2universal(lt: tdatetime; tzoffset: longint): tdatetime;

implementation

function universaltime2local(ut: tdatetime): tdatetime;
begin
  result := universaltime2local(ut, -getlocaltimeoffset);
end;

function universaltime2local(ut: tdatetime; tzoffset: longint): tdatetime;
begin
  if (tzoffset > 0) then
    result := ut + encodetime(tzoffset div 60, tzoffset mod 60, 0, 0)
  else
  if (tzoffset < 0) then
    result := ut - encodetime(abs(tzoffset) div 60, abs(tzoffset) mod 60,
      0, 0)
  else
    result := ut;
end;

function localtime2universal(lt: tdatetime): tdatetime;
begin
  result := localtime2universal(lt, -getlocaltimeoffset);
end;

function localtime2universal(lt: tdatetime; tzoffset: longint): tdatetime;
begin
  if (tzoffset > 0) then
    result := lt - encodetime(tzoffset div 60, tzoffset mod 60, 0, 0)
  else
  if (tzoffset < 0) then
    result := lt + encodetime(abs(tzoffset) div 60, abs(tzoffset) mod 60, 0, 0)
  else
    result := lt;
end;

end.
