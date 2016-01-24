{ Description: Command line routines unit.

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

unit gulpcommandline;

{$mode objfpc}
{$H+}

interface

uses gulplist, sysutils;

function checkoptions(const shortopts, longopts: rawbytestring;
  filemasks: trawbytestringlist): rawbytestring;
function hasoption(const shortopt, longopt: rawbytestring): boolean;
function getoptionvalue(const shortopt, longopt: rawbytestring): rawbytestring;

implementation

function checkoptions(const shortopts, longopts: rawbytestring;
  filemasks: trawbytestringlist): rawbytestring;
var
  i: longint = 1;
  s: rawbytestring;

  function checkoption(const options: rawbytestring): rawbytestring;
  begin
    result := '';
    if pos(s, options) > 0 then
    begin
      if (options[pos(s, options) + length(s)] = ':') and (s[2] = '-') then
      begin
        s := paramstr(i);
        if pos('=' ,s ) = length(s) then
          result := format('Option at position %d does not allow an argument: "%s"', [i, s]);
      end else
      if (options[pos(s, options) + length(s)] = ':') then
      begin
        inc(i);
        if (i > paramcount) then
          result := format('Option at position %d does not allow an argument: "%s"', [i - 1, s])
        else
        if (pos(paramstr(i), shortopts) > 0) or (pos(paramstr(i), longopts) > 0) then
          result := format('Option at position %d needs an argument : "%s"',[i - 1, s]);
      end else
      if options[pos(s, options) + length(s)] <> ' ' then
        result := format('Invalid option at position %d: "%s"', [i, s]);
    end else
      result := format('Invalid option at position %d: "%s"', [i, s]);
  end;

begin
  result := '';
  while (i <= paramcount) and (result = '') do
  begin
    s := paramstr(i);
    if s[1] = '-' then
    begin
      if length(s) <= 2 then
        result := checkoption(shortopts)
      else
      if s[2] = '-' then
      begin
        if pos('=', s) > 0 then
          setlength(s, pos('=', s) - 1);
        result := checkoption(longopts)
      end else
        result := format('Invalid option at position %d: "%s"', [i, s]);
    end else
    if assigned(filemasks) then
      filemasks.add(s);
    inc(i);
  end;
end;

function hasoption(const shortopt, longopt: rawbytestring): boolean;
var
  i: longint = 1;
  s: rawbytestring;
begin
  result := false;
  while (i <= paramcount) and (result = false) do
  begin
    s := paramstr(i);
    if s = shortopt then
      result := true
    else
    begin
      if pos('=', s) > 0 then
        setlength(s, pos('=', s) - 1);
      if s = longopt then
        result := true;
    end;
    inc(i);
  end;
end;

function getoptionvalue(const shortopt, longopt: rawbytestring): rawbytestring;
var
  i: longint = 1;
  s: rawbytestring;
begin
  result := '';
  while (i <= paramcount) and (result = '') do
  begin
    s := paramstr(i);
    if s = shortopt then
      result := paramstr(i + 1)
    else
    begin
      if pos('=', s) > 0 then
        setlength(s, pos('=', s) - 1);
      if s = longopt then
      begin
        result := paramstr(i);
        delete(result, 1, length(s) + 1);
      end;
    end;
    inc(i);
  end;
end;

end.
