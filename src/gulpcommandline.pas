{ Description: Command line routines unit.

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

unit gulpcommandline;

{$mode objfpc}
{$H+}

interface

uses
  gulplist,
  sysutils;

function checkoptions(const shortopts, longopts: rawbytestring;
  filemasks: trawbytestringlist): rawbytestring;
function hasoption(const shortopt, longopt: rawbytestring): boolean;
type
  trawbytestringarray = array of rawbytestring;

function getoptionvalue(const shortopt, longopt: rawbytestring): rawbytestring;
function getoptionvalues(const shortopt, longopt: rawbytestring): trawbytestringarray;

implementation

// Looks for an option name as a whole token in a space separated option
// list, where a trailing ':' marks an option that needs an argument.
// Returns -1 if not found, 0 if found without argument, 1 if found with argument.
function findoption(const name, options: rawbytestring): longint;
var
  i, j: longint;
  token: rawbytestring;
begin
  result := -1;
  if name = '' then exit;
  i := 1;
  while i <= length(options) do
  begin
    while (i <= length(options)) and (options[i] = ' ') do inc(i);
    j := i;
    while (j <= length(options)) and (options[j] <> ' ') do inc(j);
    token := copy(options, i, j - i);
    if token = name then
      exit(0)
    else
    if token = name + ':' then
      exit(1);
    i := j;
  end;
end;

function checkoptions(const shortopts, longopts: rawbytestring;
  filemasks: trawbytestringlist): rawbytestring;
var
  i: longint = 1;
  s: rawbytestring;

  // true if a command line argument is one of the known options
  function isoption(const arg: rawbytestring): boolean;
  var
    name: rawbytestring;
  begin
    name := arg;
    if pos('=', name) > 0 then
      setlength(name, pos('=', name) - 1);
    result := (findoption(name, shortopts) <> -1) or
              (findoption(name, longopts)  <> -1);
  end;

  function checkoption(const options: rawbytestring): rawbytestring;
  var
    k: longint;
  begin
    result := '';
    k := findoption(s, options);
    if k = -1 then
      result := format('Invalid option at position %d: "%s"', [i, s])
    else
    if (k = 1) and (s[2] = '-') then
    begin
      // long options take their argument only in the form --option=value
      if pos('=', paramstr(i)) = 0 then
        result := format('Option at position %d needs an argument in the form %s=value: "%s"', [i, s, s])
      else
      begin
        s := paramstr(i);
        if pos('=' ,s ) = length(s) then
          result := format('Option at position %d needs an argument: "%s"', [i, s]);
      end;
    end else
    if (k = 1) then
    begin
      inc(i);
      if (i > paramcount) or (paramstr(i) = '') then
        result := format('Option at position %d needs an argument: "%s"', [i - 1, s])
      else
      if isoption(paramstr(i)) then
        result := format('Option at position %d needs an argument: "%s"',[i - 1, s]);
    end else
    if (s[2] = '-') and (pos('=', paramstr(i)) > 0) then
      // long option without argument given in the form --option=value
      result := format('Option at position %d does not allow an argument: "%s"', [i, paramstr(i)]);
  end;

begin
  result := '';
  while (i <= paramcount) and (result = '') do
  begin
    s := paramstr(i);
    if (length(s) > 0) and (s[1] = '-') then
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
    if (shortopt <> '') and (s = shortopt) then
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
    if (shortopt <> '') and (s = shortopt) then
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

// Returns the values of all the occurrences of an option, in command line order.
function getoptionvalues(const shortopt, longopt: rawbytestring): trawbytestringarray;
var
  i: longint = 1;
  s: rawbytestring;
  v: rawbytestring;
begin
  result := nil;
  while i <= paramcount do
  begin
    s := paramstr(i);
    if (shortopt <> '') and (s = shortopt) then
    begin
      inc(i);
      v := paramstr(i);
      setlength(result, length(result) + 1);
      result[high(result)] := v;
    end else
    begin
      v := s;
      if pos('=', s) > 0 then
        setlength(s, pos('=', s) - 1);
      if (longopt <> '') and (s = longopt) then
      begin
        delete(v, 1, length(s) + 1);
        setlength(result, length(result) + 1);
        result[high(result)] := v;
      end;
    end;
    inc(i);
  end;
end;

end.
