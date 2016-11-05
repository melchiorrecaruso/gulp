{ Description: The gulp diff utility.

  Copyright (C) 2016 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WiTHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABiLiTY or FiTNESS
  FOR A PARTiCULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

program gdiff;

{$codepage utf8}
{$mode objfpc}
{$H+}

uses
  gulpcommon,
  gulpscanner,
  sha1,
  sysutils;

var
  b: boolean;
  i, j: longint;
  s:    rawbytestring;
  scan: array[1..2] of tscanner;

procedure showmessage(const fn1, fn2, msg: rawbytestring; var printed: boolean);
begin
  if printed = false then
  begin
    writeln;
    writeln(format('"%s" and', [fn1]));
    writeln(format('"%s" differ', [fn2]));
    printed := true;
  end;
  writeln(msg);
end;

begin
  writeln('GDIFF v0.4 diff utility, copyright (c) 2016 Melchiorre Caruso.');
  if (paramcount <> 2) or
     (directoryexists(paramstr(1)) = false) or
     (directoryexists(paramstr(2)) = false) then
  begin
    writeln('Usage: gdiff directory1 directory2');
    writeln('Compare two DIRECTORIES file by file.');
  end else
  begin

    for i := 1 to 2 do
    begin
      scan[i] := tscanner.create;
      scan[i].add(includetrailingpathdelimiter(paramstr(i)) + '*');
      writeln(format('%d object(s) in folder "%s"', [scan[i].count, paramstr(i)]));
    end;

    for i := 0 to scan[1].count - 1 do
    begin
      s := scan[1] [i];
      delete(s, 1, length(includetrailingpathdelimiter(paramstr(1))));
      s := includetrailingpathdelimiter(paramstr(2)) + s;

      j := scan[2].find(s);
      if j <> -1 then
      begin
        b := false;

        if (filegetattr(scan[1] [i]) and fadirectory = 0) and
           (filegetattr(scan[2] [j]) and fadirectory = 0) then
          if sha1match(sha1file(scan[1] [i], 4096),
            sha1file(scan[2] [j], 4096)) = false then
              showmessage(scan[1] [i], scan[2] [j], 'in data', b);

        if filegettimeutc(scan[1] [i]) <> filegettimeutc(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in time', b);

        if filegetmode(scan[1] [i]) <> filegetmode(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in mode', b);

        if filegetattr(scan[1] [i]) <> filegetattr(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in attr', b);

        if filegetuserid(scan[1] [i]) <> filegetuserid(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in user id', b);

        if filegetusername(scan[1] [i]) <> filegetusername(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in user name', b);

        if filegetgroupid(scan[1] [i]) <> filegetgroupid(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in group id', b);

        if filegetgroupname(scan[1] [i]) <> filegetgroupname(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in group name', b);        

        if filegetsize(scan[1] [i]) <> filegetsize(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in size', b);

        if filegetlinkname(scan[1] [i]) <> filegetlinkname(scan[2] [j]) then
          showmessage(scan[1] [i], scan[2] [j], 'in linkname', b);

        scan[2].delete(j);
      end else
        writeln(format('"%s" not founded in "%s"', [scan[1] [i], paramstr(2)]));

    end;

    for j := 0 to scan[2].count - 1 do
      writeln(format('"%s" not founded in "%s"', [scan[2] [j], paramstr(1)]));

    for j := 1 to 2 do
      scan[j].destroy;
  end;
end.
