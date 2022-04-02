{ Description: The gulp diff utility.

  Copyright (C) 2016-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

program gulpdiff;

{$mode objfpc}
{$H+}

uses
  dateutils,
  gulpcommon,
  gulpmessages,
  gulpscanner,
  sha1,
  sysutils;

var
  i, j: longint;
  s:    rawbytestring;
  scan: array[1..2] of tscanner;
  dig1: TSHA1Digest;
  dig2: TSHA1Digest;

begin
  writeln('Gulp-DIFF v0.4 diff utility, copyright (c) 2016 Melchiorre Caruso.');
  if (paramcount <> 2) or
     (directoryexists(paramstr(1)) = false) or
     (directoryexists(paramstr(2)) = false) then
  begin
    writeln('Usage: gulp-diff directory1 directory2');
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
      s := scan[1][i];
      delete(s, 1, length(includetrailingpathdelimiter(paramstr(1))));
      s := includetrailingpathdelimiter(paramstr(2)) + s;

      j := scan[2].find(s);
      if j <> -1 then
      begin
        if (filegetattr(scan[1][i]) and fadirectory = 0) and
           (filegetattr(scan[2][j]) and fadirectory = 0) then
        begin

          try
            dig1 := sha1file(scan[1][i], 4096);
          except
            writeln(format(gereadstream, [scan[1][i]]));
          end;

          try
            dig2 := sha1file(scan[2][j], 4096);
          except
            writeln(format(gereadstream, [scan[2][j]]));
          end;

          if sha1match(dig1, dig2) = false then
          begin
            writeln;
            writeln(format('(DATA) "%s', [scan[1][i]]));
            writeln(format('(DATA) "%s', [scan[2][j]]));
          end;
        end;

        if comparedatetime(gettimeutc(scan[1][i]), gettimeutc(scan[2][j])) <> 0 then
        begin
          writeln;
          writeln(format('(MTIME %s) "%s"', [time2str(gettimeutc(scan[1][i])), scan[1][i]]));
          writeln(format('(MTIME %s) "%s"', [time2str(gettimeutc(scan[2][j])), scan[2][j]]));
        end;

        if getmode(scan[1][i]) <> getmode(scan[2][j]) then
        begin
          writeln;
          writeln(format('(MODE %s) "%s"', [mode2str(getmode(scan[1][i])), scan[1][i]]));
          writeln(format('(MODE %s) "%s"', [mode2str(getmode(scan[2][j])), scan[2][j]]));
        end;

        if getattr(scan[1][i]) <> getattr(scan[2][j]) then
        begin
          writeln;
          writeln(format('(ATTR %s) "%s"', [attr2str(getattr(scan[1][i])), scan[1][i]]));
          writeln(format('(ATTR %s) "%s"', [attr2str(getattr(scan[2][j])), scan[2][j]]));
        end;

        if getuserid(scan[1][i]) <> getuserid(scan[2][j]) then
        begin
          writeln;
          writeln(format('(USER ID %s) "%s"', [inttostr(getuserid(scan[1][i])), scan[1][i]]));
          writeln(format('(USER ID %s) "%s"', [inttostr(getuserid(scan[2][j])), scan[2][j]]));
        end;

        if getusername(scan[1][i]) <> getusername(scan[2][j]) then
        begin
          writeln;
          writeln(format('(USER NAME %s) "%s"', [getusername(scan[1][i]), scan[1][i]]));
          writeln(format('(USER NAME %s) "%s"', [getusername(scan[2][j]), scan[2][j]]));
        end;

        if getgroupid(scan[1][i]) <> getgroupid(scan[2][j]) then
        begin
          writeln;
          writeln(format('(GROUP ID %s) "%s"', [inttostr(getgroupid(scan[1][i])), scan[1][i]]));
          writeln(format('(GROUP ID %s) "%s"', [inttostr(getgroupid(scan[2][j])), scan[2][j]]));
        end;

        if getgroupname(scan[1][i]) <> getgroupname(scan[2][j]) then
        begin
          writeln;
          writeln(format('(GROUP NAME %s) "%s"', [getgroupname(scan[1][i]), scan[1][i]]));
          writeln(format('(GROUP NAME %s) "%s"', [getgroupname(scan[2][j]), scan[2][j]]));
        end;

        if getsize(scan[1][i]) <> getsize(scan[2][j]) then
        begin
          writeln;
          writeln(format('(SIZE %s) "%s"', [size2str(getsize(scan[1][i])), scan[1][i]]));
          writeln(format('(SIZE %s) "%s"', [size2str(getsize(scan[2][j])), scan[2][j]]));
        end;

        if getsymlink(scan[1][i]) <> getsymlink(scan[2][j]) then
        begin
          writeln;
          writeln(format('(LINKNAME %s) "%s"', [getsymlink(scan[1][i]), scan[1][i]]));
          writeln(format('(LINKNAME %s) "%s"', [getsymlink(scan[2][j]), scan[2][j]]));
        end;

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
