{
  Copyright (c) 2016 Melchiorre Caruso.

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

    The gulp diff utility.

  Modified:

    v0.0.3 - 2016.01.09 by Melchiorre Caruso.
}

program GDiff;

{$codepage utf8}
{$mode objfpc}{$H+}

uses
  GulpCommon, GulpScanner, Sha1, SysUtils;

var
  I, J : longint;
  S    : rawbytestring;
  Scan : array[1..2] of TScanner;

begin
  writeln('GDIFF v0.0.3 diff utility, copyright (c) 2016 Melchiorre Caruso.');
  if (ParamCount <> 2) or
     (DirectoryExists(ParamStr(1)) = FALSE) or
     (DirectoryExists(ParamStr(2)) = FALSE) then
  begin
    writeln('Usage: gdiff directory1 directory2');
    writeln('Compare two DIRECTORIES file by file.');
  end else
  begin

    for I := 1 to 2 do
    begin
      Scan[I] := TScanner.Create;
      Scan[I].Add(IncludeTrailingPathDelimiter(ParamStr(I)) + '*');
      writeln(Format('%d object(s) in folder "%s"', [Scan[I].Count, ParamStr(I)]));
    end;

    for I := 0 to Scan[1].Count - 1 do
    begin
      S := Scan[1][I];
      Delete(S, 1, Length(IncludeTrailingPathDelimiter(ParamStr(1))));
      S := IncludeTrailingPathDelimiter(ParamStr(2)) + S;

      J := Scan[2].Find(S);
      if J <> -1 then
      begin

        if (FileGetAttr(Scan[1][I]) and faDirectory = 0) and
           (FileGetAttr(Scan[2][J]) and faDirectory = 0) then
          if SHA1Match(
               SHA1File(Scan[1][I], 4096),
               SHA1File(Scan[2][J], 4096)) = FALSE then
            writeln(Format('"%s" "%s" differ', [Scan[1][I], Scan[2][J]]));

        if FileGetTimeUTC(Scan[1][I]) <> FileGetTimeUTC(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in time', [Scan[1][I], Scan[2][J]]));

        if FileGetSize(Scan[1][I]) <> FileGetSize(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in size', [Scan[1][I], Scan[2][J]]));

        if FileGetAttr(Scan[1][I]) <> FileGetAttr(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in attr', [Scan[1][I], Scan[2][J]]));

        if FileGetMode(Scan[1][I]) <> FileGetMode(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in mode', [Scan[1][I], Scan[2][J]]));

        if FileGetLinkName(Scan[1][I]) <> FileGetLinkName(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in link', [Scan[1][I], Scan[2][J]]));

        if FileGetUserID(Scan[1][I]) <> FileGetUserID(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in uid ', [Scan[1][I], Scan[2][J]]));

        if FileGetUserName(Scan[1][I]) <> FileGetUserName(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in unm ', [Scan[1][I], Scan[2][J]]));

        if FileGetGroupID(Scan[1][I]) <> FileGetGroupID(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in gid ', [Scan[1][I], Scan[2][J]]));

        if FileGetGroupName(Scan[1][I]) <> FileGetGroupName(Scan[2][J]) then
          writeln(Format('"%s" and "%s" differ in gnm ', [Scan[1][I], Scan[2][J]]));

        Scan[2].Delete(J);
      end else
        writeln(Format('"%s" not founded in "%s"',[Scan[1][I], ParamStr(2)]));
    end;

    for J := 0 to Scan[2].Count - 1 do
      writeln(Format('"%s" not founded in "%s"',[Scan[2][J], ParamStr(1)]));

    for J := 1 to 2 do
      Scan[J].Destroy;
  end;
end.

