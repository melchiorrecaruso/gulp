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

    Command line helper routines.

  Modified:

    v0.0.3 - 2016.01.09 by Melchiorre Caruso.
}

unit GulpCommandLine;

{$mode objfpc}{$H+}

interface

uses
  GulpList,
  SysUtils;

{ Command line routines }

function CheckOptions(const ShortOpts, LongOpts: rawbytestring;
  FileMasks: TRawByteStringList): rawbytestring;
function HasOption(const ShortOpt, LongOpt: rawbytestring): Boolean;
function GetOptionValue(const ShortOpt, LongOpt: rawbytestring): rawbytestring;

implementation

{ Check command line }

function CheckOptions(const ShortOpts, LongOpts: rawbytestring;
  FileMasks: TRawByteStringList): rawbytestring;
var
  I: LongInt = 1;
  S: rawbytestring;

  function CheckOption(const Options: rawbytestring): rawbytestring;
  begin
    Result := '';
    if Pos(S, Options) <> 0 then
    begin
      if Options[Pos(S, Options) + Length(S)] = ':' then
      begin
        Inc(I);
        if (I > ParamCount) then Result :=
            Format('Option at position %d does not allow an argument: "%s"',
            [I - 1, S])
        else
        if (Pos(ParamStr(I), ShortOpts) <> 0) or
          (Pos(ParamStr(I), LongOpts) <> 0) then Result :=
            Format('Option at position %d needs an argument : "%s"',
            [I - 1, S]);
      end else
      if Options[Pos(S, Options) + Length(S)] <> ' ' then Result :=
          Format('Invalid option at position %d: "%s"', [I, S]);
    end else
      Result := Format('Invalid option at position %d: "%s"', [I, S]);
  end;

begin
  Result := '';
  while (I <= ParamCount) and (Result = '') do
  begin
    S := ParamStr(I);
    if S[1] = '-' then
    begin
      if Length(S) <= 2 then Result := CheckOption(ShortOpts)
      else
      if S[2] = '-' then Result := CheckOption(LongOpts)
      else
        Result :=
          Format('Invalid option at position %d: "%s"', [I, S]);
    end else
    if Assigned(FileMasks) then FileMasks.Add(S);
    Inc(I);
  end;
end;

function HasOption(const ShortOpt, LongOpt: rawbytestring): Boolean;
var
  I: LongInt = 1;
  S: rawbytestring;
begin
  Result := False;
  while (I <= ParamCount) and (Result = False) do
  begin
    S := ParamStr(I);
    if S = ShortOpt then Result := True
    else
    if S = LongOpt then Result := True;
    Inc(I);
  end;
end;

function GetOptionValue(const ShortOpt, LongOpt: rawbytestring): rawbytestring;
var
  I: LongInt = 1;
  S: rawbytestring;
begin
  Result := '';
  while (I <= ParamCount) and (Result = '') do
  begin
    S := ParamStr(I);
    if S = ShortOpt then Result := ParamStr(I + 1)
    else
    if S = LongOpt then Result := ParamStr(I + 1);
    Inc(I);
  end;
end;

end.
