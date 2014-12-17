{
  Copyright (c) 2014 Melchiorre Caruso.

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
}

{
  Contains:

  Modifyed:

}

unit Status;

{$I gulp.inc}

interface

const
  csNoError           =   0;
  csCreateStreamError = 200;
  csOpenStreamError   = 201;
  csReadStreamError   = 202;


  csUserAbortError    = 255;

procedure SetStatus(Value: longint);
function GetStatus: longint;
function CheckStatus: boolean;

implementation

var
  CurrStatus: longint = csNoError;

procedure SetStatus(Value: longint);
begin
  if CurrStatus = csNoError then
    CurrStatus := Value;
end;

function  GetStatus: longint;
begin
  Result := CurrStatus;
end;

function CheckStatus: boolean;
begin
  Result := CurrStatus = csNoError;
end;

end.
