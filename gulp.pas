{
  Copyright (c) 2014-2015 Melchiorre Caruso.

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

    The journaling archiver utility.

  Modified:

    v0.0.2 - 2015.05.02 by Melchiorre Caruso.
}

program Gulp;

uses
  {$IFDEF UNIX}
    cthreads,
    BaseUnix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  Application;

var
  App: TGulpApplication;

{ Control+c event }

{$IFDEF MSWINDOWS}
function CtrlHandler(CtrlType: longword): longbool;
begin
  case CtrlType of
    CTRL_C_EVENT:        App.Abort;
    CTRL_BREAK_EVENT:    App.Abort;
    CTRL_CLOSE_EVENT:    App.Abort;
    CTRL_LOGOFF_EVENT:   App.Abort;
    CTRL_SHUTDOWN_EVENT: App.Abort;
  end;
  Result := True;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure CtrlHandler(sig: cint);
begin
  case sig of
    SIGINT:  App.Abort;
    SIGQUIT: App.Abort;
    SIGKILL: App.Abort;
    SIGSTOP: App.Abort;
  end;
end;
{$ENDIF}

procedure SetCtrlCHandler(CtrlHandler: pointer);
{$IFDEF UNIX}
var
  oa, na: SigActionRec;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    SetConsoleCtrlHandler(@CtrlHandler, TRUE);
  {$ENDIF}
  {$IFDEF UNIX}
    na.sa_handler  := SigActionHandler(CtrlHandler);
    FillChar(na.sa_mask, SizeOf(na.sa_mask), #0);
    na.sa_flags    := SA_ONESHOT;
    na.sa_restorer := nil;
    fpSigAction(SIGINT, @na, @oa);
  {$ENDIF}
end;

{$R *.res}

begin
  SetCtrlCHandler(@CtrlHandler);
  App := TGulpApplication.Create(nil);
  App.Run;
  App.Destroy;
end.
