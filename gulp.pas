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

program Gulp;

{$I gulp.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} BaseUnix,  cthreads, {$ENDIF}
  Application,
  Classes;

var
  App: TGulpApplication;

{ control+c event }

{$IFDEF MSWINDOWS}
function CtrlHandler(CtrlType: longword): longbool;
begin
  case CtrlType of
    CTRL_C_EVENT:        App.Terminate;
    CTRL_BREAK_EVENT:    App.Terminate;
    CTRL_CLOSE_EVENT:    App.Terminate;
    CTRL_LOGOFF_EVENT:   App.Terminate;
    CTRL_SHUTDOWN_EVENT: App.Terminate;
  end;
  Result := True;
end;
{$ENDIF}

{$IFDEF UNIX}
procedure CtrlHandler(sig: cint);
begin
  case sig of
    SIGINT:  App.Terminate;
    SIGQUIT: App.Terminate;
    SIGKILL: App.Terminate;
    SIGSTOP: App.Terminate;
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
    SetConsoleCtrlHandler(CtrlHandler, TRUE);
  {$ENDIF}
  {$IFDEF UNIX}
    na.sa_handler  := SigActionHandler(CtrlHandler);
    FillChar(na.sa_mask, SizeOf(na.sa_mask), #0);
    na.sa_flags    := SA_ONESHOT;
    na.sa_restorer := nil;
    fpSigAction(SIGINT, @na, @oa);
  {$ENDIF}
end;

begin
  SetCtrlCHandler(@CtrlHandler);
  App := TGulpApplication.Create(nil);
  App.Run;
  App.Destroy;
end.
