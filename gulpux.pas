{ Description: X application unit.

  Copyright (C) 2014-2022 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

program GulpUX;

{$Mode Objfpc} {$H+}
{$Define UsecThreads}

uses
{$ifdef Unix}
{$ifdef UsecThreads}
  cThreads,
  cMem,
 {$endif}
 {$endif}
  Interfaces,
  Forms,
  GulpMain,
  GulpAbout;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Gulp-UX';
  Application.Initialize;
  Application.CreateForm(tmainform, mainform);
  Application.Run;
end.
