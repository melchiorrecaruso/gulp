unit gulpabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type
  { Taboutform }

  Taboutform = class(TForm)
    aboutcopyrigth: TLabel;
    aboutdescription: TLabel;
    aboutimage: TImage;
    aboutlicense: TLabel;
    aboutlink: TLabel;
    aboutname: TLabel;
    aboutversion: TLabel;
  private

  public

  end;

var
  aboutform: Taboutform;

implementation

initialization

  {$r gulpabout.lfm}

end.

