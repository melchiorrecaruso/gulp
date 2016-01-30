unit gulpsync;

{$mode objfpc}
{$h+}

interface

uses
  classes, sysutils, fileutil, dividerbevel,
  forms, controls, graphics, dialogs,
  extctrls, stdctrls, buttons;

type

  { tsyncform }

  tsyncform = class(tform)
    okbitbtn: tbitbtn;
    cancelbitbtn: tbitbtn;
    updatemodecombobox: tcombobox;
    excludedividerbevel: tdividerbevel;
    includedividerbevel: tdividerbevel;
    folderdividerbevel: tdividerbevel;
    filenamebitbtn: tbitbtn;
    optiondividerbevel: tdividerbevel;
    filenameedit: tedit;
    topshape: tshape;
    topimage: timage;
    toplabel: tlabel;
    updatemodelabel: tlabel;
    includememo: tmemo;
    excludememo: tmemo;
    savedialog: tsavedialog;
    toppanel: tpanel;
    procedure filenamebitbtnclick(sender: tobject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$r gulpsync.lfm}

{ tsyncform }

procedure tsyncform.filenamebitbtnclick(sender: tobject);
var
  folder : string;
begin
  if selectdirectory(folder, [sdallowcreate, sdperformcreate, sdprompt], 0) = true then
  begin
    filenameedit.text := folder;
  end;
end;

end.

