unit gulprestore;

{$mode objfpc}
{$h+}

interface

uses
  classes, sysutils, fileutil, dividerbevel,
  forms, controls, graphics, dialogs,
  extctrls, stdctrls, buttons;

type

  { trestoreform }

  trestoreform = class(tform)
    okbitbtn: tbitbtn;
    cancelbitbtn: tbitbtn;
    modecombobox: tcombobox;
    excludedividerbevel: tdividerbevel;
    includedividerbevel: tdividerbevel;
    folderdividerbevel: tdividerbevel;
    folderbitbtn: tbitbtn;
    revisioncombobox: tcombobox;
    optiondividerbevel: tdividerbevel;
    folderedit: tedit;
    topshape: tshape;
    topimage: timage;
    toplabel: tlabel;
    modelabel: tlabel;
    revisionlabel: tlabel;
    includememo: tmemo;
    excludememo: tmemo;
    selectdirectorydialog: tselectdirectorydialog;
    toppanel: tpanel;
    procedure folderbitbtnclick(sender: tobject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$r gulprestore.lfm}

{ trestoreform }

procedure trestoreform.folderbitbtnclick(sender: tobject);
begin
  if selectdirectorydialog.execute = true then
  begin
    folderedit.text := selectdirectorydialog.filename;
  end;
end;

end.

