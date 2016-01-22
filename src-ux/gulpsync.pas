unit gulpsync;

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TSyncForm }

  TSyncForm = class(TForm)
    CompressionModeComboBox: TComboBox;
    CompressionModeLabel: TLabel;
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    UpdateModeComboBox: TComboBox;
    ExcludeDividerBevel: TDividerBevel;
    IncludeDividerBevel: TDividerBevel;
    FolderDividerBevel: TDividerBevel;
    FileNameBitBtn: TBitBtn;
    OptionDividerBevel: TDividerBevel;
    FileNameEdit: TEdit;
    TopShape: TShape;
    TopImage: TImage;
    TopLabel: TLabel;
    UpdateModeLabel: TLabel;
    IncludeMemo: TMemo;
    ExcludeMemo: TMemo;
    SaveDialog: TSaveDialog;
    TopPanel: TPanel;
    procedure FileNameBitBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$R gulpsync.lfm}

{ TSyncForm }

procedure TSyncForm.FileNameBitBtnClick(Sender: TObject);
var
  Folder : string;
begin
  if SelectDirectory(Folder, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) = TRUE then
  begin
    FileNameEdit.Text := Folder;
  end;
end;

end.

