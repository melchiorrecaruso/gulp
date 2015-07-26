unit gulprestore;

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel,
  Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { TRestoreForm }

  TRestoreForm = class(TForm)
    OkBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    ModeComboBox: TComboBox;
    ExcludeDividerBevel: TDividerBevel;
    IncludeDividerBevel: TDividerBevel;
    FolderDividerBevel: TDividerBevel;
    FolderBitBtn: TBitBtn;
    RevisionComboBox: TComboBox;
    OptionDividerBevel: TDividerBevel;
    FolderEdit: TEdit;
    TopShape: TShape;
    TopImage: TImage;
    TopLabel: TLabel;
    ModeLabel: TLabel;
    RevisionLabel: TLabel;
    IncludeMemo: TMemo;
    ExcludeMemo: TMemo;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TopPanel: TPanel;
    procedure FolderBitBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


implementation

{$R gulprestore.lfm}

{ TRestoreForm }

procedure TRestoreForm.FolderBitBtnClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute = TRUE then
  begin
    FolderEdit.Text := SelectDirectoryDialog.FileName;
  end;
end;

end.

