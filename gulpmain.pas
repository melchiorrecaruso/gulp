unit gulpmain;

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  Menus, GulpLibrary, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    DetailsPanel: TPanel;
    ProgressLabel: TLabel;
    OpenAFileLabel: TLabel;
    NoFileOpenLabel: TLabel;
    MenuItem1: TMenuItem;
    SaveDialog: TSaveDialog;
    WelcomePanel: TPanel;
    ProgressPanel: TPanel;
    PopupMenu: TPopupMenu;
    RevisionLabel: TLabel;
    RevisionComboBox: TComboBox;
    NewSpeedButton: TSpeedButton;
    OpenSpeedButton: TSpeedButton;
    SyncBitBtn: TBitBtn;
    RestoreBitBtn: TBitBtn;
    OptionBitBtn: TBitBtn;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    TopShape: TShape;
    TreeView: TTreeView;
    procedure MenuItem1Click(Sender: TObject);
    procedure NewSpeedButtonClick(Sender: TObject);
    procedure OpenSpeedButtonClick(Sender: TObject);
    procedure OptionBitBtnClick(Sender: TObject);
    procedure RevisionComboBoxChange(Sender: TObject);

    procedure SyncBitBtnClick(Sender: TObject);
    procedure RestoreBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);



  private
    { private declarations }
    App: TGulpApplication;
    AppFolders: TStringList;
    AppList: TList;

    procedure Wait(Value: boolean);
    procedure Open(const FileName: string);

    function Sync(var FileName: string): longint;
    function Restore(FileName: string): longint;



    procedure SetPath(Node: TTreeNode);
    procedure DoList(var Item: TGulpItem);
    procedure DoMessage(const Message: string);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  GulpRestore,
  GulpSync;

{$R gulpmain.lfm}

{ TMainForm }

procedure TMainForm.Wait(Value: boolean);
begin
  TreeView        .Enabled := Value;
  SyncBitBtn      .Enabled := Value;
  RestoreBitBtn   .Enabled := Value;
  RevisionComboBox.Enabled := Value;
  OptionBitBtn    .Enabled := Value;

  ProgressPanel.Visible := not Value;
  Application.ProcessMessages;
end;

procedure TMainForm.Open(const FileName: string);
var
     I : longint;
  Root : TTreeNode;
   Ver : longword = 0;
begin
  TreeView.Items.Clear;
  Wait(FALSE);

  AppFolders.Clear;
  while AppList.Count <> 0 do
  begin
    TGulpItem(AppList[0]).Destroy;
    AppList.Delete(0);
  end;

  App.Reset;
  if RevisionComboBox.Items.Count = 0 then
    App.UntilVersion := $FFFFFFFF
  else
    App.UntilVersion := RevisionComboBox.ItemIndex + 1;

  try
    App.List(FileName);
    Root := TreeView.Items.AddChild(nil, ExtractFileName(FileName));
    Root.HasChildren   := TRUE;
    Root.ImageIndex    := 0;
    Root.SelectedIndex := 0;
    Root.StateIndex    := 0;
    Root.Expand(FALSE);


    Root := TreeView.Items.AddChild(TreeView.Items[0], '');
    Root.HasChildren   := TRUE;
    Root.ImageIndex    := 0;
    Root.SelectedIndex := 0;
    Root.StateIndex    := 0;
    //Root.Expand(FALSE);

    Root := TreeView.Items.AddChild(TreeView.Items[0], PathDelim);
    Root.HasChildren   := TRUE;
    Root.ImageIndex    := 0;
    Root.SelectedIndex := 0;
    Root.StateIndex    := 0;
    //Root.Expand(FALSE);


    Wait(TRUE);
  except
    on E: Exception do
    begin
      ShowMessage(Format('An exception was raised: "%s"', [E.Message]));
      OptionBitBtn.Click;
    end;
  end;

  if App.UntilVersion = $FFFFFFFF then
  begin
    for I := 0 to AppList.Count - 1 do
      Ver := Max(Ver, TGulpItem(AppList[I]).Version);

    RevisionComboBox.Clear;
    for I := 0 to Ver - 1 do
      RevisionComboBox.AddItem(' Revision ' + IntToStr(I + 1), nil);
    RevisionComboBox.ItemIndex := RevisionComboBox.Items.Count - 1;
  end;
end;

function TMainForm.Sync(var FileName: string): longint;
var
  I : longint;
  F : TSyncForm;
begin
  F := TSyncForm.Create(nil);
  try
    // --- COPY FORM STYLE --- //
    F.Font.Name := Font.Name;

    F.FileNameEdit.Text := FileName;
    F.UpdateModeComboBox.ItemIndex := 1;
    F.CompressionModeComboBox.ItemIndex := 0;

    Result := F.ShowModal;
    if Result = mrOk then
    begin
      App.Reset;
      FileName := F.FileNameEdit.Text;
      case F.CompressionModeComboBox.ItemIndex of
        0: App.StorageFlags := [];
        1: App.StorageFlags := [gsfGZ, gsfFASTEST];
        2: App.StorageFlags := [gsfGZ, gsfDEFAULT];
        3: App.StorageFlags := [gsfGZ, gsfMAX];
      end;
      case F.UpdateModeComboBox.ItemIndex of
        0: App.NoDelete := FALSE;
        1: App.NoDelete := TRUE;
      end;
      for I := 0 to F.IncludeMemo.Lines.Count - 1 do
        App.Include.Add(F.IncludeMemo.Lines[I]);
      for I := 0 to F.ExcludeMemo.Lines.Count - 1 do
        App.Exclude.Add(F.ExcludeMemo.Lines[I]);
    end;

  finally
    FreeAndNil(F);
  end;

  if Result = mrOK then
  begin
    try
      App.Sync(FileName);
    except
      on E: Exception do
      begin
        ShowMessage(Format('An exception was raised: "%s"', [E.Message]));
        OptionBitBtn.Click;
      end;
    end;
  end;
end;

function TMainForm.Restore(FileName: string): longint;
begin




end;










procedure TMainForm.DoList(var Item: TGulpItem);
var
  S : String;
begin
  S := ExtractFileDir(Item.Name);
  while AppFolders.IndexOf(S) = - 1 do
  begin
    AppFolders.Add(S);
    if S <> ExtractFileDir(S) then
      S := ExtractFileDir(S)
    else
      Break;
  end;

  AppList.Add(Item);
  begin
    Item := nil;
  end;
end;

procedure TMainForm.DoMessage(const Message: string);
begin

end;

procedure TMainForm.SetPath(Node: TTreeNode);
var
  Added : TStringList;
      I : longint;
   Name : string = '';
   Path : string = '';
  Start : TTreeNode;
begin
  Added := TStringList.Create;
  {$IFDEF UNIX}
    Added.CaseSensitive := TRUE;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Added.CaseSensitive := FALSE;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  Added.Duplicates := dupIgnore;
  Added.Sorted     := TRUE;

  if Node.Level > 0 then
  begin
    Start := Node;
    Path  := IncludeTrailingPathDelimiter(Start.Text);
    while Start.Level > 1 do
    begin
      Start := Start.Parent;
      Path  := IncludeTrailingPathDelimiter(Start.Text) + Path;
    end;
  end;

  TreeView.BeginUpdate;

  for I := AppFolders.Count - 1 downto 0 do
  begin


    if AnsiCompareFileName(Path, ExtractFilePath(AppFolders[I])) = 0 then
    begin
      Name := ExtractFileName(AppFolders[I]);
      if Added.IndexOf(Name) = -1 then
      begin
        Start := TreeView.Items.AddChildFirst(Node, Name);
        Start.ImageIndex    := 1;
        Start.SelectedIndex := 1;
        Start.StateIndex    := 1;
        Start.HasChildren   := TRUE;
        Added.Add(Name);
      end;
    end;
  end;

  for I := AppList.Count - 1 downto 0 do
    if AnsiCompareFileName(Path, ExtractFilePath(TGulpItem(AppList[I]).Name)) = 0 then
    begin
      Name := ExtractFileName(TGulpItem(AppList[I]).Name);
      if Added.IndexOf(Name) = -1 then
      begin
        Start := TreeView.Items.AddChild(Node, Name);
        Start.ImageIndex    := 3;
        Start.SelectedIndex := 3;
        Start.StateIndex    := 3;
        Start.HasChildren   := FALSE;
        Added.Add(Name);
      end;
    end;

  if Node.Count = 0 then
    Node.HasChildren := FALSE;
  TreeView.EndUpdate;
  FreeAndNil(Added);
end;

procedure TMainForm.RestoreBitBtnClick(Sender: TObject);
var
          F : TRestoreForm;
          I : longint;
      Start : TTreeNode;
  StartName : string;
begin
  F := TRestoreForm.Create(nil);
  try
    // --- FORM STYLE --- //
    F.Font.Name := 'Droid Sans';
    F.FolderEdit.Text := GetCurrentDir;
    F.RevisionComboBox.Clear;
    for I := 0 to RevisionComboBox.Items.Count - 1 do
      F.RevisionComboBox.AddItem(RevisionComboBox.Items[I], nil);
    F.RevisionComboBox.ItemIndex := RevisionComboBox.ItemIndex;
    F.ModeComboBox    .ItemIndex := 1;

    F.ExcludeMemo.Clear;
    F.IncludeMemo.Clear;
    for I := TreeView.SelectionCount - 1 downto 0 do
    begin
      Start     := TreeView.Selections[I];
      StartName := Start.Text;
      while Start.Level > 1 do
      begin
        Start      := Start.Parent;
        StartName  := IncludeTrailingPathDelimiter(Start.Text) + StartName;
      end;
      F.IncludeMemo.Lines.Add(StartName);
    end;
    if F.IncludeMemo.Lines.Count = 0 then
      F.IncludeMemo.Lines.Add('*');

    if F.ShowModal = mrOk then
    begin



    end;

  finally
    FreeAndNil(F);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  App           := TGulpApplication.Create;
  App.OnMessage := DoMessage;
  App.OnList    := DoList;

  AppFolders := TStringList.Create;
  {$IFDEF UNIX}
    AppFolders.CaseSensitive := TRUE;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      AppFolders.CaseSensitive := FALSE;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  AppFolders.Duplicates := dupIgnore;
  AppFolders.Sorted     := TRUE;


  AppList := TList.Create;




  // --- FORM STYLE --- //
  Font.Name := 'Droid Sans';
  NoFileOpenLabel.Font.Size  := 20;
  NoFileOpenLabel.Font.Style := [fsBold];
  OpenAFileLabel.Font.Size   := 14;
  OpenAFileLabel.Font.Style  := [];

  WelcomePanel.Color   := clNone;
  WelcomePanel.Left    := (Width  - WelcomePanel.Width ) div 2;
  WelcomePanel.Visible := TRUE;

  ProgressLabel.Font.Size  := 20;
  ProgressLabel.Font.Style := [fsBold];
  ProgressPanel.Color   := clNone;
  ProgressPanel.Left    := (Width  - ProgressPanel.Width ) div 2;
  ProgressPanel.Visible := FALSE;

  DetailsPanel.Visible  := FALSE;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  AppFolders.Destroy;
  while AppList.Count <> 0 do
  begin
    TGulpItem(AppList[0]).Destroy;
    AppList.Delete(0);
  end;
  AppList.Destroy;
  App.Destroy;
end;

procedure TMainForm.TreeViewDblClick(Sender: TObject);
begin
  if TreeView.Selected <> nil then
    TreeView.Selected.Expand(FALSE);
end;

procedure TMainForm.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  AllowExpansion := TRUE;
  if Node.Count = 0 then
  begin
    SetPath(Node);
  end;
end;

procedure TMainForm.SyncBitBtnClick(Sender: TObject);
var
  F : TSyncForm;
  I : longint;
begin
  F := TSyncForm.Create(nil);
  try
    // --- FORM STYLE --- //
    F.Font.Name := 'Droid Sans';
    F.FileNameEdit.Text := OpenDialog.FileName;
    F.UpdateModeComboBox.ItemIndex := 1;
    F.CompressionModeComboBox.ItemIndex := 0;

    if F.ShowModal = mrOk then
    begin
      Wait(FALSE);
      ProgressPanel.Visible := TRUE;

      App.Reset;
      case F.CompressionModeComboBox.ItemIndex of
        0: App.StorageFlags := [];
        1: App.StorageFlags := [gsfGZ, gsfFASTEST];
        2: App.StorageFlags := [gsfGZ, gsfDEFAULT];
        3: App.StorageFlags := [gsfGZ, gsfMAX];
      end;
      case F.UpdateModeComboBox.ItemIndex of
        0: App.NoDelete := FALSE;
        1: App.NoDelete := TRUE;
      end;
      for I := 0 to F.IncludeMemo.Lines.Count - 1 do
        App.Include.Add(F.IncludeMemo.Lines[I]);
      for I := 0 to F.ExcludeMemo.Lines.Count - 1 do
        App.Exclude.Add(F.ExcludeMemo.Lines[I]);



      try
        App.Sync(F.FileNameEdit.Text);
      except
        on E: Exception do
        begin
          ShowMessage(Format('An exception was raised: "%s"', [E.Message]));
          OptionBitBtn.Click;
        end;
      end;






    end;

  finally
    FreeAndNil(F);
  end;
end;

procedure TMainForm.RevisionComboBoxChange(Sender: TObject);
begin
  Open(TreeView.Items.GetFirstNode.Text);
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
var
    I : longint;
  Ver : longword = 0;
begin
  if OpenDialog.Execute then
  begin
    Open(OpenDialog.FileName);

    for I := 0 to AppList.Count - 1 do
      Ver := Max(Ver, TGulpItem(AppList[I]).Version);

    RevisionComboBox.Clear;
    for I := 0 to Ver - 1 do
      RevisionComboBox.AddItem(' Revision ' + IntToStr(I + 1), nil);
    RevisionComboBox.ItemIndex := RevisionComboBox.Items.Count - 1;
  end;
end;

procedure TMainForm.NewSpeedButtonClick(Sender: TObject);
var
  FileName : string;
begin
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    if Sync(FileName) = mrOK then
    begin
      Open(FileName);
    end;
  end;
end;

procedure TMainForm.OpenSpeedButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    WelcomePanel.Visible := FALSE;
    Open(OpenDialog.FileName);
  end;
end;

procedure TMainForm.OptionBitBtnClick(Sender: TObject);
begin

  while AppList.Count <> 0 do
  begin
    TGulpItem(AppList[0]).Destroy;
    AppList.Delete(0);
  end;
  RevisionComboBox.Clear;
  TreeView.Items.Clear;

  Wait(FALSE);
  WelcomePanel.Visible  := TRUE;
end;

end.

