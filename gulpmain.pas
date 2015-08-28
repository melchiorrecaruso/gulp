unit gulpmain;

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel,
  Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  Menus, GulpLibrary, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    AttributesEdit: TEdit;
    AttributesLabel: TLabel;
    ModeEdit: TEdit;
    ModeLabel: TLabel;
    MenuItem4: TMenuItem;
    ResetBitBtn: TBitBtn;
    ApplyBitBtn: TBitBtn;
    NameComboBox: TComboBox;
    PathComboBox: TComboBox;
    FiltersBitBtn: TBitBtn;
    NameLabel: TLabel;
    PathLabel: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    byNameMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    ListView: TListView;
    OpenAFileLabel: TLabel;
    NoFileOpenLabel: TLabel;
    FiltersPanel: TPanel;
    PopupMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    TopShape: TShape;
    BottomShape: TShape;
    WelcomePanel: TPanel;
    RevisionLabel: TLabel;
    RevisionComboBox: TComboBox;
    NewSpeedButton: TSpeedButton;
    OpenSpeedButton: TSpeedButton;
    SyncBitBtn: TBitBtn;
    RestoreBitBtn: TBitBtn;
    HomeBitBtn: TBitBtn;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;

    procedure ApplyBitBtnClick(Sender: TObject);
    procedure AttributesEditKeyPress(Sender: TObject; var Key: char);
    procedure FiltersBitBtnClick(Sender: TObject);
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure MenuItem1Click(Sender: TObject);
    procedure ModeEditKeyPress(Sender: TObject; var Key: char);
    procedure NameComboBoxKeyPress(Sender: TObject; var Key: char);
    procedure NewSpeedButtonClick(Sender: TObject);
    procedure OpenSpeedButtonClick(Sender: TObject);
    procedure HomeBitBtnClick(Sender: TObject);
    procedure PathComboBoxCloseUp(Sender: TObject);
    procedure PathComboBoxKeyPress(Sender: TObject; var Key: char);
    procedure ResetBitBtnClick(Sender: TObject);
    procedure RevisionComboBoxChange(Sender: TObject);

    procedure SyncBitBtnClick(Sender: TObject);
    procedure RestoreBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);




  private
    { private declarations }

    AppFileName: string;
    AppList: TList;
    AppListAux: TList;

    procedure Wait(Value: boolean);
    procedure Open(const FileName: string);

    function Sync(var FileName: string): longint;
    function Restore(FileName: string): longint;





  public
    { public declarations }
  end;

  TMyThread = class(TThread)
  private
    FApp          : TGulpApplication;
    FMessage   : string;
    FItem      :



    FOnMessage : TGulpOnMessage;

    procedure ShowItem;
    procedure ShowMessage;

  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);
    property OnMessage: TGulpOnMessage read FOnShowStatus write FOnShowStatus;
  end;


var
  MainForm: TMainForm;

implementation

uses
  GulpCommon,
  GulpRestore,
  GulpSync,
  IniFiles;

type
  TLiteGulpItem = class
  private
    FVersion  : longword;
    FName     : string;
    FPath     : string;
    FTime     : TDateTime;
    FAttr     : longint;
    FMode     : longint;
    FSize     : int64;
    FPlatform : TGulpPlatform;
    FVisible  : boolean;
  end;

{$R gulpmain.lfm}

constructor TMyThread.Create(CreateSuspended : boolean);
begin

  FApp           := TGulpApplication.Create;
  FApp.OnMessage := DoMessage;
  FApp.OnList    := DoList;




  FreeOnTerminate := FALSE;



  inherited Create(CreateSuspended);
end;

procedure TMyThread.ShowStatus;
begin
  if Assigned(FOnShowStatus) then
    FOnShowStatus(fStatusText);
end;

procedure TMyThread.Execute;
var
  NewStatus : string;
begin
  FStatusText := 'TMyThread Starting...';
  Synchronize(ShowStatus);
  FStatusText := 'TMyThread Running...';



  Synchronize(Showstatus);
end;




function Compare(Item1, Item2: TLiteGulpItem): longint;
begin
  if ((Item1.FAttr and FaDirectory = faDirectory) xor
      (Item2.FAttr and FaDirectory = faDirectory)) = FALSE then
    Result := AnsiCompareFileName(Item1.FName, Item2.FName)
  else
    if (Item1.FAttr and FaDirectory = faDirectory) then
      Result := -1
    else
      Result :=  1;

  if Result = 0 then
    Result := AnsiCompareFileName(Item1.FPath, Item2.FPath);
end;

procedure Insert(List: TList; Item: TLiteGulpItem);
var
  L, M, H, I : longint;
begin
  L := 0;
  H := List.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := Compare(TLiteGulpItem(List[M]), Item);
    if I < 0 then
      L := M + 1
    else
      if I > 0 then
        H := M - 1
      else
        H := -2;
  end;

  if List.Count <> 0 then
  begin
    if I < 0 then
      List.Insert(M + 1, Item)
    else
      if I > 0 then
        List.Insert(M, Item)
      else
        List.Insert(M, Item);
  end else
    List.Add(Item);
end;









{ TMainForm }

procedure TMainForm.Wait(Value: boolean);
begin
  WelcomePanel      .Visible := FALSE;
  SyncBitBtn        .Enabled := Value;
  RestoreBitBtn     .Enabled := Value;
  FiltersBitBtn     .Enabled := Value;
  RevisionComboBox  .Enabled := Value;
  HomeBitBtn        .Enabled := Value;


  NameComboBox      .Enabled := Value;
  AttributesEdit    .Enabled := Value;
  ModeEdit          .Enabled := Value;
  PathComboBox      .Enabled := Value;


  ResetBitBtn       .Enabled := Value;
  ApplyBitBtn       .Enabled := Value;



  ListView          .Enabled := Value;
  ListView.ShowColumnHeaders := Value;
end;

procedure TMainForm.Open(const FileName: string);
var
    I : longint;
  Ver : longword = 0;
  Folders: TStringList;
begin
  Wait(FALSE);
  AppListAux.Clear;
  while AppList.Count <> 0 do
  begin
    TLiteGulpItem(AppList[0]).Destroy;
    AppList.Delete(0);
  end;

  App.Reset;
  if RevisionComboBox.Items.Count = 0 then
    App.UntilVersion := $FFFFFFFF
  else
    App.UntilVersion := RevisionComboBox.ItemIndex + 1;

  try
    App.List(FileName);
    ResetBitBtn.Click;
    ApplyBitBtn.Click;
  except
    on E: Exception do
    begin
      ShowMessage(Format('An exception was raised: "%s"', [E.Message]));
      HomeBitBtn.Click;
    end;
  end;
  Wait(TRUE);





  if App.UntilVersion = $FFFFFFFF then
  begin
    for I := 0 to AppList.Count - 1 do
    begin
      Ver := Max(Ver, TLiteGulpItem(AppList[I]).FVersion);
    end;

    RevisionComboBox.Clear;
    for I := 0 to Ver - 1 do
      RevisionComboBox.AddItem(' Revision ' + IntToStr(I + 1), nil);
    RevisionComboBox.ItemIndex := RevisionComboBox.Items.Count - 1;
  end;

  Folders := TStringList.Create;
  {$IFDEF UNIX}
    Folders.CaseSensitive := TRUE;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Folders.CaseSensitive := FALSE;
    {$ELSE}
      Unsupported platform...
    {$ENDIF}
  {$ENDIF}
  Folders.Duplicates  := dupIgnore;
  Folders.Sorted      := TRUE;

  for I := 0 to AppList.Count - 1 do
    Folders.Add(TLiteGulpItem(AppList[I]).FPath);

  PathComboBox.Items.Clear;
  for I := 0 to Folders.Count - 1 do
    PathComboBox.Items.Add(Folders[I]);

  Folders.Destroy;
end;













procedure TMainForm.DoList(const Item: TGulpItem);
var
  T : TLiteGulpItem;
begin
  T := TLiteGulpItem.Create;
  T.FVersion  := Item.Version;
  T.FName     := ExtractFileName(Item.Name);
  T.FPath     := ExtractFilePath(Item.Name);
  T.FTime     := Item.Time;
  T.FAttr     := Item.Attributes;
  T.FMode     := Item.Mode;
  T.FSize     := Item.Size;
  T.FPlatform := Item.Platform;
  T.FVisible  := FALSE;
  AppList.Add(T);
end;

procedure TMainForm.DoMessage(const Message: string);
begin

end;


procedure TMainForm.FormCreate(Sender: TObject);
var
    I : longint;
  Ini : TIniFile;
begin
  // LOAD SETTINGS
  Ini := TIniFile.Create(GetAppConfigFile(FALSE));

  Self.Top         :=              Ini.ReadInteger('Settings', 'Main.Top',        200);
  Self.Height      :=              Ini.ReadInteger('Settings', 'Main.Height',     400);
  Self.Left        :=              Ini.ReadInteger('Settings', 'Main.Left',       400);
  Self.Width       :=              Ini.ReadInteger('Settings', 'Main.Width',      600);
  Self.WindowState := TWindowState(Ini.ReadInteger('Settings', 'Main.WindowState', 0));

  Ini.Destroy;






  App           := TGulpApplication.Create;
  App.OnMessage := DoMessage;
  App.OnList    := DoList;

  AppList       := TList.Create;
  AppListAux    := TList.Create;

  // --- FORM STYLE --- //
  Font.Name := 'Droid Sans';
  NoFileOpenLabel.Font.Size  := 20;
  NoFileOpenLabel.Font.Style := [fsBold];
  OpenAFileLabel.Font.Size   := 14;
  OpenAFileLabel.Font.Style  := [];

  FiltersPanel.AutoSize := FALSE;
  FiltersPanel.Height   := 1;

  WelcomePanel.Color   := clNone;
  WelcomePanel.Left    := (Width  - WelcomePanel.Width ) div 2;
  WelcomePanel.Visible := TRUE;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
    I : longint;
  Ini : TIniFile;
begin
  // SAVE SETTINGS
  Ini := TIniFile.Create(GetAppConfigFile(FALSE));

  if Self.WindowState = wsNormal then
  begin
    Ini.WriteInteger('Settings', 'Main.Top',    Self.Top);
    Ini.WriteInteger('Settings', 'Main.Height', Self.Height);
    Ini.WriteInteger('Settings', 'Main.Left',   Self.Left);
    Ini.WriteInteger('Settings', 'Main.Width',  Self.Width);
  end;
  Ini.WriteInteger  ('Settings', 'Main.WindowState', longint(Self.WindowState));




  Ini.Destroy;


  AppListAux.Clear;
  while AppList.Count <> 0 do
  begin
    TLiteGulpItem(AppList[0]).Destroy;
    AppList.Delete(0);
  end;
  AppListAux.Destroy;
  AppList.Destroy;
  App.Destroy;
end;







procedure TMainForm.ListViewData(Sender: TObject; Item: TListItem);
var
  T : TLiteGulpItem;
begin
  T := TLiteGulpItem(AppListAux[Item.Index]);

  if T.FVisible = TRUE then
  begin;
    Item.Caption := T.FName;
    if T.FAttr and faDirectory = 0 then
    begin
      Item.ImageIndex := 3;
      Item.StateIndex := 3;
      Item.SubItems.Add(SizeToString(T.FSize));
    end else
    begin

      Item.ImageIndex := 1;
      Item.StateIndex := 1;
      Item.SubItems.Add('-');
    end;

    Item.SubItems.Add(TimeToString(T.FTime));
    Item.SubItems.Add(AttrToString(T.FAttr));
    Item.SubItems.Add(ModeToString(T.FMode));
    Item.SubItems.Add(PlatToString(T.FPlatform));
    Item.SubItems.Add(T.FPath);
  end;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
var
  T : TLiteGulpItem;
begin
  if ListView.SelCount = 1 then
  begin
    T := TLiteGulpItem(AppListAux[ListView.Selected.Index]);
    if T.FAttr and faDirectory = faDirectory then
    begin
      NameComboBox  .Text := '*';
      AttributesEdit.Text := '*';
      ModeEdit      .Text := '*';
      PathComboBox  .Text := T.FPath + IncludeTrailingPathDelimiter(T.FName);

      ApplyBitBtn.Click;
    end;
  end;
end;








// Filter panel routines //

procedure TMainForm.NameComboBoxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ApplyBitBtn.Click;
end;

procedure TMainForm.AttributesEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ApplyBitBtn.Click;
end;

procedure TMainForm.ModeEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ApplyBitBtn.Click;
end;

procedure TMainForm.PathComboBoxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ApplyBitBtn.Click;
end;

procedure TMainForm.PathComboBoxCloseUp(Sender: TObject);
begin
  ApplyBitBtn.Click;
end;

procedure TMainForm.ResetBitBtnClick(Sender: TObject);
begin
  NameComboBox  .Text := '*';
  AttributesEdit.Text := '*';
  ModeEdit      .Text := '*';
  PathComboBox  .Text := '*';
end;

procedure TMainForm.ApplyBitBtnClick(Sender: TObject);
var
  I : longint;
  T : TLiteGulpItem;
begin
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  AppListAux.Clear;
  for I := 0 to AppList.Count- 1 do
  begin
    T := TLiteGulpItem(AppList[I]);
    T.FVisible :=
      FileNameMatch(T.FName, NameComboBox  .Text) and
      FileNameMatch(T.FPath, PathComboBox  .Text) and
      FileNameMatch(AttrToString(T.FAttr), AttributesEdit.Text) and
      FileNameMatch(ModeToString(T.FMode), ModeEdit      .Text);

    if T.FVisible = TRUE then
      Insert(AppListAux, T);
  end;
  ListView.Items.Count := AppListAux.Count;
  ListView.Items.EndUpdate;
end;

// Welcome panel routines

procedure TMainForm.NewSpeedButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    AppFileName := SaveDialog.FileName;
    if Sync(AppFileName) = mrOK then
    begin
      Open(AppFileName);
    end;
  end;
end;

procedure TMainForm.OpenSpeedButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    AppFileName := OpenDialog.FileName;
    begin
      Open(AppFileName);
    end;
  end;
end;

// Main panel routines

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
        HomeBitBtn.Click;
      end;
    end;
  end;
end;

procedure TMainForm.SyncBitBtnClick(Sender: TObject);
var
  F : TSyncForm;
  I : longint;
  IsNeededToRun: boolean;
begin
  IsNeededToRun := FALSE;
  F := TSyncForm.Create(nil);
  try
    // --- FORM STYLE --- //
    F.Font.Name := 'Droid Sans';
    F.FileNameEdit.Text := OpenDialog.FileName;
    F.UpdateModeComboBox.ItemIndex := 1;
    F.CompressionModeComboBox.ItemIndex := 0;

    if F.ShowModal = mrOk then
    begin
      IsNeededToRun := TRUE;
      Wait(FALSE);

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
    end;

  finally
    FreeAndNil(F);
  end;

  Application.ProcessMessages;
  if IsNeededToRun = TRUE then
    try
      App.Sync(F.FileNameEdit.Text);
    except
      on E: Exception do
      begin
        ShowMessage(Format('An exception was raised: "%s"', [E.Message]));
        HomeBitBtn.Click;
      end;
    end;
end;

function TMainForm.Restore(FileName: string): longint;
begin




end;

procedure TMainForm.RestoreBitBtnClick(Sender: TObject);
var
  F : TRestoreForm;
  I : longint;
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

    if F.IncludeMemo.Lines.Count = 0 then
      F.IncludeMemo.Lines.Add('*');

    if F.ShowModal = mrOk then
    begin



    end;

  finally
    FreeAndNil(F);
  end;
end;

procedure TMainForm.FiltersBitBtnClick(Sender: TObject);
begin
  if FiltersPanel.Height < 10 then
  begin
    FiltersPanel.AutoSize := TRUE;
  end else
  begin
    FiltersPanel.AutoSize := FALSE;
    FiltersPanel.Height   := 1;
  end;
end;

procedure TMainForm.RevisionComboBoxChange(Sender: TObject);
begin
  Open(AppFileName);
end;

procedure TMainForm.HomeBitBtnClick(Sender: TObject);
begin
  Wait(FALSE);

  FiltersPanel.AutoSize := FALSE;
  FiltersPanel.Height   := 1;



  ListView.Items.Clear;
  RevisionComboBox.Clear;
  while AppList.Count <> 0 do
  begin
    TLiteGulpItem(AppList[0]).Destroy;
    AppList.Delete(0);
  end;
  WelcomePanel.Visible := TRUE;
end;

end.

