unit gulpmain;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel,
  Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  Menus, Math, gulplibrary, gulplist;

type
  { tmainform }

  tliteitem = class
  private
    name     : rawbytestring;
    path     : rawbytestring;
    timeutc  : tdatetime;
    attr     : longint;
    mode     : longint;
    size     : int64;
    version  : longint;
    visible  : boolean;
  end;

  tliteitemlist = specialize tgenericlist<tliteitem>;

  { tmainform }

  TMainForm = class(TForm)
    AttributesEdit: TEdit;
    AttributesLabel: TLabel;
    IdleTimer1: TIdleTimer;
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
    PopupMenu2: TPopupMenu;
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
    app:      tgulpapplication;
    appname:  rawbytestring;
    applist1: tliteitemlist;
    applist2: tliteitemlist;


    procedure showitem(p: pgulpitem);
    procedure showmessage(const message: rawbytestring);


    procedure Wait(Value: boolean);
    procedure Open(const FileName: string);

    function Sync(var FileName: rawbytestring): longint;
    function Restore(FileName: rawbytestring): longint;


  public
    { public declarations }
  end;






var
  mainform: tmainform;

implementation

uses
  gulpcommon,
  gulpfixes,

  gulprestore,
  gulpscanner,
  gulpsync,
  inifiles;

{$r gulpmain.lfm}

function compare(item1, item2: tliteitem): longint;
begin
  if ((item1.attr and fadirectory = fadirectory) xor
      (item2.attr and fadirectory = fadirectory)) = false then
    result := ansicomparefilename(item1.name, item2.name)
  else
    if (item1.attr and fadirectory = fadirectory) then
      result := -1
    else
      result :=  1;

  if result = 0 then
    result := ansicomparefilename(item1.path, item2.path);
end;

{ tmainform }

procedure tmainform.wait(value: boolean);
begin
  welcomepanel      .visible := false;
  syncbitbtn        .enabled := value;
  restorebitbtn     .enabled := value;
  filtersbitbtn     .enabled := value;
  revisioncombobox  .enabled := value;
  homebitbtn        .enabled := value;


  namecombobox      .enabled := value;
  attributesedit    .enabled := value;
  modeedit          .enabled := value;
  pathcombobox      .enabled := value;


  resetbitbtn       .enabled := value;
  applybitbtn       .enabled := value;



  listview          .enabled := value;
  listview.showcolumnheaders := value;
end;

procedure tmainform.open(const filename: string);
var
  i:       longint;
  ver:     longword = 0;
  folders: trawbytestringlist;
begin
  wait(false);
  applist2.clear;
  while applist1.count <> 0 do
  begin
    applist1[0].destroy;
    applist1.delete(0);
  end;

  app.reset;
  if revisioncombobox.items.count = 0 then
    app.untilversion := $ffffffff
  else
    app.untilversion := revisioncombobox.itemindex + 1;

  try
    app.list(filename);
    resetbitbtn.click;
    applybitbtn.click;
  except
    on e: exception do
    begin
      showmessage(format('an exception was raised: "%s"', [e.message]));
      homebitbtn.click;
    end;
  end;
  wait(true);

  if app.untilversion = $ffffffff then
  begin
    for i := 0 to applist1.count - 1 do
      ver := max(ver, applist1[i].version);

    revisioncombobox.clear;
    for i := 0 to ver - 1 do
      revisioncombobox.additem(' revision ' + inttostr(i + 1), nil);
    revisioncombobox.itemindex := revisioncombobox.items.count - 1;
  end;

  folders := trawbytestringlist.create;
  for i := 0 to applist1.count - 1 do
    folders.add(applist1[i].path);

  pathcombobox.items.clear;
  for i := 0 to folders.count - 1 do
    pathcombobox.items.add(folders[i]);
  folders.destroy;
end;

procedure tmainform.formcreate(sender: tobject);
var
  i:   longint;
  ini: tinifile;
begin
  // load settings
  ini              := tinifile.create(getappconfigfile(false));
  self.top         :=              ini.readinteger('settings', 'main.top',        200);
  self.height      :=              ini.readinteger('settings', 'main.height',     400);
  self.left        :=              ini.readinteger('settings', 'main.left',       400);
  self.width       :=              ini.readinteger('settings', 'main.width',      600);
  self.windowstate := twindowstate(ini.readinteger('settings', 'main.windowstate', 0));
  ini.destroy;
  // gulp application core
  app               := tgulpapplication.create;
  app.onshowitem    := @showitem;
  app.onshowmessage := @showmessage;
  applist1          := tliteitemlist.create(@compare);
  applist2          := tliteitemlist.create(@compare);
  // form style
  font.name                  := 'droid sans';
  nofileopenlabel.font.size  := 20;
  nofileopenlabel.font.style := [fsbold];
  openafilelabel.font.size   := 14;
  openafilelabel.font.style  := [];

  filterspanel.autosize      := false;
  filterspanel.height        := 1;
  welcomepanel.color         := clnone;
  welcomepanel.left          := (width  - welcomepanel.width ) div 2;
  welcomepanel.visible       := true;
end;

procedure tmainform.formdestroy(sender: tobject);
var
  i:   longint;
  ini: tinifile;
begin
  // save settings
  ini := tinifile.create(getappconfigfile(false));
  if self.windowstate = wsnormal then
  begin
    ini.writeinteger('settings', 'main.top',    self.top);
    ini.writeinteger('settings', 'main.height', self.height);
    ini.writeinteger('settings', 'main.left',   self.left);
    ini.writeinteger('settings', 'main.width',  self.width);
  end;
  ini.writeinteger  ('settings', 'main.windowstate', longint(self.windowstate));
  ini.destroy;
  // destroy
  applist2.clear;
  while applist1.count <> 0 do
  begin
    applist1[0].destroy;
    applist1.delete(0);
  end;
  applist2.destroy;
  applist1.destroy;
  app.destroy;
end;

procedure tmainform.listviewdata(sender: tobject; item: tlistitem);
var
  t: tliteitem;
begin
  t := applist2[item.index];
  if t.visible = true then
  begin;
    item.caption := t.name;
    if t.attr and fadirectory = 0 then
    begin
      item.imageindex := 3;
      item.stateindex := 3;
      item.subitems.add(sizetostring(t.size));
    end else
    begin

      item.imageindex := 1;
      item.stateindex := 1;
      item.subitems.add('-');
    end;

    item.subitems.add(timetostring(t.timeutc));
    item.subitems.add(attrtostring(t.attr));
    item.subitems.add(modetostring(t.mode));
    item.subitems.add(t.path);
  end;
end;

procedure tmainform.menuitem1click(sender: tobject);
var
  t: tliteitem;
begin
  if listview.selcount = 1 then
  begin
    t := tliteitem(applist2[listview.selected.index]);
    if t.attr and fadirectory = fadirectory then
    begin
      namecombobox  .text := '*';
      attributesedit.text := '*';
      modeedit      .text := '*';
      pathcombobox  .text := t.path + includetrailingpathdelimiter(t.name);

      applybitbtn.click;
    end;
  end;
end;

procedure tmainform.showitem(p: pgulpitem);
var
  item: tliteitem;
begin
  item         := tliteitem.create;
  item.name    := extractfilename(p^.name);
  item.path    := extractfilepath(p^.name);
  item.timeutc := p^.mtimeutc;
  item.attr    := p^.attributes;
  item.mode    := p^.mode;
  item.size    := p^.size;
  item.version := p^.version;
  item.visible := false;
  applist1.add(item);
end;

procedure tmainform.showmessage(const message: rawbytestring);
begin

end;

// filter panel routines

procedure tmainform.namecomboboxkeypress(sender: tobject; var key: char);
begin
  if key = #13 then
    applybitbtn.click;
end;

procedure tmainform.attributeseditkeypress(sender: tobject; var key: char);
begin
  if key = #13 then
    applybitbtn.click;
end;

procedure tmainform.modeeditkeypress(sender: tobject; var key: char);
begin
  if key = #13 then
    applybitbtn.click;
end;

procedure tmainform.pathcomboboxkeypress(sender: tobject; var key: char);
begin
  if key = #13 then
    applybitbtn.click;
end;

procedure tmainform.pathcomboboxcloseup(sender: tobject);
begin
  applybitbtn.click;
end;

procedure tmainform.resetbitbtnclick(sender: tobject);
begin
  namecombobox  .text := '*';
  attributesedit.text := '*';
  modeedit      .text := '*';
  pathcombobox  .text := '*';
end;

procedure tmainform.applybitbtnclick(sender: tobject);
var
  i: longint;
  t: tliteitem;
begin
  listview.items.beginupdate;
  listview.items.clear;
  applist2.clear;
  for i := 0 to applist1.count - 1 do
  begin
    t := applist1[i];
    t.visible :=
      filenamematch(t.name, namecombobox.text) and
      filenamematch(t.path, pathcombobox.text) and
      filenamematch(attrtostring(t.attr), attributesedit.text) and
      filenamematch(modetostring(t.mode), modeedit      .text);
    if t.visible = true then
      applist2.add(t);
  end;
  listview.items.count := applist2.count;
  listview.items.endupdate;
end;

// welcome panel routines

procedure tmainform.newspeedbuttonclick(sender: tobject);
begin
  if savedialog.execute then
  begin
    appname := savedialog.filename;
    if sync(appname) = mrok then
      open(appname);
  end;
end;

procedure tmainform.openspeedbuttonclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    appname := opendialog.filename;
    open(appname);
  end;
end;

// main panel routines

function tmainform.sync(var filename: rawbytestring): longint;
var
  i: longint;
  f: tsyncform;
begin
  f := tsyncform.create(nil);
  f.font.name := font.name;

  f.filenameedit.text            := filename;
  f.updatemodecombobox.itemindex := 1;

  result := f.showmodal;
  if result = mrok then
  begin
    app.reset;
    case f.updatemodecombobox.itemindex of
      0: app.nodelete := false;
      1: app.nodelete := true;
    end;

    for i := 0 to f.includememo.lines.count - 1 do
      app.include.add(f.includememo.lines[i]);
    for i := 0 to f.excludememo.lines.count - 1 do
      app.exclude.add(f.excludememo.lines[i]);

    try
      app.sync(f.filenameedit.text);
    finally
      freeandnil(f);
    end;
  end;

  if result = mrok then
  begin

  end;
end;

procedure tmainform.syncbitbtnclick(sender: tobject);
begin
  sync(appname);
end;

function tmainform.restore(filename: rawbytestring): longint;
var
  i: longint;
  f: trestoreform;
begin
  f := trestoreform.create(nil);
  f.font.name := font.name;

  try
    f.folderedit.text := getcurrentdir;
    f.revisioncombobox.clear;
    for i := 0 to revisioncombobox.items.count - 1 do
      f.revisioncombobox.additem(revisioncombobox.items[i], nil);
    f.revisioncombobox.itemindex := revisioncombobox.itemindex;
    f.modecombobox    .itemindex := 1;

    f.excludememo.clear;
    f.includememo.clear;

    if f.includememo.lines.count = 0 then
      f.includememo.lines.add('*');

    if f.showmodal = mrok then
    begin



    end;

  finally
    freeandnil(f);
  end;

end;

procedure tmainform.restorebitbtnclick(sender: tobject);
begin
  restore(appname);
end;

procedure tmainform.filtersbitbtnclick(sender: tobject);
begin
  if filterspanel.height < 10 then
  begin
    filterspanel.autosize := true;
  end else
  begin
    filterspanel.autosize := false;
    filterspanel.height   := 1;
  end;
end;

procedure tmainform.revisioncomboboxchange(sender: tobject);
begin
  open(appname);
end;

procedure tmainform.homebitbtnclick(sender: tobject);
begin
  wait(false);

  filterspanel.autosize := false;
  filterspanel.height   := 1;

  listview.items.clear;
  revisioncombobox.clear;
  while applist1.count <> 0 do
  begin
    applist1[0].destroy;
    applist1.delete(0);
  end;
  welcomepanel.visible := true;
end;

end.

