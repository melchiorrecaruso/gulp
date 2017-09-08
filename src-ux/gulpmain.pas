unit gulpmain;

{$mode objfpc}
{$H+}

interface

uses
  classes, sysutils, fileutil, dividerbevel, forms, controls, graphics, dialogs,
  comctrls, stdctrls, buttons, extctrls, menus, editbtn, gulplibrary, gulplist,
  gulpcommon;

type
  { tliteitem }

  tliteitem = class
  private
    name    : rawbytestring;
    path    : rawbytestring;
    timeutc : tdatetime;
    attr    : tgulpattributes;
    mode    : tgulppermissions;
    size    : int64;
    version : longint;
    visible : boolean;
  end;

  tliteitemlist = specialize tgenericlist<tliteitem>;

  { tappthread }

  tappthread = class(tthread)
  private
    fmessage: string;
    fprogress: longint;
    procedure showstatus;
    procedure showitem(p: pointer);
    procedure showmessage(const message: rawbytestring);
    procedure showprogress(progress: longint);
  protected
    procedure execute; override;
  public
    constructor create(createsuspended: boolean);
  end;

  { Tmainform }

  Tmainform = class(TForm)
    aboutname: TLabel;
    aboutversion: TLabel;
    aboutdescription: TLabel;
    aboutlink: TLabel;
    aboutcopyrigth: TLabel;
    aboutlicense: TLabel;
    openpath: TComboBox;

    progressbar: TProgressBar;
    restorepath: TDirectoryEdit;
    aboutimage: TImage;
    welcomepurge: TSpeedButton;
    syncok: TBitBtn;
    restoreok: TBitBtn;
    synccancel: TBitBtn;
    restorecancel: TBitBtn;
    buttonspanel: TPanel;
    findbtn: TSpeedButton;
    restorebevel1: TDividerBevel;
    homebtn: TSpeedButton;
    revImg: TImage;
    logmemo: TMemo;
    restoremodelabel: TLabel;
    morebtn: TSpeedButton;
    restorebevel2: TDividerBevel;
    logpage: TPage;
    restorepanel2: TPanel;
    restorepanel4: TPanel;
    restbtn: TSpeedButton;
    restoremode: TComboBox;
    restorerevision: TComboBox;
    restorerevisionlabel: TLabel;
    listrevision: TComboBox;
    welcomecheck: TSpeedButton;
    welcomefix: TSpeedButton;
    syncbtn: TSpeedButton;
    syncroot: TComboBox;
    syncbevel2: TDividerBevel;
    syncbevel3: TDividerBevel;
    syncrootlabel: TLabel;
    syncexcludefile: TSpeedButton;
    synclistview: TListView;
    syncpanel1: TPanel;
    syncpanel3: TPanel;
    open2btn: TSpeedButton;
    openup: TSpeedButton;
    syncexcludedir: TSpeedButton;
    syncaddfile: TSpeedButton;
    syncadddir: TSpeedButton;
    syncdelete: TSpeedButton;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    AboutMenuItem: TMenuItem;
    restorepage: TPage;
    syncpage: TPage;
    MoreMenu: TPopupMenu;
    openpanel1: TPanel;
    openlistview: TListView;
    MenuItem4: TMenuItem;
    notebook: TNotebook;
    openfind: TComboBox;
    welcomenew: TSpeedButton;
    welcomenoopenlabel: TLabel;
    welcomeopenlabel: TLabel;
    welcomeopen: TSpeedButton;
    openpage: TPage;
    openpanel2: TPanel;
    openshape1: TShape;
    syncmode: TComboBox;
    syncmodelabel: TLabel;
    topshape: TShape;
    welcomepage: TPage;
    aboutpage: TPage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    byNameMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    savedialog: TSaveDialog;
    BottomShape: TShape;
    images: TImageList;
    opendialog: TOpenDialog;
    welcomepanel1: TPanel;


    procedure AboutMenuItemClick(Sender: TObject);


    procedure BitBtn2Click(Sender: TObject);


    procedure openfindKeyPress(Sender: TObject; var Key: char);
    procedure openpathEditingDone(Sender: TObject);

    procedure restorecancelClick(Sender: TObject);



    procedure PositionBitBtnClick(Sender: TObject);

    procedure findbtnClick(Sender: TObject);


    procedure openlistviewData(Sender: TObject; Item: TListItem);
    procedure openlistviewDblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);


    procedure restorepathAcceptDirectory(Sender: TObject; var Value: String);

    procedure welcomefixClick(Sender: TObject);

    procedure welcomenewClick(Sender: TObject);
    procedure welcomeopenClick(Sender: TObject);
    procedure HomeBitBtnClick(Sender: TObject);
    procedure PathComboBoxCloseUp(Sender: TObject);
    procedure PathComboBoxKeyPress(Sender: TObject; var Key: char);

    procedure listrevisionChange(Sender: TObject);
    procedure welcomecheckClick(Sender: TObject);


    procedure SpeedButton5Click(Sender: TObject);
    procedure syncdeleteClick(Sender: TObject);
    procedure syncaddfileClick(Sender: TObject);


    procedure SyncBitBtnClick(Sender: TObject);
    procedure RestBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure syncexcludedirClick(Sender: TObject);

    procedure syncexcludefileClick(Sender: TObject);
    procedure synclistviewEdited(Sender: TObject; Item: TListItem;
      var AValue: string);

    procedure syncrootcbChangeBounds(Sender: TObject);
    procedure welcomepurgeClick(Sender: TObject);


  private
    { private declarations }
    procedure clear;
    procedure start;
    procedure finish;


    procedure synclistviewupdate;
    procedure updatebuttons(value: boolean);
  public
    { public declarations }
  end;


var
  mainform: Tmainform;

  appthread:   tappthread;

  app:         tgulpapplication;
  appcommand:  rawbytestring;
  appfilename: rawbytestring;
  applist1:    tliteitemlist;
  applist2:    tliteitemlist;
  appfolders:  trawbytestringlist;
  appfolder:   rawbytestring;
  historypage: longint;


implementation

{$r gulpmain.lfm}

uses
  gulpfixes,
  gulpscanner,
  inifiles;

{ compare items routine }

function compare(item1, item2: tliteitem): longint;
begin
  if ((gadirectory in item1.attr)  xor
      (gadirectory in item2.attr)) then
    result := ansicomparefilename(item1.name, item2.name)
  else
    if (gadirectory in item1.attr) then
      result := -1
    else
      result :=  1;

  if result = 0 then
  {$IFDEF UNIX}
    result := ansicomparestr(item1.path + item1.name, item2.path + item2.name);
  {$ELSE}
  {$IFDEF MSWINDOWS}
    result := ansicomparetext(item1.path + item1.name, item2.path + item2.name);
  {$ELSE}
  {$ENDIF}
  {$ENDIF}
end;

{ tappthread }

constructor tappthread.create(createsuspended: boolean);
begin
  freeonterminate := true;
  inherited create(createsuspended);
end;

procedure tappthread.showitem(p: pointer);
var
  item: tliteitem;
begin
  item := tliteitem.create;
  with tgulpitem(p^) do
  begin
    item.name    := extractfilename(name);
    item.path    := extractfilepath(name);
    item.timeutc := gulpfixes.universaltime2local(mtime);
    item.attr    := attr;
    item.mode    := mode;
    item.size    := size;
    item.version := version;
    item.visible := false;
  end;
  applist1.add(item);
end;

procedure tappthread.showmessage(const message: rawbytestring);
begin
  fmessage := message;
  synchronize(@showstatus);
end;

procedure tappthread.showprogress(progress: longint);
begin
  fprogress := progress;
  synchronize(@showstatus);
end;

procedure tappthread.showstatus;
begin
  mainform.logmemo.lines.add(fmessage);
  mainform.progressbar.position:= fprogress;
end;

procedure tappthread.execute;
begin
  app.onshowitem     := @showitem;
  app.onshowmessage1 := @showmessage;
  //app.onshowmessage2 := @showmessage;

  synchronize(@mainform.start);
  if appcommand = 'list' then
    app.list(appfilename)
  else
  if appcommand = 'check' then
    app.check(appfilename)
  else
  if appcommand = 'fix' then
    app.fix(appfilename)
  else
  if appcommand = 'purge' then
    app.purge(appfilename)
  else
  if appcommand = 'view' then
    app.restore(appfilename);
  synchronize(@mainform.finish);
end;

{ Tmainform }

procedure Tmainform.formcreate(sender: tobject);
var
  ini: tinifile;
begin
  // load settings
  ini              := tinifile.create(getappconfigfile(false));
  self.top         :=              ini.readinteger('mainform', 'top',        200);
  self.height      :=              ini.readinteger('mainform', 'height',     400);
  self.left        :=              ini.readinteger('mainform', 'left',       400);
  self.width       :=              ini.readinteger('mainform', 'width',      600);
  self.windowstate := twindowstate(ini.readinteger('mainform', 'windowstate', 0));

  openlistview.columns[0].width := ini.readinteger('listview', 'columns[0].width', 100);
  openlistview.columns[1].width := ini.readinteger('listview', 'columns[1].width', 100);
  openlistview.columns[2].width := ini.readinteger('listview', 'columns[2].width', 100);
  openlistview.columns[3].width := ini.readinteger('listview', 'columns[3].width', 100);
  openlistview.columns[4].width := ini.readinteger('listview', 'columns[4].width', 100);
  openlistview.columns[5].width := ini.readinteger('listview', 'columns[5].width', 100);

  savedialog .initialdir := ini.readstring('savedialog', ' initialdir', '');
  opendialog .initialdir := ini.readstring('opendialog',  'initialdir', '');
  restorepath.rootdir    := ini.readstring('restorepath', 'rootdir',    '');

  ini.destroy;
  // gulp application core
  app               := tgulpapplication.create;

  applist1          := tliteitemlist.create(@compare);
  applist2          := tliteitemlist.create(@compare);

  appfolders        := trawbytestringlist.create;
  // form style
  font.name           := 'droid sans';
  logmemo.font.name := 'droid sans';


  welcomenoopenlabel.font.size  := 20;
  welcomenoopenlabel.font.style := [fsbold];
  welcomeopenlabel.font.size   := 14;
  welcomeopenlabel.font.style  := [];

  openpanel2.autosize      := true;
  openpanel1.autosize      := false;
  openpanel1.height        := 1;

  homebitbtnclick(sender);
  updatebuttons(false);
end;

procedure Tmainform.formdestroy(sender: tobject);
var
  ini: tinifile;
begin
  clear;
  // save settings
  ini := tinifile.create(getappconfigfile(false));
  if self.windowstate = wsnormal then
  begin
    ini.writeinteger('mainform', 'top',    self.top);
    ini.writeinteger('mainform', 'height', self.height);
    ini.writeinteger('mainform', 'left',   self.left);
    ini.writeinteger('mainform', 'width',  self.width);
  end;
  ini.writeinteger  ('mainform', 'windowstate', longint(self.windowstate));

  ini.writeinteger('listview', 'columns[0].width', openlistview.columns[0].width);
  ini.writeinteger('listview', 'columns[1].width', openlistview.columns[1].width);
  ini.writeinteger('listview', 'columns[2].width', openlistview.columns[2].width);
  ini.writeinteger('listview', 'columns[3].width', openlistview.columns[3].width);
  ini.writeinteger('listview', 'columns[4].width', openlistview.columns[4].width);
  ini.writeinteger('listview', 'columns[5].width', openlistview.columns[5].width);

  ini.writestring('savedialog',  'initialdir', savedialog .initialdir);
  ini.writestring('opendialog',  'initialdir', opendialog .initialdir);
  ini.writestring('restorepath', 'rootdir',    restorepath.rootdir);

  ini.destroy;
  // destroy
  appfolders.destroy;
  applist1.destroy;
  applist2.destroy;
  app.destroy;
end;

procedure Tmainform.clear;
begin
  applist2.clear;
  while applist1.count > 0 do
  begin
    applist1[0].destroy;
    applist1.delete(0);
  end;
  logmemo.clear;
  openpath.clear;
  openlistview.clear;
end;

procedure Tmainform.start;
begin
  updatebuttons(false);
  progressbar.style   := pbstmarquee;
  progressbar.visible := true;

  if appcommand = 'list' then
    notebook.pageindex := 1
  else
  if appcommand = 'check' then
    notebook.pageindex := 5
  else
  if appcommand = 'fix' then
    notebook.pageindex := 5
  else
  if appcommand = 'purge' then
    notebook.pageindex := 5
  else
  if appcommand = 'list' then
    notebook.pageindex := 1
end;

procedure tmainform.finish;
var
  i: longint = 0;
  v: longint = 0;
begin
  if appcommand = 'list' then
  begin
    if app.untilversion = $ffffffff then
    begin
      appfolder := '';
      for i := 0 to applist1.count - 1 do
        v := max(v, applist1[i].version);
      listrevision.items.clear;
      for i := 0 to v - 1 do
        listrevision.additem(' revision ' + inttostr(i + 1), nil);
      listrevision.itemindex := listrevision.items.count - 1;
    end;

    appfolders.clear;
    for i := 0 to applist1.count - 1 do
      appfolders.add(applist1[i].path);

    openpath.clear;
    for i := 0 to appfolders.count - 1 do
      openpath.additem(appfolders[i], nil);

    if appfolders.find(appfolder) = -1 then
    begin
      if appfolders.count > 0 then
        appfolder := appfolders[0]
      else
        appfolder := '';
    end;
    openpath.text := appfolder;
    openpath.editingdone;
    updatebuttons(true);
    caption := 'Gulp - ' + opendialog.filename;
  end;

  progressbar.style   := pbstnormal;
  progressbar.visible := false;
end;



procedure Tmainform.updatebuttons(value: boolean);
begin
  homebtn.enabled := true;
  syncbtn.enabled := value;
  restbtn.enabled := value;
  findbtn.enabled := value;
  openpanel1.enabled := value;
  revimg .enabled := value;
  listrevision.enabled  := value;

  logmemo.enabled  := true;
  logmemo.readonly := true;

  openpanel2.enabled := value;
  openlistview  .enabled := value;
  openlistview  .showcolumnheaders := value;
end;

procedure Tmainform.openlistviewdata(sender: tobject; item: tlistitem);
var
  t: tliteitem;
begin
  t := applist2[item.index];
  if t.visible = true then
  begin;
    item.caption := t.name;
    if gadirectory in t.attr then
    begin
      item.imageindex := 5;
      item.stateindex := 5;
      item.subitems.add(size2str(t.size));
    end else
    begin

      item.imageindex := 3;
      item.stateindex := 3;
      item.subitems.add(size2str(t.size));
    end;
    item.subitems.add(time2str(t.timeutc));
    item.subitems.add(attr2str(l2sattr(t.attr)));
    item.subitems.add(mode2str(l2smode(t.mode)));
    item.subitems.add(t.path);
  end;
end;

procedure Tmainform.openlistviewdblclick(sender: tobject);
begin
  menuitem1click(sender);
end;

procedure Tmainform.menuitem1click(sender: tobject);
var
  t: tliteitem;
begin
  if openlistview.selcount = 1 then
  begin
    t := applist2[openlistview.selected.index];
    if gadirectory in t.attr then
    begin
      openpath.text := t.path + includetrailingpathdelimiter(t.name);
      openpath.editingdone;
    end else
    begin


      caption := 'View ' + t.path + includetrailingpathdelimiter(t.name);
      begin
        clear;
        app.reset;
        appcommand   := 'view';
        appfilename  := appfilename;
        app.nodelete := true;
        app.include.add(t.path + includetrailingpathdelimiter(t.name));
        app.untilversion := $ffffffff;
        appthread    := tappthread.create(false);
      end;



    end;
  end;
end;

// filter panel routines

procedure Tmainform.aboutmenuitemclick(sender: tobject);
begin
  notebook.pageindex := 2;
end;

procedure Tmainform.bitbtn2click(sender: tobject);
begin
  moremenu.popup(
    mainform.left + morebtn.left,
    mainform.top  + morebtn.top  + morebtn.height + 25);
end;

procedure Tmainform.openfindkeypress(sender: tobject; var key: char);
begin
  if key = #13 then openpath.editingdone;
end;

procedure Tmainform.openpatheditingdone(sender: tobject);
var
  i: longint;
  t: tliteitem;
begin
  appfolder := openpath.text;
  openlistview.items.beginupdate;
  openlistview.items.clear;
  applist2.clear;
  for i := 0 to applist1.count - 1 do
  begin
    t := applist1[i];
    if filenamematch(t.path, appfolder) = true then
    begin
      if openfind.text <> '' then
        t.visible := filenamematch(t.name, openfind.text)
      else
        t.visible := true;
    end else
      t.visible := false;

    if t.visible = true then
      applist2.add(t);
  end;
  openlistview.items.count := applist2.count;
  openlistview.items.endupdate;
end;

procedure tmainform.restorecancelclick(sender: tobject);
begin
  notebook.pageindex := 1;
  updatebuttons(true);
end;









procedure tmainform.positionbitbtnclick(sender: tobject);
begin
  openpath.text := extractfilepath(excludetrailingpathdelimiter(openpath.text));
  openpath.editingdone;
end;

procedure Tmainform.pathcomboboxkeypress(sender: tobject; var key: char);
begin
  if key = #13 then
    openpath.editingdone;
end;

procedure Tmainform.pathcomboboxcloseup(sender: tobject);
begin
  openpath.editingdone;
end;

// buttons panel routines

procedure tmainform.syncbitbtnclick(sender: tobject);
begin
  syncmode.itemindex := 1;
  synclistview.clear;
  syncroot.clear;
  updatebuttons(false);
  notebook.pageindex := 3;
end;

procedure tmainform.restbitbtnclick(sender: tobject);
var
  i: longint;
begin
  restorepath.text := '';
  restoremode.itemindex := 1;
  restorerevision.clear;
  for i := 0 to listrevision.items.count - 1 do
    restorerevision.additem(listrevision.items[i], nil);
  restorerevision.itemindex := listrevision.itemindex;
  updatebuttons(false);
  notebook.pageindex := 4;
end;

procedure tmainform.findbtnclick(sender: tobject);
begin
  if openpanel1.height < 10 then
  begin
    findbtn.flat        := true;
    openpanel1.autosize := true;
    openpanel2.autosize := false;
    openpanel2.height   := 1;
  end else
  begin
    findbtn.flat        := false;
    openpanel2.autosize := true;
    openpanel1.autosize := false;
    openpanel1.height   := 1;
  end;
end;

procedure tmainform.listrevisionchange(sender: tobject);
begin
  clear;
  app.reset;
  appcommand := 'list';
  app.untilversion := listrevision.itemindex + 1;
  appthread  := tappthread.create(false);
end;

// welcome panel routines

procedure tmainform.welcomenewclick(sender: tobject);
begin
  if savedialog.execute then
  begin
    savedialog.initialdir := extractfiledir(savedialog.filename);
    caption := 'Creating ' + savedialog.filename;
    begin
      // ...
    end;
  end;
end;

procedure tmainform.welcomeopenclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    opendialog.initialdir := extractfiledir(opendialog.filename);
    caption := 'Opening ' + opendialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := 'list';
      appfilename := opendialog.filename;
      app.untilversion := $ffffffff;
      appthread   := tappthread.create(false);
    end;
  end;
end;

procedure tmainform.welcomecheckclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    opendialog.initialdir := extractfiledir(opendialog.filename);
    caption := 'Checking ' + opendialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := 'check';
      appfilename := opendialog.filename;
      appthread   := tappthread.create(false);
    end;
  end;
end;

procedure tmainform.welcomefixclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    opendialog.initialdir := extractfiledir(opendialog.filename);
    caption := 'Fixing ' + opendialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := 'fix';
      appfilename := opendialog.filename;
      appthread   := tappthread.create(false);
    end;
  end;
end;

procedure tmainform.welcomepurgeclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    opendialog.initialdir := extractfiledir(opendialog.filename);
    caption := 'Purging ' + opendialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := 'purge';
      appfilename := opendialog.filename;
      appthread   := tappthread.create(false);
    end;
  end;
end;

// list panel routines


// sync panel routines

procedure tmainform.speedbutton5click(sender: tobject);
var
  d: string;
  t: tlistitem;
begin
  if selectdirectory('Select directory', getcurrentdir, d) then
  begin
    t := synclistview.items.add;
    t.caption  := (d);
    t.subitems.add(d);
    t.imageindex := 0;
    synclistviewupdate;
    syncrootcbchangebounds(sender);
  end;
end;

procedure tmainform.syncexcludedirclick(sender: tobject);
var
  d: string;
  t: tlistitem;
begin
  if selectdirectory('select directory', getcurrentdir, d) then
  begin
    t := synclistview.items.add;
    t.caption  := (d);
    t.subitems.add(d);
    t.imageindex := 1;
    synclistviewupdate;
    syncrootcbchangebounds(sender);
  end;
end;

procedure tmainform.syncaddfileclick(sender: tobject);
var
  t: tlistitem;
begin
  if opendialog.execute then
  begin
    t := synclistview.items.add;
    t.caption:= opendialog.filename;
    t.subitems.add(opendialog.filename);
    t.imageindex := 0;
    synclistviewupdate;
    syncrootcbchangebounds(sender);
  end;
end;

procedure tmainform.syncexcludefileclick(sender: tobject);
var
  t: tlistitem;
begin
  if opendialog.execute then
  begin
    t := synclistview.items.add;
    t.caption:= opendialog.filename;
    t.subitems.add(opendialog.filename);
    t.imageindex := 1;
    synclistviewupdate;
    syncrootcbchangebounds(sender);
  end;
end;

procedure tmainform.synclistviewedited(sender: tobject; item: tlistitem; var avalue: string);
begin
  if filenamematch(
    extractfilepath(item.caption),
    extractfilepath(avalue)) = false then
    avalue := item.caption
  else
    item.subitems[0] :=
      extractfilepath(item.subitems[0]) +
      extractfilename(avalue);
  syncrootcbchangebounds(sender);
end;

procedure tmainform.syncdeleteclick(sender: tobject);
var
  i: longint;
begin
  for i := synclistview.items.count - 1 downto 0 do
    if synclistview.items[i].selected then
      synclistview.items.delete(i);
  synclistviewupdate;
  syncrootcbchangebounds(sender);
end;

procedure tmainform.synclistviewupdate;
var
  i: longint;
  s: rawbytestring;
  list: trawbytestringlist;
begin
  list := trawbytestringlist.create;
  for i := 0 to synclistview.items.count - 1 do
  begin
    s := synclistview.items[i].subitems[0];
    if directoryexists(s) then
      list.add(extractfilepath(s))
    else
      list.add(extractfilepath(excludetrailingpathdelimiter(s)));
  end;
  if list.count > 0 then
    list.add('');

  i := 0;
  while i < list.count do
  begin
    s := list[i];
    while s <> '' do
    begin
      list.add(s);
      s := extractfilepath(excludetrailingpathdelimiter(s));
    end;
    inc(i);
  end;

  s := syncroot.caption;
  syncroot.clear;
  for i := 0 to list.count - 1 do
    syncroot.items.add(list[i]);
  syncroot.enabled := syncroot.items.count <> 0;

  if list.find(s) <> -1 then
    syncroot.itemindex:= list.find(s);
  list.destroy;
end;

procedure tmainform.syncrootcbchangebounds(sender: tobject);
var
  c: rawbytestring;
  r: rawbytestring;
  i: longint;
begin
  r := syncroot.text;
  for i := 0 to synclistview.items.count - 1 do
  begin
    c := synclistview.items[i].subitems[0];
    if pos(r, c) > 0 then
      synclistview.items[i].caption :=
        copy(c, length(r) + 1, length(c) - length(r))
    else
      synclistview.items[i].caption := c;
  end;
end;

// restore panel routines

procedure tmainform.restorepathacceptdirectory(sender: tobject; var value: string);
begin
  restorepath.rootdir:= extractfilepath(value);
end;












// main panel routines


(*
function Tmainform.sync(var filename: rawbytestring): longint;
var
  i: longint;
begin





  syncmode.itemindex := 1;

  (*
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
  *)

end;

*)
















(*
function Tmainform.restore(filename: rawbytestring): longint;
var
  i: longint;
begin
  restoremode.itemindex := 1;
  (*
  f := trestoreform.create(nil);
  f.font.name := font.name;

  try
    f.folderedit.text := getcurrentdir;
    f.revisioncombobox.clear;
    for i := 0 to listrevision.items.count - 1 do
      f.revisioncombobox.additem(listrevision.items[i], nil);
    f.revisioncombobox.itemindex := listrevision.itemindex;
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
  *)

end;
*)












// SYNC PAGE //





// RESTORE PAGE //







procedure Tmainform.homebitbtnclick(sender: tobject);
begin
  caption := 'Gulp - A simple journaling archiver';
  notebook.pageindex := 0;
  updatebuttons(false);
end;

end.

