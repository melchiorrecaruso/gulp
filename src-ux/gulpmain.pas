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

type
  tfomcomponent = (
    fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
    fcopenpnl, fcrevcbox, fclogmemo, fcopenlview);

  tformcomponents = set of tfomcomponent;

type
  tappcommand = (acwelcome, acsync, acrestore, accheck, acfix, acpurge, aclist, acview);

type
  { Tmainform }

  Tmainform = class(TForm)
    logok: TBitBtn;
    openpath: TComboBox;


    progressbar: TProgressBar;
    restorepath: TDirectoryEdit;
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
    revcbox: TComboBox;
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
    openpnl1: TPanel;
    openlview: TListView;
    MenuItem4: TMenuItem;
    notebook: TNotebook;
    openfind: TComboBox;
    welcomenew: TSpeedButton;
    welcomenoopenlabel: TLabel;
    welcomeopenlabel: TLabel;
    welcomeopen: TSpeedButton;
    openpage: TPage;
    openpnl2: TPanel;
    openshape1: TShape;
    syncmode: TComboBox;
    syncmodelabel: TLabel;
    topshape: TShape;
    welcomepage: TPage;
    mi_open: TMenuItem;
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
    procedure logokClick(Sender: TObject);


    procedure BitBtn2Click(Sender: TObject);


    procedure openfindKeyPress(Sender: TObject; var Key: char);
    procedure openpathEditingDone(Sender: TObject);

    procedure restorecancelClick(Sender: TObject);



    procedure PositionBitBtnClick(Sender: TObject);

    procedure findbtnClick(Sender: TObject);


    procedure openlviewData(Sender: TObject; Item: TListItem);
    procedure openlviewDblClick(Sender: TObject);
    procedure mi_openClick(Sender: TObject);
    procedure restoreokClick(Sender: TObject);


    procedure restorepathAcceptDirectory(Sender: TObject; var Value: String);

    procedure welcomefixClick(Sender: TObject);

    procedure welcomenewClick(Sender: TObject);
    procedure welcomeopenClick(Sender: TObject);
    procedure HomeBitBtnClick(Sender: TObject);
    procedure PathComboBoxCloseUp(Sender: TObject);
    procedure PathComboBoxKeyPress(Sender: TObject; var Key: char);

    procedure revcboxChange(Sender: TObject);
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
    procedure updatecaption(const value: rawbytestring);
    procedure updatebuttons(value: tformcomponents);
  public
    { public declarations }
  end;



var
  mainform: Tmainform;

  appthread:   tappthread;

  app:         tgulpapplication;
  appcommand:  tappcommand;
  appfilename: rawbytestring;
  applist1:    tliteitemlist;
  applist2:    tliteitemlist;
  appfolders:  trawbytestringlist;
  appfolder:   rawbytestring;
  historypage: longint;


implementation

{$r gulpmain.lfm}

uses
  gulpabout,
  gulpfixes,
  gulpscanner,
  inifiles;

{ compare items routine }

function compare(item1, item2: tliteitem): longint;
begin
  if ((gadirectory in item1.attr) = (gadirectory in item2.attr)) then
    result := ansicomparefilename(item1.name, item2.name)
  else
    if (gadirectory in item1.attr) then
      result := -1
    else
      result :=  1;
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
  app.onshowmessage2 := @showmessage;

  synchronize(@mainform.start);
  case appcommand of
    acwelcome: ;
    acsync   : app.sync   (appfilename);
    acrestore: app.restore(appfilename);
    accheck  : app.check  (appfilename);
    acfix    : app.fix    (appfilename);
    acpurge  : app.purge  (appfilename);
    aclist   : app.list   (appfilename);
    acview   : app.view   (appfilename);
  end;
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

  openlview.columns[0].width := ini.readinteger('listview', 'columns[0].width', 100);
  openlview.columns[1].width := ini.readinteger('listview', 'columns[1].width', 100);
  openlview.columns[2].width := ini.readinteger('listview', 'columns[2].width', 100);
  openlview.columns[3].width := ini.readinteger('listview', 'columns[3].width', 100);
  openlview.columns[4].width := ini.readinteger('listview', 'columns[4].width', 100);
  openlview.columns[5].width := ini.readinteger('listview', 'columns[5].width', 100);

  savedialog .initialdir := ini.readstring('savedialog', ' initialdir', '');
  opendialog .initialdir := ini.readstring('opendialog',  'initialdir', '');
  restorepath.rootdir    := ini.readstring('restorepath', 'rootdir',    '');

  ini.destroy;
  // gulp application core
  app               := tgulpapplication.create;
  app.pipe          := tmemorystream.create;

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

  openpnl2.autosize      := true;
  openpnl1.autosize      := false;
  openpnl1.height        := 1;

  homebitbtnclick(sender);
  updatebuttons([]);
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

  ini.writeinteger('listview', 'columns[0].width', openlview.columns[0].width);
  ini.writeinteger('listview', 'columns[1].width', openlview.columns[1].width);
  ini.writeinteger('listview', 'columns[2].width', openlview.columns[2].width);
  ini.writeinteger('listview', 'columns[3].width', openlview.columns[3].width);
  ini.writeinteger('listview', 'columns[4].width', openlview.columns[4].width);
  ini.writeinteger('listview', 'columns[5].width', openlview.columns[5].width);

  ini.writestring('savedialog',  'initialdir', savedialog .initialdir);
  ini.writestring('opendialog',  'initialdir', opendialog .initialdir);
  ini.writestring('restorepath', 'rootdir',    restorepath.rootdir);

  ini.destroy;
  // destroy
  appfolders.destroy;
  applist1.destroy;
  applist2.destroy;
  app.pipe.destroy;
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
  openlview.clear;
end;

procedure Tmainform.start;
begin
  updatebuttons([]);
  progressbar.style   := pbstmarquee;
  progressbar.visible := true;

  case appcommand of
    acwelcome: ;
    acsync   : notebook.pageindex := 4;
    acrestore: notebook.pageindex := 4;
    accheck  : notebook.pageindex := 4;
    acfix    : notebook.pageindex := 4;
    acpurge  : notebook.pageindex := 4;
    aclist   : notebook.pageindex := 1;
    acview   : notebook.pageindex := 4;
  end;
end;

procedure tmainform.finish;
var
  i: longint = 0;
  v: longint = 0;
begin

  //acwelcome: ;
  //acsync   : notebook.pageindex := 4;
  //acrestore: notebook.pageindex := 4;
  //accheck  : notebook.pageindex := 4;
  //acfix    : notebook.pageindex := 4;
  //acpurge  : notebook.pageindex := 4;
  //   : notebook.pageindex := 1;
  //   : notebook.pageindex := 4;

  if appcommand = aclist then
  begin
    if app.untilversion = $ffffffff then
    begin
      appfolder := '';
      for i := 0 to applist1.count - 1 do
        v := max(v, applist1[i].version);
      revcbox.items.clear;
      for i := 0 to v - 1 do
        revcbox.additem(' revision ' + inttostr(i + 1), nil);
      revcbox.itemindex := revcbox.items.count - 1;
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

    updatecaption('Gulp UX shows - ' + opendialog.filename);
    updatebuttons([fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
                   fcrevcbox, fcopenpnl, fclogmemo, fcopenlview]);
  end else
  if appcommand = acview then
  begin
    app.pipe.seek(0, sobeginning);
    logmemo.lines.loadfromstream(app.pipe);
    updatecaption('Gulp UX shows - ' + app.include[0]);
    updatebuttons([fchomebtn, fclogmemo]);
  end else
  if appcommand = accheck then
  begin
    updatebuttons([fchomebtn, fclogmemo]);
  end;
  if appcommand = acfix then
  begin
    updatebuttons([fchomebtn, fclogmemo]);
  end;
  if appcommand = acpurge then
  begin
    updatebuttons([fchomebtn, fclogmemo]);
  end;

  progressbar.style   := pbstnormal;
  progressbar.visible := false;
end;

procedure Tmainform.updatecaption(const value: rawbytestring);
begin
  caption := value;
end;

procedure Tmainform.updatebuttons(value: tformcomponents);
begin
  homebtn .enabled := fchomebtn in value;
  syncbtn .enabled := fcsyncbtn in value;
  restbtn .enabled := fcrestbtn in value;
  findbtn .enabled := fcfindbtn in value;
  openpnl1.enabled := fcopenpnl in value;
  openpnl2.enabled := fcopenpnl in value;
  revcbox .enabled := fcrevcbox in value;

  logmemo .enabled  := fclogmemo in value;
  logmemo .readonly := true;

  openlview.enabled           := fcopenlview in value;
  openlview.showcolumnheaders := fcopenlview in value;
end;

procedure Tmainform.openlviewData(sender: tobject; item: tlistitem);
var
  t: tliteitem;
begin
  t := applist2[item.index];
  if t.visible = true then
  begin;
    item.caption := t.name;
    if gadirectory in t.attr then
    begin
      item.imageindex := 3;
      item.stateindex := 3;
      item.subitems.add(size2str(t.size));
    end else
    begin
      item.imageindex := 5;
      item.stateindex := 5;
      item.subitems.add(size2str(t.size));
    end;
    item.subitems.add(time2str(t.timeutc));
    item.subitems.add(attr2str(l2sattr(t.attr)));
    item.subitems.add(mode2str(l2smode(t.mode)));
    item.subitems.add(t.path);
  end;
end;

procedure Tmainform.openlviewDblClick(sender: tobject);
begin
  mi_openclick(sender);
end;

procedure tmainform.mi_openclick(sender: tobject);
var
  t: tliteitem;
begin
  if openlview.selcount = 1 then
  begin
    t := applist2[openlview.selected.index];
    if gadirectory in t.attr then
    begin
      openpath.text := t.path + includetrailingpathdelimiter(t.name);
      openpath.editingdone;
    end else
    begin
      app.reset;
      appcommand   := acview;
      appfilename  := appfilename;
      app.nodelete := true;
      app.include.add(t.path + t.name);
      app.untilversion := $ffffffff;
      appthread        := tappthread.create(false);
    end;
  end;
end;

procedure Tmainform.restoreokclick(Sender: TObject);
begin
  if sender = syncok then
    showmessage('sync not implemented')
  else
  if sender = restoreok then
    showmessage('rest not implemented');
end;

// filter panel routines

procedure tmainform.aboutmenuitemclick(sender: tobject);
begin
  aboutform := taboutform.create(nil);
  aboutform.showmodal;
  freeandnil(aboutform);
end;

procedure Tmainform.logokClick(Sender: TObject);
begin
  if appcommand = acview then
  begin
    notebook.pageindex := 1;
    updatecaption('Gulp UX shows - ' + opendialog.filename);
    updatebuttons([fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
                   fcrevcbox, fcopenpnl, fclogmemo, fcopenlview]);
  end else
  if appcommand = accheck then
  begin
    homebitbtnclick(nil);
  end else
  if appcommand = acfix then
  begin
    homebitbtnclick(nil);
  end else
  if appcommand = acpurge then
  begin
    homebitbtnclick(nil);
  end;
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
  openlview.items.beginupdate;
  openlview.items.clear;
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
  openlview.items.count := applist2.count;
  openlview.items.endupdate;
end;

procedure tmainform.restorecancelclick(sender: tobject);
begin
  notebook.pageindex := 1;
  updatebuttons([fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
                 fcrevcbox, fcopenpnl, fclogmemo, fcopenlview]);
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
  updatebuttons([fchomebtn]);
  notebook.pageindex := 2;
end;

procedure tmainform.restbitbtnclick(sender: tobject);
var
  i: longint;
begin
  restorepath.text := '';
  restoremode.itemindex := 1;
  restorerevision.clear;
  for i := 0 to revcbox.items.count - 1 do
    restorerevision.additem(revcbox.items[i], nil);
  restorerevision.itemindex := revcbox.itemindex;
  updatebuttons([fchomebtn]);
  notebook.pageindex := 3;
end;

procedure tmainform.findbtnclick(sender: tobject);
begin
  if openpnl1.height < 10 then
  begin
    findbtn.flat        := true;
    openpnl1.autosize := true;
    openpnl2.autosize := false;
    openpnl2.height   := 1;
  end else
  begin
    findbtn.flat        := false;
    openpnl2.autosize := true;
    openpnl1.autosize := false;
    openpnl1.height   := 1;
  end;
end;

procedure tmainform.revcboxchange(sender: tobject);
begin
  clear;
  app.reset;
  appcommand := aclist;
  app.untilversion := revcbox.itemindex + 1;
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
      appcommand  := aclist;
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
      appcommand  := accheck;
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
      appcommand  := acfix;
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
      appcommand  := acpurge;
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
  if selectdirectory('Select directory', getcurrentdir, d) then
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
    for i := 0 to revcbox.items.count - 1 do
      f.revisioncombobox.additem(revcbox.items[i], nil);
    f.revisioncombobox.itemindex := revcbox.itemindex;
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
  caption := 'Gulp UX - A simple journaling archiver';
  notebook.pageindex := 0;
  updatebuttons([fchomebtn]);
  clear;
end;

end.

