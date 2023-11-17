{ Description: Main form unit.

  Copyright (C) 2014-2022 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit GulpMain;

{$mode objfpc} {$H+}

interface

uses
  classes, sysutils, fileutil, dividerbevel, forms, controls, graphics, dialogs,
  comctrls, stdctrls, buttons, extctrls, menus, editbtn, Spin, gulplibrary,
  gulplist, gulpcommon;

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
  tappcommand = (acwelcome, accreate, acsync, acrestore, accheck, acfix, acpurge, aclist, acview);

type
  { Tmainform }

  tmainform = class(tform)
    revisionbox: tcombobox;
    RevisionImage: timage;
    openpath: tcombobox;
    PopupMenu1: TPopupMenu;
    progressbar: tprogressbar;
    restorepath: tdirectoryedit;
    PurgeBtn: tspeedbutton;
    SyncOk: tbitbtn;
    restoreok: tbitbtn;
    buttonspanel: tpanel;
    FindBtn: tspeedbutton;
    restorebevel1: tdividerbevel;
    HomeBtn: tspeedbutton;
    logmemo: tmemo;
    restoremodelabel: tlabel;
    MoreBtn: tspeedbutton;
    restorebevel2: tdividerbevel;
    logpage: tpage;
    restorepanel2: tpanel;
    restorepanel4: tpanel;
    RestoreBtn: tspeedbutton;
    restoremode: tcombobox;
    restorerevision: tcombobox;
    restorerevisionlabel: tlabel;
    CheckBtn: tspeedbutton;
    FixBtn: tspeedbutton;
    SyncBtn: tspeedbutton;
    SyncRoot: tcombobox;
    syncbevel2: tdividerbevel;
    syncbevel3: tdividerbevel;
    SyncRootLabel: tlabel;
    SyncExcludeFile: tspeedbutton;
    SyncListView: tlistview;
    syncpanel1: tpanel;
    syncpanel3: tpanel;
    open2btn: tspeedbutton;
    openup: tspeedbutton;
    SyncExcludeDir: tspeedbutton;
    SyncAddFile: tspeedbutton;
    SyncAddDir: tspeedbutton;
    SyncDelete: tspeedbutton;
    menuitem10: tmenuitem;
    menuitem11: tmenuitem;
    menuitem12: tmenuitem;
    menuitem13: tmenuitem;
    aboutmenuitem: tmenuitem;
    restorepage: tpage;
    syncpage: tpage;
    MoreMenu: tpopupmenu;
    openpnl1: tpanel;
    openlview: tlistview;
    menuitem4: tmenuitem;
    notebook: tnotebook;
    openfind: tcombobox;
    NewBtn: tspeedbutton;
    welcomenoopenlabel: tlabel;
    welcomeopenlabel: tlabel;
    OpenBtn: tspeedbutton;
    openpage: tpage;
    openpnl2: tpanel;
    openshape1: tshape;
    SyncMode: tcombobox;
    SyncModeLabel: tlabel;
    topshape: tshape;
    welcomepage: tpage;
    mi_open: tmenuitem;
    menuitem2: tmenuitem;
    menuitem3: tmenuitem;
    bynamemenuitem: tmenuitem;
    menuitem5: tmenuitem;
    menuitem6: tmenuitem;
    menuitem7: tmenuitem;
    menuitem8: tmenuitem;
    menuitem9: tmenuitem;
    SaveDialog: tsavedialog;
    bottomshape: tshape;
    Images: timagelist;
    OpenDialog: topendialog;
    welcomepanel1: tpanel;


    procedure AboutMenuItemClick(Sender: TObject);



    procedure BitBtn2Click(Sender: TObject);


    procedure openfindKeyPress(Sender: TObject; var Key: char);
    procedure openpathEditingDone(Sender: TObject);

    procedure restorecancelClick(Sender: TObject);



    procedure PositionBitBtnClick(Sender: TObject);

    procedure FindBtnClick(Sender: TObject);


    procedure openlviewData(Sender: TObject; Item: TListItem);
    procedure openlviewDblClick(Sender: TObject);
    procedure mi_openClick(Sender: TObject);
    procedure restoreokClick(Sender: TObject);


    procedure restorepathAcceptDirectory(Sender: TObject; var Value: String);

    procedure FixBtnClick(Sender: TObject);

    procedure NewBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure HomeBtnClick(Sender: TObject);
    procedure PathComboBoxCloseUp(Sender: TObject);
    procedure PathComboBoxKeyPress(Sender: TObject; var Key: char);

    procedure revisionboxchange(Sender: TObject);
    procedure CheckBtnClick(Sender: TObject);


    procedure SyncAddDirClick(Sender: TObject);
    procedure SyncDeleteClick(Sender: TObject);
    procedure SyncAddFileClick(Sender: TObject);


    procedure PurgeBtnClick(Sender: TObject);
    procedure RestoreBtnClick(Sender: TObject);
    procedure SyncBtnClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure SyncExcludeDirClick(Sender: TObject);
    procedure SyncExcludeFileClick(Sender: TObject);
    procedure SyncListViewEdited(Sender: TObject; Item: TListItem; var AValue: string);
    procedure SyncOkClick(Sender: TObject);

    procedure SyncRootChangeBounds(Sender: TObject);



  private
    { private declarations }
    procedure Clear;
    procedure Start;
    procedure Stop;


    procedure SyncListViewUpdate;
    procedure UpdateCaption(const aValue: rawbytestring);
    procedure Updatebuttons(value: tformcomponents);
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

constructor TAppThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
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
  App.OnShowItem     := @showitem;
  App.OnShowMessage1 := @showmessage;
  App.OnShowMessage2 := @showmessage;

  Synchronize(@MainForm.Start);
  case appcommand of
    acwelcome: ;
    acsync   : App.sync   (AppFileName);
    acrestore: App.restore(AppFileName);
    accheck  : App.check  (AppFileName);
    acfix    : App.fix    (AppFileName);
    acpurge  : App.purge  (AppFileName);
    aclist   : App.list   (AppFileName);
    acview   : App.view   (AppFileName);
  end;
  Synchronize(@MainForm.Stop);
end;

{ Tmainform }

procedure tmainform.formcreate(sender: tobject);
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

  SaveDialog .initialdir := ini.readstring('savedialog', ' initialdir', '');
  OpenDialog .initialdir := ini.readstring('opendialog',  'initialdir', '');
  restorepath.rootdir    := ini.readstring('restorepath', 'rootdir',    '');

  ini.destroy;
  // gulp application core
  app               := tgulpapplication.create;
  app.pipe          := tmemorystream.create;
  applist1          := tliteitemlist.create(@compare);
  applist2          := tliteitemlist.create(@compare);
  appfolders        := trawbytestringlist.create;

  // form style
  {$IFDEF LINUX}
  font.name         := 'Droid Sans';
  logmemo.font.name := 'Droid Sans Mono';
  {$ELSE}
  {$IFDEF MSWINDOWS}
  font.name         := 'Default';
  logmemo.font.name := 'Courier New';
  logmemo.font.size := 10;
  {$ELSE}
  ...
  {$ENDIF}
  {$ENDIF}

  welcomenoopenlabel.font.size  := 20;
  welcomenoopenlabel.font.style := [fsbold];
  welcomeopenlabel.font.size    := 12;
  welcomeopenlabel.font.style   := [];

  openpnl2.autosize := true;
  openpnl1.autosize := false;
  openpnl1.height   := 1;

  homebtnclick(sender);
  updatebuttons([]);
end;

procedure tmainform.formdestroy(sender: tobject);
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

  ini.writestring('savedialog',  'initialdir', SaveDialog .initialdir);
  ini.writestring('opendialog',  'initialdir', OpenDialog .initialdir);
  ini.writestring('restorepath', 'rootdir',    restorepath.rootdir);

  ini.destroy;
  // destroy
  appfolders.destroy;
  applist1.destroy;
  applist2.destroy;
  app.pipe.destroy;
  app.pipe := nil;
  app.destroy;
end;

procedure tmainform.clear;
begin
  AppList2.Clear;
  while AppList1.Count > 0 do
  begin
    applist1[0].destroy;
    applist1.delete(0);
  end;
  logmemo.clear;
  openpath.clear;
  openlview.clear;
end;

procedure tmainform.start;
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

procedure tmainform.Stop;
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

  if AppCommand = acSync then
  begin
    Clear;
    App.Reset;
    AppCommand := acList;
    App.UntilVersion := $ffffffff;
    AppThread := TAppThread.Create(False);
  end else
  if appcommand = aclist then
  begin
    if app.untilversion = $ffffffff then
    begin
      appfolder := '';
      for i := 0 to applist1.count - 1 do
        v := max(v, applist1[i].version);
      revisionbox.items.clear;
      for i := 0 to v - 1 do
      begin
        revisionbox.items.add(' Revision ' + inttostr(i + 1));
      end;

      revisionbox.itemindex := revisionbox.items.count - 1;
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

    updatecaption('Gulp UX shows - ' + OpenDialog.filename);
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

procedure TMainForm.UpdateCaption(const aValue: rawbytestring);
begin
  Caption := aValue;
end;

procedure Tmainform.updatebuttons(value: tformcomponents);
begin
  HomeBtn .enabled := fchomebtn in value;
  SyncBtn .enabled := fcsyncbtn in value;
  RestoreBtn.enabled := fcrestbtn in value;
  FindBtn .enabled := fcfindbtn in value;
  openpnl1.enabled := fcopenpnl in value;
  openpnl2.enabled := fcopenpnl in value;
  revisionbox.enabled := fcrevcbox in value;

  logmemo .enabled  := true;
  logmemo .readonly := true;

  openlview.enabled           := fcopenlview in value;
  openlview.showcolumnheaders := fcopenlview in value;
end;

procedure tmainform.openlviewdata(sender: tobject; item: tlistitem);
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
      item.subitems.add('-');
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

procedure tmainform.openlviewdblclick(sender: tobject);
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

procedure tmainform.restoreokclick(Sender: TObject);
begin
  if sender = SyncOk then
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

procedure tmainform.bitbtn2click(sender: tobject);
begin
  MoreMenu.popup(
    mainform.left + MoreBtn.left,
    mainform.top  + MoreBtn.top  + MoreBtn.height + 25);
end;

procedure tmainform.openfindkeypress(sender: tobject; var key: char);
begin
  if key = #13 then openpath.editingdone;
end;

procedure tmainform.openpatheditingdone(sender: tobject);
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
  homebtnclick(sender);
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

procedure tmainform.SyncBtnClick(sender: tobject);
begin
  if sender = NewBtn  then appcommand := accreate;
  if sender = SyncBtn then appcommand := acsync;

  SyncMode.itemindex := 1;
  SyncListView.clear;
  SyncRoot.clear;
  updatebuttons([fchomebtn]);
  notebook.pageindex := 2;
end;

procedure tmainform.RestoreBtnClick(sender: tobject);
var
  i: longint;
begin
  appcommand := acrestore;
  restorepath.text := '';
  restoremode.itemindex := 1;
  restorerevision.clear;
  for i := 0 to revisionbox.items.count - 1 do
    restorerevision.additem(revisionbox.items[i], nil);

  restorerevision.itemindex := revisionbox.itemindex;
  updatebuttons([fchomebtn]);
  notebook.pageindex := 3;
end;

procedure tmainform.FindBtnClick(sender: tobject);
begin
  if openpnl1.height < 10 then
  begin
    FindBtn.flat        := true;
    openpnl1.autosize := true;
    openpnl2.autosize := false;
    openpnl2.height   := 1;
  end else
  begin
    FindBtn.flat        := false;
    openpnl2.autosize := true;
    openpnl1.autosize := false;
    openpnl1.height   := 1;
  end;
end;

procedure tmainform.revisionboxchange(sender: tobject);
begin
  clear;
  app.reset;
  appcommand := aclist;
  app.untilversion := revisionbox.itemindex + 1;
  appthread  := tappthread.create(false);
end;

// welcome panel routines

procedure tmainform.NewBtnClick(sender: tobject);
begin
  SaveDialog.filter := 'Gulp Files (*.gulp)|*.gulp';
  if SaveDialog.Execute then
  begin
    SaveDialog.initialdir := extractfiledir(SaveDialog.filename);
    caption := 'Creating ' + SaveDialog.filename;
    begin
      clear;
      app.reset;
      appfilename := SaveDialog.filename;
      syncbtnclick(sender);
    end;
  end;
end;

procedure tmainform.OpenBtnClick(sender: tobject);
begin
  OpenDialog.filter := 'Gulp Files (*.gulp)|*.gulp';
  if OpenDialog.Execute then
  begin
    OpenDialog.initialdir := extractfiledir(OpenDialog.filename);
    caption := 'Opening ' + OpenDialog.filename;
    begin
      clear;
      app.reset;
      appcommand := aclist;
      appfilename := OpenDialog.filename;
      app.untilversion := $ffffffff;
      appthread := tappthread.create(false);
    end;
  end;
end;

procedure tmainform.CheckBtnClick(sender: tobject);
begin
  OpenDialog.filter := 'Gulp Files (*.gulp)|*.gulp';
  if OpenDialog.Execute then
  begin
    OpenDialog.initialdir := extractfiledir(OpenDialog.filename);
    caption := 'Checking ' + OpenDialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := accheck;
      appfilename := OpenDialog.filename;
      appthread   := tappthread.create(false);
    end;
  end;
end;

procedure tmainform.FixBtnClick(sender: tobject);
begin
  OpenDialog.filter := 'Gulp Files (*.gulp)|*.gulp';
  if OpenDialog.Execute then
  begin
    OpenDialog.initialdir := extractfiledir(OpenDialog.filename);
    caption := 'Fixing ' + OpenDialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := acfix;
      appfilename := OpenDialog.filename;
      appthread   := tappthread.create(false);
    end;
  end;
end;

procedure tmainform.PurgeBtnClick(sender: tobject);
begin
  OpenDialog.filter := 'Gulp Files (*.gulp)|*.gulp';
  if OpenDialog.Execute then
  begin
    OpenDialog.initialdir := extractfiledir(OpenDialog.filename);
    caption := 'Purging ' + OpenDialog.filename;
    begin
      clear;
      app.reset;
      appcommand  := acpurge;
      appfilename := OpenDialog.filename;
      appthread   := tappthread.create(false);
    end;
  end;
end;

// List Panel Routines


// Sync Panel Routines

procedure TMainForm.SyncAddDirClick(Sender: TObject);
var
  DirName: string;
  Item: TListItem;
begin
  if SelectDirectory('Select directory', GetCurrentDir, DirName) then
  begin
    Item := SyncListView.Items.Add;
    Item.Caption := (DirName);
    Item.SubItems.Add(DirName);
    Item.ImageIndex := 0;
    SyncListViewUpdate;
    SyncRootChangeBounds(Sender);
  end;
end;

procedure TMainForm.SyncExcludeDirClick(Sender: TObject);
var
  DirName: string;
  Item: TListItem;
begin
  if SelectDirectory('Select directory', GetCurrentDir, DirName) then
  begin
    Item := SyncListView.Items.Add;
    Item.Caption := (DirName);
    Item.SubItems.Add(DirName);
    Item.ImageIndex := 1;
    SyncListViewUpdate;
    SyncRootChangeBounds(Sender);
  end;
end;

procedure TMainForm.SyncAddFileClick(Sender: TObject);
var
  Item: TListItem;
begin
  if OpenDialog.Execute then
  begin
    Item := SyncListView.Items.Add;
    Item.Caption:= OpenDialog.FileName;
    Item.SubItems.Add(OpenDialog.FileName);
    Item.ImageIndex := 0;
    SyncListViewUpdate;
    SyncRootChangeBounds(Sender);
  end;
end;

procedure TMainForm.SyncExcludeFileClick(Sender: TObject);
var
  Item: TListItem;
begin
  if OpenDialog.Execute then
  begin
    Item := SyncListView.Items.Add;
    Item.Caption:= OpenDialog.FileName;
    Item.SubItems.Add(OpenDialog.FileName);
    Item.ImageIndex := 1;
    SyncListViewUpdate;
    SyncRootChangeBounds(Sender);
  end;
end;

procedure TMainForm.SyncListViewEdited(Sender: TObject; Item: TListItem; var aValue: String);
begin
  // if validate then
  begin
    Item.SubItems[0] := SyncRoot.Text + aValue;
  end;
  SyncListViewUpdate;
  SyncRootChangeBounds(Sender);
end;

procedure TMainForm.SyncDeleteClick(Sender: TObject);
var
  I: LongInt;
begin
  for I := SyncListView.Items.Count - 1 downto 0 do
    if SyncListView.Items[I].Selected then
      SyncListView.Items.Delete(I);
  SyncListViewUpdate;
  SyncRootChangeBounds(Sender);
end;

procedure TMainForm.SyncOkClick(Sender: TObject);
var
  I: LongInt;
begin
  Clear;
  App.Reset;
  AppCommand := acSync;
  case SyncMode.ItemIndex of
    0: App.NoDelete := False;
    1: App.NoDelete := True;
  end;

  for I := 0 to SyncListView.Items.Count -1 do
  begin
    case SyncListView.Items[I].ImageIndex of
      0: App.Include.Add(SyncListView.Items[I].Caption);
      1: App.Exclude.Add(SyncListView.Items[I].Caption);
    end;
  end;
  SetCurrentDir(SyncRoot.Text);
  AppThread := TAppThread.Create(False);
end;

procedure tmainform.synclistviewupdate;
var
  i: longint;
  s: rawbytestring;
  list: trawbytestringlist;
begin
  list := trawbytestringlist.create;
  for i := 0 to SyncListView.items.count - 1 do
    if SyncListView.items[i].imageindex = 0 then
    begin
      s := extractfiledir(SyncListView.items[i].subitems[0]);
      list.add(includetrailingbackslash(s));
      while s <> extractfiledir(s) do
      begin
        s := extractfiledir(s);
        list.add(includetrailingbackslash(s));
      end;
    end;

  s := SyncRoot.caption;
  SyncRoot.clear;
  for i := 0 to list.count - 1 do
    SyncRoot.items.add(list[i]);
  SyncRoot.enabled := SyncRoot.items.count <> 0;

  if list.find(s) <> -1 then
    SyncRoot.itemindex := list.find(s);
  list.destroy;
end;

procedure TMainForm.SyncRootChangeBounds(Sender: TObject);
var
  I: LongInt;
  Root: String;
  SubItem: String;
begin
  Root := SyncRoot.Text;
  for I := 0 to SyncListView.Items.Count - 1 do
  begin
    SubItem := SyncListView.Items[I].SubItems[0];
    if Pos(Root, SubItem) > 0 then
      SyncListView.Items[I].Caption := Copy(SubItem, Length(Root) + 1, Length(SubItem) - Length(Root))
    else
      SyncListView.Items[i].Caption := SubItem;
  end;

  if SyncRoot.Text = '' then
  begin
    SyncRoot.ItemIndex := 0;
    SyncRootChangeBounds(Sender);
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





  SyncMode.itemindex := 1;

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







procedure Tmainform.HomeBtnClick(sender: tobject);
begin
  if appcommand = acwelcome then
  begin
    caption := 'Gulp UX - A simple journaling archiver';
    notebook.pageindex := 0;
    updatebuttons([fchomebtn]);
    clear;
  end else
  if appcommand = accreate then
  begin
    caption := 'Gulp UX - A simple journaling archiver';
    notebook.pageindex := 0;
    updatebuttons([fchomebtn]);
    clear;
  end else
  if appcommand = acsync then
  begin
    appcommand := aclist;
    notebook.pageindex := 1;
    updatecaption('Gulp UX shows - ' + OpenDialog.filename);
    updatebuttons([fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
                   fcrevcbox, fcopenpnl, fclogmemo, fcopenlview]);
  end else
  if appcommand = acrestore then
  begin
    appcommand := aclist;
    notebook.pageindex := 1;
    updatecaption('Gulp UX shows - ' + OpenDialog.filename);
    updatebuttons([fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
                   fcrevcbox, fcopenpnl, fclogmemo, fcopenlview]);
  end else
  if appcommand = accheck then
  begin
    appcommand := acwelcome;
    homebtnclick(nil);
  end else
  if appcommand = acfix then
  begin
    appcommand := acwelcome;
    homebtnclick(nil);
  end else
  if appcommand = acpurge then
  begin
    appcommand := acwelcome;
    homebtnclick(nil);
  end else
  if appcommand = aclist then
  begin
    appcommand := acwelcome;
    homebtnclick(nil);
  end else
  if appcommand = acview then
  begin
    appcommand := aclist;
    notebook.pageindex := 1;
    updatecaption('Gulp UX shows - ' + OpenDialog.filename);
    updatebuttons([fchomebtn, fcsyncbtn, fcrestbtn, fcfindbtn,
                   fcrevcbox, fcopenpnl, fclogmemo, fcopenlview]);
  end;
end;

end.

