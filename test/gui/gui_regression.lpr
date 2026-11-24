program GUIRegression;
{$mode objfpc}{$H+}

uses
  {$ifdef unix}CThreads,{$endif}
  Interfaces, Forms, Classes, SysUtils, ComCtrls, GulpMain;

procedure Check(Value: Boolean; const MessageText: String);
begin
  if not Value then raise Exception.Create(MessageText);
end;

procedure WriteText(const Name, Value: String);
var
  S: TStringStream;
begin
  S := TStringStream.Create(Value);
  try
    S.SaveToFile(Name);
  finally
    S.Free;
  end;
end;

function ReadText(const Name: String): String;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    S.LoadFromFile(Name);
    Result := S.DataString;
  finally
    S.Free;
  end;
end;

procedure WaitForWorker;
var
  Started: QWord;
begin
  Started := GetTickCount64;
  while Assigned(AppThread) do
  begin
    Application.ProcessMessages;
    CheckSynchronize(1);
    if GetTickCount64 - Started > 15000 then
    begin
      WriteLn(StdErr, 'Worker timed out');
      Halt(1);
    end;
    Sleep(1);
  end;
end;

procedure AddInput(const Name: String);
var
  Item: TListItem;
begin
  Item := MainForm.SyncListView.Items.Add;
  Item.Caption := Name;
  Item.SubItems.Add(ExpandFileName(Name));
  Item.ImageIndex := 0;
end;

procedure Sync;
var
  CanClose: Boolean;
begin
  MainForm.SyncOkClick(MainForm.SyncOk);
  Check(Assigned(AppThread), 'Sync worker did not start');
  MainForm.FormCloseQuery(nil, CanClose);
  Check(not CanClose, 'Window can close while worker is active');
  Check(not MainForm.HomeBtn.Enabled, 'Navigation enabled during worker operation');
  WaitForWorker;
  Check(MainForm.Notebook.PageIndex = 1, 'Sync did not reload the browser');
  Check(not MainForm.ProgressBar.Visible, 'Progress remains visible after completion');
  MainForm.FormCloseQuery(nil, CanClose);
  Check(CanClose, 'Window cannot close after completion');
end;

var
  WorkDir, Destination: String;
begin
  Application.Initialize;
  // Run only in a newly created directory, never in the user's data directory.
  WorkDir := GetTempFileName(GetTempDir(False), 'gulp-gui-');
  DeleteFile(WorkDir);
  Check(CreateDir(WorkDir), 'Cannot create test directory');
  Check(SetCurrentDir(WorkDir), 'Cannot enter test directory');
  WriteLn('GUI test data: ', WorkDir);
  Flush(Output);
  Application.CreateForm(TMainForm, MainForm);
  MainForm.Show;
  Application.ProcessMessages;
  try
    AppFileName := ExpandFileName('history.gulp');
    WriteText('sample.txt', 'old');
    WriteText('second.txt', 'second');
    MainForm.SyncRoot.Text := WorkDir;
    MainForm.SyncMode.ItemIndex := 0;
    AddInput('sample.txt');
    AddInput('second.txt');
    Sync;
    Check(MainForm.RevisionBox.Items.Count = 1, 'First revision missing');

    WriteText('sample.txt', 'new content with a different size');
    Sync;
    Check(MainForm.RevisionBox.Items.Count = 2, 'Second revision missing');
    MainForm.RevisionBox.ItemIndex := 0;
    MainForm.RevisionBoxChange(nil);
    WaitForWorker;
    Check(Assigned(MainForm.OpenFind.OnChange), 'Search OnChange is not connected');
    MainForm.OpenFind.Text := 'sample*';
    MainForm.OpenFind.OnChange(MainForm.OpenFind);
    Check(MainForm.OpenLView.Items.Count = 1, 'Search did not filter the list');
    MainForm.OpenLView.Items[0].Selected := True;
    MainForm.mi_openClick(nil);
    WaitForWorker;
    Check(Trim(MainForm.LogMemo.Text) = 'old', 'Preview ignored the selected revision');
    MainForm.HomeBtnClick(nil);
    MainForm.OpenFind.Text := '';
    MainForm.OpenFind.OnChange(MainForm.OpenFind);
    Check(MainForm.OpenLView.Items.Count = 2, 'Clearing search did not restore the list');

    Destination := IncludeTrailingPathDelimiter(WorkDir) + 'restored';
    Check(CreateDir(Destination), 'Cannot create restore destination');
    WriteText(Destination + PathDelim + 'unrelated.txt', 'keep');
    MainForm.RestoreBtnClick(MainForm.RestoreBtn);
    MainForm.RestorePath.Text := Destination;
    MainForm.RestoreRevision.ItemIndex := 0;
    MainForm.RestoreMode.ItemIndex := 1;
    MainForm.RestoreOkClick(MainForm.RestoreOk);
    Check(Assigned(AppThread), 'Restore worker did not start');
    WaitForWorker;
    Check(ReadText(Destination + PathDelim + 'sample.txt') = 'old', 'Restore ignored revision');
    Check(ReadText(Destination + PathDelim + 'second.txt') = 'second', 'Restore omitted file');
    Check(FileExists(Destination + PathDelim + 'unrelated.txt'), 'Safe restore deleted unrelated file');
    Check(GetCurrentDir = WorkDir, 'Restore did not restore working directory');
    Check(MainForm.HomeBtn.Enabled, 'Navigation disabled after restore');
    Check(not MainForm.ProgressBar.Visible, 'Progress remains visible after restore');
    MainForm.HomeBtnClick(nil);
    Check(MainForm.Notebook.PageIndex = 1, 'Cannot return to browser after restore');

    Check(DeleteFile('second.txt'), 'Cannot delete second input');
    Sync;
    Check(MainForm.RevisionBox.Items.Count = 3, 'Deletion-only revision missing');
    Check(DeleteFile('sample.txt'), 'Cannot delete first input');
    Sync;
    Check(MainForm.RevisionBox.Items.Count = 4, 'Empty snapshot lost revision history');

    AppFileName := ExpandFileName('missing.gulp');
    MainForm.RevisionBox.ItemIndex := 0;
    MainForm.RevisionBoxChange(nil);
    WaitForWorker;
    Check(Pos('Error:', MainForm.LogMemo.Text) > 0, 'Worker error was not reported');
    Check(MainForm.HomeBtn.Enabled, 'Worker failure left navigation disabled');
    Check(not MainForm.ProgressBar.Visible, 'Worker failure left progress visible');
    MainForm.HomeBtnClick(nil);
    Check(MainForm.Notebook.PageIndex = 0, 'Cannot return home after worker failure');
    MainForm.RestoreBtnClick(MainForm.RestoreBtn);
    MainForm.RestorePath.Text := Destination;
    MainForm.RestoreRevision.ItemIndex := 0;
    MainForm.RestoreMode.ItemIndex := 1;
    MainForm.RestoreOkClick(MainForm.RestoreOk);
    WaitForWorker;
    Check(Pos('Error:', MainForm.LogMemo.Text) > 0, 'Restore failure was not reported');
    Check(GetCurrentDir = WorkDir, 'Restore failure did not restore working directory');
    WriteLn('PASS: worker lifecycle, history, preview, search and restore');
  finally
    WaitForWorker;
    MainForm.Free;
  end;
end.
