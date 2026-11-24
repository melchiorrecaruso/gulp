program TestLastVersionAPI;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, GulpLibrary;

var
  App: TGulpApplication;
  F: TFileStream;

procedure CheckVersion(Expected: LongWord);
begin
  App.Reset;
  App.List('history.gulp');
  if App.LastVersion <> Expected then
    raise Exception.CreateFmt('Expected version %d, got %d',
      [Expected, App.LastVersion]);
end;

begin
  F := TFileStream.Create('first.txt', fmCreate);
  F.Free;
  F := TFileStream.Create('second.txt', fmCreate);
  F.Free;
  App := TGulpApplication.Create;
  try
    App.Include.Add('*.txt');
    App.Sync('history.gulp');
    CheckVersion(1);
    if not DeleteFile('second.txt') then raise Exception.Create('Cannot delete second file');
    App.Reset;
    App.Include.Add('*.txt');
    App.Sync('history.gulp');
    CheckVersion(2);
    if not DeleteFile('first.txt') then raise Exception.Create('Cannot delete first file');
    App.Reset;
    App.Include.Add('*.txt');
    App.Sync('history.gulp');
    CheckVersion(3);
    App.Reset;
    App.UntilVersion := 1;
    App.List('history.gulp');
    if App.LastVersion <> 3 then raise Exception.Create('Historical listing lost total version count');
    F := TFileStream.Create('empty.gulp', fmCreate);
    F.Free;
    App.Reset;
    App.List('empty.gulp');
    if App.LastVersion <> 0 then raise Exception.Create('Empty archive has stale version count');
  finally
    App.Free;
  end;
end.
