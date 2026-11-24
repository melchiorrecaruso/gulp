program test_emptyview;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, GulpLibrary;

var
  App: TGulpApplication;
  Pipe: TMemoryStream;
  FileStream: TFileStream;
  OnlyVersion: Integer;
begin
  FileStream := TFileStream.Create('empty.txt', fmCreate);
  FileStream.Free;
  App := TGulpApplication.Create;
  Pipe := TMemoryStream.Create;
  try
    App.Include.Add('empty.txt');
    App.Sync('empty.gulp');
    App.Pipe := Pipe;
    for OnlyVersion := 0 to 1 do
    begin
      App.Reset;
      App.Include.Add('empty.txt');
      App.OnlyVersion := OnlyVersion;
      App.View('empty.gulp');
      if Pipe.Size <> 0 then
        raise Exception.CreateFmt('Empty file preview contains %d bytes (only=%d)',
          [Pipe.Size, OnlyVersion]);
    end;
  finally
    App.Pipe := nil;
    App.Free;
    Pipe.Free;
  end;
end.
