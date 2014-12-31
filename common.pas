{
  Copyright (c) 2014 Melchiorre Caruso.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{
  Contains:

  Modifyed:

}

unit Scanner;

{$I gulp.inc}

interface

uses
  Classes;

type
  { TSysScanner }

  TSysScanner = class(TObject)
  private
    FItems: TStringList;
    function GetCount: integer;
    function GetItem(Index: longint): string;
    procedure Scan(const FileMask:string);
    procedure AddItem(const FileName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(FileMask: string);
    procedure Delete(const FileMask: string; Recursive: boolean); overload;
    procedure Delete(Index: longint); overload;
    function Find(const FileName: string): longint;
    procedure Clear;
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: string read GetItem;
  end;

  { TNulStream class }

  TNulStream = class(TStream)
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { Matching routine }

  function FileNameMatch(const FileName, FileMask: string): boolean;

implementation

uses
  SysUtils,
  Masks;

{ TSysScanner class }

constructor TSysScanner.Create;
begin
  inherited Create;
  FItems               := TStringList.Create;
  FItems.CaseSensitive := FileNameCaseSensitive;
  FItems.Sorted        := TRUE;
end;

destructor TSysScanner.Destroy;
begin
  FItems.Clear;
  FItems.Destroy;
  inherited Destroy;
end;

procedure TSysScanner.Clear;
begin
  FItems.Clear;
end;

procedure TSysScanner.AddItem(const FileName: string);
begin
  if FItems.IndexOf(FileName) = -1 then
    FItems.Add(FileName);
end;

function TSysScanner.Find(const FileName: string): longint;
begin
  Result := FItems.IndexOf(FileName);
end;

procedure TSysScanner.Scan(const FileMask: string);
var
  Error: longint;
  Rec: TSearchRec;
  ScanPath: string;
  ScanMask: string;
begin
  ScanPath := ExtractFilePath(FileMask);
  ScanMask := ExtractFileName(FileMask);
  // search filemask ...
  Error := SysUtils.FindFirst(ScanPath + '*', faAnyFile, Rec);
  while Error = 0 do
  begin
    if (Rec.Attr and faDirectory) = faDirectory then
    begin
      if (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin
        Scan(ScanPath + Rec.Name + PathDelim + ScanMask);
        if FileNameMatch(ScanPath + Rec.Name, FileMask) then
          AddItem(ScanPath + Rec.Name);
      end;
    end else
      if FileNameMatch(ScanPath + Rec.Name, FileMask) then
        AddItem(ScanPath + Rec.Name);

    Error := FindNext(Rec);
  end; // end while error ...
  SysUtils.FindClose(Rec);
end;

procedure TSysScanner.Add(FileMask: string);
begin
  if FileMask = '' then Exit;
  // directory and recursive mode ...
  if DirectoryExists(FileMask) then
  begin
    AddItem(FileMask);
    Scan(IncludeTrailingPathDelimiter(FileMask) + '*');
  end else
    if FileMask[Length(FileMask)] = PathDelim then
      Scan(IncludeTrailingPathDelimiter(FileMask) + '*')
    else
      Scan(FileMask);
end;

procedure TSysScanner.Delete(const FileMask: string; Recursive: boolean);
var
  I: longint;
begin
  for I := Count - 1 downto 0 do
    if FileNameMatch(FItems[I], FileMask) then
    begin
      FItems.Delete(I);
    end;
end;

procedure TSysScanner.Delete(Index: longint);
begin
  FItems.Delete(Index);
end;

function TSysScanner.GetCount: longint;
begin
  Result := FItems.Count;
end;

function TSysScanner.GetItem(Index: longint): string;
begin
  Result := FItems[Index];
end;

{ TNulStream class }

function TNulStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

function TNulStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
end;

{ Matching routine }

function FileNameMatch(const FileName, FileMask: string): boolean;
begin
  Result := MatchesMask(FileName, FileMask, FileNameCaseSensitive);
end;

end.
