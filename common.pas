{
  Copyright (c) 2014-2015 Melchiorre Caruso.

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

unit Common;

interface

uses
  Classes;

type
  { TRecursiveMode }

  TRecursiveMode = (rmON, rmOFF, rmAUTO);

  { TSysScanner }

  TSysScanner = class(TObject)
  private
    FItems: TStringList;
    function GetCount: integer;
    function GetItem(Index: longint): string;
    procedure AddItem(const FileName: string);
    procedure Scan(const FileMask:string; Recursive:boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const FileMask: string; Mode: TRecursiveMode);
    procedure Delete(const FileMask: string; Mode: TRecursiveMode); overload;
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
    function Read (var   Buffer; Count: Longint): Longint; override;
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
  begin
    FItems.Add(FileName);
  end;
end;

procedure TSysScanner.Scan(const FileMask: string; Recursive: boolean);
var
  Error: longint;
  Rec: TSearchRec;
  ScanPath: string;
  ScanMask: string;
begin
  ScanPath := ExtractFilePath(FileMask);
  ScanMask := ExtractFileName(FileMask);
  // search filemask ...
  Error := SysUtils.FindFirst(ScanPath + '*',
    faReadOnly  or faHidden  or faSysFile or faVolumeId  or
    faDirectory or faArchive or faSymLink or faAnyFile,  Rec);
  while Error = 0 do
  begin

    if (Rec.Attr and faDirectory) <> 0 then
    begin
      if Recursive then
        if (Rec.Name <> '.') and (Rec.Name <> '..') then
        begin
          AddItem(ScanPath + Rec.Name);

          if Rec.Attr and faSymLink = 0 then
            Scan(ScanPath + Rec.Name + PathDelim + ScanMask, Recursive);
        end;
    end else
      if FileNameMatch(ScanPath + Rec.Name, FileMask) then
        AddItem(ScanPath + Rec.Name);

    Error := FindNext(Rec);
  end; // end while error ...
  SysUtils.FindClose(Rec);
end;

procedure TSysScanner.Add(const FileMask: string; Mode: TRecursiveMode);
begin
  if FileMask = '' then Exit;
  // directory and recursive mode ...
  if DirectoryExists(FileMask) then
  begin
    AddItem(FileMask);
    Scan(IncludeTrailingPathDelimiter(FileMask) + '*', Mode in [rmON, rmAUTO]);
  end else
  begin
    if FileMask[Length(FileMask)] = PathDelim then
    begin
      Scan(IncludeTrailingPathDelimiter(FileMask) + '*', Mode in [rmON, rmAUTO])
    end else
    begin
      if FileExists(FileMask) = FALSE then
        Scan(FileMask, Mode in [rmON, rmAUTO])
      else
        Scan(FileMask, Mode in [rmON]);
    end;
  end;
end;

procedure TSysScanner.Delete(const FileMask: string; Mode: TRecursiveMode);
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

function TSysScanner.Find(const FileName: string): longint;
begin
  Result := FItems.IndexOf(FileName);
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
