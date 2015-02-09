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
  Classes,
  SysUtils;

type
  { TSysScanner }

  TSysScanner = class(TObject)
  private
    FList: TStringList;
    function GetCount: integer;
    function GetItem(Index: longint): string;
    procedure AddItem(const FileName: string);
    procedure Scan(const FileMask: string; Recursive: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const FileMask: string);
    procedure Delete(const FileMask: string); overload;
    procedure Delete(Index: longint); overload;
    function Find(const FileName: string): longint;
    procedure Clear;
  public
    property Count: integer read GetCount;
    property Items[Index: longint]: string read GetItem;
  end;

  { Matching routine }

  function FileNameMatch(const FileName, FileMask: string): boolean;

implementation

uses
  Masks;

{ TSysScanner class }

constructor TSysScanner.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  {$IFDEF MSWINDOWS}  FList.CaseSensitive := FALSE; {$ENDIF}
  {$IFDEF UNIX}       FList.CaseSensitive := TRUE;  {$ENDIF}
  {$IFDEF MAC}        FList.CaseSensitive := TRUE;  {$ENDIF}
  FList.Sorted := TRUE;
end;

destructor TSysScanner.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TSysScanner.Clear;
begin
  FList.Clear;
end;

procedure TSysScanner.AddItem(const FileName: string);
begin
  if FList.IndexOf(FileName) = - 1 then
  begin
    FList.Add(FileName);
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
      if (Rec.Name <> '.') and (Rec.Name <> '..') then
      begin

        if Recursive then
        begin
          AddItem(ScanPath + Rec.Name);
          if Rec.Attr and faSymLink = 0 then
            Scan(ScanPath + Rec.Name + PathDelim + ScanMask, TRUE);
        end else
        begin
          if FileNameMatch(ScanPath + Rec.Name, FileMask) then
            AddItem(ScanPath + Rec.Name);
        end;

      end;
    end else
      if FileNameMatch(ScanPath + Rec.Name, FileMask) then
        AddItem(ScanPath + Rec.Name);

    Error := FindNext(Rec);
  end; // end while error ...
  SysUtils.FindClose(Rec);
end;

procedure TSysScanner.Add(const FileMask: string);
begin
  if FileMask = '' then Exit;
  // directory and recursive mode ...
  if DirectoryExists(FileMask) then
  begin
    AddItem(FileMask);
    Scan(FileMask + PathDelim + '*', TRUE);
  end else
  begin
    if FileMask[Length(FileMask)] = PathDelim then
    begin
      Scan(FileMask + '*', TRUE)
    end else
    begin
      if FileExists(FileMask) then
        AddItem(FileMask)
      else
        Scan(FileMask, TRUE);
    end;
  end;
end;

procedure TSysScanner.Delete(const FileMask: string);
var
  I: longint;
begin
  for I := Count - 1 downto 0 do
    if FileNameMatch(Items[I], FileMask) then
    begin
      Delete(I);
    end;
end;

procedure TSysScanner.Delete(Index: longint);
begin
  FList.Delete(Index);
end;

function TSysScanner.Find(const FileName: string): longint;
begin
  if FList.Find(FileName, Result) = FALSE then
  begin
    Result := -1;
  end;
end;

function TSysScanner.GetCount: longint;
begin
  Result := FList.Count;
end;

function TSysScanner.GetItem(Index: longint): string;
begin
  Result := FList[Index];
end;

{ Matching routine }

function FileNameMatch(const FileName, FileMask: string): boolean;
begin
  {$IFDEF MSWINDOWS} Result := MatchesMask(FileName, FileMask, FALSE); {$ENDIF}
  {$IFDEF UNIX}      Result := MatchesMask(FileName, FileMask, TRUE ); {$ENDIF}
  {$IFDEF MAC}       Result := MatchesMask(FileName, FileMask, TRUE ); {$ENDIF}
end;

end.
