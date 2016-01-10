{
  Copyright (c) 2016 Melchiorre Caruso.

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

  Contains:

    Filesystem scanner class.

  Modified:

    v0.0.3 - 2016.01.10 by Melchiorre Caruso.
}

unit GulpScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  GulpList,
  SysUtils;

type
  { TScanner }

  TScanner = class(TObject)
  PRIVATE
    FList: TRawByteStringList;
    function GetCount: Integer;
    function GetItem(Index: LongInt): rawbytestring;
    procedure Scan(const FileMask: rawbytestring; Recursive: Boolean);
  PUBLIC
    constructor Create;
    destructor Destroy; OVERRIDE;
    procedure Add(const FileMask: rawbytestring);
    function Find(const FileName: rawbytestring): LongInt;
    procedure Delete(Index: LongInt);
    procedure Clear;
  PUBLIC
    property Count: Integer read GetCount;
    property Items[Index: LongInt]: rawbytestring read GetItem; DEFAULT;
  end;

{ Matching routines }

function FileNameMatch(const FileName: rawbytestring;
  const FileMask: rawbytestring): Boolean; OVERLOAD;

function FileNameMatch(const FileName: rawbytestring;
  FileMasks: TRawByteStringList): Boolean; OVERLOAD;

implementation

{ TScanner class }

constructor TScanner.Create;
begin
  inherited Create;
  FList := TRawByteStringList.Create;
end;

destructor TScanner.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

procedure TScanner.Clear;
begin
  FList.Clear;
end;

procedure TScanner.Scan(const FileMask: rawbytestring; Recursive: Boolean);
var
  E: LongInt;
  Mask: rawbytestring;
  Path: rawbytestring;
  SR: TSearchRec;
begin
  Path := ExtractFilePath(FileMask);
  Mask := ExtractFileName(FileMask);

  E := SysUtils.FindFirst(Path + '*', faReadOnly or faHidden or
    faSysFile or faVolumeId or faDirectory or faArchive or
    faSymLink or faAnyFile, SR);
  while E = 0 do
  begin
    if SR.Attr and faDirectory = faDirectory then
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        FList.Add(Path + SR.Name);
        if Recursive then if SR.Attr and faSymLink = 0 then
            Scan(Path + IncludeTrailingPathDelimiter(SR.Name) + Mask, True);
      end;
    end else
    if FileNameMatch(Path + SR.Name, FileMask) then FList.Add(Path + SR.Name);
    E := FindNext(SR);
  end;
  SysUtils.FindClose(SR);
end;

procedure TScanner.Add(const FileMask: rawbytestring);
begin
  if FileMask = '' then Exit;
  if DirectoryExists(FileMask) then FList.Add(FileMask)
  else
  if FileExists(FileMask) then FList.Add(FileMask)
  else
    Scan(FileMask, True);
end;

procedure TScanner.Delete(Index: LongInt);
begin
  FList.Delete(Index);
end;

function TScanner.Find(const FileName: rawbytestring): LongInt;
begin
  Result := FList.Find(FileName);
end;

function TScanner.GetItem(Index: LongInt): rawbytestring;
begin
  Result := FList[Index];
end;

function TScanner.GetCount: LongInt;
begin
  Result := FList.Count;
end;

{ Matching routines }

function MatchPattern(Element, Pattern: PAnsiChar): Boolean;
begin
  if 0 = StrComp(Pattern, '*') then Result := True
  else
  if (Element^ = Chr(0)) and (Pattern^ <> Chr(0)) then Result := False
  else
  if Element^ = Chr(0) then Result := True
  else
    case Pattern^ of
      '*':
        if MatchPattern(Element, @Pattern[1]) then Result := True
        else
          Result := MatchPattern(@Element[1], Pattern);
      '?':
        Result := MatchPattern(@Element[1], @Pattern[1]);
      else
        if Element^ = Pattern^ then
          Result := MatchPattern(@Element[1], @Pattern[1])
        else
          Result := False;
    end;
end;

function FileNameMatch(const FileName: rawbytestring;
  const FileMask: rawbytestring): Boolean;
begin
  Result := MatchPattern(PAnsiChar(FileName), PAnsiChar(FileMask));
end;

function FileNameMatch(const FileName: rawbytestring;
  FileMasks: TRawByteStringList): Boolean;
var
  I: LongInt;
begin
  Result := False;
  for I := 0 to FileMasks.Count - 1 do
    if FileNameMatch(FileName, FileMasks[I]) = True then
    begin
      Result := True;
      Break;
    end;
end;

end.
