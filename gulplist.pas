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

    List classes.

  Modified:

    v0.0.3 - 2016.01.09 by Melchiorre Caruso.
}

unit GulpList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  { TGenericList }

  generic TGenericList<TGenericItem> = class(TObject)
  public
    type
    TGenericCompare =
    function(Item1, Item2: TGenericItem): longint;
  private
    FList:    TList;
    FCompare: TGenericCompare;
    function GetCount: longint;
    function Get(Index: longint): TGenericItem;
    function Search(Item: TGenericItem; var M: longint): longint;
  public
    constructor Create(Compare: TGenericCompare);
    destructor Destroy; override;
    function Add(Item: TGenericItem): longint;
    function Find(Item: TGenericItem): longint;
    procedure Delete(Index: longint);
  public
    property Items[Index: longint]: TGenericItem Read Get; default;
    property Count: longint Read GetCount;
  end;

  { TRawByteStringList }

  PRawByteStringItem = ^TRawByteStringItem;

  TRawByteStringItem = record
    FString: rawbytestring;
    FObject: TObject;
  end;

  PRawByteStringList = specialize TGenericList<PRawByteStringItem>;

  TRawByteStringList = class(TObject)
  private
    FList: PRawByteStringList;
    function GetCount: longint;
    function Get(Index: longint): rawbytestring;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const S: rawbytestring);
    function Find(const S: rawbytestring): longint;
    procedure Delete(Index: longint);
    procedure Clear;
  public
    property Items[Index: longint]: rawbytestring Read Get; default;
    property Count: longint Read GetCount;
  end;

implementation

{ TGenericList class }

constructor TGenericList.Create(Compare: TGenericCompare);
begin
  inherited Create;
  FCompare := Compare;
  FList := TList.Create;
end;

destructor TGenericList.Destroy;
begin
  FList.Destroy;
  inherited Destroy;
end;

procedure TGenericList.Delete(Index: longint);
begin
  FList.Delete(Index);
end;

function TGenericList.Search(Item: TGenericItem; var M: longint): longint;
var
  L, H, I: longint;
begin
  I := 1;
  L := 0;
  M := 0;
  H := FList.Count - 1;
  while H >= L do
  begin
    M := (L + H) div 2;
    I := FCompare(Get(M), Item);
    if I < 0 then
      L := M + 1
    else if I > 0 then
      H := M - 1
    else
      Break;
  end;

  if I <> 0 then
  begin
    if I < 0 then
      M := M + 1;
    Result := -1;
  end
  else
    Result := M;
end;

function TGenericList.Add(Item: TGenericItem): longint;
var
  M: longint;
begin
  Result := Search(Item, M);
  if Result = -1 then
  begin
    FList.Insert(M, Item);
    Result := M;
  end;
end;

function TGenericList.Find(Item: TGenericItem): longint;
var
  M: longint;
begin
  Result := Search(Item, M);
end;

function TGenericList.Get(Index: longint): TGenericItem;
begin
  Result := TGenericItem(FList[Index]);
end;

function TGenericList.GetCount: longint;
begin
  Result := FList.Count;
end;

{ TRawByteStringList class }

function Compare(Item1, Item2: PRawByteStringItem): longint;
begin
  {$IFDEF UNIX}
    Result := AnsiCompareStr(Item1^.FString, Item2^.FString);
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result := AnsiCompareText(Item1^.FString, Item2^.FString);
  {$ELSE}
    Unsupported platform...
  {$ENDIF}
  {$ENDIF}
end;

constructor TRawByteStringList.Create;
begin
  FList := PRawByteStringList.Create(@Compare);
end;

destructor TRawByteStringList.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TRawByteStringList.Delete(Index: longint);
begin
  Dispose(FList[Index]);
  FList.Delete(Index);
end;

procedure TRawByteStringList.Clear;
begin
  while GetCount > 0 do
    Delete(0);
end;

procedure TRawByteStringList.Add(const S: rawbytestring);
var
  P: PRawByteStringItem;
begin
  P := New(PRawByteStringItem);
  P^.FString := S;
  P^.FObject := nil;
  if FList.Add(P) = -1 then
    Dispose(P);
end;

function TRawByteStringList.Find(const S: rawbytestring): longint;
var
  P: PRawByteStringItem;
begin
  P := New(PRawByteStringItem);
  P^.FString := S;
  P^.FObject := nil;
  begin
    Result := FList.Find(P);
  end;
  Dispose(P);
end;

function TRawByteStringList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TRawByteStringList.Get(Index: longint): rawbytestring;
begin
  Result := FList[Index]^.FString;
end;

end.
