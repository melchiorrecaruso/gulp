{
  Copyright (C) 2014 Melchiorre Caruso

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

unit Streams;

{$I gulp.inc}

interface

uses
  Classes;

type
  { TFileReader class }

  TFileReader = class(TFileStream)
  private
    FBufferSize: longint;
    FBufferIndex: longint;
    FBuffer: array[0..$FFFF] of byte;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
  end;

  { TFileWriter class }

  TFileWriter = class(TFileStream)
  private
    FBufferIndex: longint;
    FBuffer: array[0..$FFFF] of byte;
    procedure FlushBuffer;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  { TNulStream class }

  TNulStream = class(TStream)
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


implementation

uses
  Math,
  Status;

{ TFileReader methods }

constructor TFileReader.Create(const AFileName: string; Mode: Word);
begin
  inherited Create(AFileName, Mode);
  FBufferSize  := 0;
  FBufferIndex := 0;
end;

constructor TFileReader.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
begin
  inherited Create(AFileName, Mode, Rights);
  FBufferSize  := 0;
  FBufferIndex := 0;
end;

destructor TFileReader.Destroy;
begin
  inherited Destroy;
end;

function TFileReader.Read(var Buffer; Count: longint): longint;
var
  Data: array [0..$FFFFFFF] of byte absolute Buffer;
  I: longint;
begin
  Result := 0;
  repeat
    if FBufferIndex = FBufferSize then
    begin
      FBufferSize   := inherited Read(FBuffer[0], SizeOf(FBuffer));
      FBufferIndex  := 0;

      if FBufferSize = 0 then Break;
    end;
    I := Min(Count - Result, FBufferSize - FBufferIndex);

    Move(FBuffer[FBufferIndex], Data[Result], I);
    Inc (FBufferIndex, I);
    Inc (Result, I);
  until Result = Count;
end;

function TFileReader.Write(const Buffer; Count: longint): longint;
begin
  Result := 0;
end;

function TFileReader.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if Origin <> soCurrent then
  begin
    FBufferSize  := 0;
    FBufferIndex := 0;
    Result := inherited Seek (Offset, Origin);
  end else
    Result := inherited Seek (Offset, Origin) - (FBufferSize - FBufferIndex);
end;

{ TFileWriter methods}

constructor TFileWriter.Create(const AFileName: string; Mode: Word);
begin
  inherited Create(AFileName, Mode);
  FBufferIndex := 0;
end;

constructor TFileWriter.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
begin
  inherited Create(AFileName, Mode, Rights);
  FBufferIndex := 0;
end;

destructor TFileWriter.Destroy;
begin
  if FBufferIndex <> 0 then FlushBuffer;
  inherited Destroy;
end;

procedure TFileWriter.FlushBuffer;
begin
  FBufferIndex := FBufferIndex -
    inherited Write(FBuffer[0], FBufferIndex);
end;

function TFileWriter.Read(var Buffer; Count: longint): longint;
begin
  Result := 0;
end;

function TFileWriter.Write(const Buffer; Count: longint): longint;
var
  Data: array [0..$FFFFFFF] of byte absolute Buffer;
  I: longint;
begin
  Result := 0;
  repeat
    if FBufferIndex = SizeOf(FBuffer) then
    begin
      FlushBuffer;

      if FBufferIndex <> 0 then Break;
    end;
    I := Min(Count - Result, SizeOf(FBuffer) - FBufferIndex);

    Move(Data[Result], FBuffer[FBufferIndex], I);
    Inc (FBufferIndex, I);
    Inc (Result, I);
  until Result = Count;
end;

function TFileWriter.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if Origin <> soCurrent then
  begin
    if FBufferIndex <> 0 then
      inherited Write(FBuffer[0], FBufferIndex);

    FBufferIndex := 0;

    Result := inherited Seek (Offset, Origin);
  end else
    Result := inherited Seek (Offset, Origin) + FBufferIndex;
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

end.
