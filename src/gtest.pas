program gtest;

uses
  cmem,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  sysutils,
  classes,
  math,
  bufstream,
  gulpcommon,
  gulpdmc;

//////////////////////////// main ////////////////////////////
var
  start: double;

  en: tdmccompressionstream;
  de: tdmcdecompressionstream;

  buffer : array[0..4095] of byte;
  readed :longint;


  filesize: int64;
  bits: byte;

  rs: treadbufstream;
  ws: twritebufstream;

begin

  if (paramcount <> 3) then halt;

  if (paramstr(1) = 'c') and (paramcount <> 3) then halt;
  if (paramstr(1) = 'd') and (paramcount <> 3) then halt;

  // start timer
  start := now;
  writeln('gulp v 0.1 an dmc file compressor, (c) 2017');
  writeln('by melchiorre caruso (italy)');

  // compress
  if (paramstr(1) = 'c') then
  begin

    // open files
    rs := treadbufstream.create(
          tfilestream.create(paramstr(2), fmopenread or fmsharedenywrite));

    ws := twritebufstream.create(
          tfilestream.create(paramstr(3), fmcreate));

    en := tdmccompressionstream.create(ws, 2, 24, (1024*1024*1024) div sizeof(tdmcnode));

    ws.write(rs.size, sizeof(int64));

    readed := rs.read(buffer, sizeof(buffer));
    while readed > 0 do
    begin
      en.write(buffer, readed);
      readed := rs.read(buffer, sizeof(buffer));
    end;
    en.destroy;

    ws.destroy;
    rs.destroy;
  end else
  begin // decompress

    rs.read(filesize, sizeof(filesize));
    while filesize > 0 do
    begin
      rs.read (bits, sizeof(bits));
      ws.write(bits, sizeof(bits));
      dec(filesize);
    end;

    ws.destroy;
    rs.destroy;
  end;

  // print results
  if (paramstr(1) = 'c') then
  begin
    writeln(format('%s (%d bytes) compress to %s (%d bytes) in %1.2f s.',
    [paramstr(2), getsize(paramstr(2)), paramstr(3), getsize(paramstr(3)),
      (now - start) * (24 * 60 * 60)]));
  end else
  begin
    writeln(format('%s (%d bytes) decompress to %s (%d bytes) in %1.2f s.',
    [paramstr(2), getsize(paramstr(2)), paramstr(3), getsize(paramstr(3)),
      (now - start) * (24 * 60 * 60)]));
  end;
  writeln;
end.


