unit gulpscanner;

{$mode objfpc}
{$H+}

interface

uses classes, gulplist, sysutils;

type
  tscanner = class(TObject)
  private
    flist: trawbytestringlist;
    function getcount: integer;
    function getitem(index: longint): rawbytestring;
    procedure scan(const filemask: rawbytestring; recursive: boolean);
  public
    constructor create;
    destructor destroy; override;
    procedure add(const filemask: rawbytestring);
    function find(const filename: rawbytestring): longint;
    procedure delete(index: longint);
    procedure clear;
  public
    property count: integer read getcount;
    property items[index: longint]: rawbytestring read getitem; default;
  end;

function filenamematch(const filename: rawbytestring;
  const filemask: rawbytestring): boolean;
  overload;
function filenamematch(const filename: rawbytestring;
  filemasks: trawbytestringlist): boolean; overload;

implementation

constructor tscanner.create;
begin
  inherited create;
  flist := trawbytestringlist.create;
end;

destructor tscanner.destroy;
begin
  flist.destroy;
  inherited destroy;
end;

procedure tscanner.clear;
begin
  flist.clear;
end;

procedure tscanner.scan(const filemask: rawbytestring; recursive: boolean);
var
  e:    longint;
  mask: rawbytestring;
  path: rawbytestring;
  sr:   tsearchrec;
begin
  path := extractfilepath(filemask);
  mask := extractfilename(filemask);
  e    := sysutils.findfirst(path + '*', fareadonly or fahidden or fasysfile or
    favolumeid or fadirectory or faarchive or fasymlink or faanyfile, sr);
  while e = 0 do
  begin
    if sr.attr and fadirectory = fadirectory then
    begin
      if (sr.name <> '.') and (sr.name <> '..') then
      begin
        flist.add(path + sr.name);
        if recursive then
          if sr.attr and fasymlink = 0 then
            scan(path + includetrailingpathdelimiter(sr.name) + mask, true);
      end;
    end else
    if filenamematch(path + sr.name, filemask) then
      flist.add(path + sr.name);
    e := findnext(sr);
  end;
  sysutils.findclose(sr);
end;

procedure tscanner.add(const filemask: rawbytestring);
begin
  if filemask = '' then
    exit;
  if directoryexists(filemask) then
    flist.add(filemask)
  else
  if fileexists(filemask) then
    flist.add(filemask)
  else
    scan(filemask, true);
end;

procedure tscanner.delete(index: longint);
begin
  flist.delete(index);
end;

function tscanner.find(const filename: rawbytestring): longint;
begin
  result := flist.find(filename);
end;

function tscanner.getitem(index: longint): rawbytestring;
begin
  result := flist[
    index];
end;

function tscanner.getcount: longint;
begin
  result := flist.count;
end;

function matchpattern(element, pattern: pansichar): boolean;
begin
  if 0 = strcomp(pattern, '*') then
    result := true
  else
  if (element^ = chr(0)) and (pattern^ <> chr(0)) then
    result := false
  else
  if element^ = chr(0) then
    result := true
  else
    case pattern^ of
      '*': if matchpattern(element, @pattern[1]) then
          result := true
        else
          result := matchpattern(@element[1], pattern);
      '?': result := matchpattern(@element[1], @pattern[1]);
    else
      if element^ = pattern^ then
        result := matchpattern(@element[1], @pattern[1])
      else
        result := false;
    end;
end;

function filenamematch(const filename: rawbytestring;
  const filemask: rawbytestring): boolean;
begin
  result :=
    matchpattern(pansichar(filename), pansichar(filemask));
end;

function filenamematch(const filename: rawbytestring;
  filemasks: trawbytestringlist): boolean;
var
  i: longint;
begin
  result := false;
  for i := 0 to filemasks.count - 1 do
    if filenamematch(filename, filemasks
      [i]) = true then
    begin
      result := true;
      break;
    end;
end;

end.
