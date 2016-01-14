unit gulpstream;

{$mode objfpc}
{$H+}

interface

uses classes;

type
  tnulstream = class
    (TStream)
  public
    function read(var buffer; count: longint): longint; override;
    function write(const buffer; count: longint): longint; override;
  end;

implementation

function tnulstream.read(var buffer; count: longint): longint;
begin
  result := count;
end;

function tnulstream.write(const buffer; count: longint): longint;
begin
  result := count;
end;

end.
