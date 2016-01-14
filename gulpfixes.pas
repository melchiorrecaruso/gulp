unit gulpfixes;

{$mode objfpc}
{$H+}

interface

uses sysutils;

function universaltime2local(ut: tdatetime): tdatetime;
function universaltime2local(ut: tdatetime; tzoffset: longint): tdatetime;
function localtime2universal(lt: tdatetime): tdatetime;
function localtime2universal(lt: tdatetime; tzoffset: longint): tdatetime;

implementation

function universaltime2local(ut: tdatetime): tdatetime;
begin
  result := universaltime2local(ut, -getlocaltimeoffset);
end;

function universaltime2local(ut: tdatetime; tzoffset: longint): tdatetime;
begin
  if (tzoffset > 0) then
    result := ut + encodetime(tzoffset div 60, tzoffset mod 60, 0, 0)
  else
  if (tzoffset < 0) then
    result := ut - encodetime(abs(tzoffset) div 60, abs(tzoffset) mod 60,
      0, 0)
  else
    result := ut;
end;

function localtime2universal(lt: tdatetime): tdatetime;
begin
  result := localtime2universal(lt, -getlocaltimeoffset);
end;

function localtime2universal(lt: tdatetime; tzoffset: longint): tdatetime;
begin
  if (tzoffset > 0) then
    result := lt - encodetime(tzoffset div 60, tzoffset mod 60, 0, 0)
  else
  if (tzoffset < 0) then
    result := lt + encodetime(abs(tzoffset) div 60, abs(tzoffset) mod 60, 0, 0)
  else
    result := lt;
end;

end.
