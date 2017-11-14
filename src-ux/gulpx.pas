program gulpx;

{$define usecthreads}
{$mode objfpc}
{$h+}

uses
{$ifdef unix}
{$ifdef usecthreads}
  cthreads,
  cmem,
{$endif}
{$endif}
  interfaces,
  forms,
  gulpmain, gulpabout;

{$r *.res}

begin
  requirederivedformresource := true;
  application.initialize;
  application.createform(tmainform, mainform);
  application.run;
end.

