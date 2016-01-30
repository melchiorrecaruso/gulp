program gulpui;

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
  gulpmain;

{$r *.res}

begin
  application.title :='gulp user interface';
  requirederivedformresource := true;
  application.initialize;
  application.createform(tmainform, mainform);
  application.run;
end.

