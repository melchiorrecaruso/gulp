program gulpUI;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  cmem,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  GulpMain;

{$R *.res}

begin
  RequireDerivedFormResource := TRUE;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

