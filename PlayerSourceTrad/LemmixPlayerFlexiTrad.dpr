{$include lem_directives.inc}

program LemmixPlayerFlexiTrad;

uses
  (*UDebug,*)
  LemRes,
  Forms,
  FMain in 'FMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
