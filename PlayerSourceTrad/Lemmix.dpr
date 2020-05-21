{$include lem_directives.inc}

program Lemmix;

uses
  {$ifdef develop}
  UDebug,
  {$endif}
  LemRes,
  Forms,
  FMain in 'FMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

