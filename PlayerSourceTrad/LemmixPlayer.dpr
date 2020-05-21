{$include lem_directives.inc}

program LemmixPlayer;

uses
  LemRes,
  Forms,
  FMain in 'FMain.pas' {MainForm};
  //GameVgmPlay in 'GameVgmPlay.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
