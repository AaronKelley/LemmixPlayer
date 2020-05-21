program LemResourceBuilder;

uses
  Forms,
  FMain in 'FMain.pas' {Form1},
  UCodes in 'UCodes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

