{$include lem_directives.inc}

unit FMain;

{-------------------------------------------------------------------------------
  This is the main form which does almost nothing. It's black and fullscreen to
  prevent seeing the desktop when changing forms.
-------------------------------------------------------------------------------}

{ DONE : releaserate adjusting }
{ DONE : better animated objects drawing }
{ DONE : make levelcode screen type-able }
{ DONE : mouse scrolling and minimap click }
{ DONE : perfect level logic GUI }
{ DONE : enable saving, replaying from postviewscreen }

{ TODO: make use of tbitmap32.drawto(dst, x, y, srcrect) }
{ TODO: make sure sounds en music can be set off before the bassmod is loaded }
{ TODO: safe load bassmod?}
{ TODO : maybe create palette class? }
{ TODO : Strip UTools }
{ TODO : Remove refs to kernel, when making opensource }
{ TODO : add levelcode system, randomize codesystem }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls,
  FBaseDosForm,
  //LemSettings,
  LemGame,
  AppController;

const
  LM_START = WM_USER + 1;

type
  TMainForm = class(TBaseDosForm)
    procedure FormActivate(Sender: TObject);
  private
    Started: Boolean;
    AppController: TAppController;
    procedure LMStart(var Msg: TMessage); message LM_START;
    procedure PlayGame;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.LMStart(var Msg: TMessage);
begin
  //Hide;
  PlayGame;
end;

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;
  //ProgramSettings := TProgramSettings.Create;
  GlobalGame := TLemmingGame.Create(nil);
  AppController := TAppController.Create(nil);
end;

destructor TMainForm.Destroy;
begin
  GlobalGame.Free;
  AppController.Free;
//  ProgramSettings.Free;
  inherited;
end;

procedure TMainForm.PlayGame;
begin
  try
    AppController.Execute;
    Close;
  except
    on E: Exception do
    begin
      Application.ShowException(E);
      Close;
    end;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if Started then
    Exit;
  Started := True;
  PostMessage(Handle, LM_START, 0, 0);
end;

end.
//system
