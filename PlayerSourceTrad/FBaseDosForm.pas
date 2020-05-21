unit FBaseDosForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  UMisc, Gr32,
  GameControl;

type
  {-------------------------------------------------------------------------------
    abstract black, fullscreen, ancestor form
  -------------------------------------------------------------------------------}
  TBaseDosForm = class(TForm)
  private
    fGameParams: TDosGameParams;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure BuildScreen; virtual;
    procedure PrepareGameParams(Params: TDosGameParams); virtual; // always call inherited
    property GameParams: TDosGameParams read fGameParams;
  public
    constructor Create(aOwner: TComponent); override;
    function ShowScreen(Params: TDosGameParams): Integer; virtual;
  end;

implementation

{$R *.dfm}

{ TBaseDosForm }

procedure TBaseDosForm.BuildScreen;
begin
  //
end;

constructor TBaseDosForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Caption := 'Lemmix';
  Color := clBlack;
  BorderStyle := {bsSizeable} bsNone;
  BorderIcons := [{biSystemMenu, biMinimize, biMaximize}];
  WindowState := {wsNormal} wsMaximized;
  Cursor := crNone;
end;

procedure TBaseDosForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
  begin
    Style := Style or CS_OWNDC; // maybe faster screen output
  end;
end;

procedure TBaseDosForm.PrepareGameParams(Params: TDosGameParams);
begin
  fGameParams := Params;
end;

function TBaseDosForm.ShowScreen(Params: TDosGameParams): Integer;
begin
  PrepareGameParams(Params);
  BuildScreen;
  Result := ShowModal;
end;

end.

