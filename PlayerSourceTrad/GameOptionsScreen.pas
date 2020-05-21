{$include lem_directives.inc}

unit GameOptionsScreen;

interface

uses
  Windows, Classes, SysUtils, Controls,
  UMisc,
  Gr32, Gr32_Image, Gr32_Layers,
  LemCore,
  LemTypes,
  LemStrings,
  LemLevelSystem,
  LemGame,
  GameControl,
  GameBaseScreen;

type
  TGameOptionsScreen = class(TGameBaseScreen)
  private
    fLineCount: Integer;
    function GetScreenText: string;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseClick(Button: TMouseButton);
    function GetLineStr(aLine: Integer): string;
    procedure DrawLines(aBitmap: TBitmap32; const aLines: TByteSet = []);
  protected
    procedure PrepareGameParams(Params: TDosGameParams); override;
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

uses
  Forms, LemStyle;

{ TDosGamePreview }

procedure TGameOptionsScreen.PrepareGameParams(Params: TDosGameParams);
begin
  inherited;
end;

procedure TGameOptionsScreen.BuildScreen;
var
  Temp: TBitmap32;
//  DstRect: TRect;
begin
  //fSection := aSection;
  //fLevelNumber := aLevelNumber;
  //fGameResult := aResult;

  ScreenImg.BeginUpdate;
  Temp := TBitmap32.Create;
  try
    InitializeImageSizeAndPosition(640, 350);
    ExtractBackGround;
    ExtractPurpleFont;

    Temp.SetSize(640, 350);
    Temp.Clear(0);
    TileBackgroundBitmap(0, 0, Temp);
    BackBuffer.Assign(Temp);
//    DrawPurpleText(Temp, GetScreenText, 16, 16);
    DrawLines(Temp, [0..9]);
    ScreenImg.Bitmap.Assign(Temp);
  finally
    ScreenImg.EndUpdate;
    Temp.Free;
  end;
end;

constructor TGameOptionsScreen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Stretched := True;
  OnKeyDown := Form_KeyDown;
  OnKeyPress := Form_KeyPress;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
  fLineCount := 10;
end;

destructor TGameOptionsScreen.Destroy;
begin
  inherited Destroy;
end;

function TGameOptionsScreen.GetScreenText: string;
begin
  Result := 'Gradient Bridges:  on';
end;

procedure TGameOptionsScreen.Form_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift <> [] then Exit;
  case Key of
    VK_ESCAPE: CloseScreen(gstMenu);

//    VK_RETURN: CloseScreen(gstPreview);
  end;
end;

procedure TGameOptionsScreen.Form_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HandleMouseClick(Button);
end;

procedure TGameOptionsScreen.Img_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  HandleMouseClick(Button);
end;

procedure TGameOptionsScreen.HandleMouseClick(Button: TMouseButton);
begin
  if Button = mbLeft then
    CloseScreen(gstPreview)
  else if Button = mbRight then
    CloseScreen(gstMenu);
end;

procedure TGameOptionsScreen.Form_KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '1':
      begin

      end;
  end;
end;

procedure TGameOptionsScreen.DrawLines(aBitmap: TBitmap32; const aLines: TByteSet = []);
{-------------------------------------------------------------------------------
  draws all the options
-------------------------------------------------------------------------------}
var
  x, y, i: Integer;
begin
  for i := 0 to fLineCount - 1 do
    if i in aLines then
    begin
      DrawPurpleText(aBitmap, GetLineStr(i), 16, 32 * i + 16);
    end;
end;

function TGameOptionsScreen.GetLineStr(aLine: Integer): string;
begin
  Result := Chr(Ord('A') + aLine) + '. ' + 'Gradient Bridges: ' + ' on';
end;

end.


