{$include lem_directives.inc}

unit GameLevelCodeScreen;

interface

uses
  Windows, Classes, Controls, Graphics, MMSystem, Forms,
  GR32, GR32_Image, GR32_Layers,
  UMisc,
  LemStrings, LemDosStructures, LemDosStyle,
  GameControl, GameBaseScreen;

type
  TGameLevelCodeScreen = class(TGameBaseScreen)
  private
    LevelCode      : string[10];
    CursorPosition : Integer;
    ValidLevelCode: Boolean;
    YPositions: array[0..3] of Integer;
    XPos: Integer;
    BlinkSpeedMS: Cardinal;
    PrevBlinkTime: Cardinal;
    Blinking: Boolean;
    Typing: Boolean;
    Validated: Boolean;
    LastMessage: string;
    LastCheatMessage: string;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_Close(Sender: TObject; var Action: TCloseAction );
    procedure Application_Idle(Sender: TObject; var Done: Boolean);
    function CheckLevelCode: Boolean;
    function CheckCheatCode: Boolean;
    procedure DrawChar(aCursorPos: Integer; aBlink: Boolean = False);
    procedure DrawMessage(const S: string);
    procedure UpdateCheatMessage;
  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildScreen; override;
  end;

implementation

uses SysUtils, LemLevelSystem;

{ TGameLevelCodeScreen }

procedure TGameLevelCodeScreen.Application_Idle(Sender: TObject; var Done: Boolean);
var
  CurrTime: Cardinal;
//  C: Char;
begin
  if ScreenIsClosing then
    Exit;
  if Typing then
    Exit;
  Done := False;
  Sleep(1); // relax CPU
  CurrTime := TimeGetTime;
  if CurrTime >= PrevBlinkTime + BlinkSpeedMS then
  begin
    PrevBlinkTime := CurrTime;
    Blinking := not Blinking;
    DrawChar(CursorPosition, Blinking);
(*    if Blinking then
      C := '_'
    else
      C := LevelCode[CursorPosition];
    DrawPurpleText(ScreenImg.Bitmap, C, 200 + CursorPosition * 16 - 16, 120 + 32, BackBuffer); *)
  end;
end;

procedure TGameLevelCodeScreen.BuildScreen;
var
  Mainpal: TArrayOfColor32;
//  S: string;
begin
  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;
    InitializeImageSizeAndPosition(640, 380);
    ExtractBackGround;
    ExtractPurpleFont;
    TileBackgroundBitmap(0, 0);
    BackBuffer.Assign(ScreenImg.Bitmap); // save background

    DrawPurpleText(ScreenImg.Bitmap, SEnterCode, XPos, 120);
    DrawPurpleText(ScreenImg.Bitmap, LevelCode, XPos, YPositions[1]);

    UpdateCheatMessage;

    Application.OnIdle := Application_Idle;
  finally
    ScreenImg.EndUpdate;
  end;
end;

function TGameLevelCodeScreen.CheckCheatCode: Boolean;
var
  S, CC, tstr: string;
  x: byte;
begin
  S := stringreplace(LowerCase(LevelCode), '.', '', [rfReplaceAll]);
  {$ifndef flexi}Result := CompareText(S, SCheatCode) = 0;{$endif}
  {$ifdef flexi}
  for x := 0 to 9 do
  begin
    if (tstr <> '') or (GameParams.SysDat.CheatCode[x] <> ' ') then
    begin
      tstr := tstr + GameParams.SysDat.CheatCode[x];
      if GameParams.SysDat.CheatCode[x] <> ' ' then CC := tstr;
    end;
  end;
  if (CC <> '') then Result := CompareText(S, LowerCase(CC)) = 0;
  {$endif}
end;

function TGameLevelCodeScreen.CheckLevelCode: Boolean;
var
//  i: Integer;
  s: string;
//  P, L: Integer;
  List: TStringList;
  Sys: TBaseDosLevelSystem;
  Txt: string;
begin
  Validated := True;
//  Result := False;
  s := stringreplace(LowerCase(LevelCode), '.', '', [rfReplaceAll]);

//  if not GameParams.CheatMode then
  //  Exit;

  Sys := GameParams.Style.LevelSystem as TBaseDosLevelSystem;
  List := TStringList.Create;

  try
    Result := Sys.FindLevelCode(LevelCode, GameParams.Info);

    if not Result then
      if GameParams.CheatCodesEnabled then
        Result := Sys.FindCheatCode(LevelCode, GameParams.Info);

    if Result then
    begin
      Txt := Format(SCodeForLevel_sd, [GameParams.Info.dSectionName, GameParams.Info.dLevel + 1]);
      //DrawPurpleTextCentered(ScreenImg.Bitmap, Txt, YPositions[2], BackBuffer);
      DrawMessage(Txt);
      Exit;
    end
    else
      //DrawPurpleTextCentered(ScreenImg.Bitmap, SIncorrectCode, {XPos, }YPositions[2], BackBuffer);
      DrawMessage(SIncorrectCode);
  finally

  List.Free;
  end;

end;

constructor TGameLevelCodeScreen.Create(aOwner: TComponent);
begin
  inherited;
  LevelCode := '..........';
  CursorPosition := 1;
  ScreenImg.Enabled := False;

  OnKeyDown := Form_KeyDown;
  OnKeyPress := Form_KeyPress;
  OnClose := Form_Close;

  BlinkSpeedMS := 240;

  XPos := (640 - (10 * 16)) div 2;

  YPositions[0] := 120;
  YPositions[1] := 152;
  YPositions[2] := 184;
  YPositions[3] := 216;

end;

destructor TGameLevelCodeScreen.Destroy;
begin
  Application.OnIdle := nil;
  inherited;
end;

procedure TGameLevelCodeScreen.DrawChar(aCursorPos: Integer; aBlink: Boolean);
var
  C: Char;
begin
  if aBlink then
    C := '_'
  else
    C := LevelCode[CursorPosition];
  DrawPurpleText(ScreenImg.Bitmap, C, XPos + CursorPosition * 16 - 16, YPositions[1], BackBuffer);
end;

procedure TGameLevelCodeScreen.DrawMessage(const S: string);
begin
  if LastMessage <> '' then
//    DrawPurpleText(ScreenImg.Bitmap, );
    DrawPurpleTextCentered(ScreenImg.Bitmap, LastMessage, YPositions[2], BackBuffer, True);

  LastMessage := S;

  if S = '' then
    Exit;

  DrawPurpleTextCentered(ScreenImg.Bitmap, S, YPositions[2]);
end;

procedure TGameLevelCodeScreen.UpdateCheatMessage;
begin
  Assert(GameParams <> nil);

  if LastCheatMessage <> '' then
    DrawPurpleTextCentered(ScreenImg.Bitmap, LastCheatMessage, 350- 20, BackBuffer, True);

  LastCheatMessage := '';

  if not (moCheatCodes in Gameparams.MiscOptions) then
    Exit;

  LastCheatMessage := 'Cheatcodes Enabled';

  DrawPurpleTextCentered(ScreenImg.Bitmap, LastCheatMessage, 350 - 20);

end;


procedure TGameLevelCodeScreen.Form_Close(Sender: TObject; var Action: TCloseAction);
begin
  Application.OnIdle := nil;
  if ValidLevelCode then
    GameParams.WhichLevel := wlLevelCode;
end;

procedure TGameLevelCodeScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ScreenIsClosing then
    Exit;
  if Shift = [] then
  begin
    case Key of
      VK_ESCAPE: CloseScreen(gstMenu);
      VK_RETURN:
        begin
          if CheckCheatCode then
          begin
            // toggle cheat
            GameParams.CheatCodesEnabled := not GameParams.CheatCodesEnabled;//True;
            UpdateCheatMessage;
//            DrawMessage('cheatmode enabled');
            Exit;
          end;
          if not ValidLevelCode then
          begin
            ValidLevelCode := CheckLevelCode;
            if ValidLevelCode then
            begin
              CloseDelay := 1000;
              DrawChar(CursorPosition, False);
              CloseScreen(gstMenu);
            end;
//            closedelay:=1000;
  //          CloseScreen(gstMenu);
          end
          else
            CloseScreen(gstMenu);
//          if ValidLevelCode then
  //          GameParams.WhichLevel := wlLevelCode;
        end;
    end;
  end;
end;

procedure TGameLevelCodeScreen.Form_KeyPress(Sender: TObject; var Key: Char);
var
  OldC, C: Char;
  OldPos: Integer;
  //GoBack: Boolean;
  
begin
  if ScreenIsClosing then
    Exit;

  Typing := True;
  try

    C := UpCase(Key);
    if C in ['A'..'Z', '0'..'9'] then
    begin
      DrawMessage('');
      OldC := LevelCode[CursorPosition];
      OldPos := CursorPosition;

      LevelCode[CursorPosition] := C;

      if CursorPosition < 10 then
      begin
        // maybe blinking: repair
//        if OldC <> C then
          DrawChar(CursorPosition, False);
        // next pos
        Inc(CursorPosition);
      end;

      if (OldPos <> CursorPosition) or (OldC <> C) then
      begin
        DrawChar(CursorPosition);
      end;

      ValidLevelCode := False;

    end
    else if C = Chr(8) then begin
      DrawMessage('');
//      GoBack := True;
      if CursorPosition > 1 then
      begin

        LevelCode[CursorPosition] := '.';
        // maybe blinking: repair
        DrawChar(CursorPosition, False);

        if CursorPosition > 1 then
          Dec(CursorPosition);

        ValidLevelCode := False;

      end;
    end;

  finally
    Typing := False;
  end;

end;

end.

