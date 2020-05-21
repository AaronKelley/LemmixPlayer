{$include lem_directives.inc}

unit GameMenuScreen;

{-------------------------------------------------------------------------------
  The main menu dos screen.
-------------------------------------------------------------------------------}

interface

uses
  Windows, Classes, Controls, Graphics, MMSystem, Forms,
  GR32, GR32_Image, GR32_Layers,
  UFastStrings,
  UMisc, Dialogs,
  LemCore, LemTypes, LemStrings, LemDosStructures, LemRendering, LemLevel, LemDosStyle,
  LemDosGraphicSet,
  SysUtils,
  GameControl, GameBaseScreen, GamePreviewScreen, GameLevelCodeScreen;

type
  {-------------------------------------------------------------------------------
    these are the images we need for the menuscreen.
  -------------------------------------------------------------------------------}
  TGameMenuBitmap = (
    gmbLogo,
    gmbPlay,         // 1st row, 1st button
    gmbLevelCode,    // 1st row, 2nd button
    gmbMusic,        // 1st row, 3d button
    gmbSection,      // 1st row, 4th button
    gmbExit,         // 2nd row, 1st button
    gmbNavigation,   // 2nd row, 2nd button
    gmbMusicNote,    // drawn in gmbMusic
    gmbFXSound,      // drawn in gmbMusic
    gmbGameSection1, // mayhem/havoc    drawn in gmbSection
    gmbGameSection2, // taxing/wicked   drawn in gmbSection
    gmbGameSection3, // tricky/wild     drawn in gmbSection
    gmbGameSection4, // fun/crazy       drawn in gmbSection
    gmbGameSection5, // .../tame        drawn in gmbSection
    gmbGameSection6,  // remake only
    gmbGameSection7,
    gmbGameSection8,
    gmbGameSection9,
    gmbGameSection10,
    gmbGameSection11,
    gmbGameSection12,
    gmbGameSection13,
    gmbGameSection14,
    gmbGameSection15
  );

const
  {-------------------------------------------------------------------------------
    Positions at which the images of the menuscreen are drawn
  -------------------------------------------------------------------------------}
  GameMenuBitmapPositions: array[TGameMenuBitmap] of TPoint = (
    (X:8;    Y:10),                   // gmbLogo
    (X:72;   Y:120),                  // gmbPlay
    (X:200;  Y:120),                  // gmbLevelCode
    (X:328;  Y:120),                  // gmbMusic
    (X:456;  Y:120),                  // gmbSection
    (X:200;  Y:196),                  // gmbExit
    (X:328;  Y:196),                  // gmbNavigation
    (X:328 + 27;    Y:120 + 26),      // gmbMusicNote
    (X:328 + 27;    Y:120 + 26),      // gmbFXSign,
    (X:456 + 32;    Y:120 + 24),      // gmbSection1
    (X:456 + 32;    Y:120 + 24),      // gmbSection2
    (X:456 + 32;    Y:120 + 24),      // gmbSection3
    (X:456 + 32;    Y:120 + 24),      // gmbSection4
    (X:456 + 32;    Y:120 + 24),      // gmbSection5
    (X:456 + 32;    Y:120 + 24),       // gmbSection6
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24),
    (X:456 + 32;    Y:120 + 24)
  );

  YPos_ProgramText = 272;
  YPos_Credits = 350 - 16;

  Reel_Width = 34 * 16;
  Reel_Height = 16;

  Font_Width = 16;


type
  TStringAnimationInfo = record
  end;

  TGameMenuScreen = class(TGameBaseScreen)
  private
  { enumerated menu bitmap elements }
    BitmapElements : array[TGameMenuBitmap] of TBitmap32;
  { music }
//    MusicSetting   : TGameMusicSetting;
  { section }
    CurrentSection : Integer; // game section
    LastSection    : Integer; // last game section
  { credits }
    LeftLemmingAnimation   : TBitmap32;
    RightLemmingAnimation  : TBitmap32;
    Reel                   : TBitmap32;
    ReelBuffer             : TBitmap32;
    CanAnimate             : Boolean;
  { credits animation counters }
    FrameTimeMS            : Cardinal;
    PrevTime               : Cardinal;
    ReadingPauseMS         : Cardinal; //
    ReelLetterBoxCount     : Integer; // the number of letterboxes on the reel (34)
    Pausing                : Boolean;
    UserPausing            : Boolean;
    PausingDone            : Boolean; // the current text has been paused
//    Credits                : TStringList;
//    LastCredit             : Integer;
//    CompleteCreditString   : string;
    CreditList             : TStringList;
    CreditIndex            : Integer;
    CreditString          : string;
    TextX           : Integer;
    TextPauseX      : Integer; // if -1 then no pause
    TextGoneX       : Integer;

    CurrentFrame           : Integer;
//    PausePosition          : Integer;
    ReelShift              : Integer;
  { internal }
    procedure DrawBitmapElement(aElement: TGameMenuBitmap);
    procedure SetSoundOptions(aOptions: TGameSoundOptions);
    procedure SetSection(aSection: Integer);
    procedure NextSection(Forwards: Boolean);
    procedure NextSoundSetting;
    procedure DrawWorkerLemmings(aFrame: Integer);
    procedure DrawReel;//(aReelShift, aTextX: Integer);
    procedure SetNextCredit;
    procedure DumpLevels;
    procedure DumpImages;
  { eventhandlers }
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Application_Idle(Sender: TObject; var Done: Boolean);
    function BuildText(intxt: Array of char): String;
  protected
  { overrides }
    procedure PrepareGameParams(Params: TDosGameParams); override;
    procedure BuildScreen; override;
    procedure CloseScreen(aNextScreen: TGameScreenType); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TGameMenuScreen }

procedure TGameMenuScreen.DrawBitmapElement(aElement: TGameMenuBitmap);
{-------------------------------------------------------------------------------
  Draw bitmap at appropriate place on the screen.
-------------------------------------------------------------------------------}
var
  P: TPoint;
begin
  P := GameMenuBitmapPositions[aElement];
  BitmapElements[aElement].DrawTo(ScreenImg.Bitmap, P.X, P.Y);
end;

procedure TGameMenuScreen.BuildScreen;
{-------------------------------------------------------------------------------
  extract bitmaps from the lemmingsdata and draw
-------------------------------------------------------------------------------}
var
  Mainpal: TArrayOfColor32;
  Tmp: TBitmap32;
  i: Integer;
begin
(*  stretched := false;
//  screenimg.ScaleMode := smscale;
  screenimg.BitmapAlign := baCustom;
  screenimg.ScaleMode := smScale;
  screenimg.scale := 1.5; *)

  Tmp := TBitmap32.Create;
  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;
    InitializeImageSizeAndPosition(640, 350);
    ExtractBackGround;
    ExtractPurpleFont;

    MainDatExtractor.ExtractBitmap(BitmapElements[gmbLogo], 3, $2080, 632, 94, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbPlay], 3, $9488, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbLevelCode], 3, $A2D4, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbMusic], 3, $B120, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbSection], 3, $BF6C, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbExit], 3, $CDB8, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbNavigation], 3, $DC04, 120, 61, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbMusicNote], 3, $EA50, 64, 31, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbFXSound], 3, $EE30, 64, 31, 4, MainPal);

    MainDatExtractor.ExtractAnimation(LeftLemmingAnimation, 4, $2A00, 48, 16, 16, 4, MainPal);
    MainDatExtractor.ExtractAnimation(RightLemmingAnimation, 4, $4200, 48, 16, 16, 4, MainPal);

    //@styledef
    {$ifdef 15rank}
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection1], 5, 0 + (972 * 0), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection2], 5, 0 + (972 * 1), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection3], 5, 0 + (972 * 2), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection4], 5, 0 + (972 * 3), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection5], 5, 0 + (972 * 4), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection6], 5, 0 + (972 * 5), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection7], 5, 0 + (972 * 6), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection8], 5, 0 + (972 * 7), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection9], 5, 0 + (972 * 8), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection10], 5, 0 + (972 * 9), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection11], 5, 0 + (972 * 10), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection12], 5, 0 + (972 * 11), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection13], 5, 0 + (972 * 12), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection14], 5, 0 + (972 * 13), 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection15], 5, 0 + (972 * 14), 72, 27, 4, MainPal);
    {$else}

    //@styledef
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection1], 4, $5A80, 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection2], 4, $5E4C, 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection3], 4, $6218, 72, 27, 4, MainPal);
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection4], 4, $65E4, 72, 27, 4, MainPal);

    {$ifndef fourrank}
    MainDatExtractor.ExtractBitmap(BitmapElements[gmbGameSection5], 4, $65E4 + 972, 72, 27, 4, MainPal);
    //BitmapElements[gmbGameSection5].LoadFromFile('d:\d7\lem\data\remake\easy.bmp');
    {$endif}

    {$endif}


    // reel
    MainDatExtractor.ExtractBitmap(Tmp, 4, $5A00, 16, 16, 4, MainPal);

    // a little oversize
    Reel.SetSize(ReelLetterBoxCount * 16 + 32, 16);
    for i := 0 to ReelLetterBoxCount - 1 + 4 do
      Tmp.DrawTo(Reel, i * 16, 0);

    // make sure the reelbuffer is the right size
    ReelBuffer.SetSize(ReelLetterBoxCount * 16, 16);

    // background
    TileBackgroundBitmap(0, 0);
    BackBuffer.Assign(ScreenImg.Bitmap); // save it

    // menu elements
    DrawBitmapElement(gmbLogo);
    DrawBitmapElement(gmbPlay);
    DrawBitmapElement(gmbLevelCode);
    DrawBitmapElement(gmbMusic);
    DrawBitmapElement(gmbSection);
    DrawBitmapElement(gmbExit);
    DrawBitmapElement(gmbNavigation);

    // program text
    DrawPurpleTextCentered(ScreenImg.Bitmap, {$ifdef flexi}BuildText(GameParams.SysDat.PackName) + #13 + BuildText(GameParams.SysDat.SecondLine){$else}SProgramText{$endif}, YPos_ProgramText);

    // credits animation
    DrawWorkerLemmings(0);
    DrawReel;//(0, TextX);

    // a bit weird place, but here we know the bitmaps are loaded
    SetSection(CurrentSection);
    SetSoundOptions(GameParams.SoundOptions);

    CanAnimate := True;
//    ScreenImg.Bitmap.ResamplerClassName := 'TDraftResampler';
  finally
    ScreenImg.EndUpdate;
    Tmp.Free;
  end;
end;

constructor TGameMenuScreen.Create(aOwner: TComponent);
var
  E: TGameMenuBitmap;
  Bmp: TBitmap32;
begin
  inherited Create(aOwner);

  CurrentSection := 0;

  // create bitmaps
  for E := Low(TGameMenuBitmap) to High(TGameMenuBitmap) do
  begin
    Bmp := TBitmap32.Create;
    BitmapElements[E] := Bmp;
    if not (E in [gmbMusicNote, gmbFXSound, gmbGameSection1, gmbGameSection2, gmbGameSection3, gmbGameSection4,
      gmbGameSection5, gmbGameSection6, gmbGameSection7, gmbGameSection8, gmbGameSection9, gmbGameSection10,
      gmbGameSection11, gmbGameSection12, gmbGameSection13, gmbGameSection14, gmbGameSection15])
    then Bmp.DrawMode := dmTransparent;
  end;

  LeftLemmingAnimation := TBitmap32.Create;
  LeftLemmingAnimation.DrawMode := dmTransparent;

  RightLemmingAnimation := TBitmap32.Create;
  RightLemmingAnimation.DrawMode := dmTransparent;

  Reel := TBitmap32.Create;
  ReelBuffer := TBitmap32.Create;
  CreditList := TStringList.Create;

//  CanAnimate             : Boolean;
//  Credits := TStringList.Create;
  FrameTimeMS := 32;
  ReadingPauseMS := 1000;
//  Credits.Text := SCredits;
//  LastCredit := Credits.Count - 1;
//  CompleteCreditString := FastReplace(SCredits, Chr(13), Chr(10));
  CreditList.Text := {$ifdef flexi}''{$else}
                     SCredits{$endif};//CompleteCreditString;
  CreditIndex := -1;
  ReelLetterBoxCount := 34;
  SetNextCredit;
  {$ifdef flexi}CreditIndex := -1;{$endif}
  //DisplayString := CreditList[0];
//  windlg(currentcreditstring);
//  CurrentCredit := 0;

//  ScreenImg.RepaintMode := rmOptimizer;

  // set eventhandlers
  OnKeyDown := Form_KeyDown;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
  Application.OnIdle := Application_Idle;
end;

destructor TGameMenuScreen.Destroy;
var
  E: TGameMenuBitmap;
begin
  for E := Low(TGameMenuBitmap) to High(TGameMenuBitmap) do
    BitmapElements[E].Free;

  LeftLemmingAnimation.Free;
  RightLemmingAnimation.Free;
  Reel.Free;
  ReelBuffer.Free;
  CreditList.Free;

  inherited Destroy;
end;

procedure TGameMenuScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN : CloseScreen(gstPreview);
      VK_F1     : CloseScreen(gstPreview);
      VK_F2     : CloseScreen(gstLevelCode);
      VK_F3     : NextSoundSetting;
      VK_F4     : CloseScreen(gstNavigation);
      VK_F5     : DumpLevels;
      VK_F6     : DumpImages;
      VK_ESCAPE : CloseScreen(gstExit);
      VK_UP     : NextSection(True);
      VK_DOWN   : NextSection(False);
      VK_SPACE  : UserPausing := not UserPausing;
    end;
  end;
end;

procedure TGameMenuScreen.DumpImages;
var
  I: Integer;
begin
  {$ifdef flexi}if GameParams.SysDat.Options3 and 4 = 0 then Exit;{$endif}
  I := MessageDlg('Dump all level images? Warning: This is very slow!', mtCustom, [mbYes, mbNo], 0);
  if I = mrYes then
  begin
    MessageDlg('The screen will go blank while dumping level images.' + CrLf + 'This is normal.', mtCustom, [mbOk], 0);
    GameParams.DumpMode := true;
    GameParams.WhichLevel := wlFirst;
    CloseScreen(gstPreview);
  end;
end;

procedure TGameMenuScreen.DumpLevels;
var
  I: Integer;
begin
  {$ifdef flexi}if GameParams.SysDat.Options3 and 4 = 0 then Exit;{$endif}
  if not (GameParams.Style.LevelSystem is TBaseDosLevelSystem) then exit;
  I := MessageDlg('Dump all level files? Warning: This may overwrite' + CrLf + 'LVL files currently present!', mtCustom, [mbYes, mbNo], 0);
  if I = mrYes then
    TBaseDosLevelSystem(GameParams.Style.LevelSystem).DumpAllLevels;
end;

procedure TGameMenuScreen.Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseScreen(gstPreview);
end;

procedure TGameMenuScreen.Img_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Button = mbLeft then
    CloseScreen(gstPreview);
end;

procedure TGameMenuScreen.NextSection(Forwards: Boolean);

    procedure Change;
    begin
      with GameParams.Info do
      begin
        dValid := True;
        dPack := 0;
        dSection := CurrentSection;
        dLevel := 0;
        GameParams.WhichLevel := wlFirstSection;
      end;
    end;

begin
  case Forwards of
    False:
      begin
        if CurrentSection > 0 then
        begin
          SetSection(CurrentSection - 1);
          Change;
        end
        else
          Exit;
      end;
    True:
      begin
        if CurrentSection < LastSection then
        begin
          SetSection(CurrentSection + 1);
          Change;
        end
        else
          Exit;
//          CurrentSection := LastSection;//0;
      end;
  end;
end;

procedure TGameMenuScreen.NextSoundSetting;
var
  Opt: TGameSoundOptions;
begin
  Opt := GameParams.SoundOptions;
  if Opt = [] then Include(Opt, gsoSound)
  else if Opt = [gsoSound] then Include(Opt, gsoMusic)
  else if Opt = [gsoMusic, gsoSound] then Opt := [];
  SetSoundOptions(Opt);
end;


function TGameMenuScreen.BuildText(intxt: Array of char): String;
var
  tstr : String;
  x : byte;
begin
  tstr := '';
  for x := 0 to 35 do
  begin
    if (tstr <> '') or (intxt[x] <> ' ') then
    begin
      tstr := tstr + intxt[x];
    end;
  end;
  Result := trim(tstr);
end;



procedure TGameMenuScreen.PrepareGameParams(Params: TDosGameParams);
var
  i: integer;
  k: string;
begin
  inherited PrepareGameParams(Params);
  CurrentSection := Params.Info.dSection;
  LastSection := (Params.Style.LevelSystem as TBaseDosLevelSystem).GetSectionCount - 1;
  {$ifdef flexi}
  CreditList.Text := BuildText(Params.SysDat.PackName) + #13;
  for i := 0 to 15 do
  begin
    k := BuildText(Params.SysDat.ScrollerTexts[i]);
    if Trim(k) <> '' then CreditList.Text := CreditList.Text + Trim(k) + #13;
  end;
  CreditList.Text := CreditList.Text + SCredits;
  SetNextCredit;
  {$endif}
  // fun, tricky, taxing, mayhem
end;

procedure TGameMenuScreen.SetSoundOptions(aOptions: TGameSoundOptions);
var
  Opt: TGameSoundOptions;
begin
  GameParams.SoundOptions := aOptions;
  Opt := GameParams.SoundOptions;
  if Opt = [] then DrawBitmapElement(gmbMusic)
  else if Opt = [gsoSound] then DrawBitmapElement(gmbFXSound)
  else if Opt = [gsoMusic, gsoSound] then DrawBitmapElement(gmbMusicNote);
end;

procedure TGameMenuScreen.SetSection(aSection: Integer);
begin
  CurrentSection := aSection;
  //@styledef
{$ifdef 15rank}
  case CurrentSection of
    0: DrawBitmapElement(gmbGameSection1);
    1: DrawBitmapElement(gmbGameSection2);
    2: DrawBitmapElement(gmbGameSection3);
    3: DrawBitmapElement(gmbGameSection4);
    4: DrawBitmapElement(gmbGameSection5);
    5: DrawBitmapElement(gmbGameSection6);
    6: DrawBitmapElement(gmbGameSection7);
    7: DrawBitmapElement(gmbGameSection8);
    8: DrawBitmapElement(gmbGameSection9);
    9: DrawBitmapElement(gmbGameSection10);
    10: DrawBitmapElement(gmbGameSection11);
    11: DrawBitmapElement(gmbGameSection12);
    12: DrawBitmapElement(gmbGameSection13);
    13: DrawBitmapElement(gmbGameSection14);
    14: DrawBitmapElement(gmbGameSection15);
  end;
{$else}
  {$ifdef fourrank}
  case CurrentSection of
    0: DrawBitmapElement(gmbGameSection4);
    1: DrawBitmapElement(gmbGameSection3);
    2: DrawBitmapElement(gmbGameSection2);
    3: DrawBitmapElement(gmbGameSection1);
  end;
  {$else}
  case CurrentSection of
    0: DrawBitmapElement(gmbGameSection5);
    1: DrawBitmapElement(gmbGameSection4);
    2: DrawBitmapElement(gmbGameSection3);
    3: DrawBitmapElement(gmbGameSection2);
    4: DrawBitmapElement(gmbGameSection1);
  end;
  {$endif}
{$endif}
end;

procedure TGameMenuScreen.DrawWorkerLemmings(aFrame: Integer);
var
  SrcRect, DstRect: TRect;
begin
  SrcRect := CalcFrameRect(LeftLemmingAnimation, 16, aFrame);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  OffsetRect(DstRect, 0, YPos_Credits);
  BackBuffer.DrawTo(ScreenImg.Bitmap, DstRect, DstRect);
  LeftLemmingAnimation.DrawTo(ScreenImg.Bitmap, DstRect, SrcRect);

//  SrcRect := CalcFrameRect(RightLemmingAnimation, 16, 0);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  OffsetRect(DstRect, 640 - 48, YPos_Credits);
  BackBuffer.DrawTo(ScreenImg.Bitmap, DstRect, DstRect);
  RightLemmingAnimation.DrawTo(ScreenImg.Bitmap, DstRect, SrcRect);
end;

procedure TGameMenuScreen.SetNextCredit;
var
  TextSize: Integer;
begin
  TextX := 33 * 16;

  if CreditList.Count = 0 then
  begin
    CreditString := '';
    Exit;
  end;

  Inc(CreditIndex);
  if CreditIndex > CreditList.Count - 1 then
    CreditIndex := 0;

  // set new string
  CreditString := CreditList[CreditIndex];
  Pausing := False;
  PausingDone := False;
  TextSize := Length(CreditString) * Font_Width;
  TextPauseX := (Reel_Width - TextSize) div 2;
  TextGoneX := -TextSize;// + 10 * Font_Width;
end;

procedure TGameMenuScreen.DrawReel;//(aReelShift, aTextX: Integer);
{-------------------------------------------------------------------------------
  Drawing of the moving credits. aShift = the reel shift which is wrapped every
  other 16 pixels to zero.
-------------------------------------------------------------------------------}
var
//  X: Integer;
  R: TRect;
//  S: string;
begin
//  X := 48;
  R := Reel.BoundsRect;
  R.Left := ReelShift;
  R.Right := R.Left + ReelLetterBoxCount * 16 ;


//  S := Copy(CurrentCreditString, aStringShift + 1, 50);
  // copy reel, draw text on it, draw to screen
//  ReelBuffer.Assign(Reel);


  Reel.DrawTo(ReelBuffer, ReelShift, 0);

(*
  DrawPurpleText(ReelBuffer,
//    Copy(CurrentCreditString, FirstChar, 50),
    '1234567890123456789012345678901234',
     aTextPosition, 0);//48, YPos_Credits);
*)

  DrawPurpleText(ReelBuffer, CreditString, TextX, 0);


//  DrawPurpleText(ReelBuffer, 'Scrolling credits...', 13 * 16, 0);
//  DrawPurpleText(ReelBuffer, 'Volker Oth, ccexplore and Mindless', 0, 0);


//  ReelBuffer.DrawTo(ScreenImg.Bitmap, 48, YPos_Credits, R);
  ReelBuffer.DrawTo(ScreenImg.Bitmap, 48, YPos_Credits);

//  ScreenImg.Update;

end;

procedure TGameMenuScreen.Application_Idle(Sender: TObject; var Done: Boolean);
{-------------------------------------------------------------------------------
  Animation of credits.
  - 34 characters fit into the reel.
  - text scolls from right to left. when one line is centered into the reel,
    scrolling is paused for a while.
-------------------------------------------------------------------------------}
var
  CurrTime: Cardinal;
begin
  if not CanAnimate or ScreenIsClosing then
    Exit;
  Sleep(1);
  Done := False;
  CurrTime := TimeGetTime;
  if UserPausing then
    Exit;

  { check end reading pause }
  if Pausing then
  begin
    if CurrTime > PrevTime + ReadingPauseMS then
    begin
      PrevTime := CurrTime;
      Pausing := False;
      PausingDone := True; // we only pause once per text
    end;
    Exit;
  end;

  { update frames }
  if CurrTime > PrevTime + FrameTimeMS then
  begin
    PrevTime := CurrTime;

    { workerlemmings animation has 16 frames }
    Inc(CurrentFrame);
    if CurrentFrame >= 15 then
      CurrentFrame := 0;

    { text + reel }
    Dec(ReelShift, 4);
    if ReelShift <= - 16 then
      ReelShift := 0;

    Dec(TextX, 4);
    if TextX < TextGoneX then
    begin
      SetNextCredit;
      //TextX := 33 * 16;
    end;

    if not PausingDone then
    begin
      // if text can be centered then pause if we are there
      if TextPauseX >= 0 then
        if TextX <= TextPauseX then
          Pausing := True;
    end;

    DrawWorkerLemmings(CurrentFrame);
    DrawReel;//(ReelShift, TextX);

  end;
end;

procedure TGameMenuScreen.CloseScreen(aNextScreen: TGameScreenType);
begin
  inherited CloseScreen(aNextScreen);
end;

end.

