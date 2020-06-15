{$include lem_directives.inc}

unit GameWindow;

interface

uses
  Windows, Classes, Controls, Graphics, MMSystem, Forms, SysUtils, Dialogs, Math, ExtCtrls,
  GR32, GR32_Image, GR32_Layers,
  UMisc, UTools,
  LemCore, LemLevel, LemDosStyle, LemGraphicSet, LemDosGraphicSet, LemRendering,
  LemGame,
  GameControl, GameSkillPanel, GameBaseScreen;

type
  TGameScroll = (
    gsNone,
    gsRight,
    gsLeft
  );

  TGameWindow = class(TGameBaseScreen)
  private
    fNeedReset: Boolean;
  { game eventhandler}
    procedure Game_Finished(Sender: TObject);
  { self eventhandlers }
    procedure Form_Activate(Sender: TObject);
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Form_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  { app eventhandlers }
    procedure Application_Idle(Sender: TObject; var Done: Boolean);
  { gameimage eventhandlers }
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Img_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Img_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  { skillpanel eventhandlers }
    procedure SkillPanel_MinimapClick(Sender: TObject; const P: TPoint);
  { internal }
    procedure CheckResetCursor;
    procedure CheckScroll;
    procedure AddSaveState;
    procedure NextSaveState(Forwards: Boolean);
    procedure GotoSaveState(aTargetIteration: Integer);
    procedure CheckAdjustReleaseRate;
    procedure LoadReplay;
    procedure SetAdjustedGameCursorPoint(BitmapPoint: TPoint);
    procedure StartReplay;
    procedure StartReplay2(const aFileName: string);
    procedure InitializeCursor;
    procedure SaveShot;
  protected
    fGame                : TLemmingGame;      // reference to globalgame gamemechanics
    Img                  : TImage32;          // the image in which the level is drawn (reference to inherited ScreenImg!)
    SkillPanel           : TSkillPanelToolbar;// our good old dos skill panel
    fActivateCount       : Integer;           // used when activating the form
    ForceUpdateOneFrame  : Boolean;           // used when paused
    GameScroll           : TGameScroll;       // scrollmode
    IdealFrameTimeMS     : Cardinal;          // normal frame speed in milliseconds
    IdealFrameTimeMSFast : Cardinal;          // fast forward framespeed in milliseconds
    IdealScrollTimeMS    : Cardinal;          // scroll speed in milliseconds
    PrevCallTime         : Cardinal;          // last time we did something in idle
    PrevScrollTime       : Cardinal;          // last time we scrolled in idle
    MouseClipRect        : TRect;             // we clip the mouse when there is more space
    CanPlay              : Boolean;           // use in idle en set to false whenever we don't want to play
    HCursor1             : HCURSOR;           // normal play cursor
    HCursor2             : HCURSOR;           // highlight play cursor
    LemCursorIconInfo    : TIconInfo;         // normal play cursor icon
    LemSelCursorIconInfo : TIconInfo;         // highlight play cursor icon
    MaxDisplayScale      : Integer;           // calculated in constructor
    DisplayScale         : Integer;           // what's the zoomfactor (mostly 2, 3 or 4)
    MinScroll            : Single;            // scroll boundary for image
    MaxScroll            : Single;            // scroll boundary for image
    fSaveStates          : TIntegerList;      // list of savestates (only first is used)
    fSaveStateIndex      : Integer;           // index of current savestate (not used)
    fLastNukeKeyTime     : Cardinal;
    fScrollSpeed         : Integer;
//    fSystemCursor        : Boolean;
  //  fMouseBmp            : TBitmap32;
//    fMouseLayer          : TBitmapLayer;
  { overridden}
    procedure PrepareGameParams(Params: TDosGameParams); override;
    procedure CloseScreen(aNextScreen: TGameScreenType); override;
  { internal properties }
    property Game: TLemmingGame read fGame;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses FBaseDosForm;

{ TGameControllerForm }

procedure TGameWindow.Application_Idle(Sender: TObject; var Done: Boolean);
{-------------------------------------------------------------------------------
  • Main heartbeat of the program.
  • This method together with Game.UpdateLemmings() take care of most game-mechanics.
  • A bit problematic is the releaserate handling:
    if the game is paused it RR is handled here. if not it is handled by
    Game.UpdateLemmings().
-------------------------------------------------------------------------------}
var
  CurrTime: Cardinal;
  Fast, ForceOne, TimeForFrame, TimeForFastForwardFrame, TimeForScroll, Hyper, Pause: Boolean;
begin
  if not CanPlay or not Game.Playing or Game.GameFinished then
    Exit;

  // this makes sure this method is called very often :)
  Done := False;


  Pause := (Game.Paused) and (Game.CurrentIteration > 33);
  Fast := Game.FastForward;
  ForceOne := ForceUpdateOneFrame;
  ForceUpdateOneFrame := False;
  CurrTime := TimeGetTime;
  TimeForFrame := CurrTime - PrevCallTime > IdealFrameTimeMS;
  TimeForFastForwardFrame := Fast and (CurrTime - PrevCallTime > IdealFrameTimeMSFast);
  TimeForScroll := CurrTime - PrevScrollTime > IdealScrollTimeMS;
  Hyper := Game.HyperSpeed;

  // relax CPU
  if not Hyper or Fast then
    Sleep(1);

  if ForceOne or Hyper or TimeForFastForwardFrame or TimeForFrame or TimeForScroll then
  begin
  //  PrevCallTime := CurrTime; --> line deleted and moved down

    // only in paused mode adjust RR. If not paused it's updated per frame.
    if Game.Paused then
      if (TimeForScroll and not Game.Replaying) or ForceOne then
        CheckAdjustReleaseRate;

    if TimeForScroll then
    begin
      PrevScrollTime := CurrTime;
      CheckScroll;
    end;

    //if ForceOne or not Game.Paused then THIS IS ORIGINAL BUT MAYBE WRONG
    if (TimeForFrame and not Pause)
    or (TimeForFastForwardFrame and not Pause)
    or (Forceone and Pause)
    or Hyper then
    begin
      PrevCallTime := CurrTime;
      Game.UpdateLemmings;
    end;

    if not Hyper then
    begin
      SkillPanel.RefreshInfo;
      SkillPanel.DrawMinimap(Game.MinimapBuffer);
      CheckResetCursor;
    end
    else begin
      if Game.CurrentIteration >= Game.TargetIteration then
      begin
        Game.HyperSpeedEnd;
        SkillPanel.RefreshInfo;
        SkillPanel.DrawMinimap(Game.MinimapBuffer);
        CheckResetCursor;
      end;
    end;

  end;
end;

procedure TGameWindow.GotoSaveState(aTargetIteration: Integer);
{-------------------------------------------------------------------------------
  Go in hyperspeed from the beginning to aTargetIteration
-------------------------------------------------------------------------------}
var
  CurrentlyPaused: Boolean;
  ReplayGlitchFrames: Integer;
begin
  CanPlay := False;
  CurrentlyPaused := Game.Paused;
  ReplayGlitchFrames := Game.ReplayGlitchIterations;
  Game.Start(True);
  Game.ReplayGlitchIterations := ReplayGlitchFrames;
  Game.HyperSpeedBegin(CurrentlyPaused);
  Game.TargetIteration := aTargetIteration;
  SkillPanel.RefreshInfo;
  CanPlay := True;
end;

procedure TGameWindow.CheckResetCursor;
begin
  if FindControl(GetForegroundWindow()) = nil then
  begin
    fNeedReset := true;
    exit;
  end;
  if Screen.Cursor <> Game.CurrentCursor then
  begin
    Img.Cursor := Game.CurrentCursor;
    Screen.Cursor := Game.CurrentCursor;
  end;
  if fNeedReset then
  begin
    ClipCursor(@MouseClipRect);
    fNeedReset := false;
  end;
  {if ** need proper clip check here**
  begin
    ClipCursor(@MouseClipRect);
  end;}
end;

procedure TGameWindow.CheckScroll;
begin
  case GameScroll of
    gsRight:
      begin
      //if Mouse.
      Img.OffsetHorz := Max(MinScroll * DisplayScale, Img.OffSetHorz - DisplayScale * 8 * fScrollSpeed);
      end;
(*      if Img.OffSetHorz > MinScroll * DisplayScale then
      begin
        Img.OffSetHorz := Img.OffSetHorz - DisplayScale * 8;

      end; *)
    gsLeft:
      begin
      Img.OffsetHorz := Min(MaxScroll * DisplayScale, Img.OffSetHorz + DisplayScale * 8 * fScrollSpeed);
      end;
      (*
      if Img.OffSetHorz < MaxScroll * DisplayScale then
      begin
        Img.OffSetHorz := Img.OffSetHorz + DisplayScale * 8;
      end;
      *)
  end;
end;

constructor TGameWindow.Create(aOwner: TComponent);
var
  HScale, VScale: Integer;
begin
  inherited Create(aOwner);

  // create game
  fGame := GlobalGame; // set ref to GlobalGame
  fGame.OnFinish := Game_Finished;
  fScrollSpeed := 1;

  fSaveStates := TIntegerList.Create;

  Img := ScreenImg; // set ref to inherited screenimg (just for a short name)
  Img.RepaintMode := rmOptimizer;
  Img.Color := clNone;
  Img.BitmapAlign := baCustom;
  Img.ScaleMode := smScale;

//  fMouseBmp := TBitmap32.create;

  // create toolbar
  SkillPanel := TSkillPanelToolbar.Create(Self);
  SkillPanel.Parent := Self;

//  IdealFrameTimeMS := 60;
//  IdealFrameTimeMSFast := 0;
//  IdealScrollTimeMS := 60;

  // calculate displayscale
  HScale := Screen.Width div 320;
  VScale := Screen.Height div 200;
  DisplayScale := HScale;
  if VScale < HScale then
    DisplayScale := VScale;
  MaxDisplayScale := DisplayScale;

  Self.KeyPreview := True;

  // set eventhandlers
  Self.OnActivate := Form_Activate;
  Self.OnKeyDown := Form_KeyDown;
  Self.OnKeyUp := Form_KeyUp;
  Self.OnKeyPress := Form_KeyPress;
  Self.OnMouseMove := Form_MouseMove;
  Self.OnMouseUp := Form_MouseUp;

  Img.OnMouseDown := Img_MouseDown;
  Img.OnMouseMove := Img_MouseMove;
  Img.OnMouseUp := Img_MouseUp;

  SkillPanel.Game := fGame; // this links the game to the infopainter interface as well
  SkillPanel.OnMinimapClick := SkillPanel_MinimapClick;
  Application.OnIdle := Application_Idle;



end;

destructor TGameWindow.Destroy;
begin
  CanPlay := False;
  Application.OnIdle := nil;
  fSaveStates.Free;
  if SkillPanel <> nil then
    SkillPanel.Game := nil;
  if HCursor1 <> 0 then
    DestroyIcon(HCursor1);
  if HCursor2 <> 0 then
    DestroyIcon(HCursor2);
//  if fMouseBmp <> nil then
  //  fMouseBmp.Free;
  inherited Destroy;
end;

procedure TGameWindow.Form_Activate(Sender: TObject);
// activation eventhandler
begin
  if fActivateCount = 0 then
  begin
    fGame.Start;
    CanPlay := True;
  end;
  Inc(fActivateCount)
end;

procedure TGameWindow.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CurrTime: Cardinal;
begin
  if Key = VK_ESCAPE then
  begin
    Game.Finish; // OnFinish eventhandler does the rest
  end;

  if not Game.Playing then
    Exit;

  // this is quite important: no gamecontrol if going fast
  if (Key <> VK_F11) and (Game.HyperSpeed or Game.FastForward) then
     Exit;

  with Game do
  begin
    if Shift = [] then
    begin
      if ((Key >= VK_F1) and (Key <= VK_F12)) or ((Key >= 48) and (Key <= 56)) or (Key = 187) or (Key = 189) then
      begin
        if (Key <> VK_F11) and (Key <> 48) then
          Game.RegainControl;
        case Key of
          VK_RETURN:;
          VK_F1: SetSelectedSkill(spbSlower, True);
          VK_F2: SetSelectedSkill(spbFaster, True);
          VK_F3: SetSelectedSkill(spbClimber, True);
          VK_F4: SetSelectedSkill(spbUmbrella, True);
          VK_F5: SetSelectedSkill(spbExplode, True);
          VK_F6: SetSelectedSkill(spbBlocker, True);
          VK_F7: SetSelectedSkill(spbBuilder, True);
          VK_F8: SetSelectedSkill(spbBasher, True);
          VK_F9: SetSelectedSkill(spbMiner, True);
          VK_F10: SetSelectedSkill(spbDigger, True);
          VK_F11: SetSelectedSkill(spbPause);
          189: SetSelectedSkill(spbSlower, True); // minus button
          187: SetSelectedSkill(spbFaster, True); // plus button
          // 48 through 56 correspond to the '0' through '8' keys
          48: SetSelectedSkill(spbPause);
          49: SetSelectedSkill(spbClimber, True);
          50: SetSelectedSkill(spbUmbrella, True);
          51: SetSelectedSkill(spbExplode, True);
          52: SetSelectedSkill(spbBlocker, True);
          53: SetSelectedSkill(spbBuilder, True);
          54: SetSelectedSkill(spbBasher, True);
          55: SetSelectedSkill(spbMiner, True);
          56: SetSelectedSkill(spbDigger, True);
          VK_F12: begin
                    // double keypress needed to prevent accidently nuking
                    CurrTime := TimeGetTime;
                    if CurrTime - fLastNukeKeyTime < 250 then
                      SetSelectedSkill(spbNuke)
                    else
                      fLastNukeKeyTime := CurrTime;
                  end;
        end;
      end
      // other keys
      else begin
        case Key of
          VK_RETURN : AddSaveState;
          VK_BACK   : NextSaveState(False);
          VK_LEFT   : GameScroll := gsLeft;
          VK_RIGHT  : GameScroll := gsRight;
        end;
      end;
    end;
  end;
end;

procedure TGameWindow.Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  if not Game.Playing then
    Exit;

  with Game do
  begin

    case Key of
      VK_F1    : SetSelectedSkill(spbSlower, False);
      VK_F2    : SetSelectedSkill(spbFaster, False);
      189      : SetSelectedSkill(spbSlower, False); // minus button
      187      : SetSelectedSkill(spbFaster, False); // plus button
      VK_LEFT  : GameScroll := gsNone;
      VK_RIGHT : GameScroll := gsNone;
    end;
  end;

end;

procedure TGameWindow.SetAdjustedGameCursorPoint(BitmapPoint: TPoint);
{-------------------------------------------------------------------------------
  convert the normal hotspot to the hotspot the game uses (4,9 instead of 7,7)
-------------------------------------------------------------------------------}
begin
  Game.CursorPoint := Point(BitmapPoint.X - 3, BitmapPoint.Y + 2);
end;

procedure TGameWindow.Img_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
{-------------------------------------------------------------------------------
  mouse handling of the game
-------------------------------------------------------------------------------}

var
  HandleClick: Boolean;
begin
  // interrupting hyperspeed can break the handling of savestates
  // so we're not allowing it
  if Game.Playing and not Game.HyperSpeed then
  begin
    Game.RegainControl;
    //Game.CursorPoint :=
    SetAdjustedGameCursorPoint(Img.ControlToBitmap(Point(X, Y)));
    //Game.CursorPoint := Img.ControlToBitmap(Point(X, Y));

    //umisc
    // debug creation of lemming
    (*
    if LemmixDebugMode then
    if ssCtrl in Shift then
    if ssAlt in Shift then
    begin
      Game.CreateLemmingAtCursorPoint;//lemgame
      Exit;
    end;
    *)

    // normal
    Game.RightMouseButtonHeldDown := ssRight in Shift;
    if Button = mbLeft then
    begin
      HandleClick := {not Game.Paused and} not Game.FastForward{ or UseClicksWhenPaused};
      if HandleClick then
      begin
        Game.ProcessSkillAssignment;
        if Game.Paused then
            ForceUpdateOneFrame := True;
      end;
    end;
  end;
end;

procedure TGameWindow.Img_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Game.Playing then
  begin
    Game.RightMouseButtonHeldDown := ssRight in Shift;
(*    if ssRight in Shift then
      fScrollSpeed := 2
    else
      fScrollSpeed := 1; *)

    SetAdjustedGameCursorPoint(Img.ControlToBitmap(Point(X, Y)));
    //Game.CursorPoint := Img.ControlToBitmap(Point(X, Y));

    if Game.Paused then
      Game.HitTest; // maybe move to idle

    if X >= Img.Width - 1 then
      GameScroll := gsRight
    else if X <= 0 then
      GameScroll := gsLeft
    else
      GameScroll := gsNone;
  end;

  Game.HitTestAutoFail := Y >= SkillPanel.Top;

end;

procedure TGameWindow.Img_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  Game.RightMouseButtonHeldDown := ssRight in Shift;
end;

procedure TGameWindow.InitializeCursor;
const
  PLAYCURSOR_DEFAULT = 1;
  PLAYCURSOR_LEMMING = 2;
var
  bmpMask : TBitmap;
  bmpColor : TBitmap;

    procedure scalebmp(bmp:tbitmap; ascale:integer);
    var                         //bad code but it works for now
      b: tbitmap32;
      src,dst:trect;

    begin
      if ascale=1 then exit;
      b:=tbitmap32.create;
      src:=rect(0,0,bmp.width,bmp.height);
      dst:=rect(0,0,bmp.width * ascale, bmp.height*ascale);
      b.setsize(bmp.width*ascale, bmp.height*ascale);
      b.Draw(dst,src, bmp.canvas.handle);
      bmp.Width := b.width;
      bmp.height:=b.height;
      b.drawto(bmp.canvas.handle, 0, 0);// gr32
      b.free;
    end;


begin
  bmpMask := TBitmap.Create;
  bmpColor := TBitmap.Create;

  //bmpMask.LoadFromFile(apppath + 'dosgamecursormask1.bmp');
  //bmpColor.LoadFromFile(apppath+'dosgamecursor1.bmp');
  bmpMask.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_DEFAULT_MASK');
  bmpColor.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_DEFAULT');
//  bmpcolor.canvas.pixels[3,8]:=clred;

  ScaleBmp(bmpMask, DisplayScale);
  ScaleBmp(bmpColor, DisplayScale);

  (*
  if not fSystemCursor then
  begin
    fMouseBmp.Assign(bmpColor);
    fMouseBmp.DrawMode := dmTransparent;
    fMouseLayer := TBitmapLayer.Create(Img.Layers);
    fMouseLayer.LayerOptions := LOB_VISIBLE;
    fMouseLayer.Location := FloatRect(0, 0, fMouseBmp.Width, fMouseBmp.Height)
  end;     *)

  with LemCursorIconInfo do
  begin
    fIcon := false;
    xHotspot := 7 * DisplayScale; //4 * displayscale;//7 * DisplayScale;//bmpmask.width div 2+displayscale;
    yHotspot := 7 * DisplayScale; //9 * displayscale;//7 * DisplayScale;//bmpmask.Height div 2+displayscale;
    hbmMask := bmpMask.Handle;
    hbmColor := bmpColor.Handle;
  end;

  HCursor1 := CreateIconIndirect(LemCursorIconInfo);
  Screen.Cursors[PLAYCURSOR_DEFAULT] := HCursor1;

  img.Cursor := PLAYCURSOR_DEFAULT;
  SkillPanel.img.cursor := PLAYCURSOR_DEFAULT;
  Self.Cursor := PLAYCURSOR_DEFAULT;

  bmpMask.Free;
  bmpColor.Free;

  //////////

  bmpMask := TBitmap.Create;
  bmpColor := TBitmap.Create;

//  bmpMask.LoadFromFile(apppath + 'dosgamecursormask2.bmp');
//  bmpColor.LoadFromFile(apppath+'dosgamecursor2.bmp');
  bmpMask.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_HIGHLIGHT_MASK');
  bmpColor.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_HIGHLIGHT');

  scalebmp(bmpmask, DisplayScale);
  scalebmp(bmpcolor, DisplayScale);


  with LemSelCursorIconInfo do
  begin
    fIcon := false;
    xHotspot := 7 * DisplayScale; //4 * DisplayScale;//}bmpmask.width div 2+displayscale;
    yHotspot := 7 * DisplayScale; //9 * DisplayScale;//}bmpmask.Height div 2+displayscale;
//    xHotspot := 7 * 3;////5;
//    yHotspot := 7 * 3;//15;
    hbmMask := bmpMask.Handle;
    hbmColor := bmpColor.Handle;
  end;

  HCursor2 := CreateIconIndirect(LemSelCursorIconInfo);
  Screen.Cursors[PLAYCURSOR_LEMMING] := HCursor2;

//  Screen.Cursor := MyCursor;
//  Self.Cursor := HCursor1;

  bmpMask.Free;
  bmpColor.Free;
end;


procedure TGameWindow.PrepareGameParams(Params: TDosGameParams);
{-------------------------------------------------------------------------------
  This method is called by the inherited ShowScreen
-------------------------------------------------------------------------------}
var
  Sca: Integer;
  StoredScale: Integer; // scale as stored in ini-file
begin
  inherited;
  StoredScale := Params.ZoomFactor;
//  fSystemCursor := GameParams.UseSystemCursor;

  // set the final displayscale
  Sca := MaxDisplayScale;
  if (StoredScale > 0) and (StoredScale <= MaxDisplayScale) then
  begin
     Sca := StoredScale;
     DisplayScale := Sca;
  end;

  // correct if wrong zoomfactor in inifile
  if (StoredScale <> 0) and (StoredScale > MaxDisplayScale) then
    Params.ZoomFactor := Sca;

  GameParams.TargetBitmap := Img.Bitmap;
  fGame.PrepareParams(Params);

  // set timers
  IdealFrameTimeMSFast := 10;
  IdealScrollTimeMS := 60;

  // check superlemming
  if fGame.Level.Info.SuperLemming then
    IdealFrameTimeMS := 20
  else
    IdealFrameTimeMS := 60;

  Img.Width := 320 * Sca;
  Img.Height := 160 * Sca;
  Img.Scale := Sca;
  Img.OffsetHorz := -GameParams.Level.Info.ScreenPosition * Sca;
  Img.Left := (Screen.Width - Img.Width) div 2;
  Img.Top := (Screen.Height - 200 * Sca) div 2;

  SkillPanel.Top := Img.Top + Img.Height;
  SkillPanel.left := Img.Left;
  SkillPanel.Width := Img.Width;
  SkillPanel.Height := 40 * Sca;

  MouseClipRect := Rect(Img.Left, Img.Top, Img.Left + Img.Width,
    SkillPanel.Top + SkillPanel.Height);

  SkillPanel.SetStyleAndGraph(Gameparams.Style, GameParams.GraphicSet, Sca);

  MinScroll := -(GAME_BMPWIDTH - 320);
  MaxScroll := 0;

  InitializeCursor;
  SetCursorPos(Screen.Width div 2, Screen.Height div 2);
  ClipCursor(@MouseClipRect);

end;

procedure TGameWindow.SkillPanel_MinimapClick(Sender: TObject; const P: TPoint);
{-------------------------------------------------------------------------------
  This method is an eventhandler (TSkillPanel.OnMiniMapClick),
  called when user clicks in the minimap-area of the skillpanel.
  Here we scroll the game-image.
-------------------------------------------------------------------------------}
var
//  N: TPoint;
  O: Single;
begin
//  Game.CurrentScreenPosition := Point(P.X, 0);
  O := -P.X * DisplayScale;
  O :=  O + Img.Width div 2;
  if O < MinScroll * DisplayScale then O := MinScroll * DisplayScale;
  if O > MaxScroll * DisplayScale then O := MaxScroll * DisplayScale;
  Img.OffSetHorz := O;
end;

procedure TGameWindow.Form_KeyPress(Sender: TObject; var Key: Char);
begin
  case UpCase(Key) of

    // --------- CHEAT ----------------
    'C':
      begin
        Game.Cheat; // lemgame
      end;

    // --------- LOWERCASE ------------

    // go 10 game seconds further
    ' ':
            if Game.Playing then
            begin
              if not Game.HyperSpeed then
              begin
                //windows.beep(448, 3);
                Game.HyperSpeedBegin;
                Game.TargetIteration := Game.CurrentIteration + 17 * 10;
              end
            end;

    // go back approximately 1 game second
    '[':
            if Game.Playing then
            begin
              if not Game.HyperSpeed then
              begin
                if (Game.CurrentIteration >= 17) then
                  GotoSaveState(Game.CurrentIteration - 17)
                else
                  GotoSaveState(0);
              end
            end;

    // go back 1 frame
    'B':
            if Game.Playing then
            begin
              if not Game.HyperSpeed then
              begin
                if (Game.CurrentIteration >= 2) then
                  GotoSaveState(Game.CurrentIteration - 2)
                else
                  GotoSaveState(0);
              end
            end;

    // test method ClipCursor
    {'c': ClipCursor(@MouseClipRect);}//TestClip;

    // toggle fastforward
    'F': if not Game.Paused then
           Game.FastForward := not Game.FastForward;

    '9': if not Game.Paused then
           Game.FastForward := not Game.FastForward;

    // hide/show lemming debug line
//    'h': if LemmixDebugMode then ToggleDebugLemming;

    // hide/show frame info debug line
//    'i': if LemmixDebugMode then ToggleFrameInfo;

    // hide/show lemming foot-pixel
//    'l': if LemmixDebugMode then Game.DrawLemmingPixel := not Game.DrawLemmingPixel;

    // save snapshot image
    'I': SaveShot;

    // load replay file
    'L': LoadReplay;

    // enable/disable music
    'M': begin
           if gsoMusic in Game.SoundOpts
           then Game.SoundOpts := Game.SoundOpts - [gsoMusic]
           else Game.SoundOpts := Game.SoundOpts + [gsoMusic];
           if gsoMusic in Game.SoundOpts
           then Game.SetSoundChannels(4)
           else Game.SetSoundChannels(7);
         end;

    // do next frame if paused
    'N': {if LemmixDebugMode then}
      if Game.Paused then
        //if Game.Replaying then
          ForceUpdateOneFrame := True;

     {'p':     ;}{
            if Game.Playing and Game.Paused then
            if Game.CurrentIteration > 0 then
            begin
              if not Game.HyperSpeed then
              begin
                GotoSaveState(Game.CurrentIteration - 1);

                //Game.SetSelectedSkill(spbPause, True);
//                Game.Paused := True;
                //windows.beep(448, 3);
//                Game.HyperSpeedBegin;
  //              Game.TargetIteration := Game.CurrentIteration - 1;
              end
            end;
            *)  }


    // disable/enable skill assignment when paused
//    'p': if LemmixDebugMode then UseClicksWhenPaused := not UseClicksWhenPaused;

    'P': Game.SetSelectedSkill(spbPause);

    // start replay
    'R': StartReplay;

    // enable/disable sounds
    'S': begin
           if gsoSound in Game.SoundOpts
           then Game.SoundOpts := Game.SoundOpts - [gsoSound]
           else  Game.SoundOpts := Game.SoundOpts + [gsoSound];
         end;

    // save current game to .\Replay\<leveltitle>.lrb
    // in addition a more or less readable version is saved too with the extension ".txt"
    'U': Game.Save;

    'Z': with Game do
         begin
         RegainControl;
         case SelectedSkill of
           spbUmbrella: SetSelectedSkill(spbClimber, True);
           spbExplode: SetSelectedSkill(spbUmbrella, True);
           spbBlocker: SetSelectedSkill(spbExplode, True);
           spbBuilder: SetSelectedSkill(spbBlocker, True);
           spbBasher: SetSelectedSkill(spbBuilder, True);
           spbMiner: SetSelectedSkill(spbBasher, True);
           spbDigger: SetSelectedSkill(spbMiner, True);
         end;
         end;

    'X': with Game do
         begin
         RegainControl;
         case SelectedSkill of
           spbClimber: SetSelectedSkill(spbUmbrella, True);
           spbUmbrella: SetSelectedSkill(spbExplode, True);
           spbExplode: SetSelectedSkill(spbBlocker, True);
           spbBlocker: SetSelectedSkill(spbBuilder, True);
           spbBuilder: SetSelectedSkill(spbBasher, True);
           spbBasher: SetSelectedSkill(spbMiner, True);
           spbMiner: SetSelectedSkill(spbDigger, True);
         end;
         end;

    // first stab at time pause support
    //'Z': Game.DoTimePause := not Game.DoTimePause;

    // --------- UPPERCASE ------------

    // disable/enable changing the selected skillbutton with mouse when paused
//    'C': if LemmixDebugMode then
  //         Game.SkillButtonsDisabledWhenPaused := not Game.SkillButtonsDisabledWhenPaused;


    {'F':
      begin
            if Game.Playing then
            begin
              if not Game.HyperSpeed then
              begin
                //windows.beep(448, 3);
                Game.HyperSpeedBegin;
                Game.TargetIteration := Game.CurrentIteration + 17*60;
              end
              else begin
                Game.HyperSpeedEnd;
              end;
            end;

      end; }



  end;
end;

procedure TGameWindow.Form_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  with MouseClipRect do
    if (Y >= Img.Top) and (Y <= Img.Top + Img.Height - 1) then
    begin
      if X <= Img.Left + DisplayScale then
        GameScroll := gsLeft
      else if X >= Img.Left + Img.Width - 1 + DisplayScale then
        GameScroll := gsRight
      else
        GameScroll := gsNone;
    end
    else
      GameScroll := gsNone;

end;

procedure TGameWindow.Form_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GameScroll := gsNone;
end;

procedure TGameWindow.SaveShot;
var
  Dlg : TSaveDialog;
  SaveName: String;
begin
  Dlg := TSaveDialog.Create(self);
  dlg.Filter := 'PNG Image (*.png)|*.png';
  dlg.FilterIndex := 1;
  dlg.InitialDir := '"' + ExtractFilePath(Application.ExeName) + '/"';
  dlg.DefaultExt := '.png';
  if dlg.Execute then
  begin
    SaveName := dlg.FileName;
    Game.SaveGameplayImage(SaveName); 
  end;
  Dlg.Free;
end;

procedure TGameWindow.CheckAdjustReleaseRate;
{-------------------------------------------------------------------------------
  In the mainloop the decision is made if we really have to update
-------------------------------------------------------------------------------}
begin
  with Game do
  begin
    if SpeedingUpReleaseRate then
    begin
//        if not (Replaying and Paused) then
      AdjustReleaseRate(1)
    end
    else if SlowingDownReleaseRate then
    begin
//      if not (Replaying and Paused) then
      AdjustReleaseRate(-1)
    end;
    if InstReleaseRate = -1 then
      AdjustReleaseRate(-100)
    else if InstReleaseRate = 1 then
      AdjustReleaseRate(100);
    InstReleaseRate := 0;
  end;
end;

procedure TGameWindow.StartReplay;
var
  ReplayGlitchFrames : Integer;
begin
  CanPlay := False;
  ReplayGlitchFrames := Game.ReplayGlitchIterations;
  Game.SetGameResult;
  Game.Start(True);
  Game.ReplayGlitchIterations := ReplayGlitchFrames;
  SkillPanel.RefreshInfo;
  CanPlay := True;
end;

procedure TGameWindow.StartReplay2(const aFileName: string);
begin
  CanPlay := False;
  {
  if CompareText(ExtractFileExt(aFileName), '.TXT') = 0
  then Game.Recorder.LoadFromOldTxt(aFileName)
  else }
  Game.Recorder.LoadFromFile(aFileName);
  //SaveReplay;       asdf
//  Game.Renderer.RenderWorld(Game.Level);

//  FillChar(SaveStates, SizeOf(SaveStates), 0);
  Game.Start(True);
  SkillPanel.RefreshInfo;
  CanPlay := True;
end;


procedure TGameWindow.LoadReplay;
var
  OldCanPlay: Boolean;
  Dlg : TOpenDialog;
  s: string;
begin
  OldCanPlay := CanPlay;
  CanPlay := False;
  s:='';
  dlg:=topendialog.create(nil);
  try
//    dlg.DefaultExt := '*.lrb';
    dlg.filename := '*.lrb';
    if dlg.execute then
    begin
      s:=dlg.filename;
    end;
  finally
    dlg.free;
  end;
  if s <> '' then
  begin
    StartReplay2(s);
    exit;
  end;
  CanPlay := OldCanPlay;
end;


procedure TGameWindow.Game_Finished(Sender: TObject);
begin
  {$ifdef testmode}
  if (GameParams.fTestMode and (GameParams.QuickTestMode in [2, 3])) then
  begin
    if GameParams.QuickTestMode = 3 then Game.Save(true);
    CloseScreen(gstExit)
  end else{$endif}
  CloseScreen(gstPostview);
end;

procedure TGameWindow.CloseScreen(aNextScreen: TGameScreenType);
begin
  CanPlay := False;
  Application.OnIdle := nil;
  ClipCursor(nil);
  Cursor := crNone;
//  GameParams.NextScreen := gstPostview;
  Game.SetGameResult;
  GameParams.GameResult := Game.GameResultRec;
  with GameParams, GameResult do
  begin
    if gSuccess{$ifdef testmode}and (not GameParams.fTestMode){$endif} then
      WhichLevel := wlNext;
  end;
  Img.RepaintMode := rmFull;

  inherited CloseScreen(aNextScreen);
end;

procedure TGameWindow.AddSaveState;
begin
  if fSaveStates.Count = 0 then
    fSaveStates.Add(Game.CurrentIteration)
  else
    fSaveStates[0] := Game.CurrentIteration;
  (*
  if fSaveStateIndex = fSaveStates.Count - 1 then
  begin
    fSaveStates.Add(Game.CurrentIteration);
    Inc(fSaveStateIndex);
  end
  else begin
    fSaveStates.SetCapacityAndCount(fSaveStateIndex + 1, 0);
    fSaveStates.Add(Game.CurrentIteration);
    Inc(fSaveStateIndex);
  end;
  *)

//  fSaveStates.SetCapacityAndCount(fSaveStateIndex + 1);
//  fSaveStates[fSaveStateIndex] := Game.CurrentIteration; classes utools
end;

procedure TGameWindow.NextSaveState(Forwards: Boolean);
begin
  case Forwards of
    False:
      begin
        if fSaveStates.Count > 0 then
          GotoSaveState(fSaveStates[0]);
        (*
        if fSaveStateIndex >= 0 then
        begin
          GotoSaveState(fSaveStates[fSaveStateIndex]);
          Dec(fSaveStateIndex);
        end;
        *)
      end;
    True:
      begin
        if fSaveStates.Count > 0 then
          GotoSaveState(fSaveStates[0]);

      end;
  end;
end;

end.

(*
procedure TGameWindow.Form_Close(Sender: TObject; var Action: TCloseAction);
begin
  CanPlay := False;
  Application.OnIdle := nil;
  ClipCursor(nil);
  GameParams.NextScreen := gstPostview;
  Game.SetGameResult;
  GameParams.GameResult := Game.GameResultRec;
  with GameParams, GameResult do
  begin
    if gSuccess then
      WhichLevel := wlNext;
  end;
end;


procedure TGameWindow.InitializeCursor;
const
  PLAYCURSOR_DEFAULT = 1;
  PLAYCURSOR_LEMMING = 2;
var
  bmpMask : TBitmap32;
  bmpColor : TBitmap32;

    procedure ScaleBmp(Bmp: TBitmap32; aScale: Integer);
    var                         //bad code but it works for now
      Temp: TBitmap32;
      Src, Dst: TRect;

    begin
      if aScale = 1 then
        Exit;
      Temp := TBitmap32.Create;
      Src := Rect(0, 0, Bmp.Width, Bmp.Height);
      Dst := Rect(0, 0, Bmp.Width * aScale, Bmp.Height * aScale);
      Temp.SetSize(Bmp.Width * aScale, Bmp.Height * aScale);
      Bmp.DrawTo(Temp, Dst, Src);
      Bmp.Width := Temp.Width;
      Bmp.Height := Temp.Height;
      Temp.DrawTo(Bmp, 0, 0);
      Temp.Free;
    end;


begin
  bmpMask := TBitmap32.Create;
  bmpColor := TBitmap32.Create;

  //bmpMask.LoadFromFile(apppath + 'dosgamecursormask1.bmp');
  //bmpColor.LoadFromFile(apppath+'dosgamecursor1.bmp');
  bmpMask.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_DEFAULT_MASK');
  bmpColor.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_DEFAULT');
//  bmpcolor.canvas.pixels[3,8]:=clred;

  ScaleBmp(bmpMask, DisplayScale);
  ScaleBmp(bmpColor, DisplayScale);

  with LemCursorIconInfo do
  begin
    fIcon := false;
    xHotspot := 3 * displayscale + DisplayScale;//7 * DisplayScale;//bmpmask.width div 2+displayscale;
    yHotspot := 8 * displayscale + DisplayScale;//7 * DisplayScale;//bmpmask.Height div 2+displayscale;
    hbmMask := bmpMask.Handle;
    hbmColor := bmpColor.Handle;
  end;

  HCursor1 := CreateIconIndirect(LemCursorIconInfo);
  Screen.Cursors[PLAYCURSOR_DEFAULT] := HCursor1;//CreateIconIndirect(LemCursorIconInfo);


  img.Cursor := PLAYCURSOR_DEFAULT;
  SkillPanel.img.cursor := PLAYCURSOR_DEFAULT;
  Self.Cursor := PLAYCURSOR_DEFAULT;

  bmpMask.Free;
  bmpColor.Free;

  //////////

  bmpMask := TBitmap32.Create;
  bmpColor := TBitmap32.Create;

//  bmpMask.LoadFromFile(apppath + 'dosgamecursormask2.bmp');
//  bmpColor.LoadFromFile(apppath+'dosgamecursor2.bmp');
  bmpMask.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_HIGHLIGHT_MASK');
  bmpColor.LoadFromResourceName(HINSTANCE, 'GAMECURSOR_HIGHLIGHT');

  scalebmp(bmpmask, DisplayScale);
  scalebmp(bmpcolor, DisplayScale);


  with LemSelCursorIconInfo do
  begin
    fIcon := false;
    xHotspot := 3 * DisplayScale + DisplayScale;//}bmpmask.width div 2+displayscale;
    yHotspot := 8 * DisplayScale + DisplayScale;//}bmpmask.Height div 2+displayscale;
//    xHotspot := 7 * 3;////5;
//    yHotspot := 7 * 3;//15;
    hbmMask := bmpMask.Handle;
    hbmColor := bmpColor.Handle;
  end;

  HCursor2 := CreateIconIndirect(LemSelCursorIconInfo);
  Screen.Cursors[PLAYCURSOR_LEMMING] := HCursor2;

//  Screen.Cursor := MyCursor;
//  Self.Cursor := HCursor1;

  bmpMask.Free;
  bmpColor.Free;
end;


//////////////////////////
var
  BmpColor, BmpMask: TBitmap;
  Info: TIconInfo;
  Cur : HCURSOR;

  BmpColor := TBitmap.Create;
  BmpMask := TBitmap.Create;
  BmpColor.LoadFromSomething(...)
  BmpMask.LoadFromSomething(...)

  with Info do
  begin
    fIcon := False;
    xHotspot := 7
    yHotspot := 7
    hbmMask := BmpMask.Handle;
    hbmColor := BmpColor.Handle;
  end;

  Cur := CreateIconIndirect(LemCursorIconInfo);
  Screen.Cursors[1] := Cur;

  BmpColor.Free;
  BmpMask.Free;
