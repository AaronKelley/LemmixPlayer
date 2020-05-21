{$include lem_directives.inc}

unit GamePreviewScreen;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils,
  GR32, GR32_Image, GR32_Layers,
  UMisc, Dialogs, PngImage,
  LemCore, LemStrings, LemDosStructures, LemRendering, LemLevelSystem,
  GameControl, GameBaseScreen, GameWindow;

type
  TGamePreviewScreen = class(TGameBaseScreen)
  private
    procedure SaveLevelImage;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    function GetScreenText: string;
  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildScreen; override;
    procedure BuildScreenInvis;
    procedure PrepareGameParams(Params: TDosGameParams); override;
  end;

implementation

uses LemGraphicSet, LemDosGraphicSet, LemLevel, FBaseDosForm;

{ TGamePreviewScreen }

procedure TGamePreviewScreen.BuildScreen;
var
  Inf: TRenderInfoRec;
  Mainpal: TArrayOfColor32;
  Temp, W: TBitmap32;
  DstRect: TRect;
  epf: String;
begin
  Assert(GameParams <> nil);

  ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;
    InitializeImageSizeAndPosition(640, 350);
    ExtractBackGround;
    ExtractPurpleFont;


    // prepare the renderer, this is a little bit shaky (wrong place)
    with GameParams do
    begin
      with GraphicSet do
      begin
        ClearMetaData;
        ClearData;
        GraphicSetId := Level.Info.GraphicSet;
        GraphicSetIdExt := Level.Info.GraphicSetEx;
        {$ifdef cust}{$ifndef flexi}
        epf := GameParams.ExternalPrefix;
        if epf <> '' then epf := epf + '_';
        {$else}epf := '';{$endif}
        {$else}epf := '';{$endif}
        {$ifdef extra}if Info.dSection <= 3 then GraphicSetId := GraphicSetId + 10;{$endif}
        MetaInfoFile := GameParams.Directory + epf + 'ground' + i2s(GraphicSetId) + 'o.dat';
        GraphicFile := GameParams.Directory + epf + 'vgagr' + i2s(GraphicSetId) + '.dat';
        if not FileExists(MetaInfoFile) then MetaInfoFile := GameParams.Directory + 'ground' + i2s(GraphicSetId) + 'o.dat';
        if not FileExists(GraphicFile) then GraphicFile := GameParams.Directory + 'vgagr' + i2s(GraphicSetId) + '.dat';
        {$ifdef testmode}
        if GameParams.fTestMode then
        begin
          MetaInfoFile := GameParams.fTestGroundFile;
          GraphicFile := GameParams.fTestVgagrFile;
        end;
        {$endif}
        MetaInfoFN := MetaInfoFile;
        GraphicFN := GraphicFile;
        if GraphicSetIdExt = 0 then
          begin
          GraphicExtFile := '';
          GraphicExtFN := '';
          end
        else
          begin
          GraphicExtFile := GameParams.Directory + epf + 'vgaspec' + i2s(GraphicSetIdExt - 1) + '.dat';
          if not FileExists(GraphicExtFile) then GraphicExtFile := GameParams.Directory + 'vgaspec' + i2s(GraphicSetIdExt - 1) + '.dat';
          {$ifdef testmode}
          if GameParams.fTestMode then GraphicExtFile := GameParams.fTestVgaspecFile;
          {$endif}
          GraphicExtFN := GraphicExtFile;
          end;
        ReadMetaData;
        ReadData;
      end;
      Inf.Level:=Level;
      Inf.GraphicSet := Graphicset;
      Renderer.PrepareGameRendering(Inf);
    end;



    Temp := TBitmap32.Create;
    W := TBitmap32.Create;
    try
      Temp.SetSize(640, 350);
      Temp.Clear(0);
      // draw level preview
      W.SetSize(GAME_BMPWIDTH, 160);
      W.Clear(0);
//      W.ResamplerClassName := 'TLinearResampler';//DraftResampler';
//      W.ResamplerClassName := 'TDraftResampler';
      GameParams.Renderer.RenderWorld(W, True);
      DstRect := Rect(0, 0, 400, 40); // div 4
      RectMove(DstRect, 120, 20); // set location
      W.DrawTo(Temp, DstRect, W.BoundsRect);
      // draw background
      TileBackgroundBitmap(0, 78, Temp);
      // draw text
      DrawPurpleText(Temp, GetScreenText, 0, 80);
      ScreenImg.Bitmap.Assign(Temp);
    finally
      W.Free;
      Temp.Free;
    end;


  finally
    ScreenImg.EndUpdate;
  end;
end;

procedure TGamePreviewScreen.SaveLevelImage;
var
  Dlg : TSaveDialog;
  SaveName: String;
  TempBitmap: TBitmap32;
  OutImage: TPngObject;
  X, Y: Integer;
  C: TColor32;
  R, G, B: Byte;
begin

if GameParams.DumpMode then
  SaveName := LeadZeroStr(GameParams.Info.dSection + 1, 2) + LeadZeroStr(GameParams.Info.dLevel + 1, 2) + '.png'
else begin
  Dlg := TSaveDialog.Create(self);
  dlg.Filter := 'PNG Image (*.png)|*.png';
  dlg.FilterIndex := 1;
  //dlg.InitialDir := '"' + GameParams. + '/"';
  dlg.DefaultExt := '.png';
  if dlg.Execute then
    SaveName := dlg.FileName
    else
    SaveName := '';
  Dlg.Free;
end;

  if SaveName = '' then Exit;

  TempBitmap := TBitmap32.Create;
  TempBitmap.SetSize(GAME_BMPWIDTH, 160);
  GameParams.Renderer.RenderWorld(TempBitmap, True);
  OutImage := TPngObject.CreateBlank(COLOR_RGB, 8, TempBitmap.Width, TempBitmap.Height);
  for Y := 0 to TempBitmap.Height - 1 do
    for X := 0 to TempBitmap.Width - 1 do
    begin
      C := TempBitmap.Pixel[X, Y];
      R := C shr 16;
      G := C shr 8;
      B := C;
      C := ($FF shl 24) + (B shl 16) + (G shl 8) + (R);
      OutImage.Pixels[X, Y] := C;
    end;
  OutImage.CompressionLevel := 9;
  OutImage.SaveToFile(SaveName);
  OutImage.Free;
  TempBitmap.Free;
end;



procedure TGamePreviewScreen.BuildScreenInvis;
var
  Inf: TRenderInfoRec;
  Mainpal: TArrayOfColor32;
  Temp, W: TBitmap32;
  DstRect: TRect;
  epf: String;
begin
  Assert(GameParams <> nil);

  //ScreenImg.BeginUpdate;
  try
    MainPal := GetDosMainMenuPaletteColors32;
    //InitializeImageSizeAndPosition(640, 350);
    ExtractBackGround;
    ExtractPurpleFont;


    // prepare the renderer, this is a little bit shaky (wrong place)
    with GameParams do
    begin
      with GraphicSet do
      begin
        ClearMetaData;
        ClearData;
        GraphicSetId := Level.Info.GraphicSet;
        GraphicSetIdExt := Level.Info.GraphicSetEx;
        {$ifdef cust}{$ifndef flexi}
        epf := GameParams.ExternalPrefix;
        if epf <> '' then epf := epf + '_';
        {$else}epf := '';{$endif}
        {$else}epf := '';{$endif}
        MetaInfoFile := GameParams.Directory + epf + 'ground' + i2s(GraphicSetId) + 'o.dat';
        GraphicFile := GameParams.Directory + epf + 'vgagr' + i2s(GraphicSetId) + '.dat';
        if not FileExists(MetaInfoFile) then MetaInfoFile := GameParams.Directory + 'ground' + i2s(GraphicSetId) + 'o.dat';
        if not FileExists(GraphicFile) then GraphicFile := GameParams.Directory + 'vgagr' + i2s(GraphicSetId) + '.dat';
        {$ifdef testmode}
        if GameParams.fTestMode then
        begin
          MetaInfoFile := GameParams.fTestGroundFile;
          GraphicFile := GameParams.fTestVgagrFile;
        end;
        {$endif}
        MetaInfoFN := MetaInfoFile;
        GraphicFN := GraphicFile;
        if GraphicSetIdExt = 0 then
          begin
          GraphicExtFile := '';
          GraphicExtFN := '';
          end
        else
          begin
          GraphicExtFile := GameParams.Directory + epf + 'vgaspec' + i2s(GraphicSetIdExt - 1) + '.dat';
          if not FileExists(GraphicExtFile) then GraphicExtFile := GameParams.Directory + 'vgaspec' + i2s(GraphicSetIdExt - 1) + '.dat';
          {$ifdef testmode}
          if GameParams.fTestMode then GraphicExtFile := GameParams.fTestVgaspecFile;
          {$endif}
          GraphicExtFN := GraphicExtFile;
          end;
        ReadMetaData;
        ReadData;
      end;
      Inf.Level:=Level;
      Inf.GraphicSet := Graphicset;
      Renderer.PrepareGameRendering(Inf);
    end;



    {Temp := TBitmap32.Create;
    W := TBitmap32.Create;
    try
      Temp.SetSize(640, 350);
      Temp.Clear(0);
      // draw level preview
      W.SetSize(GAME_BMPWIDTH, 160);
      W.Clear(0);
//      W.ResamplerClassName := 'TLinearResampler';//DraftResampler';
//      W.ResamplerClassName := 'TDraftResampler';
      GameParams.Renderer.RenderWorld(W, True);
      DstRect := Rect(0, 0, 400, 40); // div 4
      RectMove(DstRect, 120, 20); // set location
      W.DrawTo(Temp, DstRect, W.BoundsRect);
      // draw background
      TileBackgroundBitmap(0, 78, Temp);
      // draw text
      DrawPurpleText(Temp, GetScreenText, 0, 80);
      ScreenImg.Bitmap.Assign(Temp);
    finally
      W.Free;
      Temp.Free;
    end;}

    if GameParams.DumpMode then SaveLevelImage;

  finally
    //ScreenImg.EndUpdate;
  end;
end;



constructor TGamePreviewScreen.Create(aOwner: TComponent);
begin
  inherited;
  OnKeyDown := Form_KeyDown;
  OnMouseDown := Form_MouseDown;
  ScreenImg.OnMouseDown := Img_MouseDown;
end;

destructor TGamePreviewScreen.Destroy;
begin

  inherited;
end;

procedure TGamePreviewScreen.Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: begin
               {$ifdef testmode}if GameParams.fTestMode then
                 CloseScreen(gstExit)
               else{$endif}
                CloseScreen(gstMenu);
               end;
    VK_RETURN: CloseScreen(gstPlay);
    $49:       SaveLevelImage;
  end;
end;

procedure TGamePreviewScreen.Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseScreen(gstPlay);
end;

procedure TGamePreviewScreen.Img_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Button = mbLeft then
    CloseScreen(gstPlay);
end;


function TGamePreviewScreen.GetScreenText: string;
var
  Perc: string;
begin
  Assert(GameParams <> nil);

//  with GameParams.Level.Info do
  begin
    Perc := PadR(i2s(Percentage(GameParams.Level.Info.LemmingsCount,
                                GameParams.Level.Info.RescueCount)) + '%', 4);
    {$ifdef extra}
    if (GameParams.Info.dSection = 4) and (GameParams.Info.dLevel = 2) then
      Perc := '66% ';
    {$endif}
    {$ifdef testmode}
    if GameParams.fTestMode then GameParams.Info.dSectionName := 'TEST MODE';
    {$endif}
    Result := Format(SPreviewString,
                [GameParams.Info.dLevel + 1, // humans read 1-based
                 Trim(GameParams.Level.Info.Title),
                 GameParams.Level.Info.LemmingsCount,
                 Perc,
                 GameParams.Level.Info.ReleaseRate,
                 GameParams.Level.Info.TimeLimit,
                 GameParams.Info.dSectionName
               ]);
  end;
end;

procedure TGamePreviewScreen.PrepareGameParams(Params: TDosGameParams);
//var
  //Inf: TDosGamePlayInfoRec;
begin
  inherited;
  with Params, Info do
  begin

    case WhichLevel of
      wlFirst: Style.LevelSystem.FindFirstLevel(Info);
      wlFirstSection: Style.LevelSystem.FindFirstLevel(Info);
      wlLevelCode: Style.LevelSystem.FindLevel(Info);
      wlNext: Style.LevelSystem.FindNextLevel(Info);
      wlSame: Style.LevelSystem.FindLevel(Info);
    end;

    (*
    if not GameResult.gValid then
    begin
      if not Info.dValid then
        Style.LevelSystem.FindFirstLevel(Info)
      else
        Style.LevelSystem.FindLevel(Info) // used to fill sectionname
    end
    else if GameResult.gSuccess then
    begin
      Style.LevelSystem.FindNextLevel(Info);
    end;
    ClearGameResult;
      *)

    Style.LevelSystem.LoadSingleLevel(dPack, dSection, dLevel, Level);


    WhichLevel := wlSame;

//    Style.LevelSystem.LoadSingleLevel(0, 3, 1, Level);

  end;
end;

end.

