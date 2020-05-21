{$include lem_directives.inc}
unit GameSkillPanel;


interface

uses
  Classes, Controls, SysUtils,
  GR32, GR32_Image, GR32_Layers,
  { TODO : get rid of UMisc }
  UMisc,
  LemStrings,
  LemTypes,
  LemDosBmp,
  LemDosCmp,
  LemDosStructures,
  LemCore,
  LemLevel,
  LemDosStyle,
  LemDosGraphicSet,
  GameInterfaces,
  LemGame;

  {-------------------------------------------------------------------------------
    maybe this must be handled by lemgame (just bitmap writing)

  // info positions types:
  // 1. BUILDER(23)             1/14
  // 2. OUT 28                  15/23
  // 3. IN 99%                  24/31
  // 4. TIME 2-31               32/40

  -------------------------------------------------------------------------------}
type
  TMinimapClickEvent = procedure(Sender: TObject; const P: TPoint) of object;
type
  TSkillPanelToolbar = class(TCustomControl, IGameToolbar)
  private
    fStyle         : TBaseDosLemmingStyle;
    fGraph         : TBaseDosGraphicSet;

    fImg           : TImage32;
    //fButtonHighlightLayer: TPositionedLayer;
    //fMinimapHighlightLayer: TPositionedLayer;

    fOriginal      : TBitmap32;
    fLevel         : TLevel;
    fSkillFont     : array['0'..'9', 0..1] of TBitmap32;
    fInfoFont      : array[0..37] of TBitmap32; {%} { 0..9} {A..Z} // make one of this!
    fGame          : TLemmingGame;
    { TODO : do something with this hardcoded shit }
    fButtonRects   : array[TSkillPanelButton] of TRect;
    fRectColor     : TColor32;

    fViewPortRect  : TRect;
    fOnMinimapClick            : TMinimapClickEvent; // event handler for minimap
    fCurrentScreenOffset : Integer;




    procedure SetLevel(const Value: TLevel);

    procedure ImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure SetGame(const Value: TLemmingGame);

  protected
    //procedure Paint; override;
    procedure ReadBitmapFromStyle; virtual;
    procedure ReadFont;
    procedure SetButtonRects;

//    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    fLastDrawnStr: string[40];
    fNewDrawStr: string[40];
    RedrawnChars: Integer;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
//    procedure DrawInfo(aType: TInfoType; const S: string);
    procedure DrawNewStr;
    property Img: TImage32 read fImg;

    procedure SetViewPort(const R: TRect);
    procedure RefreshInfo;

  { IGameInfoView support }
    procedure DrawSkillCount(aButton: TSkillPanelButton; aNumber: Integer);
    procedure DrawButtonSelector(aButton: TSkillPanelButton; Highlight: Boolean);
    procedure DrawMinimap(Map: TBitmap32);
    procedure SetInfoCursorLemming(const Lem: string; Num: Integer);
    procedure SetInfoLemmingsOut(Num: Integer);
    procedure SetInfoLemmingsIn(Num, Max: Integer);
    procedure SetInfoMinutes(Num: Integer);
    procedure SetInfoSeconds(Num: Integer);


    procedure SetCurrentScreenOffset(X: Integer);
    property OnMinimapClick: TMinimapClickEvent read fOnMinimapClick write fOnMinimapClick;
  published
    procedure SetStyleAndGraph(const Value: TBaseDosLemmingStyle;
      const aGraph: TBaseDosGraphicSet; aScale: Integer);

    property Level: TLevel read fLevel write SetLevel;
    property Game: TLemmingGame read fGame write SetGame;

  end;


implementation

uses
  GameWindow;

function PtInRectEx(const Rect: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

{ TSkillPanelToolbar }

constructor TSkillPanelToolbar.Create(aOwner: TComponent);
var
  c: Char;
  i: Integer;
begin
  inherited;
//  fBitmap := TBitmap.Create;
  fImg := TImage32.Create(Self);
  fImg.Parent := Self;
  fImg.RepaintMode := rmOptimizer;

  fImg.OnMouseDown := ImgMouseDown;
  fImg.OnMouseMove := ImgMouseMove;
  fImg.OnMouseUp := ImgMouseUp;

  fRectColor := DosVgaColorToColor32(DosInLevelPalette[3]);

{  fButtonHighlightLayer := TPositionedLayer.Create(Img.Layers);
  fButtonHighlightLayer.MouseEvents := False;
  fButtonHighlightLayer.Scaled := True;
  fButtonHighlightLayer.OnPaint := ButtonHighlightLayer_Paint; }

{  fMinimapHighlightLayer := TPositionedLayer.Create(Img.Layers);
  fMinimapHighlightLayer.MouseEvents := False;
  fMinimapHighlightLayer.Scaled := True;
  fMinimapHighlightLayer.OnPaint := MinimapHighlightLayer_Paint; }

  fOriginal := TBitmap32.Create;

  for i := 0 to 37 do
    fInfoFont[i] := TBitmap32.Create;

  for c := '0' to '9' do
    for i := 0 to 1 do
      fSkillFont[c, i] := TBitmap32.Create;


  // info positions types:
  // stringspositions=cursor,out,in,time=1,15,24,32
  // 1. BUILDER(23)             1/14               0..13      14
  // 2. OUT 28                  15/23              14..22      9
  // 3. IN 99%                  24/31              23..30      8
  // 4. TIME 2-31               32/40              31..39      9
                                                           //=40
  fLastDrawnStr := StringOfChar(' ', 40);
  fNewDrawStr := StringOfChar(' ', 40);
  fNewDrawStr := SSkillPanelTemplate;
//  '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';
//  windlg([length(fnewDrawStr)]);

  Assert(length(fnewdrawstr) = 40, 'length error infostring');




end;

destructor TSkillPanelToolbar.Destroy;
var
  c: Char;
  i: Integer;
begin


  for i := 0 to 37 do
    fInfoFont[i].Free;

//  fBitmap.Free;
  for c := '0' to '9' do
    for i := 0 to 1 do
      fSkillFont[c, i].Free;

  fOriginal.Free;
  inherited;
end;

procedure TSkillPanelToolbar.DrawButtonSelector(aButton: TSkillPanelButton; Highlight: Boolean);
var
  R: TRect;
  C: TColor32;
  A: TRect;
begin
  if aButton = spbNone then
    Exit;
//  if Highlight then
  //if aButton in [bskFaster, bskSlower, bskPause, bskNuke] then
    //Exit;

   { TODO : use other WHITE ?}

  case Highlight of
    False :
      begin
        R := fButtonRects[aButton];
        Inc(R.Right);
        Inc(R.Bottom, 2);

        // top
        A := R;
        A.Bottom := A.Top + 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

        // left
        A := R;
        A.Right := A.Left + 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

        // right
        A := R;
        A.Left := A.Right - 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

        // bottom
        A := R;
        A.Top := A.Bottom - 1;
        fOriginal.DrawTo(fImg.Bitmap, A, A);

  //      fOriginal.DrawTo(fImg.Bitmap, R, R);
    //    fOriginal.DrawTo(fImg.Bitmap, R, R);
      end;
    True  :
      begin
        R := fButtonRects[aButton];
        Inc(R.Right);
        Inc(R.Bottom, 2);
        { TODO : do something with this palettes }
        C := fRectColor;// clwhite32;//DosPaletteEntryToColor32(DosInLevelPalette[3]);

        fImg.Bitmap.FrameRectS(R, C);
//        C := clWhite32;
        //fImg.Bitmap.FrameRectS(fButtonRects[aButton], clWhite);
      end;
  end;
end;
(*
procedure TSkillPanelToolbar.DrawInfo(aType: TInfoType; const S: string);
var
  C: char;
  i, x, y, idx: integer;
  LocalS: string;
begin
  // optimze this by pre-drawing
     // - "OUT "
     // - "IN "
     // - "TIME "
     // - "-"

  // info positions types:
  // 1. BUILDER(23)             1/14               0..13
  // 2. OUT 28                  15/23              14..22
  // 3. IN 99%                  24/31              23..30
  // 4. TIME 2-31               32/40              31..39

  case aType of
    itCursor: x := 0;
    itOut: x := 14 * 8;
    itIn : x := 23 * 8;
    itTime: x := 31 * 8
  end;
  y := 0;
  LocalS := PadR(S, 14);
  for i := 1 to Length(LocalS) do
  begin
    idx := -1;
    C := UpCase(LocalS[i]);
    case C of
      '%':
        begin
          idx := 0;
        end;
      '0'..'9':
        begin
          idx := ord(c) - ord('0') + 1;
        end;
      '-':
        begin
          idx := 11;
        end;
      'A'..'Z':
        begin
          idx := ord(c) - ord('A') + 12;
        end;
    end;

//    finfoforn[idx]

    //Assert(idx <= High(fInfoFont), 'infofont error');

    if idx >= 0 then
      fInfoFont[idx].DrawTo(fimg.Bitmap, x, 0)
    else
      fimg.Bitmap.FillRectS(x, y, x + 8, y + 16, 0);

    Inc(x, 8);

  end;

//  fImg.Bitmap.
end;
*)

procedure TSkillPanelToolbar.DrawNewStr;
var
  O, N: char;
  i, x, y, idx: integer;
//  LocalS: string;
  Changed: Integer;
begin

  Changed := 0;
  // optimze this by pre-drawing
     // - "OUT "
     // - "IN "
     // - "TIME "
     // - "-"

  // info positions types:
  // 1. BUILDER(23)             1/14               0..13
  // 2. OUT 28                  15/23              14..22
  // 3. IN 99%                  24/31              23..30
  // 4. TIME 2-31               32/40              31..39

{  case aType of
    itCursor: x := 0;
    itOut: x := 14 * 8;
    itIn : x := 23 * 8;
    itTime: x := 31 * 8
  end; }
  y := 0;


  x := 0;

  for i := 1 to 40 do
  begin
    idx := -1;


    O := UpCase(fLastDrawnStr[i]);
    N := UpCase(fNewDrawStr[i]);

    if O <> N then
    begin

      case N of
        '%':
          begin
            idx := 0;
          end;
        '0'..'9':
          begin
            idx := ord(n) - ord('0') + 1;
          end;
        '-':
          begin
            idx := 11;
          end;
        'A'..'Y':
          begin
            idx := ord(n) - ord('A') + 12;
          end;
        'Z':
          begin
            idx := ord(n) - ord('A') + 12;
          end;


      end;
          Inc(Changed);

  //    finfoforn[idx]

      if idx >= 0 then
        fInfoFont[idx].DrawTo(fimg.Bitmap, x, 0)
      else
        fimg.Bitmap.FillRectS(x, y, x + 8, y + 16, 0);
    end;

    Inc(x, 8);

  end;

  RedrawnChars := Changed;


//  fImg.Bitmap.
end;


procedure TSkillPanelToolbar.DrawSkillCount(aButton: TSkillPanelButton; aNumber: Integer);
var
  S: string;
  L, R: Char;
  BtnIdx: Integer;
  DstRect, SrcRect: TRect;

  c:tcolor32;

const
  FontYPos = 17;
//  Font

begin
  // x = 3, 19, 35 etc. are the "black holes" for the numbers in the image
  // y = 17

//  if aNumber < 0 then
  //  aNumber
  Restrict(aNumber, 0, 99);
//  Assert(Between(aNumber, 0, 99), 'skillpanel number error 1');

  S := LeadZeroStr(aNumber, 2);
  L := S[1];
  R := S[2];

  BtnIdx := Ord(aButton) - 1; // "ignore" the spbNone

  // white nothing if number is zero
  if aNumber = 0 then
  begin
    DstRect := Rect(BtnIdx * 16 + 4, 17, BtnIdx * 16 + 4 + 8, 17 + 8);
    c:=Color32(60*4, 52*4, 52*4);
    with DstRect do
      fImg.Bitmap.FillRect(Left, Top, Right, Bottom, c); //
      { TODO : use other WHITE }      //=60,52,52 (*4)
    Exit;
  end;

  // left
  DstRect := Rect(BtnIdx * 16 + 4, 17, BtnIdx * 16 + 4 + 4, 17 + 8);
  SrcRect := Rect(0, 0, 4, 8);
  fSkillFont[L, 1].DrawTo(fImg.Bitmap, DstRect, SrcRect); // 1 is left

  // right
  RectMove(DstRect, 4, 0);
  SrcRect := Rect(4, 0, 8, 8);
  fSkillFont[R, 0].DrawTo(fImg.Bitmap, DstRect, SrcRect); // 0 is right

//  with
//  fimg.Bitmap.FillRect(0, 0, 20, 20, clwhite32);
//  fSkillFont[R, 1].DrawTo(fImg.Bitmap, BtnIdx * 16 + 3, 17)
end;

procedure TSkillPanelToolbar.RefreshInfo;
begin
  DrawNewStr;
  fLastDrawnStr := fNewDrawStr;// := fLastDrawnStr;
end;



procedure TSkillPanelToolbar.ImgMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
{-------------------------------------------------------------------------------
  Mouse behaviour of toolbar.
  o Minimap scrolling
  o button clicks
-------------------------------------------------------------------------------}
var
  P: TPoint;
  i: TSkillPanelButton;
  R: PRect;
  Exec: Boolean;
begin
//  Exec := False;
  P := Img.ControlToBitmap(Point(X, Y));

  // check minimap scroll
  if PtInRectEx(DosMiniMapCorners, P) then
  begin
    Dec(P.X, DosMinimapCorners.Left);
    Dec(P.Y, DosMiniMapCorners.Top);
    P.X := P.X * 16;
    P.Y := P.Y * 8;
    if Assigned(fOnMiniMapClick) then
      fOnMinimapClick(Self, P);

    //Game.MiniMapClick(P);
    Exit;
  end;

  if Game.HyperSpeed or Game.FastForward then
    Exit;

  for i := Succ(Low(TSkillPanelButton)) to High(TSkillPanelButton) do // "ignore" spbNone
  begin
    R := @fButtonRects[i];
    if PtInRectEx(R^, P) then
    begin

      if Game.SkillButtonsDisabledWhenPaused then
        Exec := not Game.Paused or (i = spbPause)
      else
        Exec := True;

      if Exec then
        if i = spbNuke then
          Exec := ssDouble in Shift;

      if Exec then
      begin
        if i <> spbPause then
          Game.RegainControl;
        Game.SetSelectedSkill(i, True, (ssRight in Shift));
      end;
      Exit;
    end;
  end;

end;

procedure TSkillPanelToolbar.ImgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
begin
  if ssLeft in Shift then
  begin
    P := Img.ControlToBitmap(Point(X, Y));

    if PtInRectEx(DosMiniMapCorners, P) then
    begin
      Dec(P.X, DosMinimapCorners.Left);
      Dec(P.Y, DosMiniMapCorners.Top);
      P.X := P.X * 16;
      P.Y := P.Y * 8;
      if Assigned(fOnMiniMapClick) then
        fOnMinimapClick(Self, P);
      //Game.MiniMapClick(P);
    end;
  end;
  Game.HitTestAutoFail := true;
end;

procedure TSkillPanelToolbar.ImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
   Game.SetSelectedSkill(spbSlower, False);
   Game.SetSelectedSkill(spbFaster, False);
end;




procedure TSkillPanelToolbar.ReadBitmapFromStyle;
var
  Sty: TBaseDosLemmingStyle;//TDosOrigStyle;
  c: char; i: Integer;
//  pal: TDosVGAPalette8;
//  pal32: TArrayOfColor32;
//  Extr: TMainDatExtractor;
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
//  MainDatFileStream: TFileStream;
  DataStream: TStream;
  Planar: TDosPlanarBitmap;
  Fn: string;
  LemmixPal: TArrayOfColor32;
  HiPal: TArrayOfColor32;
//  TempColor : TColor32;
//  pal
begin


  if not (fStyle is TBaseDosLemmingStyle) then
    Exit;

  // try and concat palettes
  LemmixPal := DosPaletteToArrayOfColor32(DosInLevelPalette);
  if Game.fXmasPal then
  begin
    LemmixPal[1] := $D02020;
    LemmixPal[4] := $F0F000;
    LemmixPal[5] := $4040E0;
  end;
  HiPal := fGraph.PaletteCustom;

  {TODO: how o how is the palette constructed?????????}
  Assert(Length(HiPal) = 8, 'hipal error');
  SetLength(LemmixPal, 16);
  for i := 8 to 15 do
    LemmixPal[i] := HiPal[i - 8];
  LemmixPal[7] := LemmixPal[8];


  sections := tdosdatsectionlist.create;

  Sty := TBaseDosLemmingStyle(fStyle);
//  TDosOrigStyle(fStyle);

//  Extr := TMainDatExtractor.Create(Sty.IncludeCommonPath(Sty.MainDataFile));
  Fn := Sty.MainDataFile;// IncludeTrailingBackslash(Sty.CommonPath) + 'Main.dat';


//  MainDatFileStream := TFileStream.Create(Fn, fmOpenRead);
  Decompressor := TDosDatDecompressor.Create;
  Planar := TDosPlanarBitmap.Create;

//  with Decompressor do
  begin
    SetButtonRects;
    {$ifdef external}
    if FileExists(Fn) then
      DataStream := TFileStream.Create(Fn, fmOpenRead)
      else{$endif}
      DataStream := CreateDataStream(Fn, ldtLemmings);
    try
      Decompressor.LoadSectionList(DataStream, Sections, False);
    finally
      DataStream.Free;
    end;

      //ExtractSkillPanel(fOriginal, DosInLevelPalette{fGraph.PaletteStandard});
      Decompressor.DecompressSection(Sections[6].CompressedData, Sections[6].DecompressedData);
      //Planar.DirectRGB := False;
      Sections[6].DecompressedData.seek(0, sofrombeginning);
      Planar.LoadFromStream(Sections[6].DecompressedData, fOriginal, 0, 320, 40, 4, LemmixPal);

    fImg.Bitmap.Assign(fOriginal);

//    fOriginal.savetofile('skillpanel.bmp');

    // info fonts
    //DoExtract(6);
    Sections[6].DecompressedData.Seek($1900, soFromBeginning);
    for i := 0 to 37 do
      Planar.LoadFromStream(Sections[6].DecompressedData, fInfofont[i], -1, 8,16, 3,
      LemmixPal);//DosInLevelPalette, DosInLevelPalette);


    // skill fonts
    //DoExtract(2);

    { TODO : christmas lemmings, fix it }
  //  pal := DosInLevelPalette;// fGraph.palettestandard; {DosInLevelPalette;}
//    pal[1] := pal[3]; // WHITE
    LemmixPal[1] := LemmixPal[3]; // WHITE
    Decompressor.DecompressSection(Sections[2].CompressedData, Sections[2].DecompressedData);
    Sections[2].decompresseddata.seek($1900, sofrombeginning);
    for c := '0' to '9' do
      for i := 0 to 1 do
      begin
        Planar.LoadFromStream(sections[2].decompresseddata, fSkillFont[c, i], -1, 8, 8, 1,
          LemmixPal);//pal, pal);
        //fSkillFont[c,i].savetofile(AppPath+'temp\'+'skillfont_' + c + i2s(i) + '.bmp');
      end;

    //Free;
  end;
  //fbitmap.savetofile(apppath+'sp.bmp');
//  MainDatFileStream.free;
  decompressor.free;
  planar.free;
  sections.free;
end;

procedure TSkillPanelToolbar.ReadFont;
begin

end;

procedure TSkillPanelToolbar.SetButtonRects;
var
  Org, R: TRect;
  iButton: TSkillPanelButton;
//  Sca: Integer;

//    function ScaleRect(const ):

begin
//  Sca := 3;
  Org := Rect(1, 16, 15, 38); // exact position of first button
  R := Org;
  {R.Left := R.Left * Sca;
  R.Right := R.Right * Sca;
  R.Top := R.Top * Sca;
  R.Bottom := R.Bottom * Sca; }

  for iButton := Succ(Low(TSkillPanelButton)) to High(TSkillPanelButton) do
  begin
    fButtonRects[iButton] := R;
    RectMove(R, 16{ * Sca}, 0);
  end;

end;

procedure TSkillPanelToolbar.SetInfoCursorLemming(const Lem: string; Num: Integer);
var
  S: string;
begin
//exit;
  if Lem <> '' then
  begin

    S := PadR(Lem + ' ' + i2s(Num), 14); //
    Move(S[1], fNewDrawStr[1], 14);
  end
  else begin
    S := '              ';
    Move(S[1], fNewDrawStr[1], 14);
  end;
end;

procedure TSkillPanelToolbar.SetInfoLemmingsOut(Num: Integer);
var
  S: string;
begin
  // stringspositions cursor,out,in,time = 1,15,24,32
  //fNewDrawStr := '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';
  S := PadR(i2s(Num), 5);
  Move(S[1], fNewDrawStr[19], 5);
end;

procedure TSkillPanelToolbar.SetInfoLemmingsIn(Num, Max: Integer);
var
  S: string;
begin
  // stringspositions cursor,out,in,time = 1,15,24,32
  //fNewDrawStr := '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';
  { TODO : percentage }
  S := PadR(i2s(percentage(Max, Num)) + '%', 5);
  Move(S[1], fNewDrawStr[27], 5);
end;

procedure TSkillPanelToolbar.SetInfoMinutes(Num: Integer);
var
  S: string;
begin
  // stringspositions cursor,out,in,time = 1,15,24,32
  //fNewDrawStr := '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';
  S := PadL(i2s(Num), 2);
  Move(S[1], fNewDrawStr[36], 2);
end;

procedure TSkillPanelToolbar.SetInfoSeconds(Num: Integer);
var
  S: string;
begin
  // stringspositions cursor,out,in,time = 1,15,24,32
  //fNewDrawStr := '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';
  S := LeadZeroStr(Num, 2);
  Move(S[1], fNewDrawStr[39], 2);
end;

procedure TSkillPanelToolbar.SetLevel(const Value: TLevel);
begin
  fLevel := Value;
end;

procedure TSkillPanelToolbar.SetStyleAndGraph(const Value: TBaseDosLemmingStyle;
      const aGraph: TBaseDosGraphicSet;
      aScale: Integer);
begin
  fImg.BeginUpdate;
  fStyle := Value;
  fGraph := aGraph;
  if fStyle <> nil then
  begin
    ReadBitmapFromStyle;
    ReadFont;
  end;
//  Width := fBitmap.Width;
//  Height := fBitmap.Height;
  fImg.Scale := aScale;
  fImg.ScaleMode := smScale;
  //fImg.AutoSize := True;

  fImg.Height := fOriginal.Height * aScale;
  fImg.Width := fOriginal.Width * aScale;
  Width := fImg.Width;
  Height := fImg.Height;

//  AutoSize := True;
//  fImg.Width * fImg.Bitmap.Width

//  DrawNumber(bskClimber, 23);
//  DrawInfo('1234567890123456789012345678901234567890');


  fImg.EndUpdate;
  fImg.Changed;
  Invalidate;
end;

procedure TSkillPanelToolbar.SetViewPort(const R: TRect);
begin
  fViewPortRect := R;
//  fMinimapHighlightLayer.Changed;

end;

procedure TSkillPanelToolbar.DrawMinimap(Map: TBitmap32);
var
  X: Integer;
begin
  Map.DrawTo(Img.Bitmap, 208, 18);
  if Parent <> nil then
  begin
    X := -Round(TGameWindow(Parent).ScreenImg.OffsetHorz/(16 * fImg.Scale));
    Img.Bitmap.FrameRectS(208 + X, 18, 208 + X + 20 + 5, 38, fRectColor);
  end;
end;

procedure TSkillPanelToolbar.SetGame(const Value: TLemmingGame);
begin
  if fGame <> nil then
    fGame.InfoPainter := nil;
  fGame := Value;
  if fGame <> nil then
    fGame.InfoPainter := Self
//  else
  //  fGame.InfoPainter := nil;  
end;



procedure TSkillPanelToolbar.SetCurrentScreenOffset(X: Integer);
begin
  fCurrentScreenOffset := X;
end;

end.

