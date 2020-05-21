{$include lem_directives.inc}
unit GameBaseScreen;

interface

uses
  Windows, Classes, Controls, Graphics, MMSystem, Forms,
  GR32, GR32_Image, GR32_Layers,
  UMisc,
  FBaseDosForm,
  GameControl,
  LemDosStructures,
  LemDosMainDat;

const
  DEF_STRETCHED = TRUE;

const
  PURPLEFONTCOUNT = ord('~') - ord('!') + 1;
  PurpleFontCharSet = ['!'..'~'];

type
  TPurpleFont = class(TComponent)
  private
    function GetBitmapOfChar(Ch: Char): TBitmap32;
    procedure Combine(F: TColor32; var B: TColor32; M: TColor32);
  protected
  public
    fBitmaps: array[0..PURPLEFONTCOUNT - 1] of TBitmap32;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property BitmapOfChar[Ch: Char]: TBitmap32 read GetBitmapOfChar;
  published
  end;

type
  {-------------------------------------------------------------------------------
    This is the ancestor for all dos forms that are used in the program.
  -------------------------------------------------------------------------------}
  TGameBaseScreen = class(TBaseDosForm)
  private
    fMainDatExtractor    : TMainDatExtractor;
    fScreenImg           : TImage32;
    fBackGround          : TBitmap32;
    fBackBuffer          : TBitmap32; // general purpose buffer
    fPurpleFont          : TPurpleFont;
    fOriginalImageBounds : TRect;
    fStretched           : Boolean;
    fScreenIsClosing     : Boolean;
    fCloseDelay          : Integer;
    procedure SetStretched(const Value: Boolean);
    procedure AdjustImage;
    procedure MakeList(const S: string; aList: TStrings);
    //procedure Fade_Idle(Sender: TObject; var Done: Boolean);
  protected
    procedure PrepareGameParams(Params: TDosGameParams); override;
    procedure CloseScreen(aNextScreen: TGameScreenType); virtual;
    property MainDatExtractor: TMainDatExtractor read fMainDatExtractor;
    property ScreenIsClosing: Boolean read fScreenIsClosing;
    property CloseDelay: Integer read fCloseDelay write fCloseDelay;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure TileBackgroundBitmap(X, Y: Integer; Dst: TBitmap32 = nil);
    procedure ExtractBackGround;
    procedure ExtractPurpleFont;
    procedure DrawPurpleText(Dst: TBitmap32; const S: string; X, Y: Integer; aRestoreBuffer: TBitmap32 = nil);
    procedure DrawPurpleTextCentered(Dst: TBitmap32; const S: string;
      Y: Integer; aRestoreBuffer: TBitmap32 = nil; EraseOnly: Boolean = False);
    function CalcPurpleTextSize(const S: string): TRect;
    procedure FadeOut;
    procedure InitializeImageSizeAndPosition(aWidth, aHeight: Integer);
    property ScreenImg: TImage32 read fScreenImg;
    property BackGround: TBitmap32 read fBackGround;
    property BackBuffer: TBitmap32 read fBackBuffer;
    property Stretched: Boolean read fStretched write SetStretched default DEF_STRETCHED;
  end;

implementation

{ TPurpleFont }

procedure TPurpleFont.Combine(F: TColor32; var B: TColor32; M: TColor32);
// just show transparent
begin
  if F <> 0 then B := F;
end;

constructor TPurpleFont.Create(aOwner: TComponent);
var
  i: Integer;
{-------------------------------------------------------------------------------
  The purple font has it's own internal pixelcombine.
  I don't think this ever has to be different.
-------------------------------------------------------------------------------}
begin
  inherited;
  for i := 0 to PURPLEFONTCOUNT - 1 do
  begin
    fBitmaps[i] := TBitmap32.Create;
    fBitmaps[i].OnPixelCombine := Combine;
    fBitmaps[i].DrawMode := dmCustom;
  end;
end;

destructor TPurpleFont.Destroy;
var
  i: Integer;
begin
  for i := 0 to PURPLEFONTCOUNT - 1 do
    fBitmaps[i].Free;
  inherited;
end;

function TPurpleFont.GetBitmapOfChar(Ch: Char): TBitmap32;
var
  Idx: Integer;
begin
  Assert(Ch in ['!'..'~']);
  Idx := Ord(Ch) - ord('!');
  Result := fBitmaps[Idx];
end;

{ TGameBaseScreen }

procedure TGameBaseScreen.AdjustImage;
begin
  case fStretched of
    False:
      begin
        fScreenImg.Align := alNone;
        fScreenImg.ScaleMode := smNormal;
      end;
    True:
      begin
        fScreenImg.Align := alClient;
        fScreenImg.ScaleMode := smResize;
        fScreenImg.BitmapAlign := baCenter;
      end;
  end;
end;

procedure TGameBaseScreen.CloseScreen(aNextScreen: TGameScreenType);
begin
  Application.OnIdle := nil;
  fScreenIsClosing := True;
  if fCloseDelay > 0 then
  begin
    Update;
    Sleep(fCloseDelay);
  end;
  FadeOut;
  if GameParams <> nil then
    GameParams.NextScreen := aNextScreen;
  Close;
end;

constructor TGameBaseScreen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fScreenImg := TImage32.Create(Self);
  //fScreenImg.Color :=
  //fScreenImg.Align := alClient;
  //fScreenImg.ScaleMode := smResize;
  fScreenImg.Parent := Self;

  fPurpleFont := TPurpleFont.Create(nil);

  fBackGround := TBitmap32.Create;
  fBackBuffer := TBitmap32.Create;
  fMainDatExtractor := TMainDatExtractor.Create;
  fStretched := DEF_STRETCHED;

  ScreenImg.Cursor := crNone;
//  Screen.Cursor := crNone;
  
end;

destructor TGameBaseScreen.Destroy;
begin
  fBackGround.Free;
  fMainDatExtractor.Free;
  fPurpleFont.Free;
  fBackBuffer.Free;
  inherited Destroy;
end;

function TGameBaseScreen.CalcPurpleTextSize(const S: string): TRect;
{-------------------------------------------------------------------------------
  Linefeeds increment 16 pixels
  Spaces increment 16 pixels
-------------------------------------------------------------------------------}
var
  C: Char;
  CX, i: Integer;
begin
  CX := 0;
  FillChar(Result, SizeOf(Result), 0);
  if S <> '' then
    Result.Bottom := 16;
  for i := 1 to Length(S) do
  begin
    C := S[i];
    case C of
      #13:
        begin
          Inc(Result.Bottom, 16);
          CX := 0;
        end;
      '!'..'~', ' ':
        begin
          Inc(CX, 16);
          if CX > Result.Right then
            Result.Right := CX;
        end;
    end;
  end;
end;

procedure TGameBaseScreen.DrawPurpleText(Dst: TBitmap32; const S: string; X, Y: Integer; aRestoreBuffer: TBitmap32 = nil);
{-------------------------------------------------------------------------------
  Linefeeds increment 16 pixels
  Spaces increment 16 pixels
-------------------------------------------------------------------------------}
var
  C: Char;
  CX, CY, i: Integer;
  R: TRect;
begin

  if aRestoreBuffer <> nil then
  begin
    R := CalcPurpleTextSize(S);
    OffsetRect(R, X, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect); // oops, again watch out for sourceretangle!
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

  CX := X;
  CY := Y;
  for i := 1 to Length(S) do
  begin
    C := S[i];
    case C of
      #13:
        begin
          Inc(CY, 16);
          CX := X;
        end;
      ' ':
        begin
          Inc(CX, 16);
        end;
      '!'..'~':
        begin
          fPurpleFont.BitmapOfChar[C].DrawTo(Dst, CX, CY);
          Inc(CX, 16);
        end;
    end;
  end;

end;

procedure TGameBaseScreen.DrawPurpleTextCentered(Dst: TBitmap32; const S: string; Y: Integer; aRestoreBuffer: TBitmap32 = nil;
  EraseOnly: Boolean = False);
{-------------------------------------------------------------------------------
  Linefeeds increment 16 pixels
  Spaces increment 16 pixels
-------------------------------------------------------------------------------}
var
//  C: Char;
  X, {CY, }i: Integer;
  R: TRect;
  List: TStringList;
  H: string;

begin

  List := TStringList.Create;
  MakeList(S, List);


  if aRestoreBuffer <> nil then
  begin
    R := CalcPurpleTextSize(S);
    OffsetRect(R, (Dst.Width - (R.Right - R.Left)) div 2, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect); // oops, again watch out for sourceretangle!
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

  if not EraseOnly then
  for i := 0 to List.Count - 1 do
  begin
    H := List[i]; // <= 40 characters!!!
    X := (Dst.Width - 16 * Length(H)) div 2;
    if H <> #13 then
      DrawPurpleText(Dst, H, X, Y)
    else
      Inc(Y, 16);
    //Inc(Y, 16);
  end;

  List.Free;
end;

procedure TGameBaseScreen.ExtractBackGround;
begin
  fMainDatExtractor.ExtractBrownBackGround(fBackGround);
//  fBackground.SaveToFile('d:\bb.bmp');
end;

procedure TGameBaseScreen.ExtractPurpleFont;
var
  Pal: TArrayOfColor32;
  i: Integer;
  PurpleFontPos: Integer;
begin
  Pal := GetDosMainMenuPaletteColors32;
  with MainDatExtractor do
  begin
    PurpleFontPos := $69B0;
    //@styledef
    {$ifndef fourrank}
    PurpleFontPos := $69B0 + 972; // there are 5 signs in ohno stored before the purple font
    {$endif}
    {$ifdef 15rank}
    PurpleFontPos := $69B0 - (972 * 4); // rank signs are stored in section 5 in 15-rank MAIN.DAT files
    {$endif}

//  {$else}
  //    Adjust Drawmethod here!

    // extract first
    ExtractBitmap(fPurpleFont.fBitmaps[0], 4, PurpleFontPos, 16, 16, 3, Pal);
    // the other positions are updated automatically by the stream (-1)
    for i := 1 to PURPLEFONTCOUNT - 1 do
      ExtractBitmap(fPurpleFont.fBitmaps[i], 4, -1, 16, 16, 3, Pal);
  end;

  //asdf
end;

procedure TGameBaseScreen.InitializeImageSizeAndPosition(aWidth, aHeight: Integer);
begin
//  with fScreenImg do
  begin
    fScreenImg.Bitmap.SetSize(aWidth, aHeight);

    with fOriginalImageBounds do
    begin
      Left := (Screen.Width - aWidth) div 2;
      Top := (Screen.Height - aHeight) div 2;
      Right := Left + aWidth;
      Bottom := Top + aHeight;
    end;


    fScreenImg.BoundsRect := fOriginalImageBounds;

    AdjustImage;
    (*
      (Screen.Width - aWidth) div 2,
      (Screen.Height - aHeight) div 2,
              aWidth,
              aHeight);
    *)
//     windlg([fscreenimg.visible])
  end;
end;

procedure TGameBaseScreen.PrepareGameParams(Params: TDosGameParams);
begin
  inherited;
  fMainDatExtractor.FileName := Params.MainDatFile;
end;

procedure TGameBaseScreen.SetStretched(const Value: Boolean);
begin
  fStretched := Value;
  AdjustImage;
end;

procedure TGameBaseScreen.TileBackgroundBitmap(X, Y: Integer; Dst: TBitmap32 = nil);
var
  aX, aY: Integer;
//  S, D: TBitmap32;
//  Dst: TBitmap32;
begin

  Assert(fBackground.Width > 0);
  Assert(fBackground.Height > 0);

  if Dst = nil then
    Dst := fScreenImg.Bitmap;

  aY := Y;
  aX := X;
  while aY <= Dst.Height do
  begin
    while aX <= Dst.Width do
    begin
      fBackground.DrawTo(Dst, aX, aY);
      Inc(aX, fBackground.Width);
    end;
    Inc(aY, fBackground.Height);
    aX := X;
  end;

end;


procedure TGameBaseScreen.MakeList(const S: string; aList: TStrings);
var
//  i: Integer;
  StartP, P: PChar;
//  Start, Curr: Integer;
  NewS: string;
begin
  StartP := PChar(S);
  P := StartP;
  repeat
//  begin

    case P^ of
    #13 :
      begin
        if P >= StartP then
        begin
          SetString(NewS, StartP, P - StartP);
          aList.Add(NewS);
          //deb([NewS]);

          while P^ = #13 do
          begin
            aList.Add(#13);
            //deb(['LineFeed']);
            Inc(P);
          end;
          if P^ = #0 then
            Break;
          //aList.Add(#13);

          StartP := P;
        end;

      end;

    #0:
      begin
        if P >= StartP then
        begin
          SetString(NewS, StartP, P - StartP);
          aList.Add(NewS);
          //deb([NewS]);
          //deb(['last', p -startp]);
          break;
        end;
      end;

    end;

    Inc(P);
    if P = #0 then Break;

  until False;

end;

procedure TGameBaseScreen.FadeOut;
var
  Steps: Integer; i: Integer;
  P: PColor32;
begin
  Steps := 16;
  while Steps > 0 do begin

  with ScreenImg.Bitmap do
  begin
//    clear(c);
    P := PixelPtr[0, 0];
    for i := 0 to Width * Height - 1 do
    begin
      with TColor32Entry(P^) do
      begin
        if R > 8 then Dec(R, 8) else R := 0;
        if G > 8 then Dec(G, 8) else G := 0;
        if B > 8 then Dec(B, 8) else B := 0;
      end;
      //P^ := c;
      Inc(P);
    end;
    Changed;
    Update;
    Sleep(10);
    //Update;
  end;

  dec(Steps);
  end;
//  Invalidate;
  //Update;
  Application.ProcessMessages;
end;


end.

