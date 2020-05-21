{$include lem_directives.inc}
unit LemDosAnimationSet;

interface

uses
  Classes, SysUtils,
  UMisc, GR32,
  LemCore,
  LemTypes,
  LemDosStructures,
  LemDosCmp,
  LemDosBmp,
  LemMetaAnimation,
  LemAnimationSet;

const
  LTR = False;
  RTL = True;

const
{-------------------------------------------------------------------------------
  dos animations ordered by their appearance in main.dat
  the constants below show the exact order
-------------------------------------------------------------------------------}
  WALKING             = 0;
  JUMPING             = 1;
  WALKING_RTL         = 2;
  JUMPING_RTL         = 3;
  DIGGING             = 4;
  CLIMBING            = 5;
  CLIMBING_RTL        = 6;
  DROWNING            = 7;
  HOISTING            = 8;
  HOISTING_RTL        = 9;
  BRICKLAYING         = 10;
  BRICKLAYING_RTL     = 11;
  BASHING             = 12;
  BASHING_RTL         = 13;
  MINING              = 14;
  MINING_RTL          = 15;
  FALLING             = 16;
  FALLING_RTL         = 17;
  UMBRELLA            = 18;
  UMBRELLA_RTL        = 19;
  SPLATTING           = 20;
  EXITING             = 21;
  FRIED               = 22;
  BLOCKING            = 23;
  SHRUGGING           = 24;
  SHRUGGING_RTL       = 25;
  OHNOING             = 26;
  EXPLOSION           = 27;

  AnimationIndices : array[TBasicLemmingAction, LTR..RTL] of Integer = (
    (0,0),
    (WALKING, WALKING_RTL),                   // baWalk,
    (JUMPING, JUMPING_RTL),                   // baJumping,
    (DIGGING, DIGGING),                       // baDigging,
    (CLIMBING, CLIMBING_RTL),                 // baClimbing,
    (DROWNING, DROWNING),                     // baDrowning,
    (HOISTING, HOISTING_RTL),                 // baHoisting,
    (BRICKLAYING, BRICKLAYING_RTL),           // baBricklaying,
    (BASHING, BASHING_RTL),                   // baBashing,
    (MINING, MINING_RTL),                     // baMining,
    (FALLING, FALLING_RTL),                   // baFalling,
    (UMBRELLA, UMBRELLA_RTL),                 // baUmbrella,
    (SPLATTING, SPLATTING),                   // baSplatting,
    (EXITING, EXITING),                       // baExiting,
    (FRIED, FRIED),                           // baFried,
    (BLOCKING, BLOCKING),                     // baBlocking,
    (SHRUGGING, SHRUGGING_RTL),               // baShrugging,
    (OHNOING, OHNOING),                       // baOhnoing,
    (EXPLOSION, EXPLOSION)                    // baExploding
  );


type
  {-------------------------------------------------------------------------------
    Basic animationset for dos.
  -------------------------------------------------------------------------------}
  TBaseDosAnimationSet = class(TBaseAnimationSet)
  private
    fMainDataFile           : string;
    fAnimationPalette       : TArrayOfColor32;
    fExplosionMaskBitmap    : TBitmap32;
    fBashMasksBitmap        : TBitmap32;
    fBashMasksRTLBitmap     : TBitmap32;
    fMineMasksBitmap        : TBitmap32;
    fMineMasksRTLBitmap     : TBitmap32;
    fCountDownDigitsBitmap  : TBitmap32;
  protected
    procedure DoReadMetaData; override;
    procedure DoReadData; override;
    procedure DoClearData; override;
  public
    constructor Create; override;
  { easy references, these point to the MaskAnimations[0..5] }
    property ExplosionMaskBitmap   : TBitmap32 read fExplosionMaskBitmap;
    property BashMasksBitmap       : TBitmap32 read fBashMasksBitmap;
    property BashMasksRTLBitmap    : TBitmap32 read fBashMasksRTLBitmap;
    property MineMasksBitmap       : TBitmap32 read fMineMasksBitmap;
    property MineMasksRTLBitmap    : TBitmap32 read fMineMasksRTLBitmap;
    property CountDownDigitsBitmap : TBitmap32 read fCountDownDigitsBitmap;
    property AnimationPalette: TArrayOfColor32 read fAnimationPalette write fAnimationPalette;
  published
    property MainDataFile: string read fMainDataFile write fMainDataFile; // must be set by style
  end;

implementation

{ TBaseDosAnimationSet }

procedure TBaseDosAnimationSet.DoReadMetaData;
{-------------------------------------------------------------------------------
  We dont have to read. It's fixed in this order in the main.dat.
  foot positions from ccexpore's emails, see lemming_mechanics pseudo code

  o make lemming animations
  o make mask animations metadata
-------------------------------------------------------------------------------}

    procedure Lem(aImageLocation: Integer; const aDescription: string;
      aFrameCount, aWidth, aHeight, aBPP, aFootX, aFootY, aAnimType: Integer);
    begin
      with fMetaLemmingAnimations.Add do
      begin
        ImageLocation      := aImageLocation;
        Description        := aDescription;
        FrameCount         := aFrameCount;
        Width              := aWidth;
        Height             := aHeight;
        BitsPerPixel       := aBPP;
        FootX              := aFootX;
        FootY              := aFootY;
        AnimationType      := aAnimType;
      end;
    end;

    procedure Msk(aImageLocation: Integer; const aDescription: string;
      aFrameCount, aWidth, aHeight, aBPP: Integer);
    begin
      with fMetaMaskAnimations.Add do
      begin
        ImageLocation      := aImageLocation;
        Description        := aDescription;
        FrameCount         := aFrameCount;
        Width              := aWidth;
        Height             := aHeight;
        BitsPerPixel       := aBPP;
      end;
    end;

begin

  //  place   description            F   W   H  BPP   FX   FY   animationtype

  Lem($0000, 'Walking'           ,   8, 16, 10,   2,   8,  10,   lat_Loop); // 0
  Lem($0140, 'Jumping'           ,   1, 16, 10,   2,   8,  10,   lat_Once);
  Lem($0168, 'Walking (rtl)'     ,   8, 16, 10,   2,   8,  10,   lat_Loop);
  Lem($02A8, 'Jumping (rtl)'     ,   1, 16, 10,   2,   8,  10,   lat_Once);
  Lem($02D0, 'Digging'           ,  16, 16, 14,   3,   8,  12,   lat_Loop);
  Lem($0810, 'Climbing'          ,   8, 16, 12,   2,   8,  12,   lat_Loop);
  Lem($0990, 'Climbing (rtl)'    ,   8, 16, 12,   2,   8,  12,   lat_Loop);
  Lem($0B10, 'Drowning'          ,  16, 16, 10,   2,   8,  10,   lat_Once);
  Lem($0D90, 'Hoisting'          ,   8, 16, 12,   2,   8,  12,   lat_Once);
  Lem($0F10, 'Hoisting (rtl)'    ,   8, 16, 12,   2,   8,  12,   lat_Once);
  Lem($1090, 'Building'          ,  16, 16, 13,   3,   8,  13,   lat_Loop); // 10
  Lem($1570, 'Building (rtl)'    ,  16, 16, 13,   3,   8,  13,   lat_Loop);
  Lem($1A50, 'Bashing'           ,  32, 16, 10,   3,   8,  10,   lat_Loop);
  Lem($21D0, 'Bashing (rtl)'     ,  32, 16, 10,   3,   8,  10,   lat_Loop);
  Lem($2950, 'Mining'            ,  24, 16, 13,   3,   8,  13,   lat_Loop);
  Lem($30A0, 'Mining (rtl)'      ,  24, 16, 13,   3,   8,  13,   lat_Loop);
  Lem($37F0, 'Falling'           ,   4, 16, 10,   2,   8,  10,   lat_Loop);
  Lem($3890, 'Falling (rtl)'     ,   4, 16, 10,   2,   8,  10,   lat_Loop);
  Lem($3930, 'Umbrella'          ,   8, 16, 16,   3,   8,  16,   lat_Loop);
  Lem($3C30, 'Umbrella (rtl)'    ,   8, 16, 16,   3,   8,  16,   lat_Loop);
  Lem($3F30, 'Splatting'         ,  16, 16, 10,   2,   8,  10,   lat_Once); // 20
  Lem($41B0, 'Exiting'           ,   8, 16, 13,   2,   8,  13,   lat_Once);
  Lem($4350, 'Vaporizing'        ,  14, 16, 14,   4,   8,  14,   lat_Once);
  Lem($4970, 'Blocking'          ,  16, 16, 10,   2,   8,  10,   lat_Loop);
  Lem($4BF0, 'Shrugging'         ,   8, 16, 10,   2,   8,  10,   lat_Once);
  Lem($4D30, 'Shrugging (rtl)'   ,   8, 16, 10,   2,   8,  10,   lat_Once);
  Lem($4E70, 'Oh-No-ing'         ,  16, 16, 10,   2,   8,  10,   lat_Once);
  Lem($50F0, 'Exploding'         ,   1, 32, 32,   3,  16,  25,   lat_Once);


  //  place   description            F   W   H  BPP

  Msk($0000, 'Bashmasks'         ,   4, 16, 10,   1);
  Msk($0050, 'Bashmasks (rtl)'   ,   4, 16, 10,   1);
  Msk($00A0, 'Minemasks'         ,   2, 16, 13,   1);
  Msk($00D4, 'Minemasks (rtl)'   ,   2, 16, 13,   1);
  Msk($0108, 'Explosionmask'     ,   1, 16, 22,   1);
  Msk($0154, 'Countdown digits'  ,   5,  8,  8,   1); // we only take the digits 5 downto zero
end;

procedure TBaseDosAnimationSet.DoReadData;
var
  Fn: string;
  Mem: TMemoryStream;
  Bmp: TBitmap32;
//  MaskBitmap: TBitmap32;
  TempBitmap: TBitmap32;
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  iAnimation, iFrame: Integer;
  Planar: TDosPlanarBitmap;
  MLA: TMetaLemmingAnimation;
  MA: TMetaAnimation;
  {X, }Y: Integer;
  Pal: TArrayOfColor32;
  LocalStream: TStream;

begin
  // fried and or vaporizing has high color indices
  Assert(Length(AnimationPalette) >= 16);
  Pal := Copy(fAnimationPalette);

  // ensure metadata

  Fn := MainDataFile;//Sty.IncludeCommonPath(Sty.MainDataFile);
  Planar := TDosPlanarBitmap.Create;
  TempBitmap := TBitmap32.Create;
  Sections := TDosDatSectionList.Create;
  Decompressor := TDosDatDecompressor.Create;

  try
    {$ifdef external}if FileExists(Fn) then
      LocalStream := TFileStream.Create(Fn, fmOpenRead)
      else{$endif}
      LocalStream := CreateDataStream(Fn, ldtLemmings);
    try
      Decompressor.LoadSectionList(LocalStream, Sections, False);
    finally
      LocalStream.Free;
    end;
    Decompressor.DecompressSection(Sections[0].CompressedData, Sections[0].DecompressedData);
    Mem := Sections[0].DecompressedData;

//    windlg('load animations');

    with fMetaLemmingAnimations{.HackedList} do
      for iAnimation := 0 to Count - 1 do
      begin
        //Y := 0;
        MLA := fMetaLemmingAnimations[ianimation];//List^[iAnimation];
//        with MLA do
        begin
          Bmp := TBitmap32.Create;
          fLemmingAnimations.Add(Bmp);
          Planar.LoadAnimationFromStream(Mem, Bmp, MLA.ImageLocation,
            MLA.Width, MLA.Height, MLA.FrameCount, MLA.BitsPerPixel, Pal);

          (*
          Bmp.SetSize(MLA.Width, MLA.Height * MLA.FrameCount);
          Bmp.Clear(0);
          fLemmingAnimations.Add(Bmp);
          Mem.Seek(MLA.ImageLocation, soFromBeginning);
          for iFrame := 0 to MLA.FrameCount - 1 do
          begin
            Planar.LoadFromStream(Mem, TempBitmap, -1, MLA.Width, MLA.Height, MLA.BitsPerPixel, Pal);
            TempBitmap.DrawTo(Bmp, 0, Y);
            Inc(Y, MLA.Height);
          end;
          //ReplaceColor(Bmp, clBRICK, BrickColor);
          {$ifdef develop}
          Bmp.SaveToFile('d:\temp\'+'anim_'+leadzerostr(ianimation, 2) + '.bmp');
          {$endif}
          *)


        end;

      end;


    Pal[0] := 0;
    Pal[1] := clMask32; // the ugly pink.
    Decompressor.DecompressSection(Sections[1].CompressedData, Sections[1].DecompressedData);
    Mem := Sections[1].DecompressedData;
    with fMetaMaskAnimations.HackedList do
      for iAnimation := 0 to Count - 1 do
      begin
        Y := 0;
        MA := List^[iAnimation];
        with MA do
        begin
          Bmp := TBitmap32.Create;
          Bmp.Width := Width;
          Bmp.Height := FrameCount * Height;
          Bmp.Clear(0);
          fMaskAnimations.Add(Bmp);
          Mem.Seek(ImageLocation, soFromBeginning);
          for iFrame := 0 to FrameCount - 1 do
          begin
            Planar.LoadFromStream(Mem, TempBitmap, -1, Width, Height, BitsPerPixel, Pal);
            TempBitmap.DrawTo(Bmp, 0, Y);
            Inc(Y, Height);
          end;
          //ReplaceColor(Bmp, clBRICK, BrickColor);
          //ifdef develop}
          //Bmp.SaveToFile('d:\temp\'+'mask_'+ i2s(ianimation) + '.bmp');
          //endif}
        end;
      end;

      // refer the "easy access" bitmaps
      fBashMasksBitmap := fMaskAnimations[0];
      fBashMasksRTLBitmap := fMaskAnimations[1];
      fMineMasksBitmap := fMaskAnimations[2];
      fMineMasksRTLBitmap := fMaskAnimations[3];
      fExplosionMaskBitmap := fMaskAnimations[4];
      fCountDownDigitsBitmap := fMaskAnimations[5];

      ReplaceColor(fCountDownDigitsBitmap, clMask32, Pal[3]);

  finally
//    LocalStream.Free;
    Sections.Free;
    Decompressor.Free;
    Planar.Free;
    TempBitmap.Free;
  end;
end;


procedure TBaseDosAnimationSet.DoClearData;
begin
  fLemmingAnimations.Clear;
  fMaskAnimations.Clear;
  fExplosionMaskBitmap    := nil;
  fBashMasksBitmap        := nil;
  fBashMasksRTLBitmap     := nil;
  fMineMasksBitmap        := nil;
  fMineMasksRTLBitmap     := nil;
  fCountDownDigitsBitmap  := nil;
end;

constructor TBaseDosAnimationSet.Create;
begin
  inherited Create;
//fAnimationPalette := DosInLevelPalette;
end;


end.

