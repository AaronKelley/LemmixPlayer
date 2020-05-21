{$include lem_directives.inc}
unit LemDosStructures;

interface

uses
  Classes, SysUtils, Types,
  GR32,
  LemTypes;

{-------------------------------------------------------------------------------
  LVL raw level file structure as used by dos en winlemmings.
-------------------------------------------------------------------------------}
const
  LVL_MAXOBJECTCOUNT  = 32;
  LVL_MAXTERRAINCOUNT = 400;
  LVL_MAXSTEELCOUNT   = 32;

type
  TLVLObject = packed record
  case Byte of
    0: ( AsInt64: Int64 );
    1: ( D0, D1: DWord);
    2: ( W0, W1, W2, W3: Word);
    3: ( B0, B1, B2, B3, B4, B5, B6, B7: Byte);
    4: (
          XPos              : Word; // swap lo and hi
          YPos              : Word; // swap lo and hi
          ObjectID          : Word;
          Modifier          : Byte;
          DisplayMode       : Byte; // $8F = invert; $0F = normal
        );

  end;

  {
    bits 0..3    = modifier
    bits 4..7    shl 8 for xpos
    bits 8..15   add to xpos
    bits 16..24  9 bits number YPos
  }

  TLVLTerrain = packed record
  case Byte of
    0: ( D0: DWord );
    1: ( W0, W1: Word );
    2: ( B0, B1, B2, B3: Byte );
    3: (
          XPos              : Word;
          YPos              : Byte; // 9 bits
          TerrainID         : Byte;
       );
  end;

  TLVLSteel = packed record
  case Byte of
    0: ( D0: DWord );
    1: ( W0, W1: Word);
    2: ( B0, B1, B2, B3: Byte);
    3: (
         XPos              : Byte; // 9 bits
         YPos              : Byte; // 7 bits bits 1..7
         Area              : Byte; // bits 0..3 is height in 4 pixel units (then add 4)
                                   // bit 4..7 is width in 4 pixel units (then add 4)
         b                 : Byte; // always zero
       );
  end;

  TLVLObjects = array[0..LVL_MAXOBJECTCOUNT - 1] of TLVLObject;
  TLVLTerrains = array[0..LVL_MAXTERRAINCOUNT - 1] of TLVLTerrain;
  TLVLSteels = array[0..LVL_MAXSTEELCOUNT - 1] of TLVLSteel;
//  TChars32 = array[0..31] of Char;

  // the main record 2048 bytes
  TLVLRec = packed record
    {0000}  ReleaseRate                : Word; // big endian, swap!
    {    }  LemmingsCount              : Word; // big endian, swap!
    {    }  RescueCount                : Word; // big endian, swap!
    {    }  TimeLimit                  : Word; // big endian, swap!
    {0008}  ClimberCount               : Word; // big endian, swap!
    {    }  FloaterCount               : Word; // big endian, swap!
    {    }  BomberCount                : Word; // big endian, swap!
    {    }  BlockerCount               : Word; // big endian, swap!
    {    }  BuilderCount               : Word; // big endian, swap!
    {    }  BasherCount                : Word; // big endian, swap!
    {    }  MinerCount                 : Word; // big endian, swap!
    {    }  DiggerCount                : Word; // big endian, swap!
    {0018}  ScreenPosition             : Word; // big endian, swap!
    {001A}  GraphicSet                 : Word; // big endian, swap!
    {    }  GraphicSetEx               : Word; // big endian, swap!
    {001E}  Reserved                   : Word; // big endian, swap! $FFFF if SuperLemming else $0000
    {0020}  Objects                    : TLVLObjects;
    {0120}  Terrain                    : TLVLTerrains;
    {0760}  Steel                      : TLVLSteels;
    {07E0}  LevelName                  : array[0..31] of Char;
  end;

const
  LVL_SIZE = SizeOf(TLVLRec);

type
{SYSTEM.DAT for Flexi player}

TSysDatRec = packed record
    {0000}  PackName                   : array[0..31] of Char;
	          SecondLine                 : array[0..31] of Char;
            RankNames                  : array[0..14] of array[0..15] of Char;
            RankCount                  : Byte;
            SecretLevelCounts          : array[0..14] of Byte;
            TrackCount                 : Byte;
            CodeSeed                   : Byte;
            CheatCode                  : array[0..9] of Char;
            Options                    : Byte;
            Options2                   : Byte;
            Options3                   : Byte;
            Options4                   : Byte;
            KResult                    : array[0..2] of array[0..1] of array[0..35] of Char;
            SResult                    : array[0..8] of array[0..1] of array[0..35] of Char;
            Congrats                   : array[0..17] of array[0..35] of Char;
            ScrollerTexts              : array[0..15] of array[0..35] of Char;
            StyleNames                 : array[0..255] of array[0..15] of Char;
            VgaspecNames               : array[0..255] of array[0..15] of Char;
  end;

const
  SYSDAT_SIZE = SizeOf(TSysDatRec);

{-------------------------------------------------------------------------------
  GROUNDXX.DAT files (1056 bytes) (16 objects, 64 terrain, color palette)
  o See documentation for details
  o Little Endian words, so we can just load them from disk halleluyah!
-------------------------------------------------------------------------------}

type
  PDosMetaObject = ^TDosMetaObject;
  TDosMetaObject = packed record
    oAnimation_flags               : Word; // 2
    oStart_animation_frame_index   : Byte; // 3
    oAnimation_frame_count         : Byte; // 4
    oWidth                         : Byte; // 5
    oHeight                        : Byte; // 6
    oAnimation_frame_data_size     : Word; // 8   size in bytes of each animation frame
    oMask_offset_from_image        : Word; // 10
    oUnknown1                      : Word; // 12
    oUnknown2                      : Word; // 14
    oTrigger_left                  : Word; // 16
    oTrigger_top                   : Word; // 18
    oTrigger_width                 : Byte; // 19
    oTrigger_height                : Byte; // 20
    oTrigger_effect_id             : Byte; // 21
    oAnimation_frames_base_loc     : Word; // 23
    oPreview_image_location        : Word; // 25
    oUnknown3                      : Word; // 27
    oSound_effect_id               : Byte; // 28
  end;
  TDosMetaObjectArray = packed array[0..15] of TDOSMetaObject;

  PDosMetaTerrain = ^TDosMetaTerrain;
  TDosMetaTerrain = packed record
    tWidth        : Byte;
    tHeight       : Byte;
    tImage_loc    : Word;
    tMask_loc     : Word;
    tUnknown1     : Word
  end;

  TDosMetaTerrainArray = packed array[0..63] of TDosMetaTerrain;
  TDosEGAPalette8 = packed array[0..7] of Byte;

  PDosVgaColorRec = ^TDosVgaColorRec;
  TDosVgaColorRec = packed record
    R, G, B: Byte;
  end;

  TDosVGAPalette8 = packed array[0..7] of TDosVGAColorRec;

  // this is the total structure of a dos ground?.dat
  TDosGroundRec = packed record
    ObjectInfoArray     : TDosMetaObjectArray;
    TerrainInfoArray    : TDosMetaTerrainArray;
    EGA_PaletteCustom   : TDosEGAPalette8;
    EGA_PaletteStandard : TDOSEGAPalette8;
    EGA_PalettePreview  : TDOSEGAPalette8;
    VGA_PaletteCustom   : TDOSVGAPalette8;
    VGA_PaletteStandard : TDOSVGAPalette8;
    VGA_PalettePreview  : TDOSVGAPalette8;
  end;

  TDosVGAPalette16 = packed array[0..15] of TDosVGAColorRec;

  TDosVgaSpecPaletteHeader = packed record
    VgaPal: TDosVGAPalette8; // 24
    EgaPal: TDosEGAPalette8; // 8
    UnknownPal: array[0..7] of Byte; // maybe even less colors
  end;

  // big endian words, swap!
  TDosOddTableRec = packed record
    ReleaseRate                : Word;
    LemmingsCount              : Word;
    RescueCount                : Word;
    TimeLimit                  : Word;
    ClimberCount               : Word;
    FloaterCount               : Word;
    BomberCount                : Word;
    BlockerCount               : Word;
    BuilderCount               : Word;
    BasherCount                : Word;
    MinerCount                 : Word;
    DiggerCount                : Word;
    LevelName                  : array[0..31] of Char;
  end;


type
  {-------------------------------------------------------------------------------
    So this is a very non-oop class to easily load the oddtable in records
  -------------------------------------------------------------------------------}
  TDosOddTable = class
  public
    Recs: array of TDosOddTableRec;
    procedure LoadFromFile(const aFilename: string);
    procedure LoadFromStream(S: TStream);
  end;

const
  DosInLevelPalette: TDosVGAPalette8 =
{$ifndef christmaspalette}
  (
    (R: 000; G: 000; B: 000), // black
    (R: 016; G: 016; B: 056), // blue
    (R: 000; G: 044; B: 000), // green
    (R: 060; G: 052; B: 052), // white
    (R: 044; G: 044; B: 000), // yellow
    (R: 060; G: 008; B: 008), // red
    (R: 032; G: 032; B: 032), // gray
    (R: 000; G: 000; B: 000) // not used: probably this color is replaced with the standard palette entry in ground??.dat
  );
{$else}
  (
    (R: 000; G: 000; B: 000), // black
    (R: 052; G: 008; B: 008), // red
    (R: 000; G: 044; B: 000), // green
    (R: 060; G: 052; B: 052), // white
    (R: 060; G: 060; B: 000), // yellow
    (R: 016; G: 016; B: 060), // blue
    (R: 032; G: 032; B: 032), // gray
    (R: 000; G: 000; B: 000) // not used: probably this color is replaced with the standard palette entry in ground??.dat
  );
{$endif}

//const
  //LemmingBlue: TColor32 =
  //LemmixInLevelPallette =

const                             
  { TODO : do not mix original rgb and converted rgb }
  DosMainMenuPalette: TDosVGAPalette16 = (
    (R: 000; G: 000; B: 000), // black
    (R: 128; G: 064; B: 032), // browns
    (R: 096; G: 048; B: 032),
    {$ifndef christmaspalette}
    (R: 048; G: 000; B: 016),
    {$else}
    (R: 000; G: 000; B: 000),
    {$endif}
    (R: 032; G: 008; B: 124), // purples
    (R: 064; G: 044; B: 144),
    (R: 104; G: 088; B: 164),
    (R: 152; G: 140; B: 188),
    (R: 000; G: 080; B: 000), // greens
    (R: 000; G: 096; B: 016),
    (R: 000; G: 112; B: 032),
    (R: 000; G: 128; B: 064),
    (R: 208; G: 208; B: 208), // white
    (R: 176; G: 176; B: 000), // yellow
    {$ifndef christmaspalette}
    (R: 064; G: 080; B: 176), // blue
    {$else}
    (R: 200; G: 16; B: 16), // red
    {$endif}
    (R: 224; G: 128; B: 144)  // pink
  );

function DosVgaColorToColor32(const ColorRec: TDosVgaColorRec): TColor32;
function GetDosMainMenuPaletteColors32(UseXmas: Boolean = false): TArrayOfColor32;

implementation

function GetDosMainMenuPaletteColors32(UseXmas: Boolean = false): TArrayOfColor32;
var
  i: Integer;
  P: PColor32;

begin
  SetLength(Result, 16);
  { TODO : move this conversion somewhere else }
  for i := 0 to 15 do // bad code
  begin
    P := @Result[i];
    TColor32Entry(P^).A := 0;
    TColor32Entry(P^).R := DosMainMenuPalette[i].R;
    TColor32Entry(P^).G := DosMainMenuPalette[i].G;
    TColor32Entry(P^).B := DosMainMenuPalette[i].B;
  end;
  {$ifdef flexi}
  if UseXmas then
  begin
    P := @Result[3];
    TColor32Entry(P^).A := 0;
    TColor32Entry(P^).R := 0;
    TColor32Entry(P^).G := 0;
    TColor32Entry(P^).B := 0;
    P := @Result[14];
    TColor32Entry(P^).A := 0;
    TColor32Entry(P^).R := 240;
    TColor32Entry(P^).G := 32;
    TColor32Entry(P^).B := 32;
  end;
  {$endif}
end;

function DosVgaColorToColor32(const ColorRec: TDosVgaColorRec): TColor32;
begin
  with TColor32Entry(Result) do
  begin
    R := ColorRec.R * 4;
    G := ColorRec.G * 4;
    B := ColorRec.B * 4;
  end;
end;

procedure TDosOddTable.LoadFromFile(const aFilename: string);
var
  F: TStream;//TFileStream;
  //i: Integer;
//  s: string;
begin
  {
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end; }
  F := CreateDataStream(aFileName, ldtLemmings);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;


procedure TDosOddTable.LoadFromStream(S: TStream);
var
  i: Integer;
begin
  SetLength(Recs, S.Size div SizeOf(TDosOddTableRec));
  for i := 0 to Length(Recs) - 1 do
    S.ReadBuffer(Recs[i], Sizeof(TDosOddTableRec));
end;

end.

