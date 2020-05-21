{$include lem_directives.inc}
unit LemLVLLoader;

interface

uses
  Classes, SysUtils,
  UMisc,
  LemTerrain,
  LemInteractiveObject,
  LemSteel,
  LemDosStructures,
  LemLevel,
  LemLevelLoad;

type
  TLVLLoader = class(TLevelLoader)
  private
  protected
  public
    class procedure LoadLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Boolean = false); override;
    class procedure StoreLevelInStream(aLevel: TLevel; aStream: TStream); override;
  end;

implementation

{ TLVLLoader }

class procedure TLVLLoader.LoadLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Boolean = false);
{-------------------------------------------------------------------------------
  Translate a LVL file and fill the collections.
  For decoding and technical details see documentation or read the code :)
-------------------------------------------------------------------------------}
var
  Buf: TLVLRec;
  H, i: Integer;
  O: TLVLObject;
  T: TLVLTerrain;
  S: TLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;

begin
  with aLevel do
  begin
    ClearLevel;
    aStream.ReadBuffer(Buf, LVL_SIZE);
    {-------------------------------------------------------------------------------
      Get the statics. This is easy
    -------------------------------------------------------------------------------}
    with Info do
    begin
      if OddLoad = false then
      begin
      ReleaseRate      := SwapWord(Buf.ReleaseRate) mod 256;
      LemmingsCount    := SwapWord(Buf.LemmingsCount);
      RescueCount      := SwapWord(Buf.RescueCount);
      TimeLimit        := SwapWord(Buf.TimeLimit) mod 256;
      ClimberCount     := SwapWord(Buf.ClimberCount) mod 256;
      FloaterCount     := SwapWord(Buf.FloaterCount) mod 256;
      BomberCount      := SwapWord(Buf.BomberCount) mod 256;
      BlockerCount     := SwapWord(Buf.BlockerCount) mod 256;
      BuilderCount     := SwapWord(Buf.BuilderCount) mod 256;
      BasherCount      := SwapWord(Buf.BasherCount) mod 256;
      MinerCount       := SwapWord(Buf.MinerCount) mod 256;
      DiggerCount      := SwapWord(Buf.DiggerCount) mod 256;
      MusicTrack       := Buf.GraphicSet mod 256;
      SuperLemming     := (Buf.Reserved = $FFFF);
      Title            := Buf.LevelName;
      end;

      ScreenPosition   := SwapWord(Buf.ScreenPosition);
      GraphicSet       := SwapWord(Buf.GraphicSet) mod 256;
      GraphicSetEx     := SwapWord(Buf.GraphicSetEx) mod 256;
      if (Buf.GraphicSetEx and $10 = 0) then
        OddTarget      := $FFFF
        else
        OddTarget      := ((Buf.BuilderCount mod 256) shl 8) + (Buf.BasherCount mod 256);
    end;

    {-------------------------------------------------------------------------------
      Get the objects
    -------------------------------------------------------------------------------}
    for i := 0 to LVL_MAXOBJECTCOUNT - 1 do
    begin
      O := Buf.Objects[i];
      if O.AsInt64 = 0 then
        Continue;
      Obj := InteractiveObjects.Add;
      Obj.Left := ((Integer(O.B0) shl 8 + Integer(O.B1)) and not 7) - 16;
      Obj.Top := Integer(O.B2) shl 8 + Integer(O.B3);
      if Obj.Top > 32767 then Obj.Top := Obj.Top - 65536;
      Obj.Identifier := Integer(O.B5 and 15);
      if O.Modifier and $80 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_NoOverwrite;
      if O.Modifier and $40 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_OnlyOnTerrain;
      if O.DisplayMode = $8F then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_UpsideDown;
      Obj.IsFake := (i >= 16);
    end;

    {-------------------------------------------------------------------------------
      Get the terrain.
    -------------------------------------------------------------------------------}
    for i := 0 to LVL_MAXTERRAINCOUNT - 1 do
    begin
      T := Buf.Terrain[i];
      if T.D0 = $FFFFFFFF then
        Continue;
      Ter := Terrains.Add;
      Ter.Left := Integer(T.B0 and 15) shl 8 + Integer(T.B1) - 16; // 9 bits
      Ter.DrawingFlags := T.B0 shr 5; // the bits are compatible
      H := Integer(T.B2) shl 1 + Integer(T.B3 and $80) shr 7;
      if H >= 256 then
        Dec(H, 512);
      Dec(H, 4);
      Ter.Top := H;
      Ter.Identifier := T.B3 and 63; // max = 63.  bit7 belongs to ypos
      if T.B0 and 16 <> 0 then Ter.Identifier := Ter.Identifier + 64;
    end;

    {-------------------------------------------------------------------------------
      Get the steel.
    -------------------------------------------------------------------------------}
    for i := 0 to LVL_MAXSTEELCOUNT - 1 do
    begin
      S := Buf.Steel[i];
      if S.D0 = 0 then
        Continue;
      Steel := Steels.Add;  
      Steel.Left := ((Integer(S.B0) shl 1) + (Integer(S.B1 and Bit7) shr 7)) * 4 - 16;  // 9 bits
      Steel.Top := Integer(S.B1 and not Bit7) * 4;  // bit 7 belongs to steelx
      Steel.Width := Integer(S.B2 shr 4) * 4 + 4;  // first nibble bits 4..7 is width in units of 4 pixels (and then add 4)
      Steel.Height := Integer(S.B2 and $F) * 4 + 4;  // second nibble bits 0..3 is height in units of 4 pixels (and then add 4)
    end;

  end; // with aLevel
end;

class procedure TLVLLoader.StoreLevelInStream(aLevel: TLevel; aStream: TStream);
var
  Int16: SmallInt; Int32: Integer;
  H, i: Integer;
  M: Byte;
  W: Word;
  O: ^TLVLObject;
  T: ^TLVLTerrain;
  S: ^TLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;
  Buf: TLVLRec;
begin


  with aLevel do
  begin
    FillChar(Buf, SizeOf(Buf), 0);
    FillChar(Buf.Terrain, Sizeof(Buf.Terrain), $FF);
    FillChar(Buf.LevelName, 32, ' ');

    {-------------------------------------------------------------------------------
      Set the statics.
    -------------------------------------------------------------------------------}
    with Info do
    begin
       Buf.ReleaseRate                    := ReleaseRate;
       Buf.LemmingsCount                  := LemmingsCount;
       Buf.RescueCount                    := RescueCount;
       Buf.TimeLimit                      := TimeLimit;
       Buf.ClimberCount                   := ClimberCount;
       Buf.FloaterCount                   := FloaterCount;
       Buf.BomberCount                    := BomberCount;
       Buf.BlockerCount                   := BlockerCount;
       Buf.BuilderCount                   := BuilderCount;
       Buf.BasherCount                    := BasherCount;
       Buf.MinerCount                     := MinerCount;
       Buf.DiggerCount                    := DiggerCount;
       Buf.ScreenPosition                 := ScreenPosition;
       Buf.GraphicSet                     := GraphicSet;
       Buf.GraphicSet                     := Buf.GraphicSet + (MusicTrack shl 8);
       Buf.GraphicSetEx                   := GraphicSetEx;
       // swap
       Buf.ReleaseRate                    := SwapWord(Buf.ReleaseRate);
       Buf.LemmingsCount                  := SwapWord(Buf.LemmingsCount);
       Buf.RescueCount                    := SwapWord(Buf.RescueCount);
       Buf.TimeLimit                      := SwapWord(Buf.TimeLimit);
       Buf.ClimberCount                   := SwapWord(Buf.ClimberCount);
       Buf.FloaterCount                   := SwapWord(Buf.FloaterCount);
       Buf.BomberCount                    := SwapWord(Buf.BomberCount);
       Buf.BlockerCount                   := SwapWord(Buf.BlockerCount);
       Buf.BuilderCount                   := SwapWord(Buf.BuilderCount);
       Buf.BasherCount                    := SwapWord(Buf.BasherCount);
       Buf.MinerCount                     := SwapWord(Buf.MinerCount);
       Buf.DiggerCount                    := SwapWord(Buf.DiggerCount);
       Buf.ScreenPosition                 := SwapWord(Buf.ScreenPosition);
       Buf.GraphicSet                     := SwapWord(Buf.GraphicSet);
       Buf.GraphicSetEx                   := SwapWord(Buf.GraphicSetEx);
       // encode superlemming
       if SuperLemming then
         Buf.Reserved := $FFFF;
       if Length(Title) > 0 then
         System.Move(Title[1], Buf.LevelName, Length(Title));
    end;

    {-------------------------------------------------------------------------------
      Set the objects.
    -------------------------------------------------------------------------------}
    for i := 0 to InteractiveObjects.Count - 1 do
    begin
      Obj := InteractiveObjects[i];
      O := @Buf.Objects[i];
      // set xpos: revert the misery
      Int16 := Obj.Left;
      Inc(Int16, 16);
      W := Word(Int16);
      W := System.Swap(W);
      O^.XPos := W;
      // set ypos: revert the misery
      Int16 := Obj.Top;
      if Int16 < 0 then Int16 := Int16 + 65536;
      W := Word(Int16);
      W := System.Swap(W);
      O^.Ypos := W;
      // set object id
      TWordRec(O^.ObjectID).Hi := Byte(Obj.Identifier);
      // set modifier
      if odf_NoOverwrite and Obj.DrawingFlags <> 0 then
        O^.Modifier := $80
      else if odf_OnlyOnTerrain and Obj.DrawingFlags <> 0 then
        O^.Modifier := $40;
      // set displaymode
      if odf_UpsideDown and Obj.DrawingFlags <> 0 then
        O^.DisplayMode := $8F
      else
        O^.DisplayMode := $0F; {default}
    end;

    {-------------------------------------------------------------------------------
      set the terrain
    -------------------------------------------------------------------------------}
    for i := 0 to Terrains.Count - 1 do
    begin
      Ter := Terrains[i];
      T := @Buf.Terrain[i];

      // GET: TerrainX := Integer(T.B0 and 15) shl 8 + Integer(T.B1) - 16;
      H := Ter.Left;
      Inc(H, 16);
      T.B0 := Byte(H shr 8) and 15;
      T.B1 := Byte(H);// + 16;

      // GET: TerrainDrawingFlags := TTerrainDrawingFlags(Byte(T.B0 shr 5));
      M := Byte(Ter.DrawingFlags) shl 5;
      T.B0 := T.B0 or M;
      // GET:
        (*
        H := Integer(T.B2) shl 1 + Integer(T.B3 and $80) shr 7;
        if H >= 256 then
          Dec(H, 512);
        Dec(H, 4);
        TerrainY := Map(H);
        *)
      H := Ter.Top;
      Inc(H, 4);
      if H < 0 then
        Inc(H, 512);
      T.B3 := Byte((H or Bit8) shl 7); // still don't know if this is right this "or bit8"
//      H := H and not Bit8;
      H := H shr 1;
      T.B2 := Byte(H);
      // GET: TerrainId := T.B3 and 63; // max = 63.
      T.B3 := T.B3 or (Byte(Ter.Identifier) and 63);
      if Ter.Identifier > 63 then T.B0 := T.B0 + 16;

    end;

    {-------------------------------------------------------------------------------
      set the steel.
    -------------------------------------------------------------------------------}
    for i := 0 to Steels.Count - 1 do
    begin
      Steel := Steels[i];
      S := @Buf.Steel[i];
      // GET: SteelX := ((Integer(B0) shl 1) + (Integer(B1 and Bit7) shr 7)) * 4 - 16;
      Int32 := Steel.Left;
      Int32 := (Int32 + 16) div 4;
      S^.B1 := Byte((Int32 or Bit8) shl 7); // still don't know this "or bit8"
      // Int32 := Int32 and not Bit8; <-- I THINK THIS WAS WRONG!!
      Int32 := Int32 shr 1;
      S^.B0 := Byte(Int32);
      // GET: SteelY := Integer(S.B1 and not Bit7) * 4;
      Int32 := Steel.Top div 4;
      S^.B1 := S^.B1 or (Byte(Int32) and not Bit7);
      // GET: SteelWidth  := Integer(S.B2 shr 4) * 4 + 4;
      Int32 := (Steel.Width - 4) div 4;
      S^.B2 := Byte(Int32) shl 4;
      // GET: SteelHeight := Integer(S.B2 and $F) * 4 + 4;
      Int32 := (Steel.Height - 4) div 4;
      S^.B2 := S^.B2 or (Byte(Int32) and not $F0); // highest bit set already
    end;

    aStream.WriteBuffer(Buf, LVL_Size);

  end;
end;

end.

