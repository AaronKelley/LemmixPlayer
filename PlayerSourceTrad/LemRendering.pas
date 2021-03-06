{$include lem_directives.inc}

unit LemRendering;

{-------------------------------------------------------------------------------
  Some notes on the rendering here...

  Levels consist of terrains and objects.
  1) Objects kan animate and terrain can be changed.
  2) Lemmings only have collisions with terrain

  The alpha channel of the pixels is used to put information about the pixels
  in the bitmap:
  Bit0 = there is terrain in this pixel
  Bit1 = there is interactive object in this pixel (maybe this makes no sense)

  This is done to optimize the drawing of (funny enough) static and triggered
  objects. mmm how are we going to do that????

  (Other ideas: pixel builder-brick, pixel erased by basher/miner/digger, triggerarea)
-------------------------------------------------------------------------------}

interface

uses
  Classes, Contnrs,
  GR32, GR32_LowLevel,
  UMisc,
  LemTypes,
  LemMetaObject,
  LemTerrain,
  LemInteractiveObject,
  LemGraphicSet,
  LemLevel;

  // we could maybe use the alpha channel for rendering, ok thats working!
  // create gamerenderlist in order of rendering

type
  TDrawItem = class
  private
  protected
    fOriginal: TBitmap32; // reference
  public
    constructor Create(aOriginal: TBitmap32);
    destructor Destroy; override;
    property Original: TBitmap32 read fOriginal;
  end;

  TDrawList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDrawItem;
  protected
  public
    function Add(Item: TDrawItem): Integer;
    procedure Insert(Index: Integer; Item: TDrawItem);
    property Items[Index: Integer]: TDrawItem read GetItem; default;
  published
  end;

  TAnimation = class(TDrawItem)
  private
    procedure Check;
    procedure CheckFrame(Bmp: TBitmap32);
  protected
    fFrameHeight: Integer;
    fFrameCount: Integer;
    fFrameWidth: Integer;
  public
    constructor Create(aOriginal: TBitmap32; aFrameCount, aFrameWidth, aFrameHeight: Integer);
    function CalcFrameRect(aFrameIndex: Integer): TRect;
    function CalcTop(aFrameIndex: Integer): Integer;
    procedure InsertFrame(Bmp: TBitmap32; aFrameIndex: Integer);
    procedure GetFrame(Bmp: TBitmap32; aFrameIndex: Integer);
    property FrameCount: Integer read fFrameCount default 1;
    property FrameWidth: Integer read fFrameWidth;
    property FrameHeight: Integer read fFrameHeight;
  end;

  TObjectAnimation = class(TAnimation)
  private
  protected
    fInverted: TBitmap32; // copy of original
    procedure Flip;
  public
    constructor Create(aOriginal: TBitmap32; aFrameCount, aFrameWidth, aFrameHeight: Integer);
    destructor Destroy; override;
    property Inverted: TBitmap32 read fInverted;
  end;


type
  // temp solution
  TRenderInfoRec = record
//    World        : TBitmap32; // the actual bitmap
    TargetBitmap : TBitmap32; // the visual bitmap
    Level        : TLevel;
    GraphicSet   : TBaseGraphicSet;
  end;

  TRenderer = class
  private
    TempBitmap         : TBitmap32;
    ObjectRenderList   : TDrawList; // list to accelerate object drawing
    Inf                : TRenderInfoRec;
    //Prepared: Boolean;

    fWorld: TBitmap32;

    procedure CombineTerrainDefault(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineTerrainNoOverwrite(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineTerrainErase(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineObjectDefault(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineObjectNoOverwrite(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineObjectOnlyOnTerrain(F: TColor32; var B: TColor32; M: TColor32);

    procedure PrepareTerrainBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
    procedure PrepareObjectBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure PrepareGameRendering(const Info: TRenderInfoRec);

    procedure DrawTerrain(Dst: TBitmap32; T: TTerrain);
    procedure DrawObject(Dst: TBitmap32; O: TInteractiveObject; aFrame: Integer;
      aOriginal: TBitmap32 = nil);
    procedure EraseObject(Dst: TBitmap32; O: TInteractiveObject;
      aOriginal: TBitmap32 = nil);
    procedure DrawSpecialBitmap(Dst, Spec: TBitmap32);

    procedure RenderMinimap(Dst, World: TBitmap32; aColor: TColor32);

    procedure DrawObjects(Dst: TBitmap32);

//    procedure Restore(Original, Target)
    procedure RenderAnimatedObject(O: TInteractiveObject; aFrame: Integer);

    function HasPixelAt(X, Y: Integer): Boolean;

//    procedure DrawMask(Dst: TBitmap32; Mask: TBitmap32; X, Y: Integer);
// drawmask to: world, targetbitmap, minimap

    procedure RenderWorld(World: TBitmap32; DoObjects: Boolean);

    procedure Highlight(World: TBitmap32; M: TColor32);
  end;

const
  COLOR_MASK    = $80FFFFFF; // transparent black flag is included!
  ALPHA_MASK    = $FF000000;

  ALPHA_TERRAIN          = $01000000;
  ALPHA_OBJECT           = $02000000; // not really needed, but used

  // to enable black terrain. bitmaps with transparent black should include
  // this bit
  ALPHA_TRANSPARENTBLACK = $80000000;

implementation

uses
  UTools;

{ TDrawItem }

constructor TDrawItem.Create(aOriginal: TBitmap32);
begin
  inherited Create;
  fOriginal := aOriginal;
end;

destructor TDrawItem.Destroy;
begin
  inherited Destroy;
end;

{ TDrawList }

function TDrawList.Add(Item: TDrawItem): Integer;
begin
  Result := inherited Add(Item);
end;

function TDrawList.GetItem(Index: Integer): TDrawItem;
begin
  Result := inherited Get(Index);
end;

procedure TDrawList.Insert(Index: Integer; Item: TDrawItem);
begin
  inherited Insert(Index, Item);
end;

{ TAnimation }

function TAnimation.CalcFrameRect(aFrameIndex: Integer): TRect;
begin
  with Result do
  begin
    Left := 0;
    Top := aFrameIndex * fFrameHeight;
    Right := Left + fFrameWidth;
    Bottom := Top + fFrameHeight;
  end;
end;

function TAnimation.CalcTop(aFrameIndex: Integer): Integer;
begin
  Result := aFrameIndex * fFrameHeight;
end;

procedure TAnimation.Check;
begin
  Assert(fFrameCount <> 0);
  Assert(Original.Width = fFrameWidth);
  Assert(fFrameHeight * fFrameCount = Original.Height);
end;

procedure TAnimation.CheckFrame(Bmp: TBitmap32);
begin
  Assert(Bmp.Width = Original.Width);
  Assert(Bmp.Height * fFrameCount = Original.Height);
end;

constructor TAnimation.Create(aOriginal: TBitmap32; aFrameCount, aFrameWidth, aFrameHeight: Integer);
begin
  inherited Create(aOriginal);
  fFrameCount := aFrameCount;
  fFrameWidth := aFrameWidth;
  fFrameHeight := aFrameHeight;
  Check;
end;

procedure TAnimation.GetFrame(Bmp: TBitmap32; aFrameIndex: Integer);
// unsafe
var
  Y, W: Integer;
  SrcP, DstP: PColor32;
begin
  Check;
  Bmp.SetSize(fFrameWidth, fFrameHeight);
  DstP := Bmp.PixelPtr[0, 0];
  SrcP := Original.PixelPtr[0, CalcTop(aFrameIndex)];
  W := fFrameWidth;
  for Y := 0 to fFrameHeight - 1 do
    begin
      MoveLongWord(SrcP^, DstP^, W);
      Inc(SrcP, W);
      Inc(DstP, W);
    end;
end;

procedure TAnimation.InsertFrame(Bmp: TBitmap32; aFrameIndex: Integer);
// unsafe
var
  Y, W: Integer;
  SrcP, DstP: PColor32;
begin
  Check;
  CheckFrame(Bmp);

  SrcP := Bmp.PixelPtr[0, 0];
  DstP := Original.PixelPtr[0, CalcTop(aFrameIndex)];
  W := fFrameWidth;

  for Y := 0 to fFrameHeight - 1 do
    begin
      MoveLongWord(SrcP^, DstP^, W);
      Inc(SrcP, W);
      Inc(DstP, W);
    end;
end;

{ TObjectAnimation }

constructor TObjectAnimation.Create(aOriginal: TBitmap32; aFrameCount,
  aFrameWidth, aFrameHeight: Integer);
begin
  inherited;
  fInverted := TBitmap32.Create;
  fInverted.Assign(aOriginal);
  Flip;
end;

destructor TObjectAnimation.Destroy;
begin
  fInverted.Free;
  inherited;
end;

procedure TObjectAnimation.Flip;
//unsafe, can be optimized by making a algorithm
var
  Temp: TBitmap32;
  i: Integer;

      procedure Ins(aFrameIndex: Integer);
      var
        Y, W: Integer;
        SrcP, DstP: PColor32;
      begin
//        Check;
        //CheckFrame(TEBmp);

        SrcP := Temp.PixelPtr[0, 0];
        DstP := Inverted.PixelPtr[0, CalcTop(aFrameIndex)];
        W := fFrameWidth;

        for Y := 0 to fFrameHeight - 1 do
          begin
            MoveLongWord(SrcP^, DstP^, W);
            Inc(SrcP, W);
            Inc(DstP, W);
          end;
      end;

begin
  if fFrameCount = 0 then
    Exit;
  Temp := TBitmap32.Create;
  try
    for i := 0 to fFrameCount - 1 do
    begin
      GetFrame(Temp, i);
      Temp.FlipVert;
      Ins(i);
    end;
  finally
    Temp.Free;
  end;
end;

{ TRenderer }

procedure TRenderer.CombineTerrainDefault(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then
  begin
    //B := F ;
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_TERRAIN; // put terrain bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

procedure TRenderer.CombineTerrainNoOverwrite(F: TColor32; var B: TColor32; M: TColor32);
begin
  if (F <> 0) and (B and ALPHA_TERRAIN = 0) then
  begin
    //B := F;
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_TERRAIN; // put terrain bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

procedure TRenderer.CombineTerrainErase(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then
    B := 0;
end;

procedure TRenderer.CombineObjectDefault(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then
  begin
    //B := F;
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_OBJECT; // put object bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

procedure TRenderer.CombineObjectNoOverwrite(F: TColor32; var B: TColor32; M: TColor32);
begin
  if (F <> 0) and (B and ALPHA_MASK = 0) then
  begin
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_OBJECT; // put object bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

procedure TRenderer.CombineObjectOnlyOnTerrain(F: TColor32; var B: TColor32; M: TColor32);
begin
  if (F <> 0) and (B and ALPHA_TERRAIN <> 0) then
  begin
    //B := F;
    B := B and not COLOR_MASK; // erase color
    B := B or ALPHA_OBJECT; // put object bit
    B := B or (F and COLOR_MASK) // copy color
  end;
end;

procedure TRenderer.PrepareTerrainBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
begin
  if DrawingFlags and tdf_NoOverwrite <> 0 then
  begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineTerrainNoOverwrite;
  end
  else if DrawingFlags and tdf_Erase <> 0 then
  begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineTerrainErase;
  end
  else begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineTerrainDefault;
  end;
end;

procedure TRenderer.PrepareObjectBitmap(Bmp: TBitmap32; DrawingFlags: Byte);
begin
  if DrawingFlags and odf_OnlyOnTerrain <> 0 then
  begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineObjectOnlyOnTerrain;
  end
  else if DrawingFlags and odf_NoOverwrite <> 0 then
  begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineObjectNoOverwrite;
  end
  else begin
    Bmp.DrawMode := dmCustom;
    Bmp.OnPixelCombine := CombineObjectDefault;
  end;
end;

procedure TRenderer.DrawTerrain(Dst: TBitmap32; T: TTerrain);
var
  Src: TBitmap32;
begin
  if T.Identifier >= Inf.GraphicSet.MetaTerrains.Count then Exit;
  Src := Inf.GraphicSet.TerrainBitmaps.List^[T.Identifier];
  if T.DrawingFlags and tdf_Invert = 0 then
  begin
    PrepareTerrainBitmap(Src, T.DrawingFlags);
    Src.DrawTo(Dst, T.Left, T.Top)
  end else begin
    Src.FlipVert(TempBitmap);
    PrepareTerrainBitmap(TempBitmap, T.DrawingFlags);
    TempBitmap.DrawTo(Dst, T.Left, T.Top);
  end;
end;

procedure TRenderer.DrawSpecialBitmap(Dst, Spec: TBitmap32);
begin
  Spec.DrawMode := dmCustom;
  Spec.OnPixelCombine := CombineTerrainDefault;
  Spec.DrawTo(Dst, (784 - (Spec.Width div 2)), 80 - (Spec.Height div 2));
end;

procedure TRenderer.DrawObject(Dst: TBitmap32; O: TInteractiveObject; aFrame: Integer; aOriginal: TBitmap32 = nil);
{-------------------------------------------------------------------------------
  Draws a interactive object
  � Dst = the targetbitmap
  � O = the object
  � aOriginal = if specified then first a part of this bitmap (world when playing)
    is copied to Dst to restore
-------------------------------------------------------------------------------}
var
  SrcRect, DstRect, R: TRect;
  Item: TObjectAnimation;// TDrawItem;
  Src: TBitmap32;
begin
  if O.Identifier >= Inf.GraphicSet.MetaObjects.Count then Exit;
  Assert(ObjectRenderList[O.Identifier] is TObjectAnimation);
  Item := TObjectAnimation(ObjectRenderList[O.Identifier]);
  //ObjectBitmapItems.List^[O.Identifier];

  if odf_UpsideDown and O.DrawingFlags = 0
  then Src := Item.Original
  else Src := Item.Inverted;

  PrepareObjectBitmap(Src, O.DrawingFlags);

  SrcRect := Item.CalcFrameRect(aFrame);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  OffsetRect(DstRect, O.Left, O.Top);

  if aOriginal <> nil then
  begin
    IntersectRect(R, DstRect, aOriginal.BoundsRect); // oops important!
    aOriginal.DrawTo(Dst, R, R);
  end;
  Src.DrawTo(Dst, DstRect, SrcRect);
end;

procedure TRenderer.EraseObject(Dst: TBitmap32; O: TInteractiveObject; aOriginal: TBitmap32);
{-------------------------------------------------------------------------------
  Draws a interactive object
  o Dst = the targetbitmap
  o O = the object
  o aOriginal = if specified then first a part of this bitmap (world when playing)
    is copied to Dst to restore
-------------------------------------------------------------------------------}
var
  SrcRect, DstRect, R: TRect;
  Item: TObjectAnimation;// TDrawItem;
  Src: TBitmap32;
begin
  if aOriginal = nil then
    Exit;

  Assert(ObjectRenderList[O.Identifier] is TObjectAnimation);
  Item := TObjectAnimation(ObjectRenderList[O.Identifier]);
  //ObjectBitmapItems.List^[O.Identifier];

  SrcRect := Item.CalcFrameRect(0);
  DstRect := SrcRect;
  DstRect := ZeroTopLeftRect(DstRect);
  OffsetRect(DstRect, O.Left, O.Top);

  IntersectRect(R, DstRect, aOriginal.BoundsRect); // oops important!
  aOriginal.DrawTo(Dst, R, R);
end;


constructor TRenderer.Create;
begin
  inherited Create;
  TempBitmap := TBitmap32.Create;
  ObjectRenderList := TDrawList.Create;
end;

destructor TRenderer.Destroy;
begin
  TempBitmap.Free;
  ObjectRenderList.Free;
  inherited Destroy;
end;

procedure TRenderer.RenderWorld(World: TBitmap32; DoObjects: Boolean);
var
  i: Integer;
  Ter: TTerrain;
  Bmp: TBitmap32;
  Obj: TInteractiveObject;
//  Bmp: TBitmap32;
//  Item : TObjectBitmapItem;
  MO: TMetaObject;
//  tid: Integer;

begin
//  windlg([bit32str(alpha_terrain), bit32str(alpha_object)]);
  //windlg([alpha_terrain shr 24, alpha_object shr 24]);

  World.Clear(0);

  if inf.level=nil then exit;
  if inf.graphicset=nil then exit;

  with Inf do
  begin

    if Inf.GraphicSet.GraphicSetIdExt > 0 then
    begin
      Bmp := Inf.GraphicSet.SpecialBitmap;
      DrawSpecialBitmap(World, Bmp);
    end
    else begin
      with Level.Terrains.HackedList do
        for i := 0 to Count - 1 do
        begin
          Ter := List^[i];
          DrawTerrain(World, Ter);
        end;
    end;


    if DoObjects then
    with Level.InteractiveObjects.HackedList do
    begin

      for i := 0 to Count - 1 do
      begin
        Obj := List^[i];
        MO := Inf.GraphicSet.MetaObjects[obj.identifier];
        if odf_OnlyOnTerrain and Obj.DrawingFlags <> 0 then
          DrawObject(World, Obj, MO.PreviewFrameIndex);
      end;

      for i := 0 to Count - 1 do
      begin
        Obj := List^[i];
        MO := Inf.GraphicSet.MetaObjects[obj.identifier];
        if odf_OnlyOnTerrain and Obj.DrawingFlags = 0 then
          DrawObject(World, Obj, MO.PreviewFrameIndex);
      end;

    end;


  end;
end;

procedure TRenderer.PrepareGameRendering(const Info: TRenderInfoRec);
var
  i: Integer;
  Item: TObjectAnimation;
  Bmp: TBitmap32;
  MO: TMetaObject;
//  R: TRect;
begin

  Inf := Info;

  // create cache to draw from
  ObjectRenderList.Clear;

  with Inf, GraphicSet do
    for i := 0 to ObjectBitmaps.Count - 1 do
    begin
      MO := MetaObjects[i];
      Bmp := ObjectBitmaps.List^[i];

      //ReplaceColor(Bmp, 0, clBlue32);

      Item := TObjectAnimation.Create(Bmp, MO.AnimationFrameCount, MO.Width, MO.Height);
      ObjectRenderList.Add(Item);

//      Item.Normal.SetSizes(MO.AnimationFrameCount, MO.Width, MO.Height);
//      Item.Inverted.SetSizes(MO.AnimationFrameCount, MO.Width, MO.Height);
  //    Item.Normal.Assign(Bmp);
    //  Item.Inverted.Assign(Bmp);


      //Item.Inverted.FlipVertFrames;
    end;

end;

procedure TRenderer.Highlight(World: TBitmap32; M: TColor32);
var
  i: Integer;
  P: PColor32;
begin

  with World do
  begin
    P := PixelPtr[0, 0];
    for i := 0 to Width * Height - 1 do
    begin
      if P^ and M <> 0 then
        P^ := clRed32
      else
        P^ := 0;
      Inc(P);
    end;
  end;
end;

function TRenderer.HasPixelAt(X, Y: Integer): Boolean;
begin
  Result := fWorld.PixelS[X, Y] and ALPHA_TERRAIN = 0;
end;

procedure TRenderer.RenderAnimatedObject(O: TInteractiveObject; aFrame: Integer);
begin
//  windlg('renderobject')
end;

procedure TRenderer.DrawObjects;
begin
{
    with Level.InteractiveObjects.HackedList do
    begin

      for i := 0 to Count - 1 do
      begin
        Obj := List^[i];
        MO := Inf.GraphicSet.MetaObjects[obj.identifier];
        if odf_OnlyOnTerrain and Obj.DrawingFlags <> 0 then
          DrawObject(World, Obj, MO.PreviewFrameIndex);
      end;

      for i := 0 to Count - 1 do
      begin
        Obj := List^[i];
        MO := Inf.GraphicSet.MetaObjects[obj.identifier];
        if odf_OnlyOnTerrain and Obj.DrawingFlags = 0 then
          DrawObject(World, Obj, MO.PreviewFrameIndex);
      end;

    end;
 }
end;

procedure TRenderer.RenderMinimap(Dst, World: TBitmap32; aColor: TColor32);
var
  dx, dy, sx, sy: Integer;
begin
  (*
  sx := 0;
  sy := 0;
  dy := 0;
  dx := 0;
  repeat
    Inc(sx);
    if sx >
  until False; *)
end;

end.

