{$include lem_directives.inc}
unit LemLemminiLoader;

interface

uses
  Classes, SysUtils,
  LemTerrain,
  LemInteractiveObject,
  LemSteel,
  LemLevel;

type
  TLemminiLevelLoader = class(TLevelLoader)
  public
//    class procedure LoadLevel(aStream: TStream; aLevel: TLevel); override;
  end;

implementation

end.

uses
  UMisc, UFastStrings;

class procedure TLemminiLevelLoader.LoadLevel(aStream: TStream; aLevel: TLevel);

var
  List: TStringList;
  N, V: string;
  i, H: Integer;
  T: TTerrain;
  O: TInteractiveObject;
  S: TSteel;
//  TDF: TTerrainDrawingFlags;
//  ODF: TObjectDrawingFlags;

    // get integer from lemmini ini file
    function GetInt(const aName: string): Integer;
    begin
      Result := StrToIntDef(List.Values[aName], -1);
      if Result = -1 then
        if aName = 'numToRecue' then
           Result := StrToIntDef(List.Values['numToRescue'], -1);

      //Log([aName, Result]);
    end;

    function CheckSuperLemming: Boolean;
    begin
      Result := Trim(UpperCase(List.Values['superlemming'])) = 'TRUE';
    end;

    // get style
    function GetStyleNo: Integer;
    var
      i: Integer;
      aStyle: string;
    begin
      aStyle := List.Values['style'];
      raise exception.create('getstyle how are we going to do this');
      //Result := Style.graphbyname(aStyle).GraphicSetId;
    end;

    procedure GetGraphicSet;
    var
      i: Integer;
      aStyle: string;
    begin
      aStyle := List.Values['style'];
      //if fStyle = nil then
        //fStyle := StyleMgr.StyleList[STYLE_LEMMINI];
      raise exception.create('getgraphicset how are we going to do this');
      //fGraph := fStyle.GraphicSetList.FindByName(aStyle);
      //fGraph.EnsureMetaData;
    end;


begin
  with aLevel do
  begin
  List := TStringList.Create;
  BeginUpdate;
  try
    ClearLevel;
    List.NameValueSeparator := '=';
    List.LoadFromStream(aStream);
    {-------------------------------------------------------------------------------
      o First a little adjusting. TStringList is too dumb to recognize name/value pairs
        when spaces are involved.
      o Then delete all comments
    -------------------------------------------------------------------------------}
    for i := 0 to List.Count - 1 do
    begin
      V := List[i];
      repeat
        if FastPos(V, ' =', Length(V), 2, 1) = 0 then
          if FastPos(V, '= ', Length(V), 2, 1) = 0 then
            Break;
         V := FastReplace(V, ' =', '=');
         V := FastReplace(V, '= ', '=');
      until False;

      H := Pos('#', V);
      if H >= 1 then
      begin
//        Log(['voor', V]);
        V := Copy(V, 1, H - 1);
  //      Log(['na', V]);
      end;

      List[i] := V;//FastReplace(List[i], ' = ', '=');
    end;


    GetGraphicSet;

    with LevelInfo do
    begin
      ReleaseRate      := GetInt('releaseRate');
      LemmingsCount    := GetInt('numLemmings');
      RescueCount      := GetInt('numToRecue');
      TimeLimit        := GetInt('timeLimit');
      ClimberCount     := GetInt('numClimbers');
      FloaterCount     := GetInt('numFloaters');
      BomberCount      := GetInt('numBombers');
      BlockerCount     := GetInt('numBlockers');
      BuilderCount     := GetInt('numBuilders');
      BasherCount      := GetInt('numBashers');
      MinerCount       := GetInt('numMiners');
      DiggerCount      := GetInt('numDiggers');
      ScreenPosition   := GetInt('xPos');
      assert(false);
      GraphicSet       := 123;//Graph.GraphicSetId;

      Superlemming     := CheckSuperLemming;
    end;

    {-------------------------------------------------------------------------------
      Get the objects

      # Objects
      # id, xpos, ypos, paint mode (), upside down (0,1)
      # paint modes: 8=VIS_ON_TERRAIN, 4=NO_OVERWRITE, 0=FULL (only one value possible)
      object_0 = 6, 1664, 64, 4, 0
      object_1 = 5, 1152, 288, 0, 0
    -------------------------------------------------------------------------------}
    i := 0;
    repeat
      V := List.Values['object_' + i2s(i)];
      if V = '' then
        Break;
      // paint modes: 8=VIS_ON_TERRAIN, 4=NO_OVERWRITE, 0=FULL (only one value possible)
      O := InteractiveObjects.Add;
      O.Identifier := StrToIntDef(SplitString(V, 0, ','), -1);
      O.Left  := StrToIntDef(SplitString(V, 1, ','), -1);
      O.Top := StrToIntDef(SplitString(V, 2, ','), -1);
      ODF := [];
      H := StrToIntDef(SplitString(V, 3, ','), -1);
      if H = -1 then
        raise Exception.Create('Lemming ini file object drawingflag error');
      if H and 8 <> 0 then
        Include(ODF, odfOnlyShowOnTerrain);
      if H and 4 <> 0 then
        Include(ODF, odfNoOverwrite);
      H := StrToIntDef(SplitString(V, 4, ','), -1);
      if H = -1 then
        raise Exception.Create('Lemming ini file upside down indicator error');
      if H = 1 then
        Include(ODF, odfInvert);
      O.ObjectDrawingFlags := ODF;
      Inc(i);
    until False;

    {-------------------------------------------------------------------------------
      Get the terrain

      # Terrain
      # id, xpos, ypos, modifier
      # modifier: 8=NO_OVERWRITE, 4=UPSIDE_DOWN, 2=REMOVE (combining allowed, 0=FULL)
      terrain_0 = 7, 1360, 130, 0
      terrain_1 = 7, 1420, 130, 0
    -------------------------------------------------------------------------------}
    i := 0;
    repeat
      V := List.Values['terrain_' + i2s(i)];
      if V = '' then
        Break;
      T := TerrainCollection.Add;
      T.TerrainId := StrToIntDef(SplitString(V, 0, ','), -1);
      T.TerrainX  := StrToIntDef(SplitString(V, 1, ','), -1);
      T.TerrainY := StrToIntDef(SplitString(V, 2, ','), -1);
      H := StrToIntDef(SplitString(V, 3, ','), -1);
      if H = -1 then
        raise Exception.Create('Lemming ini file terrain drawingflag error (' + i2s(i) + ')');
      // 8=NO_OVERWRITE, 4=UPSIDE_DOWN, 2=REMOVE
      TDF := [];
      if H and 8 <> 0 then
        Include(TDF, tdfNoOverwrite);
      if H and 4 <> 0 then
        Include(TDF, tdfInvert);
      if H and 2 <> 0 then
        Include(TDF, tdfErase);
      T.TerrainDrawingFlags := TDF;
      //Log(['terrain', i]);
      Inc(i);
    until False;

    {-------------------------------------------------------------------------------
      Get the steel

      #Steel
      # id, xpos, ypos, width, height
      steel_0 = 1328, 80, 48, 128
      steel_1 = 1256, 80, 48, 128
    -------------------------------------------------------------------------------}
    i := 0;
    repeat
      V := List.Values['steel_' + i2s(i)];
      if V = '' then
        Break;
      S := SteelCollection.Add;
      S.SteelX  := StrToIntDef(SplitString(V, 0, ','), -1);
      S.SteelY := StrToIntDef(SplitString(V, 1, ','), -1);
      S.SteelWidth := StrToIntDef(SplitString(V, 2, ','), -1);
      S.SteelHeight := StrToIntDef(SplitString(V, 3, ','), -1);
      Inc(i);
    until False;

    {-------------------------------------------------------------------------------
      Get the name

      #Name
      name =  Worra load of old blocks!
    -------------------------------------------------------------------------------}
    LevelName := Trim(List.Values['name']);


    //if fResolution = LoRes then
      //AdjustResolution(0.5);
    CacheSizes;
  finally
    List.Free;
    EndUpdate;
  end;

  end;

end;

end.

