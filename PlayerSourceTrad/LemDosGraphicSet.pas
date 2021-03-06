{$include lem_directives.inc}
unit LemDosGraphicSet;

interface

uses
  Classes, SysUtils,
  GR32,
  UMisc,
  LemTypes,
  LemMetaObject,
  LemMetaTerrain,
  LemGraphicSet,
  LemDosStructures,
  LemDosBmp,
  LemDosCmp,
  LemDosMisc;

type
  TBaseDosGraphicSet = class(TBaseGraphicSet)
  private
  protected
    fMetaInfoFile    : string; // ground?.dat
    fGraphicFile     : string; // vgagr?.dat
    fGraphicExtFile  : string; // vgaspec?.dat
    fMetaInfoFN    : string; // ground?.dat
    fGraphicFN     : string; // vgagr?.dat
    fGraphicExtFN  : string; // vgaspec?.dat
    fPaletteCustom   : TArrayOfColor32;
    fPaletteStandard : TArrayOfColor32;
    fPalettePreview  : TArrayOfColor32;
    fPalette         : TArrayOfColor32;
    fBrickColor      : TColor32;
    fExtPal          : Byte;
    fExtLoc          : Boolean;
    procedure DoReadMetaData; override;
    procedure DoReadData; override;
    procedure DoClearMetaData; override;
    procedure DoClearData; override;
  public
    property PaletteCustom: TArrayOfColor32 read fPaletteCustom;
    property PaletteStandard: TArrayOfColor32 read fPaletteStandard;
    property PalettePreview: TArrayOfColor32 read fPalettePreview;
    property Palette: TArrayOfColor32 read fPalette;
    property BrickColor: TColor32 read fBrickCOlor;
  published
    property MetaInfoFile: string read fMetaInfoFile write fMetaInfoFile;
    property GraphicFile: string read fGraphicFile write fGraphicFile;
    property GraphicExtFile: string read fGraphicExtFile write fGraphicExtFile;
    property MetaInfoFN: string read fMetaInfoFN write fMetaInfoFN;
    property GraphicFN: string read fGraphicFN write fGraphicFN;
    property GraphicExtFN: string read fGraphicExtFN write fGraphicExtFN;
  end;


implementation

{ TBaseDosGraphicSet }

procedure TBaseDosGraphicSet.DoReadMetaData;
{-------------------------------------------------------------------------------
  Read metadata from the ground??.dat file.
-------------------------------------------------------------------------------}
var
  G, G2: TDosGroundRec;
  O: PDosMetaObject;
  T: PDosMetaTerrain;
  MO: TMetaObject;
  MT: TMetaTerrain;
  i: Integer;
//  TempColor: TColor32;

    procedure LoadDosGroundRec;
    var
      D: TStream;
      i: integer;
      b: ^byte;
    begin
      {$ifdef external}if FileExists(fMetaInfoFN) then
        D := TFileStream.Create(fMetaInfoFN, fmOpenRead)
        else{$endif}
        D := CreateDataStream(fMetaInfoFile, ldtLemmings);
      try
        D.ReadBuffer(G, SizeOf(G));
        if D.Size > SizeOf(G) then D.ReadBuffer(G2, SizeOf(G))
        else begin
          b := @G2;
          for i := 0 to SizeOf(G2) - 1 do
          begin
            b^ := 0;
            Inc(b);
          end;
        end;
      finally
        D.Free;
      end;
    end;

    function LoadSpecPalette: TDosVGAPalette8;
    var
      SpecBmp: TVgaSpecBitmap;
      DataStream: TStream;
    begin
      SpecBmp := TVgaSpecBitmap.Create;
      {$ifdef external}if FileExists(GraphicExtFN) then
        DataStream := TFileStream.Create(GraphicExtFN, fmOpenRead)
        else{$endif}
        DataStream := CreateDataStream(GraphicExtFile, ldtLemmings); //fuckface
      try
        SpecBmp.LoadPaletteFromStream(DataStream, Result);
      finally
        SpecBmp.Free;
        DataStream.Free;
      end;
    end;

    procedure AssemblePalette;
    {-------------------------------------------------------------------------------
      Concatenate the fixed palette and the loaded custompalette.
      Then copy the first customcolor to the last fixed color (bridges, minimap).
      For special graphics we use a hardcoded color for now.
    -------------------------------------------------------------------------------}
    var
      i: Integer;
    begin
      SetLength(fPalette, 32);
      for i := 0 to 7 do
        fPalette[i] := DosPaletteEntryToColor32(DosInlevelPalette[i]);
      for i := 8 to 15 do
        fPalette[i] := fPaletteCustom[i - 8];
      for i := 16 to 23 do
        fPalette[i] := fPaletteStandard[i - 16];
      for i := 24 to 31 do
        fPalette[i] := fPalettePreview[i - 24];
      if GraphicSetIdExt > 0 then
        fPalette[8] := Color32(124, 124, 0, 0);
      fPalette[7] := fPalette[8];
      fBrickColor := fPalette[7];
    end;


begin
  LoadDosGroundRec;
  with G do
  begin
    // convert palettes
    DosVgaPalette8ToLemmixPalette(VGA_PaletteCustom,   fPaletteCustom);
    DosVgaPalette8ToLemmixPalette(VGA_PaletteStandard, fPaletteStandard);
    DosVgaPalette8ToLemmixPalette(VGA_PalettePreview,  fPalettePreview);

    // if special graphic then overwrite the custom palette
    if GraphicSetIdExt > 0 then
      DosVgaPalette8ToLemmixPalette(LoadSpecPalette, fPaletteCustom);

    // make 16 color palette
    AssemblePalette;

    // meta objects
    for i := 0 to 31 do
    begin
      if i < 16 then
        O := @ObjectInfoArray[i]
        else
        O := @G2.ObjectInfoArray[i-16];
      if O^.oWidth = 0 then
        Break;
      MO := MetaObjects.Add;
      with MO do
      begin
        if i = 0 then
          begin
           //check extended properties
           if ((O^.oAnimation_flags and $0100) <> 0) then fExtPal := 1 else fExtPal := 0;
           //if ((O^.oAnimation_flags and $0200) <> 0) then fAdjTrig := true else fAdjTrig := false;
           if ((O^.oAnimation_flags and $0800) <> 0) then
           begin
             fPalette[1] := $D02020;
             fPalette[4] := $F0F000;
             fPalette[5] := $4040E0;
           end;
           if ((O^.oAnimation_flags and $2000) <> 0) then fExtLoc := true else fExtLoc := false;
           if ((O^.oAnimation_flags and $4000) <> 0) then fExtPal := 15;
           O^.oAnimation_flags := O^.oAnimation_flags and 3;
          end;
        AnimationType            := O^.oAnimation_flags;
        StartAnimationFrameIndex := O^.oStart_animation_frame_index;
        AnimationFrameCount      := O^.oAnimation_frame_count;
        AnimationFrameDataSize   := O^.oAnimation_frame_data_size;
        MaskOffsetFromImage      := O^.oMask_offset_from_image;
        Width                    := O^.oWidth;
        Height                   := O^.oHeight;
        TriggerLeft              := O^.oTrigger_left * 4; // encoded
        TriggerTop               := O^.oTrigger_top * 4 - 4; // encoded
        TriggerWidth             := O^.oTrigger_width * 4; // encoded
        TriggerHeight            := O^.oTrigger_height * 4; // encoded
        TriggerEffect            := O^.oTrigger_effect_id;
        AnimationFramesBaseLoc   := O^.oAnimation_frames_base_loc;
        if fExtLoc then AnimationFramesBaseLoc := AnimationFramesBaseLoc + (O^.oUnknown1 shl 16);
        PreviewFrameIndex        :=
          (O^.oPreview_image_location - O^.oAnimation_frames_base_loc) div O^.oAnimation_frame_data_size;
        SoundEffect              := O^.oSound_effect_id;
      end;
    end;

    // if extended graphic then no terrain
    if fGraphicSetIdExt <> 0 then
      Exit;

    // meta terrains
    for i := 0 to 127 do
    begin
      if i < 64 then
        T := @TerrainInfoArray[i]
        else
        T := @G2.TerrainInfoArray[i-64];
      if T^.tWidth = 0 then
        Break;
      MT := MetaTerrains.Add;
      with MT do
      begin
        Width := T^.tWidth;
        Height := T^.tHeight;
        ImageLocation := T.tImage_loc;
        if fExtLoc then ImageLocation := ImageLocation + (((T.tUnknown1 mod 256) shr 1) shl 16);
      end;
    end;

  end; // with G
end;

procedure TBaseDosGraphicSet.DoReadData;
{-------------------------------------------------------------------------------
  read all terrain- and object bitmaps from dos file
-------------------------------------------------------------------------------}
var
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  Mem: TMemoryStream;
  Planar: TDosPlanarBitmap;
  T: TMetaTerrain;
  MO: TMetaObject;
  Bmp: TBitmap32;
  i, f, y, loc: Integer;
  FrameBitmap: TBitmap32;
  DataStream: TStream;
  SpecBmp: TVgaSpecBitmap;
begin
  DataStream := nil;

  Sections := TDosDatSectionList.Create;
  try
    // first decompress all data into the sectionlist
    Decompressor := TDosDatDecompressor.Create;
    try
      {$ifdef external}if FileExists(fGraphicFN) then
        DataStream := TFileStream.Create(fGraphicFN, fmOpenRead)
        else{$endif}
        DataStream := CreateDataStream(fGraphicFile, ldtLemmings); //fuckface
      Decompressor.LoadSectionList(DataStream, Sections, True);
    finally
      Decompressor.Free;
      DataStream.Free;
    end;

    // then load the terrains and objects
    Planar := TDosPlanarBitmap.Create;
    FrameBitmap := TBitmap32.Create;
    try

      // get terrains from the first section
      if GraphicSetIdExt = 0 then
      begin
        Mem := Sections[0].DecompressedData;
        with MetaTerrains.HackedList do
          for i := 0 to Count - 1 do
          begin
            T := List^[i];
            Bmp := TBitmap32.Create;
            TerrainBitmaps.Add(Bmp);
            Planar.LoadFromStream(Mem, Bmp, T.ImageLocation, T.Width, T.Height, 4 + fExtPal, fPalette);
          end;
      end
      // or get the terrainbitmap from the vgaspec
      else begin
        SpecBmp := TVgaSpecBitmap.Create;
        try
          {$ifdef external}if FileExists(GraphicExtFN) then
            DataStream := TFileStream.Create(GraphicExtFN, fmOpenRead)
            else{$endif}
            DataStream := CreateDataStream(GraphicExtFile, ldtLemmings); //fuckface
          try
            SpecBmp.LoadFromStream(DataStream, SpecialBitmap);
          finally
            DataStream.Free;
          end;
        finally
          SpecBmp.Free;
        end;
      end;

      // get objects from the second section
      Mem := Sections[1].DecompressedData;
      with MetaObjects.HackedList do
        for i := 0 to Count - 1 do
        begin
          MO := List^[i];
          Bmp := TBitmap32.Create;
          Bmp.SetSize(MO.Width, MO.Height * MO.AnimationFrameCount);
          ObjectBitmaps.Add(Bmp);
          y := 0;
          Loc := MO.AnimationFramesBaseLoc;

          // load all animation frames and glue together
          for f := 0 to MO.AnimationFrameCount - 1 do
          begin
            Planar.LoadFromStream(Mem, FrameBitmap, Loc, MO.Width, MO.Height, 4 + fExtPal, fPalette);
            FrameBitmap.DrawTo(Bmp, 0, Y);
            //FrameBitmap.SaveToFile('d:\temp\' + 'frame_' + LeadZeroStr(i, 2) + '_' + LeadZeroStr(f, 2) + '.bmp');
            Inc(Y, MO.Height);
            Inc(Loc, MO.AnimationFrameDataSize);
          end;
          //Bmp.SaveToFile('d:\temp\' + 'obj_' + LeadZeroStr(i, 2) + '.bmp');
        end;

    finally
      Planar.Free;
      FrameBitmap.Free;
    end;

  finally
    Sections.Free;
  end;

end;


procedure TBaseDosGraphicSet.DoClearData;
begin
  inherited DoClearData;
end;

procedure TBaseDosGraphicSet.DoClearMetaData;
begin
  inherited DoClearMetaData;
  fMetaInfoFile    := '';
  fGraphicFile     := '';
  fGraphicExtFile  := '';
  fPaletteCustom   := nil;
  fPaletteStandard := nil;
  fPalettePreview  := nil;
  fPalette         := nil;
  fBrickColor      := 0;
end;

end.

