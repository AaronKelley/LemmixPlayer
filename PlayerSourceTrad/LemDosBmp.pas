{$include lem_directives.inc}

// code onder end bevat ook nog oude code om te saven!!!!!!!!!!!!!
// en nog wat zooi. voorzichtig weggooien.

unit LemDosBmp;

interface

uses
  Classes, Types, SysUtils, Contnrs, Math,
  UMisc,
  GR32, GR32_OrdinalMaps,
  LemStrings,
  LemTypes,
  LemDosStructures,
  LemDosCmp;

type
  TColor32Array8  = array[0..7] of TColor32;
  TColor32Array16 = array[0..15] of TColor32;
//  TPlaneFilter    = set of 0..7;

  {-------------------------------------------------------------------------------
    planar bitmaps in the dosfiles are stored per plane, not interlaced
  -------------------------------------------------------------------------------}
  TDosPlanarBitmap = class
  private
    //fDirectRGB: Boolean;
  protected
  public

  { load new }
    procedure GetByteMap(S: TStream; aByteMap: TByteMap; aPos, aWidth, aHeight: Integer; BPP: Byte);

    procedure LoadFromFile(const aFilename: string; aBitmap: TBitmap32;
      aPos, aWidth, aHeight: Integer; BPP: Byte; const aPalette: TArrayOfColor32); overload;

    procedure LoadFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer;
      BPP: Byte; const aPalette: TArrayOfColor32); overload;

    procedure LoadAnimationFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight, aFrameCount: Integer;
      BPP: Byte; const aPalette: TArrayOfColor32);


  published
  end;

  {-------------------------------------------------------------------------------
    class to extract the "special" bitmaps from dos-files.
    can also create vgaspec-files
  -------------------------------------------------------------------------------}
  TVgaSpecBitmap = class
  private
{$ifdef vgaspecwrite}
    fOnCompressProgress: TProgressEvent;
{$endif}
  { reading methods}
    procedure GetSectionsAndPalette(Src, Dst: TStream; var Pal: TDosVGAPalette8; var PalInfo: TDosVgaSpecPaletteHeader);
{$ifdef vgaspecwrite}
  { writing methods }
    procedure DoCompressProgress(aPosition, aMaximum: Integer);
    procedure CheckBitmap(aBitmap: TBitmap32);
    procedure CreatePal(aBitmap: TBitmap32; var Pal: TDosVgaSpecPaletteHeader);
{$endif}
  public
  { reading methods }
    function DecodeSection(var Src, Dst: PBytes; SrcSize: Integer): Integer;
    procedure LoadFromFile(const aFileName: string; aBitmap: TBitmap32);
    procedure LoadFromStream(S: TStream; aBitmap: TBitmap32);
    procedure LoadPaletteFromFile(const aFileName: string; var Pal: TDosVGAPalette8);
    procedure LoadPaletteFromStream(S: TStream; var Pal: TDosVGAPalette8);
{$ifdef vgaspecwrite}
  { writing methods }
    function EncodeSection(var Src, Dst: PBytes; SrcSize: Integer): Integer;
    procedure EncodeSectionStream(Src, Dst: TStream);
    procedure SaveToFile(aBitmap: TBitmap32; const aFileName: string);
    procedure SaveToStream(aBitmap: TBitmap32; Dst: TStream);
  { progress when compressing, this takes a few seconds! }
    property OnCompressProgress: TProgressEvent read fOnCompressProgress write fOnCompressProgress;
{$endif}
  end;


function DosPaletteEntryToColor32(Red, Green, Blue: Byte): TColor32; overload;
function DosPaletteEntryToColor32(const Entry: TDOSVGAColorRec): TColor32; overload;
function DosPaletteToArrayOfColor32(const Pal: TDosVGAPalette8): TArrayOfColor32;
function Color32ToDosPaletteEntry(C: TColor32): TDOSVGAColorRec;

const
  VGASPEC_SECTIONSIZE = 14400;
  VGASPEC_SECTIONSIZE_EXT = 86400;
implementation


function DosPaletteEntryToColor32(Red, Green, Blue: Byte): TColor32;
begin
  with TColor32Entry(Result) do
  begin
    A := $FF;
    R := Red shl 2;
    G := Green shl 2;
    B := Blue shl 2;
    // handle transparancy
    if (R = 0) and (G = 0) and (B = 0) then
      A := 0
    else
      A := $FF;


(*
    for i := 0 to 7 do
    begin
      E := @LemmixPal[i];
      D := @DosPal[i];
      with TColor32Entry(LemmixPal[i]) do
      begin
        E^.A := 0;
        E^.R := (Integer(D^.R) * 255) div 63;
        E^.G := (Integer(D^.G) * 255) div 63;
        E^.B := (Integer(D^.B) * 255) div 63;
      end;
    end;
*)



  end;
end;

function DosPaletteEntryToColor32(const Entry: TDOSVGAColorRec): TColor32;
begin
  with Entry do
    Result := DosPaletteEntryToColor32(R, G, B);
end;

function Color32ToDosPaletteEntry(C: TColor32): TDOSVGAColorRec;
begin
  with Result do
  begin
    R := TColor32Entry(C).R shr 2;
    G := TColor32Entry(C).G shr 2;
    B := TColor32Entry(C).B shr 2;
  end;
end;

function DosPaletteToArrayOfColor32(const Pal: TDosVGAPalette8): TArrayOfColor32;
var
  i: Integer;
begin
  SetLength(Result, Length(Pal));
  for i := 0 to Length(Pal) - 1 do
  begin
    Result[i] := DosPaletteEntryToColor32(Pal[i]);
  end;
end;

{ TDosPlanarBitmap }

procedure TDosPlanarBitmap.LoadFromFile(const aFilename: string;
  aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer; BPP: Byte;
  const aPalette: TArrayOfColor32);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F, aBitmap, aPos, aWidth, aHeight, BPP, aPalette);
  finally
    F.Free;
  end;
end;

const
  ALPHA_TRANSPARENTBLACK = $80000000;

procedure TDosPlanarBitmap.LoadFromStream(S: TStream; aBitmap: TBitmap32;
  aPos, aWidth, aHeight: Integer; BPP: Byte;
  const aPalette: TArrayOfColor32);
{-------------------------------------------------------------------------------
  load bytemap and convert it to bitmap, using the palette parameter
-------------------------------------------------------------------------------}
var
  ByteMap: TByteMap;
  x, y: Integer;
  C: PColor32;
  B: Types.PByte;
  PalLen: Integer;

    procedure PreparePal;
    var
      i: Integer;
    begin
      for i := 1 to PalLen - 1 do
        if aPalette[i] = 0 then
          aPalette[i] := aPalette[i] or ALPHA_TRANSPARENTBLACK;
    end;

begin
  PalLen := Length(aPalette);
  PreparePal;
//  Assert(PalLen >= BPP div 8)

  ByteMap := TByteMap.Create;
  try
    GetByteMap(S, ByteMap, aPos, aWidth, aHeight, BPP);
    aBitmap.SetSize(aWidth, aHeight);
    aBitmap.Clear(0);
    C := aBitMap.PixelPtr[0, 0];
    B := ByteMap.ValPtr[0, 0];
    for y := 0 to aHeight - 1 do
      for x := 0 to aWidth - 1 do
      begin
        if not (BPP in [18, 19]) then Assert(B^ < PalLen, 'error color ' + i2s(B^) + ',' + i2s(PalLen));
        (*
        if B^ >= PalLen then
        begin
          C^ := aPalette[B^ - 8];
          //C^ := clYellow32
          windlg(['this is not good yet', x , y, B^, pallen]);
        end
        else *)
        if BPP in [18, 19] then
        begin
          C^ := B^ shl 18;
          Inc(B);
          C^ := C^ + (B^ shl 10);
          Inc(B);
          C^ := C^ + (B^ shl 2);
          Inc(B);
          if BPP = 18 then
          begin
            if C^ <> 0 then C^ := C^ or $FF000000;
          end else begin
            if (B^ and $1 <> 0) then
              C^ := C^ or $FF000000
              else
              C^ := 0;
          end;
        end else
          C^ := aPalette[B^];
        Inc(C); // typed pointer increment
        Inc(B); // typed pointer increment
      end;

  finally
    ByteMap.Free;
  end;
end;


procedure TDosPlanarBitmap.GetByteMap(S: TStream; aByteMap: TByteMap;
  aPos, aWidth, aHeight: Integer; BPP: Byte);
{-------------------------------------------------------------------------------
  This should be the key routine: it converts a planar stored bitmap to
  a simple two-dimensional palette entry array.
  We use the Graphics32 TByteMap for that.
-------------------------------------------------------------------------------}
var
  NumBytes : Integer;
  Buf: PBytes;
  PlaneSize: Integer;
  LineSize: Integer;

    // local function
    function GetPix(X, Y: Integer): LongWord;
    var
      BytePtr: PByte;
      //B: Byte;
      i, P: Integer;
      BitNumber, Mask: Byte;
    begin
      Result := 0;
      // adres of pixel in the first plane
      P := Y * LineSize + X div 8;
      // get the right bit (Wow, totally excellent, its backwards !)
      BitNumber := 7 - X mod 8; // downwards!
      Mask := 1 shl BitNumber;
      // get the seperated bits from the planes
      BytePtr := @Buf^[P];
      for i := 0 to BPP - 1 do
      begin
        if BytePtr^ and Mask <> 0 then
          Result := Result or (1 shl i);
        Inc(BytePtr, PlaneSize);
      end;
    end;

var
  x, y: Integer;
  Entry: LongWord;
begin
  Assert(BPP in [1..8, 18, 19], 'bpp error');

  LineSize := aWidth div 8; // Every bit is a pixel (in each plane)
  PlaneSize := LineSize * aHeight; // Now we know the planesize
  NumBytes := PlaneSize * BPP; // and the total bytes

  GetMem(Buf, NumBytes);
  if aPos >= 0 then
    S.Seek(aPos, soFromBeginning);
  S.ReadBuffer(Buf^, NumBytes);
  if BPP in [18, 19] then
    aByteMap.SetSize(aWidth * 4, aHeight)
    else
    aByteMap.SetSize(aWidth, aHeight);
  aByteMap.Clear(0);

  for y := 0 to aHeight-1 do
  begin
    for x := 0 to aWidth - 1 do
    begin
      Entry := GetPix(x, y);
      if BPP in [18, 19] then
      begin
        aByteMap[(x*4), y] := Entry shr 12;
        aByteMap[(x*4)+1, y] := (Entry shr 6) mod $40;
        aByteMap[(x*4)+2, y] := Entry mod $40;
        if BPP = 18 then
        begin
          if (Entry and $FFFFFF) = 0 then
            aByteMap[(x*4)+3, y] := 0
            else
            aByteMap[(x*4)+3, y] := 1;
        end else
          aByteMap[(x*4)+3, y] := (Entry shr 18) mod 2;
      end else
        aByteMap[x, y] := Entry;
    end;
  end;

  FreeMem(Buf);
end;


procedure TDosPlanarBitmap.LoadAnimationFromStream(S: TStream;
  aBitmap: TBitmap32; aPos, aWidth, aHeight, aFrameCount: Integer;
  BPP: Byte; const aPalette: TArrayOfColor32);
{-------------------------------------------------------------------------------
  We assume that
  o frames are in the stream in a row
  o same size
  o same palette
  o same BPP
-------------------------------------------------------------------------------}

var
  FrameBitmap: TBitmap32;
  i: Integer;
begin
  // set initial position, the rest is read automatically
  if aPos >= 0 then
    S.Seek(aPos, soFromBeginning);

  FrameBitmap := TBitmap32.Create;
  try

    aBitmap.SetSize(aWidth, aHeight * aFrameCount);
    aBitmap.Clear(0);

    for i := 0 to aFrameCount - 1 do
    begin
      LoadFromStream(S, FrameBitmap, -1, aWidth, aHeight, BPP, aPalette);
      FrameBitmap.DrawTo(aBitmap, 0, aHeight * i);
    end;

  finally
    FrameBitmap.Free;
  end;

end;

{ TVgaSpecBitmap }


procedure TVgaSpecBitmap.GetSectionsAndPalette(Src, Dst: TStream; var Pal: TDosVGAPalette8; var PalInfo: TDosVgaSpecPaletteHeader);
var
  CurByte: Byte;
  {i, }{CSize, }Cnt, Rd, {Wr, }CurSection: Integer;
  Value: Byte;
  normsize: Boolean;
  Buf: PBytes;
begin
  Buf := nil;
  Src.Seek(0, soFromBeginning);
  Dst.Seek(0, soFromBeginning);
  //CSize := Src.Size;
  Src.Read(PalInfo, Sizeof(PalInfo));
  Pal := PalInfo.VgaPal;
  normsize := PalInfo.EgaPal[1] <> 255;
  CurSection := 0;

  try
    repeat
      Rd := Src.Read(CurByte, 1);
      if Rd <> 1 then
        Break;
      case CurByte of
        // end section
        128:
          begin
            if ((Dst.Position mod VGASPEC_SECTIONSIZE <> 0) and (Dst.Position mod VGASPEC_SECTIONSIZE_EXT <> 0)) and normsize then
              raise Exception.Create('vga spec section size error');
            //deb(['currsection', cursection, src.position]);
            Inc(CurSection);
            if (CurSection > 3) or not normsize then
              Break;
          end;
        // raw bytes
        0..127:
          begin
            Cnt := CurByte + 1;
            Dst.CopyFrom(Src, Cnt);
          end;
        // repeated bytes
        129..255:
          begin
            Cnt := 257 - CurByte;
            ReallocMem(Buf, Cnt); // we could use just a 256 byte buffer or so, no realloc needed
            Src.Read(Value, 1);
            FillChar(Buf^, Cnt, Value);
            Dst.Write(Buf^, Cnt);
          end;
      end; //case
    until False;

  finally
    FreeMem(Buf);
  end;

{  dst.seek(0,sofrombeginning);
  with tfilestream.create('d:\allsecs.tmp', fmcreate) do
  begin
    copyfrom(dst, 0);
    free;
  end; }

end;


function TVgaSpecBitmap.DecodeSection(var Src, Dst: PBytes; SrcSize: Integer): Integer;
var
  CodeByte, Value: Byte;
  i, si, di, Cnt, Allocated: Integer;

    procedure EnsureMem(aSize: Integer; Exact: Boolean = False);
    begin
      case Exact of
        False:
          if aSize > Allocated then
          begin
            ReallocMem(Dst, aSize);
            Allocated := aSize;
          end;
        True:
          if aSize <> Allocated then
          begin
            ReallocMem(Dst, aSize);
            Allocated := aSize;
          end;
      end;
    end;

begin
  Result := 0;
  si := 0;
  di := 0;
  Allocated := 0;
  EnsureMem(SrcSize, True);
  FillChar(Dst^, Allocated, 0);
  while si <= SrcSize - 1 do
  begin
    CodeByte := Src^[si];
    case CodeByte of
      // end section
      128:
        begin
          Exit;
        end;
      // raw bytes
      0..127:
        begin
          Cnt := CodeByte + 1;
          Inc(si);
          EnsureMem(di + Cnt);
          Move(Src^[si], Dst^[di], Cnt);
          Inc(si, Cnt);
          Inc(di, Cnt);
        end;
      // repeated bytes
      129..255:
        begin
          Cnt := 257 - CodeByte;
          Inc(si);
          EnsureMem(di + Cnt);
          Value := Src^[si];
          for i := 0 to Cnt - 1 do
            Dst^[di + i] := Value;
          Inc(di, Cnt);
          Inc(si);
        end;
    end; //case

  end;

  EnsureMem(di, True);
  Result := di;
end;


procedure TVgaSpecBitmap.LoadFromFile(const aFileName: string; aBitmap: TBitmap32);
{-------------------------------------------------------------------------------
  method to load a bitmap from the vgaspec?.dat files
-------------------------------------------------------------------------------}
var
  F: TFileStream;
begin
  if aBitmap = nil then
    Exit;
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F, aBitmap);
  finally
    F.Free;
  end;
end;

procedure TVgaSpecBitmap.LoadFromStream(S: TStream; aBitmap: TBitmap32);
{-------------------------------------------------------------------------------
  So here we are at the decoding of vgaspec?.dat:
  o Step 1: Decompress with the "default" dos lemming decompression code
  o Step 2: Get the vga-palette from the first few bytes
  o Step 3: Decode 4 sections with the "bitmap" decompression code
  o Step 4: Now in each of the 4 sections, which should be 14400 bytes, extract
            a planar bitmap (3 BPP, 960x40)
  o Step 5: Create one big bitmap of this 4 planar bitmaps
-------------------------------------------------------------------------------}
var
  Decompressor: TDosDatDecompressor;
  Mem, PMem: TMemoryStream;
//  Header: TCompressionHeaderRec;
//  InSize, OutSize: Integer;
//  InBuf, OutBuf: PBytes;
//  CheckSum: Integer;
  Planar: TDosPlanarBitmap;
  TempBitmap: TBitmap32;
  Sec: Integer;
  DosPal: TDosVGAPalette8;
  Pal: TArrayOfColor32;
  PalInfo: TDosVgaSpecPaletteHeader;
  normsize: Boolean;



    procedure LogPal;
    var
      i: Integer;
    begin
      deb(['spec pal']);
      for i := 0 to 7 do
      with TColor32Entry(pal[i]) do
      begin
        deb(['RGB (' + i2s(i) + ')' + i2s(r) + ',' + i2s(g) +',' + i2s(b)])
      end;
    end;



begin
  Assert(aBitmap <> nil);

  //Mem := nil;
  //Decompressor := nil;

  Decompressor := TDosDatDecompressor.Create;
  Mem := TMemoryStream.Create;
  try

    { step 1: decompress }
    try
      Decompressor.DecompressSection(S, Mem);
    finally
      Decompressor.Free;
    end;

    PMem := TMemoryStream.Create;
    TempBitmap := TBitmap32.Create;
    try
      { step 2 + 3 : getpalette, extract 4 sections }
      GetSectionsAndPalette(Mem, PMem, DosPal, PalInfo);
      Pal := DosPaletteToArrayOfColor32(DosPal);
      normsize := PalInfo.EgaPal[1] <> 255;

      logpal;

      Planar := TDosPlanarBitmap.Create;
      try
      if normsize then
      begin
        aBitmap.SetSize(960, 160);
        aBitmap.Clear(0); // clear with #transparent black
        for Sec := 0 to 3 do
        begin
          { step 4: read planar bitmap part from section }
          TempBitmap.Clear(0); // clear with #transparent black
          //Planar.LoadFromStream(PMem, TempBitmap, Sec * VGASPEC_SECTIONSIZE, 960, 40, 3, Pal, Pal);
          if DosPal[1].R = 255 then
            Planar.LoadFromStream(PMem, TempBitmap, Sec * VGASPEC_SECTIONSIZE_EXT, 960, 40, 18, Pal)
            else
            Planar.LoadFromStream(PMem, TempBitmap, Sec * VGASPEC_SECTIONSIZE, 960, 40, 3, Pal);
          { step 5: draw to bitmap }
          aBitmap.Draw(0, Sec * 40, TempBitmap);
        end;
      end else begin

        aBitmap.SetSize((PalInfo.UnknownPal[0] * 256) + PalInfo.UnknownPal[1], (PalInfo.UnknownPal[2] * 256) + PalInfo.UnknownPal[3]);
        aBitmap.Clear(0);
        if DosPal[1].R = 255 then
            Planar.LoadFromStream(PMem, aBitmap, 0, aBitmap.Width, aBitmap.Height, 18, Pal)
            else
            Planar.LoadFromStream(PMem, aBitmap, 0, aBitmap.Width, aBitmap.Height, 3, Pal);
      end;
      finally
        Planar.Free;
      end;
    finally
      PMem.Free;
      TempBitmap.Free;
    end;

   finally
     Mem.Free;
   end;
end;

procedure TVgaSpecBitmap.LoadPaletteFromFile(const aFileName: string; var Pal: TDosVGAPalette8);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadPaletteFromStream(F, Pal);
  finally
    F.Free;
  end;
end;

procedure TVgaSpecBitmap.LoadPaletteFromStream(S: TStream; var Pal: TDosVGAPalette8);
var
  Decompressor: TDosDatDecompressor;
  Mem: TMemoryStream;
//  Planar: TDosPlanarBitmap;
begin
//  Mem := nil;
//  Decompressor := nil;

  Decompressor := TDosDatDecompressor.Create;
  Mem := TMemoryStream.Create;
  try

    try
      Decompressor.DecompressSection(S, Mem);
    finally
      Decompressor.Free;
    end;

    Mem.Seek(0, soFromBeginning);
    Mem.Read(Pal, Sizeof(Pal)); // first section, first bytes = vgapalette

   finally
     Mem.Free;
   end;
end;

{$ifdef vgaspecwrite}
procedure TVgaSpecBitmap.CheckBitmap(aBitmap: TBitmap32);
begin
  if (aBitmap.Width <> 960) or (aBitmap.Height <> 160) then
    raise Exception.CreateFmt(SVgaSpecDimensionError_dd, [aBitmap.Width, aBitmap.Height]);
end;

procedure TVgaSpecBitmap.CreatePal(aBitmap: TBitmap32; var Pal: TDosVgaSpecPaletteHeader);
var
  C: PColor32;
  {i,} y, x: Integer;
  //R, G, B: Integer;
  LastEntry: Integer;
  ColorArray: array[0..7] of TColor32;

    procedure AddColor;
    var
      i, Entry: Integer;
    begin
      if C^ = clBlack32 then
        Exit;
      for i := 0 to LastEntry do
        if ColorArray[i] = C^ then
          Exit;
      Entry := LastEntry + 1;
      if Entry > 7 then
        raise Exception.Create('vgaspec error bitmap has too many colors');
      LastEntry := Entry;
      ColorArray[Entry] := C^;
      with TColor32Entry(C^) do
      begin
        //deb(['addcolor', i, r div 4,g div 4,b div 4]);
        Pal.VgaPal[Entry].R := R div 4;
        Pal.VgaPal[Entry].G := G div 4;
        Pal.VgaPal[Entry].B := B div 4;
      end;
    end;

begin
  FillChar(ColorArray, SizeOf(ColorArray), 0);
  LastEntry := 0;
  FillChar(Pal, SizeOf(0), 0);
  with aBitmap do
  begin
    C := PixelPtr[0, 0];
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
      begin
        AddColor;
        Inc(C);
      end;
  end;
end;

function TVgaSpecBitmap.EncodeSection(var Src, Dst: PBytes; SrcSize: Integer): Integer;
{-------------------------------------------------------------------------------
  Encoding of bytes as done in VGASPEC?.DAT
  Returns number of allocated bytes of Dst after encoding.
  Dst should be nil at entrance
-------------------------------------------------------------------------------}
var
  DstPtr: Integer;
  Allocated: Integer;
  //OutSize: Integer;
  //NeededCalcSize: Integer;

    function NextSection(ix: Integer; var aRepeated: Boolean): Integer;
    {-------------------------------------------------------------------------------
       Local proc: Get the next section from index ix
       o Repeated will be true if there are repeated characters
       o Returnvalue = length of the section
    -------------------------------------------------------------------------------}
    var
      i: Integer;
    begin
      aRepeated := False;
      Result := 0;

      if ix > SrcSize - 1 then // beyond end
        Exit
      else if ix = SrcSize - 1 then // last byte
      begin
        Result := 1;
        aRepeated := True;
        Exit;
      end;

      aRepeated := Src^[ix] = Src^[ix + 1];
      i := ix;
      case aRepeated of
        True:
          while Src^[i] = Src^[i + 1] do
          begin
            Inc(i);
            if i - ix >= 127 then
              Break; // maximum count = 128
            if i >= SrcSize - 1 then
              Break; // check end
          end;
        False :
          while (Src^[i] <> Src^[i + 1]) do
          begin
            Inc(i);
            if i - ix >= 128 then
              Break; // maximum count = 128
            if i >= SrcSize - 1 then
              Break; // check end
          end;
      end;
      Result := i - ix;

      if aRepeated or (i = SrcSize - 1) then
        Inc(Result)
      else if Result = 1 then
        aRepeated := True; // one character is stored as a single repeat
    end;

    procedure Store(ix, Cnt: Integer; aRepeated: Boolean);
    {-------------------------------------------------------------------------------
      Local proc: Store bytes in dst
    -------------------------------------------------------------------------------}
    begin
      //Deb([ix, cnt, arepeated, dstptr]);
      if DstPtr + Cnt + 2 > Allocated then
      begin
        ReallocMem(Dst, DstPtr + Cnt + 2);
        Allocated := DstPtr + Cnt + 2;
      end;

      case aRepeated of
        False:
          begin
            Dst^[DstPtr] := Cnt - 1;
            Inc(DstPtr);
            Move(Src^[ix], Dst^[DstPtr], Cnt);
            Inc(DstPtr, Cnt);
          end;
        True:
          begin
//            deb([dstptr]);
            Dst^[DstPtr] := 257 - Cnt;
            Inc(DstPtr);
            Dst^[DstPtr] := Src^[ix];
            Inc(DstPtr);
          end;
      end;

    end;

var
  ix: Integer;
  Cnt: Byte;
  Rep: Boolean;
begin
  Allocated := SrcSize;
  ReallocMem(Dst, Allocated);
  FillChar(Dst^, Allocated, 0);
  ix := 0;
  DstPtr := 0;

  repeat
    Cnt := NextSection(ix, Rep);
    if Cnt = 0 then
      Break;
//    deb([ix, cnt, rep]);
    Store(ix, Cnt, Rep);
    Inc(Ix, Cnt);
  until False;

  if DstPtr <> Allocated then
    ReallocMem(Dst, DstPtr);

  Result := DstPtr;
//  deb(['result=', result]);

//  for ix := 0 to 500 do
//    deb([ix, dst^[ix]])

end;

procedure TVgaSpecBitmap.EncodeSectionStream(Src, Dst: TStream);
var
  InSize, OutSize: Integer;
  A, B: PBytes;
begin
  Src.Seek(0, soFromBeginning);
  InSize := Src.Size;
  GetMem(A, InSize);
  Src.Read(A^, InSize);
  B := nil;
  OutSize := EncodeSection(A, B, InSize);
  Dst.Write(B^, OutSize);
  //deb(['encodesectionstream', insize, outsize]);
  FreeMem(A);
  Freemem(B);
end;

procedure TVgaSpecBitmap.SaveToFile(aBitmap: TBitmap32; const aFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(aBitmap, F);
  finally
    F.Free;
  end;
end;

procedure TVgaSpecBitmap.SaveToStream(aBitmap: TBitmap32; Dst: TStream);
{-------------------------------------------------------------------------------
  Here the code to create a vgaspec.dat file from a bitmap.
  o Check bitmap 960x160
  o Create palette entries (max 7 colors)
  o Write Palette to the stream
  o Encode bitmap to 4 planarbitmap sections

-------------------------------------------------------------------------------}
(*
var
  TempBitmap: TBitmap32;
  H, Sec: Integer;
  Pal: TDosVgaSpecPaletteHeader;
  DosCompressor: TDosDatCompressor;
  DstRect, SrcRect: TRect;
  PlanarBitmap: TDosPlanarBitmap;
  PlanarMem: TMemoryStream;
  Mem1: TMemoryStream;
  *)
begin

  raise Exception.Create('vgaspec.save');

  (*

  CheckBitmap(aBitmap);
  CreatePal(aBitmap, Pal);

  DstRect := Rect(0, 0, 960, 40);
  SrcRect := DstRect;
  TempBitmap := TBitmap32.Create;
  PlanarBitmap := TDosPlanarBitmap.Create;
  Mem1 := TMemoryStream.Create;
  PlanarMem := TMemoryStream.Create;
  DosCompressor := TDosDatCompressor.Create;
  DosCompressor.OnProgress := DoCompressProgress;
  try
    TempBitmap.SetSize(960, 40);
    Mem1.Write(Pal, SizeOf(Pal));
    for Sec := 0 to 3 do
    begin
      TempBitmap.Draw(DstRect, SrcRect, aBitmap);
      RectMove(SrcRect, 0, 40);
      PlanarMem.Clear;
      PlanarBitmap.SaveToStream(TempBitmap, PlanarMem, 3, Pal.VgaPal, Pal.VgaPal);
      PlanarMem.Seek(0, soFromBeginning);
      EncodeSectionStream(PlanarMem, Mem1);
    end;
    Mem1.Seek(0, soFromBeginning);

    H := DosCompressor.Compress(Mem1, Dst);
    //windlg([mem1.size, dst.size]);
  finally
    TempBitmap.Free;
    PlanarBitmap.Free;
    Mem1.Free;
    DosCompressor.Free;
    PlanarMem.Free;
  end;

  *)
end;


procedure TVgaSpecBitmap.DoCompressProgress(aPosition, aMaximum: Integer);
begin
  if Assigned(fOnCompressProgress) then
    fOnCompressProgress(aPosition, aMaximum);
end;

{$endif}


end.


     (*



    class procedure MergePals(const LoPal, HiPal: TDosVGAPalette8;
      var Pal: TDosVGAPalette16);
    class procedure SplitPals(const Pal: TDosVGAPalette16;
      var LoPal, HiPal: TDosVGAPalette8);
    class procedure CalculateCustomPalette32FromBitmap(aBitmap: TBitmap32;
      var aColors: TColor32Array8; var UsedCount: Integer);
    class procedure CalculateDosPalette16FromBitmap(aBitmap: TBitmap32;
      var aDosPal: TDosVGAPalette16; var UsedCount: Integer);



  { load old }

    procedure LoadFromFile(const aFilename: string; aBitmap: TBitmap32;
      aPos, aWidth, aHeight: Integer; BPP: Byte;
      const aLoPalette, aHiPalette: TDosVGAPalette8); overload;

    procedure LoadFromFile(const aFilename: string; aBitmap: TBitmap32;
      aPos, aWidth, aHeight: Integer; BPP: Byte;
      const aPalette: TDosVGAPalette16); overload;

    procedure LoadFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer;
      BPP: Byte; const aLoPalette, aHiPalette: TDosVGAPalette8;
      aText: TStrings = nil); overload; // key method

    procedure LoadFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer;
      BPP: Byte; const aPalette: TDosVGAPalette16); overload;


  { save }
    procedure SaveToStream(aBitmap: TBitmap32; S: TStream; BPP: Byte;
      const aLoPalette, aHiPalette: TDosVGAPalette8); overload;

    procedure SaveToStream(aBitmap: TBitmap32; S: TStream; BPP: Byte;
      const aPalette: TDosVGAPalette16); overload;


  { properties }
    property DirectRGB: Boolean read fDirectRGB write fDirectRGB;

    *)

(*
procedure TDosPlanarBitmap.LoadFromFile(const aFilename: string; aBitmap: TBitmap32;
  aPos, aWidth, aHeight: Integer; BPP: Byte; const aLoPalette,
  aHiPalette: TDosVGAPalette8);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F, aBitmap, aPos, aWidth, aHeight, BPP, aLoPalette, aHiPalette);
  finally
    F.Free;
  end;
end;

class procedure TDosPlanarBitmap.CalculateCustomPalette32FromBitmap(
  aBitmap: TBitmap32; var aColors: TColor32Array8; var UsedCount: Integer);
{-------------------------------------------------------------------------------
  Create 8 entries for the colors in the bitmap.
  black is not included!
-------------------------------------------------------------------------------}
var
  C: PColor32;
  x, y: integer;
  CurrentEntry: Integer;
//  Colors: array[0..7] of TColor32;

    procedure AddColor;
    var
      i: Integer;
    begin
      if C^ = 0 then
        Exit;
      for i := 0 to CurrentEntry do
      begin
        if aColors[i] = C^ then
          Exit;
      end;
      if CurrentEntry > 7 then
        raise Exception.Create(ClassName + '.CalculateCustomPaletteFromBitmap: too many colors');
        //deb(['pal', currententry, redcomponent(c^), greencomponent(c^), bluecomponent(c^)]);
      aColors[CurrentEntry] := C^;
      Inc(CurrentEntry);
      Inc(UsedCount);
    end;

begin
  if not Between(UsedCount, 0, 8) then
    raise Exception.CreateFmt(ClassName + '.CalculateCustomPaletteFromBitmap invalid param %d', [UsedCount]);
  CurrentEntry := UsedCount;
  if UsedCount = 0 then
    FillChar(aColors, SizeOf(aColors) , 0);

  with aBitmap do
  begin
    C := PixelPtr[0, 0];
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
      begin
        AddColor;
        Inc(C); // typed pointer increment (by 4)
      end;
  end;
end;

class procedure TDosPlanarBitmap.CalculateDosPalette16FromBitmap(
  aBitmap: TBitmap32; var aDosPal: TDosVGAPalette16;
  var UsedCount: Integer);
var
  Colors: TColor32Array16;
  C: PColor32;
  x, y: integer;
  CurrentEntry: Integer;

    procedure InitPal;
    var
      i: Integer;
    begin
      Colors[0] := 0;
      for i := 1 to 15 do
        Colors[i] := clBlack32;
//      FillChar(Colors, SizeOf(Colors), 0);


      for i := 0 to UsedCount - 1 do
      begin
        TColor32Entry(Colors[i]).R := aDosPal[i].R * 4;
        TColor32Entry(Colors[i]).G := aDosPal[i].G * 4;
        TColor32Entry(Colors[i]).B := aDosPal[i].B * 4;
      end;

      // clear colors from usedcount
      for i := UsedCount to 15 do
      begin
        aDosPal[i].R := 0;
        aDosPal[i].G := 0;
        aDosPal[i].B := 0;
      end;

      {deb(['initpal---', usedcount]);
      for i := 0 to 15 do
      begin
        with adospal[i] do
          deb([i, r,g,b]);
      end;}
    end;

    procedure ExitPal;
    var
      i: Integer;
    begin
      for i := 0 to 15 do
      begin
        aDosPal[i].R := TColor32Entry(Colors[i]).R div 4;
        aDosPal[i].G := TColor32Entry(Colors[i]).G div 4;
        aDosPal[i].B := TColor32Entry(Colors[i]).B div 4;
      end;
      {deb(['exitpal---', usedcount]);
      for i := 0 to 15 do
      begin
        with adospal[i] do
          deb([i, r,g,b]);
      end;}
    end;

    procedure AddColor;
    var
      i: Integer;
      //EntryFound: Integer;
    begin
      if C^ = 0 then
        Exit;
      //EntryFound := -1;
      for i := 0 to CurrentEntry do
      begin
        if Colors[i] = C^ then
          Exit;
      end;
      if CurrentEntry > 15 then
      begin
        with tcolor32entry(c^) do
        raise Exception.Create(ClassName + '.CalculateDosPalette16FromBitmap: too many colors. The offending color = ' +
          i2s(r) + ','+i2s(g) + ',' + i2s(b));
      end;
//      deb(['add to pal', currententry, redcomponent(c^), greencomponent(c^), bluecomponent(c^)]);
      Colors[CurrentEntry] := C^;
      Inc(CurrentEntry);
      Inc(UsedCount);
    end;


begin
  if not Between(UsedCount, 0, 16) then
    raise Exception.CreateFmt(ClassName + '.CalculateDosPalette16FromBitmap invalid param %d', [UsedCount]);
  InitPal;
  CurrentEntry := UsedCount;


  with aBitmap do
  begin
    C := PixelPtr[0, 0];
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
      begin
        AddColor;
        Inc(C); // typed pointer increment (by 4)
      end;
  end;

  ExitPal;
end;

procedure TDosPlanarBitmap.LoadFromFile(const aFilename: string;
  aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer; BPP: Byte;
  const aPalette: TDosVGAPalette16);
//var
 // A, B: TDosVGAPalette8;
//  i: Integer;
var
  F: TFileStream;
begin
{  for i := 0 to 7 do
    A[i] := aPalette[i];
  for i := 8 to 15 do
    B[i-8] := aPalette[i]; }
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadFromStream(F, aBitmap, aPos, aWidth, aHeight, BPP, aPalette);
  finally
    F.Free;
  end;
end;

procedure TDosPlanarBitmap.LoadFromStream(S: TStream; aBitmap: TBitmap32;
  aPos, aWidth, aHeight: Integer; BPP: Byte;
  const aPalette: TDosVGAPalette16);
var
  A, B: TDosVGAPalette8;
  i: Integer;
begin
  for i := 0 to 7 do
    A[i] := aPalette[i];
  for i := 8 to 15 do
    B[i-8] := aPalette[i];
  LoadFromStream(S, aBitmap, aPos, aWidth, aHeight, BPP, A, B);
end;


procedure TDosPlanarBitmap.LoadFromStream(S: TStream; aBitmap: TBitmap32; aPos, aWidth, aHeight: Integer;
  BPP: Byte; const aLoPalette, aHiPalette: TDosVGAPalette8;
  aText: TStrings = nil);
{-------------------------------------------------------------------------------
  This function loads a planar bitmap (like stored in VgaGr?.dat or Main.dat).
  Parameters are typically read from ground??.dat.
  aPos = Position in stream
  BPP (bits per pixel) means in fact: How many planes are there
-------------------------------------------------------------------------------}

var
 { i,} {Rd, }NumBytes : Integer;
  Buf: PBytes;
  PlaneSize: Integer;
  LineSize: Integer;

    function GetPix(X, Y: Integer): Byte;
    var
      BytePtr: PByte;
      //B: Byte;
      i, P: Integer;
      BitNumber, Mask: Byte;
    begin
      Result := 0;
      // adres of pixel in the first plane
      P := Y * LineSize + X div 8;
      // get the right bit (Wow, totally excellent!)
      BitNumber := 7 - X mod 8; // downwards!
      Mask := 1 shl BitNumber;
      // get the seperated bits from the planes
      BytePtr := @Buf^[P];
      for i := 0 to BPP - 1 do
      begin
        if BytePtr^ and Mask <> 0 then
          Result := Result or (1 shl i);
        Inc(BytePtr, PlaneSize);
      end;
    end;

var
  x, y: Integer;
  Entry: Byte;
  Mul: Byte;
  //ds: tstrings;
  {$ifdef develop}
  cs: string;
  {$endif}
begin
  {$ifdef develop}
    if aText <> nil then
      aText.Clear;
  {$endif}
  Assert(BPP in [1..8], 'bpp error');

  LineSize := aWidth div 8; // Every bit is a pixel (in each plane)
  PlaneSize := LineSize * aHeight; // Now we know the planesize
  NumBytes := PlaneSize * BPP; // and the total bytes

//  deb(['numbytes', numbytes]);

  GetMem(Buf, NumBytes);
  if aPos >= 0 then
    S.Seek(aPos, soFromBeginning);
  {Rd := }S.ReadBuffer(Buf^, NumBytes);
  aBitmap.SetSize(aWidth, aHeight);
  aBitmap.Clear(0); // clear with #transparent black
  if fDirectRGB then
    Mul := 1
  else
    Mul := 4;

//  ds:=tstringlist.create;

  for y := 0 to aHeight-1 do
  BEGIN

    {$ifdef develop}
    if aText <> nil then
      cs := '';
    {$endif}

    for x := 0 to aWidth - 1 do
    begin
      Entry := GetPix(x, y);
      {$ifdef develop}
      if aText <> nil then
        cs := cs + inttohex(entry, 1);
      {$endif}
      // get color from high palette
      if Entry > 7 then
        with aHiPalette[Entry - 8] do
          aBitmap.Pixel[x, y] := Color32(R * Mul, G * Mul, B * Mul)
{      else if Entry = 7 then
        with aHiPalette[0] do
          aBitmap.Pixel[x, y]:= Color32(R * Mul, G * Mul, B * Mul) }
      // get color from low palette
      else if Entry > 0 then
        with aLoPalette[Entry] do
          aBitmap.Pixel[x, y]:= Color32(R * Mul, G * Mul, B * Mul);
    end;

    {$ifdef develop}
    if aText <> nil then
      aText.Add(cs)
    {$endif}

  END;

  //ds.savetofile(applicationpath + '_bmp.txt');
  //ds.free;

  FreeMem(Buf);
end;

procedure TDosPlanarBitmap.SaveToStream(aBitmap: TBitmap32; S: TStream; BPP: Byte;
      const aLoPalette, aHiPalette: TDosVGAPalette8);
{-------------------------------------------------------------------------------
  encoding of raw rgb to bitplanes
-------------------------------------------------------------------------------}

var
  LineSize, PlaneSize, NumBytes, x, y: Integer;
  Buf: PBytes;
  C: PColor32;
  ColorArray: array[0..15] of TColor32;


    procedure InitColorArray;
    var
      i: Integer;
    begin
      for i := 0 to 7 do
      begin
        with TColor32Entry(ColorArray[i]) do
        begin
          if i > 0 then
            A := 255
          else
            A := 0;
          R := aLoPalette[i].R * 4;
          G := aLoPalette[i].G * 4;
          B := aLoPalette[i].B * 4;
//          deb([i, r,g,b,a]);
        end;
      end;
      for i := 8 to 15 do
      begin
        with TColor32Entry(ColorArray[i]) do
        begin
          A := 255;
          R := aHiPalette[i - 8].R * 4;
          G := aHiPalette[i - 8].G * 4;
          B := aHiPalette[i - 8].B * 4;
          //deb(['write', i, r,g,b]);
        end;
      end;
    end;

    function FindPalEntry(aColor: TColor32): Byte;
    var
      r,g,b:integer;
    begin
      for Result := 0 to 15 do
        if ColorArray[Result] = aColor then
          Exit;

      r:=RedComponent(acolor);
      g:=greenComponent(acolor);
      b:=blueComponent(acolor);
      raise Exception.CreateFmt('planarbitmap error colorentry not found %d, %d, %d', [r,g,b]);
    end;

    procedure SetPix(x, y: Integer; PalEntry: Byte);
    var
      BytePtr: PByte;
      //B: Byte;
      i, P: Integer;
      BitNumber, Mask: Byte;
    begin
      // adres of pixel in the first plane
      P := Y * LineSize + X div 8;
      // get the right bit (Wow, totally excellent!)
      BitNumber := 7 - X mod 8;
      Mask := 1 shl BitNumber;
      // get the seperated bits from the planes
      BytePtr := @Buf^[P];
      for i := 0 to BPP - 1 do
      begin
        if PalEntry and (1 shl i) <> 0 then
          BytePtr^ := BytePtr^ or Mask;
        Inc(BytePtr, PlaneSize);
      end;
    end;

begin

  InitColorArray;
  LineSize := aBitmap.Width div 8; // Every bit is a pixel (in each plane)
  PlaneSize := LineSize * aBitmap.Height; // Now we know the planesize
  NumBytes := PlaneSize * BPP; // and the total bytes
  GetMem(Buf, NumBytes);
  FillChar(Buf^, NumBytes, 0);

  try

    with aBitmap do
    begin
      C := PixelPtr[0, 0];
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          SetPix(x, y, FindPalEntry(C^));
          Inc(C);
        end;
    end;
//    if bpp=1 then deb(['1 bpp ', numbytes]);

    S.Write(Buf^, NumBytes);

  finally
    FreeMem(Buf{, NumBytes});
  end;
end;

procedure TDosPlanarBitmap.SaveToStream(aBitmap: TBitmap32; S: TStream;
  BPP: Byte; const aPalette: TDosVGAPalette16);
var
  L, H: TDosVGAPalette8;
begin
  SplitPals(aPalette, L, H);
  SaveToStream(aBitmap, S, BPP, L, H);
end;


class procedure TDosPlanarBitmap.MergePals(const LoPal,
  HiPal: TDosVGAPalette8; var Pal: TDosVGAPalette16);
begin
  Move(LoPal, Pal[0], SizeOf(LoPal));
  Move(HiPal, Pal[8], SizeOf(HiPal));
end;

class procedure TDosPlanarBitmap.SplitPals(
  const Pal: TDosVGAPalette16; var LoPal,
  HiPal: TDosVGAPalette8);
begin
  Move(Pal[0], LoPal, SizeOf(LoPal));
  Move(Pal[8], HiPal, SizeOf(HiPal));
end;


    procedure GetSectionsAndPalette(S, Dest: TStream; var Pal: TDosVGAPalette8);

procedure TVgaSpecBitmap.GetSectionsAndPalette(S, Dest: TStream; var Pal: TDosVGAPalette8);
var
  ci, di, {xi,} ds: Integer;//Word;
  curbyte: byte;
  //ptr: pbyte;
  cnt: Integer; //value: byte;
  csize:integer;
  cdata: PBytes;
  ddata: array[0..3] of PBytes;
  i: integer; //umisc
  //wr,wri: integer;
begin
  csize:=s.size;
  s.seek(0,sofrombeginning);
  s.read(pal, sizeof(pal));

  s.seek(0,sofrombeginning);
  dest.seek(0,sofrombeginning);

  getmem(cdata, csize);
  s.read(cdata^, csize);

{  for i:=0 to 127-1 do
//    if cdata^[i] = 128 then
    begin
      deb([leadzerostr(i,3), cdata^[i]]);
    end;}

  for i := 0 to 3 do
  begin
    getmem(ddata[i], VGASPEC_SECTIONSIZE);
    fillchar(ddata[i]^, VGASPEC_SECTIONSIZE, 0);
  end;

  // the first section contains 40 bytes palette-info
  ci := SizeOf(TDosVgaSpecPaletteHeader); {=40}
  di := 0;

  //xi := 0;
  ds := 0;

  while ci < csize do
  begin

//    if di > ddata_section_size then print __LINE__: sleep
    if ds > 3 then break;

    CurByte := CData^[ci];

    case CurByte of
      // end section
      128:
        begin
            //deb(['end section', ci, di]);
          Inc(ds);
          di := 0;
          inc(ci);//ci += 1
        end;
      // raw bytes
      0..127:
        begin
          Cnt := CurByte + 1;
          Inc(ci);
          Move(CData^[ci], DData[ds]^[di], Cnt);
          Inc(di, Cnt);
          Inc(ci, Cnt);   //classes
        end;
      // repeated bytes
      129..255:
        begin
          Cnt := 257 - CurByte;
          Inc(ci);
          FillChar(DData[ds]^[di], Cnt, CData^[ci]);
          Inc(di, Cnt);
          Inc(ci);
        end;
    end; //case

  end;


  //wr:=0;
  //wri:=0;
  freemem(cdata);
  for i := 0 to 3 do
  begin
    {wr := }dest.Write(ddata[i]^, VGASPEC_SECTIONSIZE);
//    inc(wri,wr);
  //  windlg([wri, wr]);
    freemem(ddata[i]);
  end;
end;


