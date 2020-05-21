{$include lem_directives.inc}
unit LemDosMainDat;

interface

uses
  Classes, GR32, SysUtils,
  LemTypes, LemDosStructures, LemDosCmp, LemDosBmp;

type
  {-------------------------------------------------------------------------------
    Tool to extract data from the dos main dat file
  -------------------------------------------------------------------------------}
  TMainDatExtractor = class
  private
    fFileName: string;
    fDecompressor: TDosDatDecompressor;
    fSections: TDosDatSectionList;
    fPlanar: TDosPlanarBitmap;
    {$ifdef flexi}fSysDat: TSysDatRec;
    fSysDatLoaded: Boolean;{$endif}
    procedure EnsureLoaded;
    procedure EnsureDecompressed(aSection: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExtractBrownBackGround(Bmp: TBitmap32);
    procedure ExtractLogo(Bmp: TBitmap32);
    procedure ExtractBitmap(Bmp: TBitmap32; aSection, aPosition,
      aWidth, aHeight, BPP: Integer; aPal: TArrayOfColor32);
    procedure ExtractAnimation(Bmp: TBitmap32; aSection, aPos,
      aWidth, aHeight, aFrameCount: Integer; BPP: Byte; aPal: TArrayOfColor32);
    {$ifdef flexi}
    function GetSysData: TSysDatRec;
    procedure LoadSysData;
    {$endif}
    property FileName: string read fFileName write fFileName;
  end;

implementation

{ TMainDatExtractor }

constructor TMainDatExtractor.Create;
begin
  inherited Create;
  fSections := TDosDatSectionList.Create;
  fPlanar := TDosPlanarBitmap.Create;
  fDecompressor := TDosDatDecompressor.Create;
end;

destructor TMainDatExtractor.Destroy;
begin
  fSections.Free;
  fPlanar.Free;
  fDecompressor.Free;
  inherited;
end;

procedure TMainDatExtractor.EnsureDecompressed(aSection: Integer);
var
  Sec: TDosDatSection;
begin
  EnsureLoaded;
  Sec := fSections[aSection];
  if Sec.DecompressedData.Size = 0 then
    fDecompressor.DecompressSection(Sec.CompressedData, Sec.DecompressedData)
end;

procedure TMainDatExtractor.EnsureLoaded;
var
  DataStream: TStream;
begin
  if fSections.Count = 0 then
  begin
    {$ifdef external}if FileExists(fFileName) then
      DataStream := TFileStream.Create(fFileName, fmOpenRead)
      else{$endif}
      DataStream := CreateDataStream(fFileName, ldtLemmings);
    try
      fDecompressor.LoadSectionList(DataStream, fSections, False);
    finally
      DataStream.Free;
    end;
  end;
end;

procedure TMainDatExtractor.ExtractAnimation(Bmp: TBitmap32; aSection, aPos, aWidth,
  aHeight, aFrameCount: Integer; BPP: Byte; aPal: TArrayOfColor32);
begin
  EnsureDecompressed(aSection);
  {$ifdef flexi}
  LoadSysData;
  if fSysDat.Options2 and 2 <> 0 then
  begin
    if (aSection = 0) or (aSection = 6) then
    begin
      aPal[1] := $D02020;
      aPal[4] := $F0F000;
      aPal[5] := $4040E0;
    end else if (aSection > 2) and (aSection < 6) then
    begin
      aPal := GetDosMainMenuPaletteColors32(true);
    end;
  end;
  {$endif}
  fPlanar.LoadAnimationFromStream(fSections[aSection].DecompressedData,
    Bmp, aPos, aWidth, aHeight, aFrameCount, BPP, aPal);
end;

procedure TMainDatExtractor.ExtractBitmap(Bmp: TBitmap32; aSection,
  aPosition, aWidth, aHeight, BPP: Integer; aPal: TArrayOfColor32);
begin
  EnsureDecompressed(aSection);
  {$ifdef flexi}
  LoadSysData;
  if fSysDat.Options2 and 2 <> 0 then
  begin
    if (aSection = 0) or (aSection = 6) then
    begin
      aPal[1] := $D02020;
      aPal[4] := $F0F000;
      aPal[5] := $4040E0;
    end else if (aSection > 2) and (aSection < 6) then
    begin
      aPal := GetDosMainMenuPaletteColors32(true);
    end;
  end;
  {$endif}
  fPlanar.LoadFromStream(fSections[aSection].DecompressedData, Bmp, aPosition,
    aWidth, aHeight, BPP, aPal);
end;

procedure TMainDatExtractor.ExtractBrownBackGround(Bmp: TBitmap32);
{-------------------------------------------------------------------------------
  Extract hte brown background, used in several screens
-------------------------------------------------------------------------------}
begin
  EnsureDecompressed(3);
  {$ifdef flexi}
  LoadSysData;
  {$endif}
  fPlanar.LoadFromStream(fSections[3].DecompressedData, Bmp, 0, 320, 104, 2,
    GetDosMainMenuPaletteColors32{$ifdef flexi}(fSysDat.Options2 and 2 <> 0){$endif});
end;

procedure TMainDatExtractor.ExtractLogo(Bmp: TBitmap32);
{-------------------------------------------------------------------------------
  Extract the LemmingLogo
-------------------------------------------------------------------------------}
begin
  EnsureDecompressed(3);
  {$ifdef flexi}
  LoadSysData;
  {$endif}
  fPlanar.LoadFromStream(fSections[3].DecompressedData, Bmp, $2080, 632, 94, 4,
    GetDosMainMenuPaletteColors32{$ifdef flexi}(fSysDat.Options2 and 2 <> 0){$endif});
end;

{$ifdef flexi}
function TMainDatExtractor.GetSysData(): TSysDatRec;
begin
  LoadSysData;
  Result := fSysDat;
end;

procedure TMainDatExtractor.LoadSysData;
var
  TempRec : TSysDatRec;
  TempStream : TStream;
begin
  if fSysDatLoaded then exit;
  //EnsureDecompressed(7);
  //fSections[7].DecompressedData.SaveToFile('sys2.dat');
  {$ifdef external}
  if FileExists(ExtractFilePath(ParamStr(0)) + 'system.dat') then
    TempStream := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'system.dat', fmOpenRead)
    else{$endif}
    TempStream := CreateDataStream('system.dat', ldtLemmings);
  TempStream.Seek(0, soFromBeginning);
  TempStream.ReadBuffer(fSysDat, SYSDAT_SIZE);
  fSysDatLoaded := true;
  TempStream.Free;
end;
{$endif}

end.

