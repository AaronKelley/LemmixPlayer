{$include lem_directives.inc}
unit LemTypes;

interface

uses
  Classes, SysUtils, Contnrs,
  GR32, GR32_LowLevel,
  UZip,
  //Dialogs,
  UTools;

type
  TLemDataType = (
    ldtNone,
    ldtLemmings,  // resource 'lemdata'
    ldtSound,     // resource 'lemsounds'
    ldtMusic,
    ldtParticles
  );

type


  TBitmaps = class(TObjectList)
  private
    function GetItem(Index: Integer): TBitmap32;
  protected
  public
    function Add(Item: TBitmap32): Integer;
    procedure Insert(Index: Integer; Item: TBitmap32);
    property Items[Index: Integer]: TBitmap32 read GetItem; default;
    property List;
  published
  end;

  TBasicWrapper = class(TComponent)
  private
    fPersistentObject: TPersistent;
  protected
  public
  published
    property PersistentObject: TPersistent read fPersistentObject write fPersistentObject;
  end;

  // lemlowlevel
procedure ReplaceColor(B: TBitmap32; FromColor, ToColor: TColor32);
function CalcFrameRect(Bmp: TBitmap32; FrameCount, FrameIndex: Integer): TRect;
procedure PrepareFramedBitmap(Bmp: TBitmap32; FrameCount, FrameWidth, FrameHeight: Integer);
procedure InsertFrame(Dst, Src: TBitmap32; FrameCount, FrameIndex: Integer);
function AppPath: string;
function LemmingsPath: string;
function MusicsPath: string;

function CreateDataStream(const aFileName: string; aType: TLemDataType): TStream;

implementation

var
  _AppPath: string;

function AppPath: string;
begin
  if _AppPath = '' then
    _AppPath := ExtractFilePath(ParamStr(0));
  Result := _AppPath;
end;

function LemmingsPath: string;
begin
  // @styledef
    Result := AppPath;
end;

function MusicsPath: string;
begin
  // @styledef
    Result := AppPath;
end;

procedure ReplaceColor(B: TBitmap32; FromColor, ToColor: TColor32);
var
  P: PColor32;
  i: Integer;
begin
  P := B.PixelPtr[0, 0];
  for i := 0 to B.Height * B.Width - 1 do
    begin
      if P^ = FromColor then
        P^ := ToColor;
      Inc(P);
    end;
end;

function CalcFrameRect(Bmp: TBitmap32; FrameCount, FrameIndex: Integer): TRect;
var
  Y, H, W: Integer;
begin
  W := Bmp.Width;
  H := Bmp.Height div FrameCount;
  Y := H * FrameIndex;
//  Assert(Bmp.Height mod FrameCount = 0)
  Result.Left := 0;
  Result.Top := Y;
  Result.Right := W;
  Result.Bottom := Y + H;
end;

procedure PrepareFramedBitmap(Bmp: TBitmap32; FrameCount, FrameWidth, FrameHeight: Integer);
begin
  Bmp.SetSize(FrameWidth, FrameCount * FrameHeight);
  Bmp.ResetAlpha(0);
end;

procedure InsertFrame(Dst, Src: TBitmap32; FrameCount, FrameIndex: Integer);
var
  H, Y: Integer;
  W: Integer;
  SrcP, DstP: PColor32;
begin
  Assert(FrameCount > 0);
  Assert(Dst.Height = FrameCount * Src.Height);
  Assert(Dst.Width = Src.Width);

  H := Dst.Height div FrameCount;
  Y := H * FrameIndex;

  SrcP := Src.PixelPtr[0, 0];
  DstP := Dst.PixelPtr[0, Y];
  W := Dst.Width;

  for Y := 0 to H - 1 do
    begin
      MoveLongWord(SrcP^, DstP^, W);
      Inc(SrcP, W);
      Inc(DstP, W);
    end;

end;

function CreateDataStream__(const aFileName: string; aType: TLemDataType): TStream;
{-------------------------------------------------------------------------------
  Dependent on the compiled mode we create a filestream or a
  decompressed memorystream from resource.
  Note the ExtractFileName(): we did *not* include path info in the
  archived resource files
-------------------------------------------------------------------------------}
{$ifdef resourcelemmings}
var
  Arc: TArchive;
{$endif}
begin
{$ifdef resourcelemmings}
  Result := TMemoryStream.Create;
  Arc := TArchive.Create;
  try
    try
      case aType of
        ldtLemmings: Arc.OpenResource(HINSTANCE, 'lemdata', 'archive');
        ldtSound: Arc.OpenResource(HINSTANCE, 'lemsounds', 'archive');
        ldtMusic: Arc.OpenResource(HINSTANCE, 'lemmusic', 'archive');
      end;
      Arc.ExtractFile(ExtractFileName(aFileName), Result);
    except
      FreeAndNil(Result);
      raise;
    end;
    Result.Seek(0, soFromBeginning);
  finally
    Arc.Free;
  end;
{$else}
  Result := TFileStream.Create(aFileName, fmOpenRead);
{$endif}
end;

function CreateDataStream(const aFileName: string; aType: TLemDataType): TStream;
{-------------------------------------------------------------------------------
  Dependent on the compiled mode we create a filestream or a
  decompressed memorystream from resource.
  Note the ExtractFileName(): we did *not* include path info in the
  archived resource files
-------------------------------------------------------------------------------}
{$ifdef resourcelemmings}
var
  Arc: TArchive;
  tk: String;
{$endif}
begin


  case aType of
    ldtLemmings:
      begin
      {$ifdef resourcelemdata}
        Result := TMemoryStream.Create;
        Arc := TArchive.Create;
        try
          Arc.OpenResource(HINSTANCE, 'lemdata', 'archive');
          Arc.ExtractFile(ExtractFileName(aFileName), Result);
        finally
          Arc.Free;
        end;
      {$else}
        Result := TFileStream.Create(aFileName, fmOpenRead);
      {$endif}
      end;
    ldtSound:
      begin
      {$ifdef resourcelemsounds}
        Result := TMemoryStream.Create;
        Arc := TArchive.Create;
        try
          Arc.OpenResource(HINSTANCE, 'lemsounds', 'archive');
          Arc.ExtractFile(ExtractFileName(aFileName), Result);
        finally
          Arc.Free;
        end;
      {$else}
        Result := TFileStream.Create(aFileName, fmOpenRead);
      {$endif}
      end;
    ldtMusic:
      begin
      {$ifdef resourcelemmusic}
        Result := TMemoryStream.Create;
        Arc := TArchive.Create;
        try
          {$ifdef extmusic}
          tk := ChangeFileExt(ParamStr(0), '') + '_MUSIC.DAT';
          //MessageDlg(tk, mtcustom, [mbok], 0);
          if FileExists(tk) then Arc.OpenArchive(tk, amOpen)
            else
          {$endif}
            Arc.OpenResource(HINSTANCE, 'lemmusic', 'archive');

          tk := ChangeFileExt(aFileName, '') + '.ogg';
          if Arc.ArchiveList.IndexOf(ExtractFileName(tk)) = -1 then tk := aFileName + '.it';
          Arc.ExtractFile(ExtractFileName(tk), Result);
        finally
          Arc.Free;
        end;
      {$else}
        tk := ChangeFileExt(aFileName, '') + '.ogg';
        Result := TFileStream.Create(tk, fmOpenRead);
      {$endif}
      end;
    ldtParticles:
      begin
      {$ifdef resourceparticles}
        Result := TMemoryStream.Create;
        Arc := TArchive.Create;
        try
          Arc.OpenResource(HINSTANCE, 'lemparticles', 'archive');
          Arc.ExtractFile(ExtractFileName(aFileName), Result);
        finally
          Arc.Free;
        end;
      {$else}
        Result := TFileStream.Create(aFileName, fmOpenRead);
      {$endif}
      end;
  else
    Result := nil;

  end;


  Assert(Result <> nil);

  Result.Seek(0, soFromBeginning);

end;

{ TBitmaps }

function TBitmaps.Add(Item: TBitmap32): Integer;
begin
  Result := inherited Add(Item);
end;

function TBitmaps.GetItem(Index: Integer): TBitmap32;
begin
  Result := inherited Get(Index);
end;

procedure TBitmaps.Insert(Index: Integer; Item: TBitmap32);
begin
  inherited Insert(Index, Item);
end;

end.

