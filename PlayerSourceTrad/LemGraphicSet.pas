{$include lem_directives.inc}
unit LemGraphicSet;

interface

uses
  Classes,
  GR32,
  LemTypes,
  LemMetaObject,
  LemMetaTerrain;

type
  TBaseGraphicSetClass = class of TBaseGraphicSet;
  TBaseGraphicSet = class(TPersistent)
  private
  protected
    fGraphicSetId    : Integer; // number identifier
    fGraphicSetIdExt : Integer; // extended graphics identifier
    fGraphicSetName  : string; // string identifier
    fDescription     : string; // displayname
    fMetaObjects     : TMetaObjects;
    fMetaTerrains    : TMetaTerrains;
    fTerrainBitmaps  : TBitmaps;
    fObjectBitmaps   : TBitmaps;
    fSpecialBitmap   : TBitmap32;
    procedure SetMetaObjects(Value: TMetaObjects); virtual;
    procedure SetMetaTerrains(Value: TMetaTerrains); virtual;
  { dynamic creation }
    function DoCreateMetaObjects: TMetaObjects; dynamic;
    function DoCreateMetaTerrains: TMetaTerrains; dynamic;
  { these must be overridden }
    procedure DoReadMetaData; dynamic; abstract;
    procedure DoReadData; dynamic; abstract;
    procedure DoClearMetaData; dynamic;
    procedure DoClearData; dynamic;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ReadMetaData;
    procedure ReadData;
    procedure ClearMetaData;
    procedure ClearData;
    property TerrainBitmaps: TBitmaps read fTerrainBitmaps;
    property ObjectBitmaps: TBitmaps read fObjectBitmaps;
    property SpecialBitmap: TBitmap32 read fSpecialBitmap;
  published
    property Description: string read fDescription write fDescription;
    property GraphicSetId: Integer read fGraphicSetId write fGraphicSetId;
    property GraphicSetIdExt: Integer read fGraphicSetIdExt write fGraphicSetIdExt;
    property GraphicSetName: string read fGraphicSetName write fGraphicSetName;
    property MetaObjects: TMetaObjects read fMetaObjects write SetMetaObjects;
    property MetaTerrains: TMetaTerrains read fMetaTerrains write SetMetaTerrains;
  end;

implementation

{ TBaseGraphicSet }

procedure TBaseGraphicSet.ClearData;
begin
  DoClearData;
end;

procedure TBaseGraphicSet.ClearMetaData;
begin
  DoClearMetaData;
end;

constructor TBaseGraphicSet.Create;
begin
  inherited Create;
  fMetaObjects := DoCreateMetaObjects;
  fMetaTerrains := DoCreateMetaTerrains;
  fTerrainBitmaps := TBitmaps.Create;
  fObjectBitmaps := TBitmaps.Create;
  fSpecialBitmap := TBitmap32.Create;
end;

destructor TBaseGraphicSet.Destroy;
begin
  fMetaObjects.Free;
  fMetaTerrains.Free;
  fTerrainBitmaps.Free;
  fObjectBitmaps.Free;
  fSpecialBitmap.Free;
  inherited Destroy;
end;

procedure TBaseGraphicSet.DoClearData;
begin
  fGraphicSetId    := 0;
  fGraphicSetIdExt := 0;
  fGraphicSetName  := '';
  fDescription     := '';
  fTerrainBitmaps.Clear;
  fObjectBitmaps.Clear;
  fSpecialBitmap.SetSize(0, 0);
end;

procedure TBaseGraphicSet.DoClearMetaData;
begin
  fMetaObjects.Clear;
  fMetaTerrains.Clear;
end;

function TBaseGraphicSet.DoCreateMetaObjects: TMetaObjects;
begin
  Result := TMetaObjects.Create;
end;

function TBaseGraphicSet.DoCreateMetaTerrains: TMetaTerrains;
begin
  Result := TMetaTerrains.Create;
end;

procedure TBaseGraphicSet.ReadData;
begin
  DoReadData;
end;

procedure TBaseGraphicSet.ReadMetaData;
begin
  DoReadMetaData;
end;

procedure TBaseGraphicSet.SetMetaObjects(Value: TMetaObjects);
begin
  fMetaObjects.Assign(Value);
end;

procedure TBaseGraphicSet.SetMetaTerrains(Value: TMetaTerrains);
begin
  fMetaTerrains.Assign(Value);
end;

end.

