{$include lem_directives.inc}
unit LemTerrain;

interface

uses
  Classes,
  UTools,
  LemPiece;

const
  // Terrain Drawing Flags
  tdf_Erase                    = 1;    // bit 0 use terrain bitmap as eraser
  tdf_Invert                   = 2;    // bit 1 invert terrain bitmap
  tdf_NoOverwrite              = 4;    // bit 2 do not overwrite existing terrain pixels

type
  TTerrainClass = class of TTerrain;

  TTerrain = class(TIdentifiedPiece)
  private
  protected
    fDrawingFlags : Byte;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DrawingFlags: Byte read fDrawingFlags write fDrawingFlags;
  end;

type
  TTerrains = class(TPieces)
  private
    function GetItem(Index: Integer): TTerrain;
    procedure SetItem(Index: Integer; const Value: TTerrain);
  protected
  public
    constructor Create(aItemClass: TTerrainClass);
    function Add: TTerrain;
    function Insert(Index: Integer): TTerrain;
    property Items[Index: Integer]: TTerrain read GetItem write SetItem; default;
  published
  end;

implementation

{ TTerrain }

procedure TTerrain.Assign(Source: TPersistent);
var
  T: TTerrain absolute Source;
begin
  if Source is TTerrain then
  begin
    inherited Assign(Source);
    DrawingFlags := T.DrawingFlags;
  end
  else inherited Assign(Source);
end;

{ TTerrains }

function TTerrains.Add: TTerrain;
begin
  Result := TTerrain(inherited Add);
end;


constructor TTerrains.Create(aItemClass: TTerrainClass);
begin
  inherited Create(aItemClass);
end;

function TTerrains.GetItem(Index: Integer): TTerrain;
begin
  Result := TTerrain(inherited GetItem(Index))
end;

function TTerrains.Insert(Index: Integer): TTerrain;
begin
  Result := TTerrain(inherited Insert(Index))
end;

procedure TTerrains.SetItem(Index: Integer; const Value: TTerrain);
begin
  inherited SetItem(Index, Value);
end;

end.

