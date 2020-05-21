{$include lem_directives.inc}
unit LemInteractiveObject;

interface

uses
  Classes,
  LemPiece;

const
  // Object Drawing Flags
  odf_OnlyOnTerrain = 1; // bit 0
  odf_UpsideDown    = 2; // bit 1
  odf_NoOverwrite   = 4; // bit 2

type
  TInteractiveObjectClass = class of TInteractiveObject;
  TInteractiveObject = class(TIdentifiedPiece)
  private
  protected
    fDrawingFlags: Byte; // odf_xxxx
    fFake: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DrawingFlags: Byte read fDrawingFlags write fDrawingFlags;
    property IsFake: Boolean read fFake write fFake;
  end;

type
  TInteractiveObjects = class(TPieces)
  private
    function GetItem(Index: Integer): TInteractiveObject;
    procedure SetItem(Index: Integer; const Value: TInteractiveObject);
  protected
  public
    constructor Create(aItemClass: TInteractiveObjectClass);
    function Add: TInteractiveObject;
    function Insert(Index: Integer): TInteractiveObject;
    property Items[Index: Integer]: TInteractiveObject read GetItem write SetItem; default;
  published
  end;

implementation


{ TInteractiveObjects }

function TInteractiveObjects.Add: TInteractiveObject; 
begin 
  Result := TInteractiveObject(inherited Add); 
end;

constructor TInteractiveObjects.Create(aItemClass: TInteractiveObjectClass);
begin
  inherited Create(aItemClass);
end; 

function TInteractiveObjects.GetItem(Index: Integer): TInteractiveObject; 
begin 
  Result := TInteractiveObject(inherited GetItem(Index)) 
end; 

function TInteractiveObjects.Insert(Index: Integer): TInteractiveObject; 
begin 
  Result := TInteractiveObject(inherited Insert(Index)) 
end; 

procedure TInteractiveObjects.SetItem(Index: Integer; const Value: TInteractiveObject);
begin 
  inherited SetItem(Index, Value); 
end; 

{ TInteractiveObject }

procedure TInteractiveObject.Assign(Source: TPersistent);
var
  O: TInteractiveObject absolute Source;
begin
  if Source is TInteractiveObject then
  begin
    inherited Assign(Source);
    DrawingFlags := O.DrawingFlags;
  end
  else inherited Assign(Source);
end;

end.

