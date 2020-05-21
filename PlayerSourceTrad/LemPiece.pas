{$include lem_directives.inc}
unit LemPiece;

interface

uses
  Classes, SysUtils,
  UTools;

type
  // abstract ancestor for object, terrain and steel
  TPieceClass = class of TPiece;
  TPiece = class(TCollectionItem)
  private
  protected
    fLeft : Integer;
    fTop  : Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
  end;

type
  // basically ancestor for object and terrain
  TIdentifiedPiece = class(TPiece)
  private
  protected
    fIdentifier: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Identifier: Integer read fIdentifier write fIdentifier;
  end;

type
  // basically ancestor for steel
  TSizedPiece = class(TPiece)
  private
  protected
    fHeight: Integer;
    fWidth: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
  end;

type
  TPieces = class(TCollectionEx)
  public
    constructor Create(aItemClass: TPieceClass);
  end;

implementation


{ TPieces }

constructor TPieces.Create(aItemClass: TPieceClass);
begin
  inherited Create(aItemClass);
end;

{ TPiece }

procedure TPiece.Assign(Source: TPersistent);
var
  P: TPiece absolute Source;
begin
  if Source is TPiece then
  begin
    Left := P.Left;
    Top := P.Top;
  end
  else inherited Assign(Source);
end;

{ TIdentifiedPiece }

procedure TIdentifiedPiece.Assign(Source: TPersistent);
var
  IP: TIdentifiedPiece absolute Source;
begin
  if Source is TIdentifiedPiece then
  begin
    inherited Assign(Source);
    Identifier := IP.Identifier;
  end
  else inherited Assign(Source);
end;

{ TSizedPiece }

procedure TSizedPiece.Assign(Source: TPersistent);
var
  SP: TSizedPiece absolute Source;
begin
  if Source is TSizedPiece then
  begin
    inherited Assign(Source);
    Width := SP.Width;
    Height := SP.Height;
  end
  else inherited Assign(Source);
end;

end.



(*
definitions of:
characters, pieces, objects, terrains, animations

TDescriptor---------------------------------------------------------------------

  TMetaPiece(TDescriptor)-------------------------------------------------------
    -width
    -height
    TMetaObject(TMetaPiece)-----------------------------------------------------
      -picture
      -data fetch info
      -animationcount
      -triggerarea
      -triggereffect
      -loop information
      -preview index
      -sound
      ---LoadFromStream
      ---SaveToStream
    TMetaTerrain(TMetaPiece)----------------------------------------------------
    TSteelDescriptor------------------------------------------------------------

  TMetaAnimationFrame-----------------------------------------------------------
    -deltax
    -deltay
    ---LoadFromStream
    ---SaveToStream
  TMetaAnimation----------------------------------------------------------------
    -width
    -height
    -deltax
    -deltay
    -animationcount
    ---LoadFromStream
    ---SaveToStream




TDescriptors
  ---LoadFromStream
  ---SaveToStream



*)
