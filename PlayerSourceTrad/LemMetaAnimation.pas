{$include lem_directives.inc}
unit LemMetaAnimation;

interface

uses
  Classes, UTools,
  LemCore;

const
  lat_Loop = 0; // always animate
  lat_Once = 1; // state change if finished

type
  TMetaAnimationClass = class of TMetaAnimation;
  TMetaAnimation = class(TCollectionItem)
  private
  protected
    fDescription     : string;  // for fun, really
    fFrameCount      : Integer; // number of frames
    fWidth           : Integer; // width of a single frame picture
    fHeight          : Integer; // height of a single frame
    fImageLocation   : Integer; // dos only
    fBitsPerPixel    : Integer; // dos only
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Description      : string read fDescription write fDescription;
    property FrameCount       : Integer read fFrameCount write fFrameCount;
    property Width            : Integer read fWidth write fWidth;
    property Height           : Integer read fHeight write fHeight;
    property BitsPerPixel     : Integer read fBitsPerPixel write fBitsPerPixel;
    property ImageLocation    : Integer read fImageLocation write fImageLocation;
  end;

  {
  TMetaAnimationFrame = class(TCollectionItem)
  private
    fFootX           : Integer;
    fFootY           : Integer;
  protected
  public
  published
    property FootX            : Integer read fFootX write fFootX;
    property FootY            : Integer read fFootY write fFootY;
  end; }

  TMetaLemmingAnimationClass = class of TMetaLemmingAnimation;
  TMetaLemmingAnimation = class(TMetaAnimation)
  private
    fAnimationType   : Integer; // at_xxxx
    fFootX           : Integer;
    fFootY           : Integer;
  protected
  public
    procedure Assign(Source: TPersistent); override;
  published
  { mechanics }
    property AnimationType    : Integer read fAnimationType write fAnimationType;
    property FootX            : Integer read fFootX write fFootX;
    property FootY            : Integer read fFootY write fFootY;
  end;

  TMetaAnimations = class(TCollectionEx)
  private
    function GetItem(Index: Integer): TMetaAnimation;
    procedure SetItem(Index: Integer; const Value: TMetaAnimation);
  protected
  public
    constructor Create(aItemClass: TMetaAnimationClass);
    function Add: TMetaAnimation;
    function Insert(Index: Integer): TMetaAnimation;
    property Items[Index: Integer]: TMetaAnimation read GetItem write SetItem; default;
  published
  end;

  TMetaLemmingAnimations = class(TMetaAnimations)
  private
    function GetItem(Index: Integer): TMetaLemmingAnimation;
    procedure SetItem(Index: Integer; const Value: TMetaLemmingAnimation);
  protected
  public
    constructor Create(aItemClass: TMetaLemmingAnimationClass);
    function Add: TMetaLemmingAnimation;
    function Insert(Index: Integer): TMetaLemmingAnimation;
    property Items[Index: Integer]: TMetaLemmingAnimation read GetItem write SetItem; default;
  published
  end;

implementation

{ TMetaAnimation }

procedure TMetaAnimation.Assign(Source: TPersistent);
var
  M: TMetaAnimation absolute Source;
begin
  if Source is TMetaAnimation then
  begin
    Description      := M.Description;
    FrameCount       := M.FrameCount;
    Width            := M.Width;
    Height           := M.Height;
    BitsPerPixel     := M.BitsPerPixel;
    ImageLocation    := M.ImageLocation;
  end
  else inherited Assign(Source);
end;

{ TMetaLemmingAnimation }

procedure TMetaLemmingAnimation.Assign(Source: TPersistent);
var
  A: TMetaLemmingAnimation absolute Source;
begin
  if Source is TMetaLemmingAnimation then
  begin
    inherited Assign(Source);
    AnimationType    := A.AnimationType;
    FootX            := A.FootX;
    FootY            := A.FootY;
  end
  else inherited Assign(Source);
end;

{ TMetaAnimations }

function TMetaAnimations.Add: TMetaAnimation;
begin
  Result := TMetaAnimation(inherited Add);
end;

constructor TMetaAnimations.Create(aItemClass: TMetaAnimationClass);
begin
  inherited Create(aItemClass);
end;

function TMetaAnimations.GetItem(Index: Integer): TMetaAnimation;
begin
  Result := TMetaAnimation(inherited GetItem(Index))
end;

function TMetaAnimations.Insert(Index: Integer): TMetaAnimation;
begin
  Result := TMetaAnimation(inherited Insert(Index))
end;

procedure TMetaAnimations.SetItem(Index: Integer; const Value: TMetaAnimation);
begin
  inherited SetItem(Index, Value);
end;


{ TMetaLemmingAnimations }

function TMetaLemmingAnimations.Add: TMetaLemmingAnimation;
begin
  Result := TMetaLemmingAnimation(inherited Add);
end;

constructor TMetaLemmingAnimations.Create(aItemClass: TMetaLemmingAnimationClass);
begin
  inherited Create(aItemClass);
end;

function TMetaLemmingAnimations.GetItem(Index: Integer): TMetaLemmingAnimation;
begin
  Result := TMetaLemmingAnimation(inherited GetItem(Index))
end;

function TMetaLemmingAnimations.Insert(Index: Integer): TMetaLemmingAnimation;
begin
  Result := TMetaLemmingAnimation(inherited Insert(Index))
end;

procedure TMetaLemmingAnimations.SetItem(Index: Integer; const Value: TMetaLemmingAnimation);
begin
  inherited SetItem(Index, Value);
end;


end.

