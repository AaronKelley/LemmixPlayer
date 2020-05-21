{$include lem_directives.inc}

unit LemAnimationSet;

interface

uses
  Classes, GR32,
  LemCore,
  LemTypes,
  LemMetaAnimation,
  LemAnimation;

type
  {-------------------------------------------------------------------------------
    base class animationset
  -------------------------------------------------------------------------------}
  TBaseAnimationSet = class(TPersistent)
  private
    procedure SetMetaLemmingAnimations(Value: TMetaLemmingAnimations);
    procedure SetMetaMaskAnimations(Value: TMetaAnimations);
  protected
    fMetaLemmingAnimations : TMetaLemmingAnimations; // meta data lemmings
    fMetaMaskAnimations    : TMetaAnimations; // meta data masks (for bashing etc.)
    fLemmingAnimations     : TBitmaps; // the list of lemmings bitmaps
    fMaskAnimations        : TBitmaps; // the list of masks (for bashing etc.)
    fBrickColor            : TColor32;
  { new virtuals }
    function DoCreateMetaLemmingAnimations: TMetaLemmingAnimations; virtual;
    function DoCreateMetaMaskAnimations: TMetaAnimations; virtual;
  { internals }
    procedure DoReadMetaData; virtual; abstract;
    procedure DoReadData; virtual; abstract;
    procedure DoClearData; virtual; abstract;
  public
    constructor Create; virtual; // watch out, it's virtual!
    destructor Destroy; override;

    procedure ReadMetaData;
    procedure ReadData;
    procedure ClearData;
    procedure ReplaceLemmingAnimationColor(OldColor, NewColor: TColor32);

    property BrickColor: TColor32 read fBrickColor write fBrickColor;

    property LemmingAnimations: TBitmaps read fLemmingAnimations;
    property MaskAnimations: TBitmaps read fMaskAnimations;
  published
    property MetaLemmingAnimations: TMetaLemmingAnimations read fMetaLemmingAnimations write SetMetaLemmingAnimations;
    property MetaMaskAnimations: TMetaAnimations read fMetaMaskAnimations write SetMetaMaskAnimations;
  end;

implementation

{ TBaseAnimationSet }

constructor TBaseAnimationSet.Create;
begin
  inherited Create;
  //fBrickColor := clMask32;
  fMetaLemmingAnimations := DoCreateMetaLemmingAnimations;
  fMetaMaskAnimations := DoCreateMetaMaskAnimations;
  fLemmingAnimations := TBitmaps.Create;
  fMaskAnimations := TBitmaps.Create;
end;

destructor TBaseAnimationSet.Destroy;
begin
  fMetaLemmingAnimations.Free;
  fMetaMaskAnimations.Free;
  fMaskAnimations.Free;
  fLemmingAnimations.Free;
  inherited Destroy;
end;

procedure TBaseAnimationSet.ReadMetaData;
begin
  DoReadMetaData;
end;

procedure TBaseAnimationSet.ReadData;
//var
  //i: Integer;
begin
  DoReadData;
  (*
  for i := 0 to fAnimationCache.Count - 1 do
  begin
    ReplaceColor(TBitmap32(fAnimationCache[i]), clBRICK32, fBrickColor);
  end;
  *)
end;

procedure TBaseAnimationSet.ClearData;
begin
  DoClearData;
end;


procedure TBaseAnimationSet.ReplaceLemmingAnimationColor(OldColor, NewColor: TColor32);
var
  i: Integer;
begin
  for i := 0 to fLemmingAnimations.Count - 1 do
    //ReplaceColor(TBitmap32(fLemmingAnimations[i]), fBrickColor, aColor);
    ReplaceColor(fLemmingAnimations[i], OldColor, NewColor);
end;


function TBaseAnimationSet.DoCreateMetaLemmingAnimations: TMetaLemmingAnimations;
begin
  Result := TMetaLemmingAnimations.Create(TMetaLemmingAnimation);
end;

function TBaseAnimationSet.DoCreateMetaMaskAnimations: TMetaAnimations;
begin
  Result := TMetaAnimations.Create(TMetaAnimation);
end;

procedure TBaseAnimationSet.SetMetaLemmingAnimations(Value: TMetaLemmingAnimations);
begin
  fMetaLemmingAnimations.Assign(Value);
end;

procedure TBaseAnimationSet.SetMetaMaskAnimations(Value: TMetaAnimations);
begin
  fMetaMaskAnimations.Assign(Value);
end;

end.

