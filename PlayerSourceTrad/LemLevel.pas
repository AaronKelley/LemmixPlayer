{$include lem_directives.inc}
unit LemLevel;

interface

uses
  Classes, SysUtils,
  UMisc,
  LemTerrain,
  LemInteractiveObject,
  LemSteel;

type
  TLevelInfo = class(TPersistent)
  private
  protected
    fReleaseRate    : Integer;
    fLemmingsCount  : Integer;
    fRescueCount    : Integer;
    fTimeLimit      : Integer;
    fClimberCount   : Integer;
    fFloaterCount   : Integer;
    fBomberCount    : Integer;
    fBlockerCount   : Integer;
    fBuilderCount   : Integer;
    fBasherCount    : Integer;
    fMinerCount     : Integer;
    fDiggerCount    : Integer;
    fGraphicSet     : Integer;
    fGraphicSetEx   : Integer;
    fSuperLemming   : Boolean;
    fScreenPosition : Integer;
    fMusicTrack     : Integer;
    fOddtarget      : Integer;
    fTitle          : string;
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
  published
    property ReleaseRate    : Integer read fReleaseRate write fReleaseRate;
    property LemmingsCount  : Integer read fLemmingsCount write fLemmingsCount;
    property RescueCount    : Integer read fRescueCount write fRescueCount;
    property TimeLimit      : Integer read fTimeLimit write fTimeLimit;
    property ClimberCount   : Integer read fClimberCount write fClimberCount;
    property FloaterCount   : Integer read fFloaterCount write fFloaterCount;
    property BomberCount    : Integer read fBomberCount write fBomberCount;
    property BlockerCount   : Integer read fBlockerCount write fBlockerCount;
    property BuilderCount   : Integer read fBuilderCount write fBuilderCount;
    property BasherCount    : Integer read fBasherCount write fBasherCount;
    property MinerCount     : Integer read fMinerCount write fMinerCount;
    property DiggerCount    : Integer read fDiggerCount write fDiggerCount;
    property GraphicSet     : Integer read fGraphicSet write fGraphicSet;
    property GraphicSetEx   : Integer read fGraphicSetEx write fGraphicSetEx;
    property SuperLemming   : Boolean read fSuperLemming write fSuperLemming;
    property ScreenPosition : Integer read fScreenPosition write fScreenPosition;
    property MusicTrack     : Integer read fMusicTrack write fMusicTrack;
    property OddTarget      : Integer read fOddTarget write fOddTarget;
    property Title          : string read fTitle write fTitle;
  end;

  TLevel = class(TComponent)
  private
  protected
    fLevelInfo       : TLevelInfo;
    fTerrains           : TTerrains;
    fInteractiveObjects : TInteractiveObjects;
    fSteels             : TSteels;
  { internal }
    fUpdateCounter      : Integer;
  { property access }
    procedure SetTerrains(Value: TTerrains); virtual;
    procedure SetInteractiveObjects(Value: TInteractiveObjects); virtual;
    procedure SetSteels(Value: TSteels); virtual;
    procedure SetLevelInfo(const Value: TLevelInfo); virtual;
  { dynamic creation }
    function DoCreateLevelInfo: TLevelInfo; dynamic;
    function DoCreateTerrains: TTerrains; dynamic;
    function DoCreateInteractiveObjects: TInteractiveObjects; dynamic;
    function DoCreateSteels: TSteels; dynamic;
  { protected overrides }
  public
    { TODO : TLevel maybe does not need to be a Component }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToFile(const aFileName: string);
    procedure SaveToStream(S: TStream);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearLevel;
  published
    property Info: TLevelInfo read fLevelInfo write SetLevelInfo;
    property InteractiveObjects: TInteractiveObjects read fInteractiveObjects write SetInteractiveObjects;
    property Terrains: TTerrains read fTerrains write SetTerrains;
    property Steels: TSteels read fSteels write SetSteels;
  end;

  {
  T_Level = class
  private
  protected
  public
  published
  end;}

implementation

{ TLevelInfo }

procedure TLevelInfo.Assign(Source: TPersistent);
var
  L: TLevelInfo absolute Source;
begin
  if Source is TLevelInfo then
  begin
    ReleaseRate    := L.ReleaseRate;
    LemmingsCount  := L.LemmingsCount;
    RescueCount    := L.RescueCount;
    TimeLimit      := L.TimeLimit;
    ClimberCount   := L.ClimberCount;
    FloaterCount   := L.FloaterCount;
    BomberCount    := L.BomberCount;
    BlockerCount   := L.BlockerCount;
    BuilderCount   := L.BuilderCount;
    BasherCount    := L.BasherCount;
    MinerCount     := L.MinerCount;
    DiggerCount    := L.DiggerCount;
    GraphicSet     := L.GraphicSet;
    GraphicSetEx   := L.GraphicSetEx;
    SuperLemming   := L.SuperLemming;
    ScreenPosition := L.ScreenPosition;
    Title          := L.Title;
  end
  else inherited Assign(Source);
end;

procedure TLevelInfo.Clear;
begin
  ReleaseRate    := 1;
  LemmingsCount  := 1;
  RescueCount    := 1;
  TimeLimit      := 1;
  ClimberCount   := 0;
  FloaterCount   := 0;
  BomberCount    := 0;
  BlockerCount   := 0;
  BuilderCount   := 0;
  BasherCount    := 0;
  MinerCount     := 0;
  DiggerCount    := 0;
  GraphicSet     := 0;
  GraphicSetEx   := 0;
  SuperLemming   := False;
  ScreenPosition := 0;
  Title          := '';
end;

constructor TLevelInfo.Create;
begin
  inherited Create;
  Clear;
end;

{ TLevel }

procedure TLevel.Assign(Source: TPersistent);
var
  L: TLevel absolute Source;
begin
  if Source is TLevel then
  begin
    BeginUpdate;
    try
      Info := L.Info;
      InteractiveObjects := L.InteractiveObjects;
      Terrains := L.Terrains;
      Steels := L.Steels;
    finally
      EndUpdate;
    end;
  end
  else inherited Assign(Source)
end;

procedure TLevel.BeginUpdate;
begin
{ TODO : a little more protection for beginupdate endupdate }
  Inc(fUpdateCounter);
  if fUpdateCounter = 1 then
  begin
    fInteractiveObjects.BeginUpdate;
    fTerrains.BeginUpdate;
    fSteels.BeginUpdate;
  end;
end;

procedure TLevel.ClearLevel;
begin
  //fLevelInfo.Clear;
  fInteractiveObjects.Clear;
  fTerrains.Clear;
  fSteels.Clear;
end;

constructor TLevel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLevelInfo := DoCreateLevelInfo;
  fInteractiveObjects := DoCreateInteractiveObjects;
  fTerrains := DoCreateTerrains;
  fSteels := DoCreateSteels;
end;

destructor TLevel.Destroy;
begin
  fLevelInfo.Free;
  fInteractiveObjects.Free;
  fTerrains.Free;
  fSteels.Free;
  inherited Destroy;
end;

function TLevel.DoCreateInteractiveObjects: TInteractiveObjects;
begin
  Result := TInteractiveObjects.Create(TInteractiveObject);
end;

function TLevel.DoCreateLevelInfo: TLevelInfo;
begin
  Result := TLevelInfo.Create;
end;

function TLevel.DoCreateSteels: TSteels;
begin
  Result := TSteels.Create(TSteel);
end;

function TLevel.DoCreateTerrains: TTerrains;
begin
  Result := TTerrains.Create(TTerrain);
end;

procedure TLevel.EndUpdate;
begin
  if fUpdateCounter > 0 then
  begin
    Dec(fUpdateCounter);
    if fUpdateCounter = 0 then
    begin
      fInteractiveObjects.EndUpdate;
      fTerrains.EndUpdate;
      fSteels.EndUpdate;
    end;
  end;
end;

procedure TLevel.SaveToFile(const aFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmCreate);
  try
     SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TLevel.SaveToStream(S: TStream);
begin
  S.WriteComponent(Self);
end;

procedure TLevel.SetLevelInfo(const Value: TLevelInfo);
begin
  fLevelInfo.Assign(Value);
end;

procedure TLevel.SetInteractiveObjects(Value: TInteractiveObjects);
begin
  fInteractiveObjects.Assign(Value);
end;

procedure TLevel.SetTerrains(Value: TTerrains);
begin
  fTerrains.Assign(Value);
end;

procedure TLevel.SetSteels(Value: TSteels);
begin
  fSteels.Assign(Value);
end;

end.

