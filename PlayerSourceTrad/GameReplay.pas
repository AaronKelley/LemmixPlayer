{$include lem_directives.inc}
unit GameReplay;

interface

uses
  Classes, SysUtils,
  UMisc, UTools,
  LemCore, LemTypes;

{ TODO : make None versions for our types. Replay is suffering from unclearness }

type
  TRecordedType = (
    rtNone,
    rtStartIncreaseRR,
    rtStartDecreaseRR,
    rtStopChangingRR,
    rtSelectSkill,
    rtAssignSkill,
    rtNuke
  );


  TReplayItem = class(TCollectionItem)
  private
    fIteration    : Integer;
    fRecTyp       : TRecordedType;
    fSkill        : TBasicLemmingAction;
    fLemmingIndex : Integer;
    fLemmingX     : Integer;
    fLemmingY     : Integer;
    fReleaseRate  : Integer;
    fButtonSkill  : TButtonSkill; // only the assign-buttons
  protected
  public
    constructor Create(aCollection: TCollection); override;
  published
    property Iteration: Integer read fIteration write fIteration;
    property RecTyp: TRecordedType read fRecTyp write fRecTyp;
    property Skill: TBasicLemmingAction read fSkill write fSkill default baWalking;
    property LemmingIndex : Integer read fLemmingIndex write fLemmingIndex default -1;
    property LemmingX: Integer read fLemmingX write fLemmingX default 0;
    property LemmingY: Integer read fLemmingY write fLemmingY default 0;
    property ReleaseRate: Integer read fReleaseRate write fReleaseRate default 0;
    property ButtonSkill: TButtonSkill read fButtonSkill write fButtonSkill default bskSlower;
  end;

  TReplayItems = class(TCollectionEx)
  private
    function GetItem(Index: Integer): TReplayItem;
    procedure SetItem(Index: Integer; const Value: TReplayItem);
  protected
  public
    constructor Create;
    procedure SaveToFile(const aFileName: string);
    procedure SaveToTxt(const aFileName: string);
    procedure SaveToStream(S: TStream);

    procedure LoadFromFile(const aFileName: string);
    procedure LoadFromTxt(const aFileName: string);
    procedure LoadFromStream(S: TStream);

    function Add: TReplayItem;
    function Insert(Index: Integer): TReplayItem;
    property Items[Index: Integer]: TReplayItem read GetItem write SetItem; default;
  published
    // level information + checksum
  end;

  // replay wrapper to stream it
  TReplay = class(TComponent)
  private
    fReplayItems: TReplayItems;
  published
    property ReplayItems: TReplayItems read fReplayItems write fReplayItems;
  end;

implementation


{ TReplayItems } 

function TReplayItems.Add: TReplayItem;
begin 
  Result := TReplayItem(inherited Add); 
end; 

constructor TReplayItems.Create; 
begin 
  inherited Create(TReplayItem); 
end; 

function TReplayItems.GetItem(Index: Integer): TReplayItem; 
begin 
  Result := TReplayItem(inherited GetItem(Index)) 
end; 

function TReplayItems.Insert(Index: Integer): TReplayItem; 
begin 
  Result := TReplayItem(inherited Insert(Index)) 
end; 

procedure TReplayItems.SaveToFile(const aFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFilename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TReplayItems.SaveToStream(S: TStream);
var
  R: TReplay;
begin
  R := TReplay.Create(nil);
  try
    R.fReplayItems := Self;
    S.WriteComponent(R)
  finally
    R.Free;
  end;
end;

procedure TReplayItems.SaveToTxt(const aFileName: string);
var
  R: TReplay;
begin
  R := TReplay.Create(nil);
  try
    R.fReplayItems := Self;
    ComponentToTextFile(R, aFileName);
  finally
    R.Free;
  end;
end;

procedure TReplayItems.LoadFromFile(const aFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TReplayItems.LoadFromStream(S: TStream);
var
  R: TReplay;
begin
  R := TReplay.Create(nil);
  try
    R.fReplayItems := Self;
    S.ReadComponent(R)
  finally
    R.Free;
  end;
end;

procedure TReplayItems.LoadFromTxt(const aFileName: string);
begin
  raise exception.create('loadfromtxt not yet implemented')
end;

procedure TReplayItems.SetItem(Index: Integer; const Value: TReplayItem);
begin
  inherited SetItem(Index, Value);
end;


{ TReplayItem }

constructor TReplayItem.Create(aCollection: TCollection);
begin
  inherited;
  fLemmingIndex := -1;
end;

end.

