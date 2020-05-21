{$include lem_directives.inc}
unit LemTemp;

interface

uses
  Classes, SysUtils,
  UMisc, UTools, TypInfo,
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

type
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

type
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

    procedure LoadFromOldTxt(const aFileName: string);


    function Add: TReplayItem;
    function Insert(Index: Integer): TReplayItem;
    property Items[Index: Integer]: TReplayItem read GetItem write SetItem; default;
  published
    // level information + checksum
  end;

  // replay wrapper to stream it
type
  TReplay = class(TComponent)
  private
    fReplayItems: TReplayItems;
  published
    property ReplayItems: TReplayItems read fReplayItems write fReplayItems;
  end;

  // disk access

//  rro_LevelInfo

(*
    dgoDisableObjectsAfter15, // objects with index higher than 15 will not work
    dgoMinerOneWayRightBug, // the miner bug check
    dgoDisableButtonClicksWhenPaused, // skillpanel mouse behaviour
    dgoSplattingExitsBug, // NOT IMPLEMENTED
    dgoOldEntranceABBAOrder,
    dgoEntranceX25,
    dgoFallerStartsWith3
*)

(*
const
    dgo_DisableObjectsAfter15, // objects with index higher than 15 will not work
    dgo_MinerOneWayRightBug, // the miner bug check
    dgo_DisableButtonClicksWhenPaused, // skillpanel mouse behaviour
    dgo_SplattingExitsBug, // NOT IMPLEMENTED
    dgo_OldEntranceABBAOrder,
    dgo_EntranceX25,
    dgo_FallerStartsWith3



		gmf_DisableObjectsAfter15          = Bit 0
		gmf_MinerOneWayRightBug            = Bit 1
		gmf_DisableButtonClicksWhenPaused  = Bit 2 
		gmf_SplattingExitsBug              = Bit 3
		gmf_OldEntranceABBAOrder           = Bit 4
		gmf_EntranceX25                    = Bit 5
		gmf_FallerStartsWith3              = Bit 6
		gmf_Max4EnabledEntrances           = Bit 7
    *)

  TReplayHeaderRec = packed record
    Id                : array[0..2] of Char;  // must be "LRB"
    Version           : Byte;                 // = 1
    FileSize          : Integer;              // filesize including this header
    Mechanics         : Word;
    RecordingOptions  : Word;
    FirstRecordPos    : Word;                 // for this version just right after the header
    ReplayRecordSize  : Word;
    ReplayRecordCount : Word;                 // size
    LevelTitle        : array[0..31] of Char;
  end;

const
  raf_StartPause = Bit0;
  raf_EndPause = Bit1;
  raf_Pausing = Bit2;
  raf_StartIncreaseRR = Bit3;
  raf_StartDecreaseRR = Bit4;
  raf_StopChangingRR = Bit5;
  raf_SkillSelection = Bit6;
  raf_SkillAssignment = Bit7;
  raf_Nuke = Bit8;



type
  TReplayRec_v1 = packed record
    Check          : Char;         //  1 byte  -  1
    Iteration      : Integer;      //  4 bytes -  7
    ActionFlags    : Word;         //  2 bytes -  9
    AssignedSkill  : Byte;         //  1 byte  - 10
    SelectedButton : Byte;         //  1 byte  - 11
    ReleaseRate    : Byte;         //  1 byte  - 12
    LemmingIndex   : Integer;      //  4 bytes - 16
    LemmingX       : Integer;      //  4 bytes - 20
    LemmingY       : Integer;      //  4 bytes - 24
  end;

  TReplayItem_v1 = class
  private
  protected
  public
    Rec: TReplayRec_v1;
  end;

  TReplayEx = class
    // list
    // header
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



procedure TReplayItems.LoadFromOldTxt(const aFileName: string);
(*

19, rrStartIncrease, 1
19, rrStop, 85
19, rrStartIncrease, 85
19, rrStop, 86
55, raSkillAssignment, baClimbing, 0
58, rrStartDecrease, 86
58, rrStop, 1
66, raSkillAssignment, baClimbing, 1
77, raSkillAssignment, baExplosion, 0
85, raSkillAssignment, baFloating, 0
96, raSkillAssignment, baFloating, 1
118, raSkillAssignment, baFloating, 2
184, raSkillAssignment, baBuilding, 1
202, raSkillAssignment, baBuilding, 1
219, raSkillAssignment, baBashing, 1
226, raSkillAssignment, baBuilding, 1
243, raSkillAssignment, baBashing, 1
249, raSkillAssignment, baBuilding, 1
412, rrStartIncrease, 1
412, rrStop, 99
518, raSkillAssignment, baBlocking, 1

*)


(*

  TRecordedAction = (
    raNone,
    raSkillAssignment,
    raNuke
  );

  TReleaseRateAction = (
    rrNone,
    rrStop,
    rrStartIncrease,
    rrStartDecrease
  );


*)
var
  L: TStringList;
  i,j: integer;
  s,t: string;

  Cnt: integer;
  RR, ITER: integer;
  TYP: TRecordedType;
  SKILL: TBasicLemmingAction;
  LIX: Integer;
  It: TReplayItem;

begin


  L:= TStringList.create;
  try
    l.loadfromfile(aFileName);
    for i := 0 to l.count-1 do
    begin
      s := l[i];
      cnt := SplitStringCount(s, ',');
      if cnt < 3 then
        continue;

      RR := 0;
      ITER:=-1;
      TYP:=rtNone;
      SKILL:=baWalking;
      LIX:=-1;

      for j := 0 to cnt - 1 do
      begin


        t:=SplitString(s, j, ',');

        case j of
          0: // currentiteration     umisc
            begin
            ITER := StrToIntDef(t, -1)
            end;
          1: // typ
            begin
              if comparetext(t, 'raNone') = 0 then
                TYP := rtNone
              else if comparetext(t, 'raSkillAssignment') = 0 then
                TYP := rtAssignSkill
              else if comparetext(t, 'raNuke') = 0 then
                TYP := rtNuke
              else if comparetext(t, 'rrNone') = 0 then
                TYP := rtNone
              else if comparetext(t, 'rrStop') = 0 then
                TYP := rtStopChangingRR
              else if comparetext(t, 'rrStartDecrease') = 0 then
                TYP := rtStartDecreaseRR
              else if comparetext(t, 'rrStartIncrease') = 0 then
                TYP := rtStartIncreaseRR;
            end;
          2: // assign of RR
            begin
             if Cnt = 3 then
             begin
               RR := StrToIntDef(t, -1);
             end
             else begin
               SKILL := TBasiclemmingaction(GetEnumValue(typeinfo(tbasiclemmingaction), t));
             end;
            end;

          3: // lemming index
            begin
              LIX := StrToIntDef(t, -1);
            end;
        end;

      end;

      if (ITER<>-1) and (TYP<>rtNone) then
      begin
        //deb(['item:', iter]);
        It := Add;
         it.Iteration := ITER;
         it.RecTyp := TYP;
        It.Skill := SKILL;
        It.LemmingIndex :=LIX;
        It.ReleaseRate  := RR;
      end;

    end;
  finally
    l.free;
  end;


end;

{ TReplayItem }

constructor TReplayItem.Create(aCollection: TCollection);
begin
  inherited;
  fLemmingIndex := -1;
end;

end.

