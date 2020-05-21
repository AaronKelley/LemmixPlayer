{$include lem_directives.inc}

unit LemStyle;

interface

uses
  Classes,
  {$ifdef flexi}LemDosStructures,{$endif}
  LemLevel, LemGraphicSet, LemAnimationSet, LemLevelSystem, LemMusicSystem;


type
  TBaseLemmingStyle = class(TPersistent)
  published
  private
  protected
    fStyleName        : string;
    fStyleDescription : string;
    fCommonPath       : string;
    fAnimationSet     : TBaseAnimationSet;
    fLevelSystem      : TBaseLevelSystem;
    fMusicSystem      : TBaseMusicSystem;
    function DoCreateAnimationSet: TBaseAnimationSet; virtual;
    function DoCreateLevelSystem: TBaseLevelSystem; virtual;
    function DoCreateMusicSystem: TBaseMusicSystem; virtual;
  public
    {$ifdef flexi}SysDat : TSysDatRec;{$endif}
    constructor Create;
    destructor Destroy; override;
    function CreateGraphicSet: TBaseGraphicSet; virtual;
    property AnimationSet: TBaseAnimationSet read fAnimationSet;
    property LevelSystem: TBaseLevelSystem read fLevelSystem;
    property MusicSystem: TBaseMusicSystem read fMusicSystem;
  published
    property StyleName: string read fStyleName write fStyleName;
    property StyleDescription: string read fStyleDescription write fStyleDescription;
    property CommonPath: string read fCommonPath write fCommonPath;
  end;

implementation

{ TBaseLemmingStyle }

constructor TBaseLemmingStyle.Create;
begin
  inherited Create;
  fAnimationSet := DoCreateAnimationSet;
  fLevelSystem := DoCreateLevelSystem;
  fMusicSystem := DoCreateMusicSystem;
end;

function TBaseLemmingStyle.CreateGraphicSet: TBaseGraphicSet;
begin
  Result := nil;
end;

destructor TBaseLemmingStyle.Destroy;
begin
  fAnimationSet.Free;
  fLevelSystem.Free;
  fMusicSystem.Free;
  inherited;
end;

function TBaseLemmingStyle.DoCreateAnimationSet: TBaseAnimationSet;
begin
  Result := nil;
end;

function TBaseLemmingStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := nil;
end;

function TBaseLemmingStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := nil;
end;

end.




(*
  TStyleParamsRec = record
    Resolution   : Single;
    LevelWidth   : Integer;
    LevelHeight  : Integer;
    MultiPacks   : Integer;
  end;
*)

(*  with aParams do
  begin
    Resolution   := 2.0;
    LevelWidth   := 3200;
    LevelHeight  := 320;
    MultiPacks   := True;
  end;
*)

(*

  TLevelInfo = class(TCollectionItem)
  private
    function GetOwnerSection: TPackSection;
  protected
    fTrimmedTitle            : string; // the title: this is only for display purposes.
    fDosLevelPackFileName    : string; // dos file where we can find this level (levelxxx.dat)
    fDosLevelPackIndex       : Integer; // dos file position in doslevelpackfilename(in fact: which decompression section)
    fDosIsOddTableClone      : Boolean; // true if this is a cloned level with other title and skills
    fDosOddTableFileName     : string; // only dosorig: oddtable.dat
    fDosOddTableIndex        : Integer; // were can we find this other title and skills in the dosoddtable file
    fOwnerFile               : string; // lemmini: lemmini ini file containing this level
  public
    property OwnerSection: TPackSection read GetOwnerSection;
  published
    // stylename --> for tracing the style
    

    property TrimmedTitle: string read fTrimmedTitle write fTrimmedTitle;
    property DosLevelPackFileName: string read fDosLevelPackFileName write fDosLevelPackFileName;
    property DosLevelPackIndex: Integer read fDosLevelPackIndex write fDosLevelPackIndex;
    property DosIsOddTableClone: Boolean read fDosIsOddTableClone write fDosIsOddTableClone;
    property DosOddTableFileName: string read fDosOddTableFileName write fDosOddTableFileName;
    property DosOddTableIndex: Integer read fDosOddTableIndex write fDosOddTableIndex;
    property OwnerFile: string read fOwnerFile write fOwnerFile;
  end;


*)

(*type

  TLoadProperties = class(TPersistent)
  private
    fPath: string;
  protected
  public
  published
    property Path: string read fPath write fPath;
  end;
  *)

