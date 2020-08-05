{$include lem_directives.inc}

unit GameControl;

{-------------------------------------------------------------------------------
  The gamecontrol class is in fact just a global var which is passed through as
  a parameter to all screens.
-------------------------------------------------------------------------------}

interface

uses
  Classes,
  GR32, GR32_Image,
  UTools,
  LemCore, LemTypes, LemLevel, LemDosStyle, LemGraphicSet, LemDosGraphicSet,
  {$ifdef flexi}LemDosStructures,{$endif}
  LemLevelSystem, LemRendering;

type
  TGameResultsRec = record
    gSuccess            : Boolean; // level played successfully?
    gCheated            : Boolean; // level cheated?
    gCount              : Integer; // number
    gToRescue           : Integer;
    gRescued            : Integer;
    gTarget             : Integer;
    gDone               : Integer;
    gTimeIsUp           : Boolean;
  end;

type
  TGameScreenType = (
    gstUnknown,
    gstMenu,
    gstPreview,
    gstPlay,
    gstPostview,
    gstLevelCode,
    gstSounds,
    gstExit,
    gstNavigation
  );

type
  // how do we load the level
  TWhichLevel = (
    wlFirst,
    wlFirstSection,
    wlLevelCode,
    wlNext,
    wlSame,
    wlCongratulations
  );

type
  TGameSoundOption = (
    gsoSound,
    gsoMusic
  );
  TGameSoundOptions = set of TGameSoundOption;

type
  TMiscOption = (
    moGradientBridges,  // 0
    moFastForward,      // 1
    moSaveState,        // 2
    moHyperJumps,       // 3
    moCheatCodes,       // 4
    moShowParticles,    // 5
    moLookForLVLFiles,  // 6
//    moUseSystemCursor,  // 7
    moLemmixTrapBug,
    moStateControlEnabled,
    moPauseAssignEnabled,
    mo11, mo12, mo13, mo14, mo15,
    mo16, mo17, mo18, mo19, mo20, mo21, mo22, mo23,
    mo24, mo25, mo26, mo27, mo28, mo29, mo30, mo31
  );

  TMiscOptions = set of TMiscOption;

const
  DEF_MISCOPTIONS = [
    moGradientBridges,
    moFastForward,
    moSaveState,
    moHyperJumps,
//    moCheatCodes,
    moShowParticles,
    moStateControlEnabled,
    moPauseAssignEnabled
    {$ifdef cust}, moLemmixTrapBug{$endif}

//    moLookForLVLFiles
//    moUseSystemCursor
  ];

type             (*
  TOptionsRec = packed record
    oSignature   : array[0..2] of Char; // LOF LemmingsOptions File
    oVersion     : Byte; // version of this record
    oRecordSize  : Integer;
    oOptions     : TMiscOptions; // 32 bits
    oSoundOptions: TGameSoundOptions; // 8 bits
  end;         *)

  TDosGameParams = class(TPersistent)
  private
    fDirectory    : string;
    fLevelPackName: String;
    fDumpMode     : Boolean;
    function GetLevelPackName: String;
    procedure SetLevelPackName(Value: String);
    function GetCheatCodesEnabled: Boolean;
    procedure SetCheatCodesEnabled(Value: Boolean);
    function GetMusicEnabled: Boolean;
    function GetSoundEnabled: Boolean;
    procedure SetMusicEnabled(Value: Boolean);
    procedure SetSoundEnabled(Value: Boolean);
    function GetShowParticles: Boolean;
    procedure SetShowParticles(const Value: Boolean);
    function GetLookForLVLFiles: Boolean;
    procedure SetLookForLVLFiles(Value: Boolean);
    function GetUseSystemCursor: Boolean;
    procedure SetUseSystemCursor(const Value: Boolean);
    function GetLemmixTrapBug: Boolean;
    procedure SetLemmixTrapBug(Value: Boolean);
    function GetStateControlEnabled: Boolean;
    procedure SetStateControlEnabled(Value: Boolean);
    function GetPauseAssignEnabled: Boolean;
    procedure SetPauseAssignEnabled(Value: Boolean);
  public
    // this is initialized by appcontroller
    MainDatFile  : string;

    Info         : TDosGamePlayInfoRec;
    WhichLevel   : TWhichLevel;

    SoundOptions : TGameSoundOptions;

    Level        : TLevel;
    Style        : TBaseDosLemmingStyle;
    GraphicSet   : TBaseDosGraphicSet;
    Renderer     : TRenderer;

    // this is initialized by the window in which the game will be played
    TargetBitmap : TBitmap32;

    // this is initialized by the game
    GameResult: TGameResultsRec;

    // this is set by the individual screens when closing (if they know)
    NextScreen: TGameScreenType;

    // resource vars
    LemDataInResource   : Boolean;
    LemDataOnDisk       : Boolean;
    LemSoundsInResource : Boolean;
    LemSoundsOnDisk     : Boolean;
    LemMusicInResource  : Boolean;
    LemMusicOnDisk      : Boolean;

    // cheat
//    Cheatmode: Boolean; // levelcode screen
    MiscOptions         : TMiscOptions;

    fZoomFactor          : Integer;
    fLevelPack           : String;
    fExternalPrefix      : String;
    // set if replay loaded in postviewscreen
    //StartupWithReplay   : Boolean;

    {$ifdef testmode}
    fTestMode : Boolean;
    fTestGroundFile : String;
    fTestVgagrFile : String;
    fTestVgaspecFile : String;
    fTestLevelFile : String;
    fQuickTestMode : integer;
    {$endif}

    {$ifdef cust}{$ifndef flexi}
    fMechanics : Integer;
    {$endif}{$endif}

    {$ifdef flexi}
    SysDat               : TSysDatRec;
    {$endif}

    constructor Create;
    procedure LoadFromIniFile(const aFileName: string);
    procedure SaveToIniFile(const aFileName: string);

    {$ifdef flexi}property LevelPack: String read fLevelPack write fLevelPack;{$endif}
    {$ifndef cust}property LevelPack: String read fLevelPack write fLevelPack;{$endif}
    property Directory: string read fDirectory write fDirectory;
    property DumpMode: boolean read fDumpMode write fDumpMode;
  published
  { easy readable published props for streaming to inifile }
    property CheatCodesEnabled: Boolean read GetCheatCodesEnabled write SetCheatCodesEnabled;
    property MusicEnabled: Boolean read GetMusicEnabled write SetMusicEnabled;
    property SoundEnabled: Boolean read GetSoundEnabled write SetSoundEnabled;
    property ShowParticles: Boolean read GetShowParticles write SetShowParticles;
    property LookForLVLFiles: Boolean read GetLookForLVLFiles write SetLookForLVLFiles;
    {$ifdef cust}{$ifndef flexi}property Mechanics: Integer read fMechanics write fMechanics;
    property LevelPack: String read fLevelPack write fLevelPack;
    property ExternalPrefix: String read fExternalPrefix write fExternalPrefix;{$endif}{$endif}
    property LemmixTrapBug: boolean read GetLemmixTrapBug write SetLemmixTrapBug;
    {$ifdef testmode}property QuickTestMode: Integer read fQuickTestMode write fQuickTestMode;{$endif}
    property StateControlEnabled: Boolean read GetStateControlEnabled write SetStateControlEnabled;
    property PauseAssignEnabled: Boolean read GetPauseAssignEnabled write SetPauseAssignEnabled;

  { zoom }
    property ZoomFactor: Integer read fZoomFactor write fZoomFactor;

  end;


implementation

{ TDosGameParams }

constructor TDosGameParams.Create;
begin
  inherited Create;

  MiscOptions := DEF_MISCOPTIONS;
  LevelPack := 'LEVELPAK.DAT';
  fDumpMode := false;

  {$ifdef develop}
    Include(MiscOptions, moCheatCodes);
  {$endif}

  {$ifdef resourcelemdata}
    LemDataInResource := True;
  {$endif}

  {$ifdef resourcelemsounds}
    LemSoundsInResource := True;
  {$endif}

  {$ifdef resourcelemmusic}
    LemMusicInResource := True;
  {$endif}

  {.$ifdef resourcebassmod}
  {.$endif}



end;

function TDosGameParams.GetCheatCodesEnabled: Boolean;
begin
  Result := moCheatCodes in MiscOptions;
end;

function TDosGameParams.GetLevelPackName: String;
begin
  Result := fLevelPackName;
end;

procedure TDosGameParams.SetLevelPackName(Value: String);
begin
  fLevelPackName := Value;
end;

function TDosGameParams.GetMusicEnabled: Boolean;
begin
  Result := gsoMusic in SoundOptions;
end;

function TDosGameParams.GetShowParticles: Boolean;
begin
  Result := moShowParticles in MiscOptions;
end;

function TDosGameParams.GetSoundEnabled: Boolean;
begin
  Result := gsoSound in SoundOptions;
end;

function TDosGameParams.GetLookForLVLFiles: Boolean;
begin
  Result := moLookForLVLFiles in MiscOptions;
end;

function TDosGameParams.GetLemmixTrapBug: Boolean;
begin
  Result := moLemmixTrapBug in MiscOptions;
end;

function TDosGameParams.GetStateControlEnabled: Boolean;
begin
  Result := moStateControlEnabled in MiscOptions;
end;

function TDosGameParams.GetPauseAssignEnabled: Boolean;
begin
  Result := moPauseAssignEnabled in MiscOptions;
end;

procedure TDosGameParams.LoadFromIniFile(const aFileName: string);
begin
  IniToObject(aFileName, 'GameSettings', Self, False);
end;

procedure TDosGameParams.SaveToIniFile(const aFileName: string);
begin
  ObjectToIni(aFileName, 'GameSettings', Self, False);
end;

procedure TDosGameParams.SetCheatCodesEnabled(Value: Boolean);
begin
  case Value of
    False: Exclude(MiscOptions, moCheatCodes);
    True:  Include(MiscOptions, moCheatCodes);
  end;
end;

procedure TDosGameParams.SetMusicEnabled(Value: Boolean);
begin
  case Value of
    False: Exclude(SoundOptions, gsoMusic);
    True:  Include(SoundOptions, gsoMusic);
  end;
end;

procedure TDosGameParams.SetShowParticles(const Value: Boolean);
begin
  case Value of
    False: Exclude(MiscOptions, moShowParticles);
    True:  Include(MiscOptions, moShowParticles);
  end;
end;

procedure TDosGameParams.SetSoundEnabled(Value: Boolean);
begin
  case Value of
    False: Exclude(SoundOptions, gsoSound);
    True:  Include(SoundOptions, gsoSound);
  end;
end;

procedure TDosGameParams.SetLookForLVLFiles(Value: Boolean);
begin
  case Value of
    False: Exclude(MiscOptions, moLookForLVLFiles);
    True:  Include(MiscOptions, moLookForLVLFiles);
  end;
end;

procedure TDosGameParams.SetLemmixTrapBug(Value: Boolean);
begin
  case Value of
    False: Exclude(MiscOptions, moLemmixTrapBug);
    True:  Include(MiscOptions, moLemmixTrapBug);
  end;
end;

procedure TDosGameParams.SetStateControlEnabled(Value: Boolean);
begin
  case Value of
    False: Exclude(MiscOptions, moStateControlEnabled);
    True:  Include(MiscOptions, moStateControlEnabled);
  end;
end;

procedure TDosGameParams.SetPauseAssignEnabled(Value: Boolean);
begin
  case Value of
    False: Exclude(MiscOptions, moPauseAssignEnabled);
    True:  Include(MiscOptions, moPauseAssignEnabled);
  end;
end;

function TDosGameParams.GetUseSystemCursor: Boolean;
begin
//  Result := moUseSystemCursor in MiscOptions;
end;

procedure TDosGameParams.SetUseSystemCursor(const Value: Boolean);
begin
  {case Value of
    False: Exclude(MiscOptions, moUseSystemCursor);
    True:  Include(MiscOptions, moUseSystemCursor);
  end;}
end;

end.

