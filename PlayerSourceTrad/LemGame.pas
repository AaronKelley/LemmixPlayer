{$include lem_directives.inc}

{-------------------------------------------------------------------------------
  Some source code notes:

  • Places with optional mechanics are marked with a comment
  :
    "// @Optional Game Mechanic"

  • Note that a lot of routines start with "Result := False". I removed
    redundant code, so if you see just an "Exit" call then it is always
    to be interpreted as "Return False".

  • Transition() method: It has a default parameter. So if you see a
    call to Transition() with three parameters and the last one is TRUE, it means
    that the lemming has to turn around as well. I commented this too at all
    places where this happens.
-------------------------------------------------------------------------------}

unit LemGame;

interface

uses
  Windows, Classes, Contnrs, SysUtils, Math, Forms, Dialogs,
  Controls, PngImage,
  UMisc, TypInfo,
  GR32, GR32_OrdinalMaps, GR32_Layers,
  LemCore, LemTypes, LemDosBmp, LemDosStructures, LemStrings, LemMetaAnimation,
  LemMetaObject, LemInteractiveObject, LemSteel, LemLevel, LemStyle,
  LemGraphicSet, LemDosGraphicSet, LemRendering, LemDosAnimationSet,
  LemMusicSystem,
  GameInterfaces, GameControl, GameSound;

type
  TParticleRec = packed record
    DX, DY: ShortInt
  end;
  TParticleArray = packed array[0..79] of TParticleRec;
  TParticleTable = packed array[0..51] of TParticleArray;

const
  ParticleColorIndices: array[0..15] of Byte = (
    4, 15, 14, 13, 12, 11, 10, 9, 8, 11, 10, 9, 8, 7, 6, 2
  );

type
  tFloatParameterRec = record
    Dy: Integer;
    AnimationFrameIndex: Integer;
  end;

const
  {-------------------------------------------------------------------------------
    So what's this: A table which describes what to do when floating.
    The floaters animation has 8 frames: 0..3 is opening the umbrella
    and 4..7 is the actual floating.
    This table "fakes" 16 frames of floating and what should happen with
    the Y-position of the lemming each frame. Frame zero is missing, because that's
    automatically frame zero.
    Additionally: after 15 go back to 8
  -------------------------------------------------------------------------------}
  FloatParametersTable: array[0..15] of TFloatParameterRec = (
    (Dy:     3; AnimationFrameIndex:   1),
    (Dy:     3; AnimationFrameIndex:   2),
    (Dy:     3; AnimationFrameIndex:   3),
    (Dy:     3; AnimationFrameIndex:   5),
    (Dy:    -1; AnimationFrameIndex:   5),
    (Dy:     0; AnimationFrameIndex:   5),
    (Dy:     1; AnimationFrameIndex:   5),
    (Dy:     1; AnimationFrameIndex:   5),
    (Dy:     2; AnimationFrameIndex:   5),
    (Dy:     2; AnimationFrameIndex:   6),
    (Dy:     2; AnimationFrameIndex:   7),
    (Dy:     2; AnimationFrameIndex:   7),
    (Dy:     2; AnimationFrameIndex:   6),
    (Dy:     2; AnimationFrameIndex:   5),
    (Dy:     2; AnimationFrameIndex:   4),
    (Dy:     2; AnimationFrameIndex:   4)
  );

type
  TLemming = class
  private
    function GetLocationBounds: TRect; // rect in world
    function GetFrameBounds: TRect; // rect from animation bitmap
    function GetCountDownDigitBounds: TRect; // countdown
    function GetLemRTL: Boolean; // direction rtl?
    function GetLemHint: string;
  public
  { misc sized }
    LemSavedMap                   : array[0..8] of Byte; // saves part of ObjectMap of game when blocking
    LemEraseRect                  : TRect; // the rectangle of the last drawaction (can include space for countdown digits)
  { integer sized fields }
    LemIndex                      : Integer;        // index in the lemminglist
    LemX                          : Integer;        // the "main" foot x position
    LemY                          : Integer;        // the "main" foot y position
    LemDX                         : Integer;        // x speed (1 if left to right, -1 if right to left)
    LemFallen                     : Integer;        // number of pixels a faller has fallen
    LemExplosionTimer             : Integer;        // 79 downto 0
    LMA                           : TMetaLemmingAnimation; // ref to Lemming Meta Animation
    LAB                           : TBitmap32;      // ref to Lemming Animation Bitmap
    LemFrame                      : Integer;        // current animationframe
    LemMaxFrame                   : Integer;        // copy from LMA
    LemAnimationType              : Integer;        // copy from LMA
    LemParticleTimer              : Integer;        // @particles, 52 downto 0, after explosion
    LemParticleFrame              : Integer;        // the "frame" of the particle drawing algorithm
    FrameTopDy                    : Integer;        // = -LMA.FootY (ccexplore compatible)
    FrameLeftDx                   : Integer;        // = -LMA.FootX (ccexplore compatible)
    LemFloatParametersTableIndex  : Integer;        // index for floaters
    LemNumberOfBricksLeft         : Integer;        // for builder
    LemBorn                       : Integer;        // game iteration the lemming is created
  { byte sized fields }
    LemAction                     : TBasicLemmingAction; // current action of the lemming
    LemObjectBelow                : Byte;
    LemObjectInFront              : Byte;
    LemRemoved                    : Boolean; // the lemming is not in the level anymore
    LemEndOfAnimation             : Boolean;
    LemIsClimber                  : Boolean;
    LemIsFloater                  : Boolean;
    LemIsBlocking                 : Boolean; // not always exactly in sync with the action
    LemIsNewDigger                : Boolean;
    LemHighlightReplay            : Boolean;
    LemExploded                   : Boolean; // @particles, set after a Lemming actually exploded, used to control particles-drawing

  { properties }
    property LemRTL: Boolean read GetLemRTL; // direction rtl?
    property LemHint: string read GetLemHint;
  end;

  TLemmingList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLemming;
  protected
  public
    function Add(Item: TLemming): Integer;
    procedure Insert(Index: Integer; Item: TLemming);
    property Items[Index: Integer]: TLemming read GetItem; default;
    property List; // for fast access
  end;

  // internal object used by game
  TInteractiveObjectInfo = class
  private
    function GetBounds: TRect;
  public
    MetaObj        : TMetaObject;
    Obj            : TInteractiveObject;
    CurrentFrame   : Integer;
    Triggered      : Boolean;
    //SoundIndex     : Integer; // cached soundindex
    property Bounds: TRect read GetBounds;
  end;

  // internal list, used by game
  TInteractiveObjectInfoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TInteractiveObjectInfo;
  protected
  public
    function Add(Item: TInteractiveObjectInfo): Integer;
    procedure Insert(Index: Integer; Item: TInteractiveObjectInfo);
    property Items[Index: Integer]: TInteractiveObjectInfo read GetItem; default;
  published
  end;


  // never change this anymore. it's stored in replayfile
  TDosGameOption = (
    dgoDisableObjectsAfter15,    // objects with index higher than 15 will not work
    dgoMinerOneWayRightBug,      // the miner bug check
    dgoObsolete,                 // deleted skillbutton option. never replace.
    dgoSplattingExitsBug,
    dgoOldEntranceABBAOrder,
    dgoEntranceX25,
    dgoFallerStartsWith3,
    dgoMax4EnabledEntrances,
    dgoAssignClimberShruggerActionBug
  );
  TDosGameOptions = set of TDosGameOption;

const
  DOSORIG_GAMEOPTIONS = [
    dgoDisableObjectsAfter15,
    dgoMinerOneWayRightBug,
    dgoObsolete,
    dgoSplattingExitsBug,
    dgoOldEntranceABBAOrder,
    dgoFallerStartsWith3,
    dgoMax4EnabledEntrances,
    dgoAssignClimberShruggerActionBug
  ];

  DOSOHNO_GAMEOPTIONS = [
    dgoDisableObjectsAfter15,
    dgoMinerOneWayRightBug,
    dgoObsolete,
    dgoSplattingExitsBug,
    dgoFallerStartsWith3,
    dgoEntranceX25,
    dgoMax4EnabledEntrances
  ];

  CUSTLEMM_GAMEOPTIONS = [
    dgoDisableObjectsAfter15,
    dgoMinerOneWayRightBug,
    dgoObsolete,
    dgoSplattingExitsBug,
    dgoEntranceX25,
    dgoMax4EnabledEntrances
  ];

  FULL_GAMEOPTIONS = [
    dgoDisableObjectsAfter15,
    dgoMinerOneWayRightBug,
    dgoObsolete,
    dgoSplattingExitsBug,
    dgoFallerStartsWith3,
    dgoOldEntranceABBAOrder,
    dgoEntranceX25,
    dgoAssignClimberShruggerActionBug,
    dgoMax4EnabledEntrances
  ];

{-------------------------------------------------------------------------------
  Replay things
-------------------------------------------------------------------------------}
type
  TReplayFileHeaderRec = packed record
    Signature         : array[0..2] of Char;     //  3 bytes -  3
    Version           : Byte;                    //  1 byte  -  4
    FileSize          : Integer;                 //  4 bytes -  8
    HeaderSize        : Word;                    //  2 bytes - 10
    Mechanics         : TDosGameOptions;         //  2 bytes - 12
    FirstRecordPos    : Integer;                 //  4 bytes - 16
    ReplayRecordSize  : Word;                    //  2 bytes - 18
    ReplayRecordCount : Word;                    //  2 bytes - 20
    Reserved1         : Integer;                 //  4 bytes - 24
    Reserved2         : Integer;                 //  4 bytes - 28
    Reserved3         : Integer;                 //  4 bytes - 32
    LevelTitle        : array[0..31] of Char;    // 32 bytes - 64
  end;

const
  // never change, do NOT trust the bits are the same as the enumerated type.

  //Recorded Game Flags
  rgf_DisableObjectsAfter15          = Bit0;
	rgf_MinerOneWayRightBug            = Bit1;
	rgf_DisableButtonClicksWhenPaused  = Bit2;
	rgf_SplattingExitsBug              = Bit3;
	rgf_OldEntranceABBAOrder           = Bit4;
	rgf_EntranceX25                    = Bit5;
	rgf_FallerStartsWith3              = Bit6;
  rgf_Max4EnabledEntrances           = Bit7;
	rgf_AssignClimberShruggerActionBug = Bit8;

  //Recorded Action Flags
	raf_StartPause        = Bit0;
	raf_EndPause          = Bit1;
	raf_Pausing           = Bit2;
	raf_StartIncreaseRR   = Bit3;  // only allowed when not pausing
	raf_StartDecreaseRR   = Bit4;  // only allowed when not pausing
	raf_StopChangingRR    = Bit5;  // only allowed when not pausing
	raf_SkillSelection    = Bit6;
	raf_SkillAssignment   = Bit7;
	raf_Nuke              = Bit8;  // only allowed when not pausing, as in the game
  raf_NewNPLemming      = Bit9;  // related to emulation of right-click bug

  //Recorded Lemming Action
  rla_None       = 0;
  rla_Walking    = 1;  // not allowed
  rla_Jumping    = 2;  // not allowed
  rla_Digging    = 3;
  rla_Climbing   = 4;
  rla_Drowning   = 5;  // not allowed
  rla_Hoisting   = 6;  // not allowed
  rla_Building   = 7;
  rla_Bashing    = 8;
  rla_Mining     = 9;
  rla_Falling    = 10; // not allowed
  rla_Floating   = 11;
  rla_Splatting  = 12; // not allowed
  rla_Exiting    = 13; // not allowed
  rla_Vaporizing = 14; // not allowed
  rla_Blocking   = 15;
  rla_Shrugging  = 16; // not allowed
  rla_Ohnoing    = 17; // not allowed
  rla_Exploding  = 18;

  // Recorded Selected Button
  rsb_None       = 0;
  rsb_Slower     = 1;  // not allowed
  rsb_Faster     = 2;  // not allowed
  rsb_Climber    = 3;
  rsb_Umbrella   = 4;
  rsb_Explode    = 5;
  rsb_Stopper    = 6;
  rsb_Builder    = 7;
  rsb_Basher     = 8;
  rsb_Miner      = 9;
  rsb_Digger     = 10;
  rsb_Pause      = 11; // not allowed
  rsb_Nuke       = 12; // not allowed

type
  TLemmingGame = class;

  TReplayRec = packed record
    Check          : Char;         //  1 byte  -  1
    Iteration      : Integer;      //  4 bytes -  5
    ActionFlags    : Word;         //  2 bytes -  7
    AssignedSkill  : Byte;         //  1 byte  -  8
    SelectedButton : Byte;         //  1 byte  -  9
    ReleaseRate    : Integer;      //  1 byte  - 13
    LemmingIndex   : Integer;      //  4 bytes - 17
    LemmingX       : Integer;      //  4 bytes - 21
    LemmingY       : Integer;      //  4 bytes - 25
    CursorX        : SmallInt;     //  2 bytes - 27
    CursorY        : SmallInt;     //  2 bytes - 29
    Reserved1      : Byte;
    Reserved2      : Byte;
    Reserved3      : Byte;         // 32
  end;

  TReplayItem = class
  private
    fIteration      : Integer;
    fActionFlags    : Word;
    fAssignedSkill  : Byte;
    fSelectedButton : Byte;
    fReleaseRate    : Byte;
    fLemmingIndex   : Integer;
    fLemmingX       : Integer;
    fLemmingY       : Integer;
    fCursorY        : Integer;
    fCursorX        : Integer;
  protected
  public
    property Iteration: Integer read fIteration write fIteration;
    property ActionFlags: Word read fActionFlags write fActionFlags;
    property AssignedSkill: Byte read fAssignedSkill write fAssignedSkill;
    property SelectedButton: Byte read fSelectedButton write fSelectedButton;
    property ReleaseRate: Byte read fReleaseRate write fReleaseRate;
    property LemmingIndex: Integer read fLemmingIndex write fLemmingIndex;
    property LemmingX: Integer read fLemmingX write fLemmingX;
    property LemmingY: Integer read fLemmingY write fLemmingY;
    property CursorX: Integer read fCursorX write fCursorX;
    property CursorY: Integer read fCursorY write fCursorY;
  end;

  TRecorder = class
  private
    fGame : TLemmingGame; // backlink to game
    List  : TObjectList;
  protected
  public
    constructor Create(aGame: TLemmingGame);
    destructor Destroy; override;

    function Add: TReplayItem;
    procedure Clear;
    procedure Truncate(aCount: Integer);
    procedure SaveToFile(const aFileName: string);
    procedure SaveToStream(S: TStream);
    procedure SaveToTxt(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    procedure LoadFromOldTxt(const aFileName: string); // antique
    procedure LoadFromStream(S: TStream);
  end;


  TLemmingMethod = function (L: TLemming): Boolean of object;
  TLemmingMethodArray = array[TBasicLemmingAction] of TLemmingMethod;
  TSkillMethod = function (Lemming1, Lemming2: TLemming): Boolean of object;
  TSkillMethodArray = array[TBasicLemmingAction] of TSkillMethod;
  TLemmingEvent = procedure (L: TLemming) of object;

  TLemmingGame = class(TComponent)
  private
    fTargetBitmap              : TBitmap32; // reference to the drawing bitmap on the gamewindow
    fSelectedSkill             : TSkillPanelButton; // TUserSelectedSkill; // currently selected skill restricted by F3-F9
    fOptions                   : TDosGameOptions; // mechanic options
    fParticles                 : TParticleTable; // all particle offsets
    fParticleColors            : array[0..15] of TColor32;

    fHitTestAutoFail           : Boolean;

  { internal objects }
    LemmingList                : TLemmingList; // the list of lemmings
    World                      : TBitmap32; // actual bitmap that is changed by the lemmings
    ObjectMap                  : TByteMap; // dos compatible 4 pixel resolution map
    MiniMap                    : TBitmap32; // minimap of world
    fMinimapBuffer             : TBitmap32; // drawing buffer minimap
    fRecorder                  : TRecorder;

  { reference objects, mostly for easy access in the mechanics-code }
    fGameParams                : TDosGameParams; // ref
    fRenderer                  : TRenderer; // ref to gameparams.renderer
    fInfoPainter               : IGameToolbar; // ref to interface to enable this component to draw to skillpanel
    fLevel                     : TLevel; // ref to gameparams.level
    Style                      : TBaseLemmingStyle; // ref to gameparams.style
    Graph                      : TBaseDosGraphicSet; // ref to gameparams.graph
    CntDownBmp                 : TBitmap32; // ref to style.animationset.countdowndigits
    ExplodeMaskBmp             : TBitmap32; // ref to style.animationset.explosionmask
    BashMasks                  : TBitmap32; // ref to style.animationset.bashmasks
    BashMasksRTL               : TBitmap32; // ref to style.animationset.bashmasksrtl
    MineMasks                  : TBitmap32; // ref to style.animationset.minemasks
    MineMasksRTL               : TBitmap32; // ref to style.animationset.minemasksrtl

  { vars }
    fCurrentIteration          : Integer;
    fGlitchIterations          : Integer;
    fReplayGlitchIterations    : Integer;
    fClockFrame                : Integer; // 17 frames is one game-second
    LemmingsReleased           : Integer; // number of lemmings that were created
    LemmingsOut                : Integer; // number of lemmings currently walking around
    LemmingsIn                 : integer; // number of lemmings that made it to heaven
    LemmingsRemoved            : Integer; // number of lemmings removed
    fCursorPoint               : TPoint;
    fRightMouseButtonHeldDown  : Boolean;
    Minutes                    : Integer; // minutes left
    Seconds                    : Integer; // seconds left
    fPlaying                   : Boolean; // game in active playing mode?
    EntriesOpened              : Boolean;
    LemmingMethods             : TLemmingMethodArray; // a method for each basic lemming state
    SkillMethods               : TSkillMethodArray; // a method for assigning jobs (including dummies)
    fCheckWhichLemmingOnly     : Boolean; // use during replays only, to signal the AssignSkill methods
                                          // to only indicate which Lemming gets the assignment, without
                                          // actually doing the assignment
    WhichLemming               : TLemming; // see above
    LastNPLemming              : TLemming; // for emulation of right-click bug
    ObjectInfos                : TInteractiveObjectInfoList; // list of objects excluding entrances
    Entries                    : TInteractiveObjectInfoList; // list of entrances (NOT USED ANYMORE)
    DosEntryTable              : array[0..3] of Integer; // table for entrance release order
    fSlowingDownReleaseRate    : Boolean;
    fSpeedingUpReleaseRate     : Boolean;
    fPaused                    : Boolean;
    fPausedSkillAssignmentAllowed : Boolean;
    MaxNumLemmings             : Integer;
    CurrReleaseRate            : Integer;
    CurrClimberCount           : Integer;
    CurrFloaterCount           : Integer;
    CurrBomberCount            : Integer;
    CurrBlockerCount           : Integer;
    CurrBuilderCount           : Integer;
    CurrBasherCount            : Integer;
    CurrMinerCount             : Integer;
    CurrDiggerCount            : Integer;
    UserSetNuking              : Boolean;
    ExploderAssignInProgress   : Boolean;
    Index_LemmingToBeNuked     : Integer;
    fCurrentCursor             : Integer; // normal or highlight
    BrickPixelColor            : TColor32;
    BrickPixelColors           : array[0..11] of TColor32; // gradient steps
    fGameFinished              : Boolean;
    fGameCheated               : Boolean;
    NextLemmingCountDown       : Integer;
    fDrawLemmingPixel          : Boolean;
    fFastForward               : Boolean;
    fReplaying                 : Boolean;
    fReplayIndex               : Integer;
    fCurrentScreenPosition     : TPoint; // for minimap, this really sucks but works ok for the moment
    fLastCueSoundIteration     : Integer;
    fSoundToPlay               : Integer;
    fFading                    : Boolean;
    fReplayCommanding          : Boolean;
    fTargetIteration           : Integer; // this is used in hyperspeed
    fHyperSpeedCounter         : Integer; // no screenoutput
    fHyperSpeed                : Boolean; // we are at hyperspeed no targetbitmap output
    fLeavingHyperSpeed         : Boolean; // in between state (see UpdateLemmings)
    fPauseOnHyperSpeedExit     : Boolean; // to maintain pause state before invoking a savestate
    fEntranceAnimationCompleted: Boolean;
    fStartupMusicAfterEntry    : Boolean;
    fCurrentlyDrawnLemming     : TLemming; // needed for pixelcombining bridges in combinebuilderpixels
    fUseGradientBridges        : Boolean;
    fExplodingGraphics         : Boolean;
    fDoTimePause               : Boolean;
  { sound vars }
    fSoundOpts                 : TGameSoundOptions;
    SoundMgr                   : TSoundMgr;
  { sound indices in list of soundmgr}
    SFX_BUILDER_WARNING        : Integer;
    SFX_ASSIGN_SKILL           : Integer;
    SFX_YIPPEE                 : Integer;
    SFX_SPLAT                  : Integer;
    SFX_LETSGO                 : Integer;
    SFX_ENTRANCE               : Integer;
    SFX_VAPORIZING             : Integer;
    SFX_DROWNING               : Integer;
    SFX_EXPLOSION              : Integer;
    SFX_HITS_STEEL             : Integer;
    SFX_OHNO                   : Integer;
    SFX_SKILLBUTTON            : Integer;
    SFX_ROPETRAP               : Integer;
    SFX_TENTON                 : Integer;
    SFX_BEARTRAP               : Integer;
    SFX_ELECTROTRAP            : Integer;
    SFX_SPINNINGTRAP           : Integer;
    SFX_SQUISHINGTRAP          : Integer;
  { errors }
    //fErrorFrameCountDown       : Integer;
    //fErrorMsg                  : string;
    //fErrorCode                 : Integer;
  { events }
    fOnDebugLemming            : TLemmingEvent; // eventhandler for debugging lemming under the cursor
    fOnFinish                  : TNotifyEvent;
    fParticleFinishTimer       : Integer; // extra frames to enable viewing of explosions
  { pixel combine eventhandlers }
    procedure CombineDefaultPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineLemmingPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineBuilderPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineLemmingHighlight(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineMaskPixels(F: TColor32; var B: TColor32; M: TColor32);
    procedure CombineMinimapWorldPixels(F: TColor32; var B: TColor32; M: TColor32);

  { internal methods }
    procedure ApplyBashingMask(L: TLemming; MaskFrame: Integer);
    procedure ApplyExplosionMask(L: TLemming);
    procedure ApplyMinerMask(L: TLemming; MaskFrame, X, Y: Integer);
    function CalculateNextLemmingCountdown: Integer;
    procedure CheckAdjustReleaseRate;
    procedure CheckForGameFinished;
    procedure CheckForInteractiveObjects(L: TLemming);
    function CheckForLevelTopBoundary(L: TLemming; LocalFrameTopDy: Integer = 0): Boolean;
    function CheckForOverlappingField(L: TLemming): Boolean;
    procedure CheckForPlaySoundEffect;
    procedure CheckForReplayAction;
    procedure CheckLemmings;
    procedure CheckReleaseLemming;
    procedure CheckUpdateNuking;
    procedure CueSoundEffect(aSoundId: Integer);
    function DigOneRow(L: TLemming; Y: Integer): Boolean;
    procedure DrawAnimatedObjects;
    procedure DrawDebugString(L: TLemming);
    procedure DrawLemmings;
    procedure DrawParticles(L: TLemming);
    procedure DrawStatics;
    procedure EraseLemmings;
    procedure EraseParticles(L: TLemming);
    function GetTrapSoundIndex(aDosSoundEffect: Integer): Integer;
    function HasPixelAt(X, Y: Integer): Boolean;
    function HasPixelAt_ClipY(X, Y, minY: Integer): Boolean;
    procedure IncrementIteration;
    procedure InitializeBrickColors(aBrickPixelColor: TColor32);
    procedure InitializeMiniMap;
    procedure InitializeObjectMap;
    procedure LayBrick(L: TLemming);
    function PrioritizedHitTest(out Lemming1, Lemming2: TLemming;
                                MousePos: TPoint;
                                CheckRightMouseButton: Boolean = True): Integer;
    function ReadObjectMap(X, Y: Integer): Byte;
    procedure RecordStartPause;
    procedure RecordEndPause;
    procedure RecordNuke;
    procedure RecordReleaseRate(aActionFlag: Byte);
    procedure RecordSkillAssignment(L: TLemming; aSkill: TBasicLemmingAction);
    procedure RecordSkillSelection(aSkill: TSkillPanelButton);
    procedure RemoveLemming(L: TLemming);
    procedure RemovePixelAt(X, Y: Integer);
    procedure ReplaySkillAssignment(aReplayItem: TReplayItem);
    procedure ReplaySkillSelection(aReplayItem: TReplayItem);
    procedure RestoreMap(L: TLemming);
    procedure SaveMap(L: TLemming);
    procedure SetBlockerField(L: TLemming);
    procedure Transition(L: TLemming; aAction: TBasicLemmingAction; DoTurn: Boolean = False);
    procedure TurnAround(L: TLemming);
    function UpdateExplosionTimer(L: TLemming): Boolean;
    procedure UpdateInteractiveObjects;
    procedure WriteObjectMap(X, Y: Integer; aValue: Byte);

  { lemming actions }
    function HandleLemming(L: TLemming): Boolean;
    function HandleWalking(L: TLemming): Boolean;
    function HandleJumping(L: TLemming): Boolean;
    function HandleDigging(L: TLemming): Boolean;
    function HandleClimbing(L: TLemming): Boolean;
    function HandleDrowning(L: TLemming): Boolean;
    function HandleHoisting(L: TLemming): Boolean;
    function HandleBuilding(L: TLemming): Boolean;
    function HandleBashing(L: TLemming): Boolean;
    function HandleMining(L: TLemming): Boolean;
    function HandleFalling(L: TLemming): Boolean;
    function HandleFloating(L: TLemming): Boolean;
    function HandleSplatting(L: TLemming): Boolean;
    function HandleExiting(L: TLemming): Boolean;
    function HandleVaporizing(L: TLemming): Boolean;
    function HandleBlocking(L: TLemming): Boolean;
    function HandleShrugging(L: TLemming): Boolean;
    function HandleOhNoing(L: TLemming): Boolean;
    function HandleExploding(L: TLemming): Boolean;

  { interaction }
    function AssignSkill(Lemming1, Lemming2: TLemming; aSkill: TBasicLemmingAction): Boolean; // key method
    function AssignClimber(Lemming1, Lemming2: TLemming): Boolean;
    function AssignFloater(Lemming1, Lemming2: TLemming): Boolean;
    function AssignBomber(Lemming1, Lemming2: TLemming): Boolean;
    function AssignBlocker(Lemming1, Lemming2: TLemming): Boolean;
    function AssignBuilder(Lemming1, Lemming2: TLemming): Boolean;
    function AssignBasher(Lemming1, Lemming2: TLemming): Boolean;
    function AssignMiner(Lemming1, Lemming2: TLemming): Boolean;
    function AssignDigger(Lemming1, Lemming2: TLemming): Boolean;
    procedure SetOptions(const Value: TDosGameOptions);
    procedure SetSoundOpts(const Value: TGameSoundOptions);
  public
    GameResult                     : Boolean;
    GameResultRec                  : TGameResultsRec;
    SkillButtonsDisabledWhenPaused : Boolean; // this really should move somewere else
    UseReplayPhotoFlashEffect      : Boolean;
    fXmasPal                   : Boolean;
    InstReleaseRate                : Integer;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { iteration }
    procedure PrepareParams(aParams: TDosGameParams);
    procedure Start(aReplay: Boolean = False);
    procedure UpdateLemmings;

  { callable }
    procedure AdjustReleaseRate(Delta: Integer);
    procedure CreateLemmingAtCursorPoint;
    procedure Finish;
    procedure Cheat;
    procedure HitTest;
    procedure HyperSpeedBegin(PauseWhenDone: Boolean = False);
    procedure HyperSpeedEnd;
    function ProcessSkillAssignment: Boolean;
    procedure RegainControl;
    procedure Save(TestSave: Boolean = false);
    procedure SaveGameplayImage(Filename: String);
    procedure SetGameResult;
    procedure SetSelectedSkill(Value: TSkillPanelButton; MakeActive: Boolean = True; RightClick: Boolean = False);
    procedure SetSoundChannels(i: integer);

  { properties }
    property CurrentCursor: Integer read fCurrentCursor;
    property CurrentIteration: Integer read fCurrentIteration;
    property GlitchIterations: Integer read fGlitchIterations;
    property ReplayGlitchIterations: Integer read fReplayGlitchIterations write fReplayGlitchIterations;
    property ClockFrame: Integer read fClockFrame;
    property CursorPoint: TPoint read fCursorPoint write fCursorPoint;
    property DrawLemmingPixel: Boolean read fDrawLemmingPixel write fDrawLemmingPixel;
    property Fading: Boolean read fFading;
    property FastForward: Boolean read fFastForward write fFastForward;
    property GameFinished: Boolean read fGameFinished;
    property HyperSpeed: Boolean read fHyperSpeed;
    property InfoPainter: IGameToolbar read fInfoPainter write fInfoPainter;
    property Level: TLevel read fLevel write fLevel;
    property CurrentScreenPosition: TPoint read fCurrentScreenPosition write fCurrentScreenPosition;
    property MiniMapBuffer: TBitmap32 read fMiniMapBuffer;
    property Options: TDosGameOptions read fOptions write SetOptions default DOSORIG_GAMEOPTIONS;
    property Paused: Boolean read fPaused write fPaused;
    property PausedSkillAssignmentAllowed: Boolean read fPausedSkillAssignmentAllowed write fPausedSkillAssignmentAllowed;
    property Playing: Boolean read fPlaying write fPlaying;
    property Renderer: TRenderer read fRenderer;
    property Replaying: Boolean read fReplaying;
    property Recorder: TRecorder read fRecorder;
    property RightMouseButtonHeldDown: Boolean read fRightMouseButtonHeldDown write fRightMouseButtonHeldDown;
    property SlowingDownReleaseRate: Boolean read fSlowingDownReleaseRate;
    property SoundOpts: TGameSoundOptions read fSoundOpts write SetSoundOpts;
    property SpeedingUpReleaseRate: Boolean read fSpeedingUpReleaseRate;
    property TargetIteration: Integer read fTargetIteration write fTargetIteration;
    property DoTimePause: Boolean read fDoTimePause write fDoTimePause;

    property SelectedSkill: TSkillPanelButton read fSelectedSkill;
    property HitTestAutoFail: Boolean read fHitTestAutoFail write fHitTestAutoFail;

  { events }
    property OnDebugLemming: TLemmingEvent read fOnDebugLemming write fOnDebugLemming;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
  end;

{-------------------------------------------------------------------------------
  The global game mechanics instance, which has to be global  (for now) because
  we have to be able to start/save/load replay from the postview screen.
  GlobalGame is initialized by the mainform.
-------------------------------------------------------------------------------}
var
  GlobalGame: TLemmingGame; 

implementation

uses
  LemDosStyle;

const
  OBJMAPOFFSET = 16;
  OBJMAPADD = OBJMAPOFFSET div 4;

const
  LEMMIX_REPLAY_VERSION    = 1;
  MAX_REPLAY_RECORDS       = 32768;
  MAX_FALLDISTANCE         = 60;

const
  // values for the (4 pixel resolution) Dos Object Map (for triggereffects)
  DOM_NONE             = 128 + 0;
  DOM_EXIT             = 128 + 1;
  DOM_FORCELEFT        = 128 + 2; // left arm of blocker
  DOM_FORCERIGHT       = 128 + 3; // right arm of blocker
//  DOM_TRAP             = 128 + 4; // triggered trap
  DOM_WATER            = 128 + 5; // causes drowning
  DOM_FIRE             = 128 + 6; // causes vaporizing
  DOM_ONEWAYLEFT       = 128 + 7;
  DOM_ONEWAYRIGHT      = 128 + 8;
  DOM_STEEL            = 128 + 9;
  DOM_BLOCKER          = 128 + 10; // the middle part of blocker

  HEAD_MIN_Y = -5;
  LEMMING_MIN_X = 0;
  LEMMING_MAX_X = 1647;
  LEMMING_MAX_Y = 163;

  PARTICLE_FRAMECOUNT = 52;
  PARTICLE_FINISH_FRAMECOUNT = 52;

function CheckRectCopy(const A, B: TRect): Boolean;
begin
  Result := (RectWidth(A) = RectWidth(B))
            and (Rectheight(A) = Rectheight(B));
end;

{ TLemming }

function TLemming.GetCountDownDigitBounds: TRect;
begin
  with Result do
  begin
    Left := LemX - 1;
    Top := LemY + FrameTopDy - 12;
    Right := Left + 8;
    Bottom := Top + 8;
  end;
end;

function TLemming.GetFrameBounds: TRect;
begin
  Assert(LAB <> nil, 'getframebounds error');
  with Result do
  begin
    Left := 0;
    Top := LemFrame * LMA.Height;
    Right := LMA.Width;
    Bottom := Top + LMA.Height;
  end;
end;

function TLemming.GetLocationBounds: TRect;
begin
  Assert(LMA <> nil, 'meta animation error');
  with Result do
  begin
    Left := LemX - LMA.FootX;
    Top := LemY - LMA.FootY;
    Right := Left + LMA.Width;
    Bottom := Top + LMA.Height;
  end;
end;

function TLemming.GetLemRTL: Boolean;
begin
  Result := LemDx < 0;
end;

function TLemming.GetLemHint: string;
const
  BoolStrings: array[Boolean] of string = ('false', 'true');
begin
  Result := 'Action=' + CutLeft(GetEnumName(TypeInfo(TBasicLemmingAction), Integer(LemAction)), 2) + ', ' +
            'Index=' + i2s(LemIndex) + ', ' +
            'X=' + i2s(LemX) + ', ' +
            'Y=' + i2s(LemY) + ', ' +
            'Dx=' + i2s(LemDX) + ', ' +
            'Fallen=' + i2s(LemFallen) + ', ' +
            'ExplosionTimer=' + i2s(LemExplosionTimer) + ', ' +
            'Frame=' + i2s(LemFrame) + ', ' +
            'MaxFrame=' + i2s(LemMaxFrame) + ', ' +
            'FrameLeftDx=' + i2s(FrameLeftDx) + ', ' +
            'FrameTopDy=' + i2s(FrameTopDy) + ', ' +
            'FloatParamTableIndex=' + i2s(LemFloatParametersTableIndex) + ', ' +
            'NumberOfBricksLeft=' + i2s(LemNumberOfBricksLeft) + ', ' +
            'Born=' + i2s(LemBorn) + ', ' +
            'ObjBelow=' + i2s(LemObjectBelow) + ', ' +
            'ObjInFront=' + i2s(LemObjectInFront) + ', ' +
            'IsNewDigger=' + BoolStrings[LemIsNewDigger] + ', ' +
            'IsBlocking=' + BoolStrings[LemIsBlocking] + ', ' +
            'CanClimb=' + BoolStrings[LemIsClimber] + ', ' +
            'CanFloat=' + BoolStrings[LemIsFloater];

// LemSavedMap, LemObjectBelow, LemObjectInFront
end;

{ TLemmingList }

function TLemmingList.Add(Item: TLemming): Integer;
begin
  Result := inherited Add(Item);
end;

function TLemmingList.GetItem(Index: Integer): TLemming;
begin
  Result := inherited Get(Index);
end;

procedure TLemmingList.Insert(Index: Integer; Item: TLemming);
begin
  inherited Insert(Index, Item);
end;

{ TInteractiveObjectInfo }

function TInteractiveObjectInfo.GetBounds: TRect;
begin
  Result.Left := Obj.Left;
  Result.Top := Obj.Top;
  Result.Right := Result.Left + MetaObj.Height;
  Result.Bottom := Result.Top + MetaObj.Width;
end;


{ TObjectAnimationInfoList }

function TInteractiveObjectInfoList.Add(Item: TInteractiveObjectInfo): Integer;
begin
  Result := inherited Add(Item);
end;

function TInteractiveObjectInfoList.GetItem(Index: Integer): TInteractiveObjectInfo;
begin
  Result := inherited Get(Index);
end;

procedure TInteractiveObjectInfoList.Insert(Index: Integer; Item: TInteractiveObjectInfo);
begin
  inherited Insert(Index, Item);
end;

{ TLemmingGame }

constructor TLemmingGame.Create(aOwner: TComponent);
var
  P: string;
begin
  inherited Create(aOwner);

  LemmingList    := TLemmingList.Create;
  World          := TBitmap32.Create;
  ObjectInfos    := TInteractiveObjectInfoList.Create;
  Entries        := TInteractiveObjectInfoList.Create;
  ObjectMap      := TByteMap.Create;
  MiniMap        := TBitmap32.Create;
  fMinimapBuffer := TBitmap32.Create;
  fRecorder      := TRecorder.Create(Self);
  fOptions       := DOSORIG_GAMEOPTIONS;
  SoundMgr       := TSoundMgr.Create;

  LemmingMethods[baNone]       := nil;
  LemmingMethods[baWalking]    := HandleWalking;
  LemmingMethods[baJumping]    := HandleJumping;
  LemmingMethods[baDigging]    := HandleDigging;
  LemmingMethods[baClimbing]   := HandleClimbing;
  LemmingMethods[baDrowning]   := HandleDrowning;
  LemmingMethods[baHoisting]   := HandleHoisting;
  LemmingMethods[baBuilding]   := HandleBuilding;
  LemmingMethods[baBashing]    := HandleBashing;
  LemmingMethods[baMining]     := HandleMining;
  LemmingMethods[baFalling]    := HandleFalling;
  LemmingMethods[baFloating]   := HandleFloating;
  LemmingMethods[baSplatting]  := HandleSplatting;
  LemmingMethods[baExiting]    := HandleExiting;
  LemmingMethods[baVaporizing] := HandleVaporizing;
  LemmingMethods[baBlocking]   := HandleBlocking;
  LemmingMethods[baShrugging]  := HandleShrugging;
  LemmingMethods[baOhnoing]    := HandleOhNoing;
  LemmingMethods[baExploding]  := HandleExploding;

  SkillMethods[baNone]         := nil;
  SkillMethods[baWalking]      := nil;
  SkillMethods[baJumping]      := nil;
  SkillMethods[baDigging]      := AssignDigger;
  SkillMethods[baClimbing]     := AssignClimber;
  SkillMethods[baDrowning]     := nil;
  SkillMethods[baHoisting]     := nil;
  SkillMethods[baBuilding]     := AssignBuilder;
  SkillMethods[baBashing]      := AssignBasher;
  SkillMethods[baMining]       := AssignMiner;
  SkillMethods[baFalling]      := nil;
  SkillMethods[baFloating]     := AssignFloater;
  SkillMethods[baSplatting]    := nil;
  SkillMethods[baExiting]      := nil;
  SkillMethods[baVaporizing]   := nil;
  SkillMethods[baBlocking]     := AssignBlocker;
  SkillMethods[baShrugging]    := nil;
  SkillMethods[baOhnoing]      := nil;
  SkillMethods[baExploding]    := AssignBomber;

  P := AppPath;

  with SoundMgr do
  begin
    SFX_BUILDER_WARNING := AddSoundFromFileName(P + 'sounds\win\' + 'dosbrickclink.wav');
    SFX_ASSIGN_SKILL    := AddSoundFromFileName(P + 'sounds\win\' + 'dosskillassign.wav');
    SFX_YIPPEE          := AddSoundFromFileName(P + 'sounds\win\' + 'doslemmingexit.wav');
    SFX_SPLAT           := AddSoundFromFileName(P + 'sounds\win\' + 'doslemsplat.wav');
    SFX_LETSGO          := AddSoundFromFileName(P + 'sounds\win\' + 'dosletsgo.wav');
    SFX_ENTRANCE        := AddSoundFromFileName(P + 'sounds\win\' + 'doshatchopen.wav');
    SFX_VAPORIZING      := AddSoundFromFileName(P + 'sounds\win\' + 'doslemburn.wav');
    SFX_DROWNING        := AddSoundFromFileName(P + 'sounds\win\' + 'doslemdrown.wav');
    SFX_EXPLOSION       := AddSoundFromFileName(P + 'sounds\win\' + 'dosexplosion.wav');
    SFX_HITS_STEEL      := AddSoundFromFileName(P + 'sounds\win\' + 'dosbrokepick.wav');
    SFX_OHNO            := AddSoundFromFileName(P + 'sounds\win\' + 'dosohno.wav');
    SFX_SKILLBUTTON     := AddSoundFromFileName(P + 'sounds\win\' + 'dosskillchange.wav');
    SFX_ROPETRAP        := AddSoundFromFileName(P + 'sounds\win\' + 'doslemspintrap.wav');
    SFX_TENTON          := AddSoundFromFileName(P + 'sounds\win\' + 'doslemweight.wav');
    SFX_BEARTRAP        := AddSoundFromFileName(P + 'sounds\win\' + 'doslembeartrap.wav');
    SFX_ELECTROTRAP     := AddSoundFromFileName(P + 'sounds\win\' + 'doslemdisintegrate.wav');
    SFX_SPINNINGTRAP    := AddSoundFromFileName(P + 'sounds\win\' + 'doslemspintrap.wav');
    SFX_SQUISHINGTRAP   := AddSoundFromFileName(P + 'sounds\win\' + 'doslemsmasher.wav');

    BrickSound := SFX_BUILDER_WARNING;
  end;

  fHitTestAutoFail := false;

end;

destructor TLemmingGame.Destroy;
begin
  LemmingList.Free;
  ObjectInfos.Free;
  World.Free;
  Entries.Free;
  ObjectMap.Free;
  MiniMap.Free;
  fMinimapBuffer.Free;
  fRecorder.Free;
  SoundMgr.Free;
  inherited Destroy;
end;

procedure TLemmingGame.PrepareParams(aParams: TDosGameParams);
var
  //Inf: TRenderInfoRec;
  Ani: TBaseDosAnimationSet;
  i: Integer;
  Bmp: TBitmap32;
  LowPal, HiPal, Pal: TArrayOfColor32;
  MusicSys: TBaseMusicSystem;
  //LemBlack: TColor32;
  S: TStream;
//  TempColor: TColor32;
begin
  fGameParams := aParams;
  {$ifdef flexi}fXmasPal := fGameParams.SysDat.Options2 and 2 <> 0;
  {$else}fXmasPal := false;{$endif}

  fStartupMusicAfterEntry := True;

  fSoundOpts := fGameParams.SoundOptions;
  fUseGradientBridges := moGradientBridges in fGameParams.MiscOptions;

  fRenderer := fGameParams.Renderer; // set ref
  fTargetBitmap := fGameParams.TargetBitmap;
  Level := fGameParams.Level;
  Style := fGameParams.Style;
  Graph := fGameParams.GraphicSet;

  {-------------------------------------------------------------------------------
    Initialize the palette of AnimationSet.
    Low part is the fixed palette
    Hi part comes from the graphicset.
    After that let the AnimationSet read the animations
  -------------------------------------------------------------------------------}
  LowPal := DosPaletteToArrayOfColor32(DosInLevelPalette);
  if fXmasPal then
  begin
    LowPal[1] := $D02020;
    LowPal[4] := $F0F000;
    LowPal[5] := $4040E0;
  end;
  HiPal := Graph.PaletteCustom;
  LowPal[7] := HiPal[0]; // copy the brickcolor
  SetLength(Pal, 16);
  for i := 0 to 7 do
    Pal[i] := LowPal[i];
  for i := 8 to 15 do
    Pal[i] := HiPal[i - 8];
  Ani := Style.AnimationSet as TBaseDosAnimationSet;
  Ani.AnimationPalette := Copy(Pal);
  Ani.ClearData;
  Ani.ReadData;

  // initialize explosion particle colors
  for i := 0 to 15 do
    fParticleColors[i] := Pal[ParticleColorIndices[i]];

  // prepare masks for drawing
  CntDownBmp := Ani.CountDownDigitsBitmap;
  CntDownBmp.DrawMode := dmCustom;
  CntDownBmp.OnPixelCombine := CombineDefaultPixels;

  ExplodeMaskBmp := Ani.ExplosionMaskBitmap;
  ExplodeMaskBmp.DrawMode := dmCustom;
  ExplodeMaskBmp.OnPixelCombine := CombineMaskPixels;

  BashMasks := Ani.BashMasksBitmap;
  BashMasks.DrawMode := dmCustom;
  BashMasks.OnPixelCombine := CombineMaskPixels;

  BashMasksRTL := Ani.BashMasksRTLBitmap;
  BashMasksRTL.DrawMode := dmCustom;
  BashMasksRTL.OnPixelCombine := CombineMaskPixels;

  MineMasks := Ani.MineMasksBitmap;
  MineMasks.DrawMode := dmCustom;
  MineMasks.OnPixelCombine := CombineMaskPixels;

  MineMasksRTL := Ani.MineMasksRTLBitmap;
  MineMasksRTL.DrawMode := dmCustom;
  MineMasksRTL.OnPixelCombine := CombineMaskPixels;

  // prepare animationbitmaps for drawing (set eventhandlers)
  with Ani.LemmingAnimations do
    for i := 0 to Count - 1 do
    begin
      Bmp := List^[i];
      Bmp.DrawMode := dmCustom;
      if i in [BRICKLAYING, BRICKLAYING_RTL] then
        Bmp.OnPixelCombine := CombineBuilderPixels
      else
        Bmp.OnPixelCombine := CombineLemmingPixels;
    end;

  World.SetSize(GAME_BMPWIDTH, 160);

  MusicSys := fGameParams.Style.MusicSystem;
  if MusicSys <> nil then
    with fGameParams.Info do
      if Level.Info.MusicTrack = 0 then
      begin
        {$ifdef orig}
        if ((Level.Info.GraphicSetEx mod 256) > 0) and ((Level.Info.GraphicSetEx mod 256) < 5) then
        begin
          case (Level.Info.GraphicSetEx mod 256) of
            1: SoundMgr.AddMusicFromFileName(MusicsPath + 'track_20');
            2: SoundMgr.AddMusicFromFileName(MusicsPath + 'track_19');
            3: SoundMgr.AddMusicFromFileName(MusicsPath + 'track_21');
            4: SoundMgr.AddMusicFromFileName(MusicsPath + 'track_18');
          end;
        end else
        {$endif}
          SoundMgr.AddMusicFromFileName(MusicsPath + MusicSys.GetMusicFileName(dPack, dSection, dLevel));
      end else
        SoundMgr.AddMusicFromFileName(MusicsPath + 'track_' + LeadZeroStr(Level.Info.MusicTrack, 2));

  S := CreateDataStream('explode.dat', ldtParticles);
  S.Seek(0, soFromBeginning);
  S.Read(fParticles, S.Size);
  S.Free;

end;

procedure TLemmingGame.Start(aReplay: Boolean = False);
{-------------------------------------------------------------------------------
  part of the initialization is still in FGame. bad programming
  (i.e: renderer.levelbitmap, level, infopainter)
-------------------------------------------------------------------------------}
var
  i:integer;
  O: TInteractiveObject;
  MO: TMetaObject;
  Inf: TInteractiveObjectInfo;
  numEntries:integer;
//  Bmp: TBitmap32;
const
  OID_EXIT                  = 0;
  OID_ENTRY                 = 1;
begin
  Assert(InfoPainter <> nil);

  if fGameParams.MusicEnabled then
    SetSoundChannels(4)
    else
    SetSoundChannels(7);

  Playing := False;

  fRenderer.RenderWorld(World, False);

  { TODO : this is slowing down hyperspeed/gotosavestate }
  fTargetBitmap.Assign(World);

  // hyperspeed things
  fTargetIteration := 0;
  fHyperSpeedCounter := 0;
  fHyperSpeed := False;
  fLeavingHyperSpeed := False;
  fPauseOnHyperSpeedExit := False;
  fEntranceAnimationCompleted := False;

  fFastForward := False;

  //Inc(fStartCalls); // this is used for music, we do not want to restart music each start
  fGameFinished := False;
  fGameCheated := False;
  LemmingsReleased := 0;
//  fTargetBitmap := Renderer.LevelBitmap;
  World.Assign(fTargetBitmap);
  World.OuterColor := 0;
  Minutes := Level.Info.TimeLimit;
  Seconds := 0;
//  Style := Level.Style;
//  Graph := Level.Graph;

  {$ifdef flexi}
  Options := FULL_GAMEOPTIONS;
   {dgoFallerStartsWith3,
    dgoOldEntranceABBAOrder,
    dgoEntranceX25,
    dgoAssignClimberShruggerActionBug,}
  if fGameParams.SysDat.Options2 and 64 = 0 then
    Exclude(fOptions, dgoFallerStartsWith3);
  if fGameParams.SysDat.Options2 and 128 = 0 then
    Exclude(fOptions, dgoOldEntranceABBAOrder);
  if fGameParams.SysDat.Options3 and 1 <> 0 then
    Exclude(fOptions, dgoEntranceX25);
  if fGameParams.SysDat.Options3 and 2 = 0 then
    Exclude(fOptions, dgoAssignClimberShruggerActionBug);
  {$else}
  if Style is TDosCustStyle then
  begin
    Options := CUSTLEMM_GAMEOPTIONS;
    {$ifdef cust}{$ifndef flexi}case fGameParams.Mechanics of
      1: Options := DOSORIG_GAMEOPTIONS;
      2: Options := DOSOHNO_GAMEOPTIONS;
      //else Options := CUSTLEMM_GAMEOPTIONS;
    end;{$endif}{$endif}
  end else if Style is TDosOrigStyle then
    Options := DOSORIG_GAMEOPTIONS
  else
    Options := DOSOHNO_GAMEOPTIONS;
  {$endif}

  SkillButtonsDisabledWhenPaused := false;

  FillChar(GameResultRec, SizeOf(GameResultRec), 0);
  GameResultRec.gCount  := Level.Info.LemmingsCount;
  GameResultRec.gToRescue := Level.Info.RescueCount;


  fReplayIndex := 0;
  LemmingsReleased := 0;
  LemmingsOut := 0;
  LemmingsIn := 0;
  LemmingsRemoved := 0;
  fRightMouseButtonHeldDown := False;
  fCurrentIteration := 0;
  fGlitchIterations := 0;
  //fReplayGlitchIterations := 0;
  fLastCueSoundIteration := 0;
  fClockFrame := 0;
  fFading := False;
  EntriesOpened := False;
  ObjectInfos.Clear;
  Entries.Clear;

  // below not accurate emulation, but not likely to find levels out there
  // with 0 entrances.
  // We'll use -1 to represent "no entrance".
  DosEntryTable[0] := -1;
  DosEntryTable[1] := -1;
  DosEntryTable[2] := -1;
  DosEntryTable[3] := -1;

  fSlowingDownReleaseRate := False;
  fSpeedingUpReleaseRate := False;
  fPaused := False;
  fPausedSkillAssignmentAllowed := True;
  UserSetNuking := False;
  ExploderAssignInProgress := False;
  Index_LemmingToBeNuked := 0;
  fCurrentCursor := 0;
  fParticleFinishTimer := 0;
  LemmingList.Clear;
  LastNPLemming := nil;
  fSoundToPlay := -1;
  if not aReplay then
    fRecorder.Clear;

  fReplaying := aReplay;
  // fReplayedLemmingIndex := -1;

  fExplodingGraphics := False;


  with Level.Info do
  begin
    MaxNumLemmings := LemmingsCount;

    currReleaseRate    := ReleaseRate  ;
    currClimberCount   := ClimberCount ;
    currFloaterCount   := FloaterCount ;
    currBomberCount    := BomberCount  ;
    currBlockerCount   := BlockerCount ;
    currBuilderCount   := BuilderCount ;
    currBasherCount    := BasherCount  ;
    currMinerCount     := MinerCount   ;
    currDiggerCount    := DiggerCount  ;
  end;

  NextLemmingCountDown := 20;

  ObjectInfos.Clear;
  numEntries := 0;

  with Level do
  for i := 0 to InteractiveObjects.Count - 1 do
  begin
    O := InteractiveObjects[i];
    MO := Graph.MetaObjects[O.Identifier];

    Inf := TInteractiveObjectInfo.Create;
    Inf.Obj := O;
    Inf.MetaObj := MO;
    Inf.CurrentFrame := MO.StartAnimationFrameIndex;

(*    // add to the right list (entries or other objects)
    if O.Identifier = OID_ENTRY then     //lemcore
      Entries.Add(Inf)
    else*)
    if O.Identifier = OID_ENTRY then
      if numEntries < 4 then
      begin
        dosentrytable[numEntries] := i;
        numEntries := numEntries + 1;
      end;
    ObjectInfos.Add(Inf);
  end;

  case numEntries of
    1:
      begin
        dosentrytable[1] := dosentrytable[0];
        dosentrytable[2] := dosentrytable[0];
        dosentrytable[3] := dosentrytable[0];
      end;
    2:
      // @Optional Game Mechanic
      if dgoOldEntranceABBAOrder in Options then
      begin       // ABBA
        dosentrytable[2] := dosentrytable[1];
        dosentrytable[3] := dosentrytable[0];
      end
      else begin  // ABAB
        dosentrytable[2] := dosentrytable[0];
        dosentrytable[3] := dosentrytable[1];
      end;
    3:
      begin       // ABCB
        dosentrytable[3] := dosentrytable[1];
      end;
  end;

  InitializeBrickColors(Graph.BrickColor);
  InitializeObjectMap;
  InitializeMiniMap;

  DrawAnimatedObjects; // first draw needed

  //if Entries.Count = 0 then raise exception.Create('no entries');

  with InfoPainter do
  begin
    SetInfoMinutes(Minutes);
    SetInfoSeconds(Seconds);
    SetInfoLemmingsOut(LemmingsOut);
    SetInfoLemmingsIn(0, 1);
  end;

  InfoPainter.DrawButtonSelector(fSelectedSkill, False);
  // force update
  fSelectedSkill := spbNone;
  SetSelectedSkill(spbClimber, True); // default

  DrawStatics;
  Playing := True;
end;



procedure TLemmingGame.CombineLemmingPixels(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then B := F;
end;

procedure TLemmingGame.CombineBuilderPixels(F: TColor32; var B: TColor32; M: TColor32);
{-------------------------------------------------------------------------------
  This trusts the CurrentlyDrawnLemming variable.
-------------------------------------------------------------------------------}
begin
  if F = BrickPixelColor then
    B := BrickPixelColors[12 - fCurrentlyDrawnLemming.LemNumberOfBricksLeft]
  else if F <> 0 then
    B := F;
end;

procedure TLemmingGame.CombineDefaultPixels(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then B := F;
end;

procedure TLemmingGame.CombineLemmingHighlight(F: TColor32; var B: TColor32; M: TColor32);
begin
  // photoflash
  if F <> 0 then B := clBlack32 else B := clWhite32;
//  if F <> 0 then B := clYellow32;
end;


procedure TLemmingGame.CombineMaskPixels(F: TColor32; var B: TColor32; M: TColor32);
// copy masks to world
begin
  if F <> 0 then B := 0;
end;

procedure TLemmingGame.CombineMinimapWorldPixels(F: TColor32; var B: TColor32; M: TColor32);
// copy world to minimap
begin
  if F <> 0 then B := BrickPixelColor;
end;

function TLemmingGame.HasPixelAt(X, Y: Integer): Boolean;
{-------------------------------------------------------------------------------
  Read value from world.
  The function returns True when the value at (x, y) is terrain
-------------------------------------------------------------------------------}
begin
  with World do
    Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height)
              and (Pixel[X, Y] and ALPHA_TERRAIN <> 0);

//  with World do
  //  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height)
    //          and (Pixel[X, Y] and COLOR_MASK <> 0);
end;

function TLemmingGame.HasPixelAt_ClipY(X, Y, minY: Integer): Boolean;
begin
  if Y >= minY then
    Result := HasPixelAt(X, Y)
  else
    Result := HasPixelAt(X, minY);
end;

procedure TLemmingGame.RemovePixelAt(X, Y: Integer);
begin
  World.PixelS[x, y] := 0;
end;

function TLemmingGame.ReadObjectMap(X, Y: Integer): Byte;
// original dos objectmap has a resolution of 4
begin
  // the "and not 3" ensures rounding down when 
  // operand is negative (eg. -0.25 -> -1)
  X := (X and not 3) div 4;
  Y := (Y and not 3) div 4;

  Inc(X, OBJMAPADD);
  Inc(Y, OBJMAPADD);

  with ObjectMap do
  begin
    if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
      Result := ObjectMap.Bits^[X + Y * Width]
    else
      Result := DOM_NONE; // whoops, important
  end;
end;

procedure TLemmingGame.WriteObjectMap(X, Y: Integer; aValue: Byte);
begin
  // the "and not 3" ensures rounding down when 
  // operand is negative (eg. -0.25 -> -1)
  X := (X and not 3) div 4;
  Y := (Y and not 3) div 4;

  Inc(X, OBJMAPADD);
  Inc(Y, OBJMAPADD);

  with ObjectMap do
  begin
    if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
      ObjectMap.Bits^[X + Y * Width] := aValue;
  end;
end;

procedure TLemmingGame.SaveMap(L: TLemming);
begin
  with L do
  begin
    LemSavedMap[0] := ReadObjectMap(LemX - 4, LemY - 6);
    LemSavedMap[1] := ReadObjectMap(LemX,     LemY - 6);
    LemSavedMap[2] := ReadObjectMap(LemX + 4, LemY - 6);
    LemSavedMap[3] := ReadObjectMap(LemX - 4, LemY - 2);
    LemSavedMap[4] := ReadObjectMap(LemX,     LemY - 2);
    LemSavedMap[5] := ReadObjectMap(LemX + 4, LemY - 2);
    LemSavedMap[6] := ReadObjectMap(LemX - 4, LemY + 2);
    LemSavedMap[7] := ReadObjectMap(LemX,     LemY + 2);
    LemSavedMap[8] := ReadObjectMap(LemX + 4, LemY + 2);
  end;
end;

procedure TLemmingGame.RestoreMap(L: TLemming);
begin
  with L do
  begin
    WriteObjectMap(LemX - 4, LemY - 6, LemSavedMap[0]);
    WriteObjectMap(LemX,     LemY - 6, LemSavedMap[1]);
    WriteObjectMap(LemX + 4, LemY - 6, LemSavedMap[2]);
    WriteObjectMap(LemX - 4, LemY - 2, LemSavedMap[3]);
    WriteObjectMap(LemX,     LemY - 2, LemSavedMap[4]);
    WriteObjectMap(LemX + 4, LemY - 2, LemSavedMap[5]);
    WriteObjectMap(LemX - 4, LemY + 2, LemSavedMap[6]);
    WriteObjectMap(LemX,     LemY + 2, LemSavedMap[7]);
    WriteObjectMap(LemX + 4, LemY + 2, LemSavedMap[8]);
  end;
end;

procedure TLemmingGame.SetBlockerField(L: TLemming);
begin
  with L do
  begin
    WriteObjectMap(LemX - 4, LemY - 6, DOM_FORCELEFT);
    WriteObjectMap(LemX,     LemY - 6, DOM_BLOCKER);
    WriteObjectMap(LemX + 4, LemY - 6, DOM_FORCERIGHT);
    WriteObjectMap(LemX - 4, LemY - 2, DOM_FORCELEFT);
    WriteObjectMap(LemX,     LemY - 2, DOM_BLOCKER);
    WriteObjectMap(LemX + 4, LemY - 2, DOM_FORCERIGHT);
    WriteObjectMap(LemX - 4, LemY + 2, DOM_FORCELEFT);
    WriteObjectMap(LemX,     LemY + 2, DOM_BLOCKER);
    WriteObjectMap(LemX + 4, LemY + 2, DOM_FORCERIGHT);
  end;
end;

function TLemmingGame.CheckForOverlappingField(L: TLemming): Boolean;
const
  BytesToCheck = [DOM_FORCELEFT, DOM_BLOCKER, DOM_FORCERIGHT];
begin
  with L do
  Result := (ReadObjectMap(LemX - 4, LemY - 6) in BytesToCheck) or
            (ReadObjectMap(LemX,     LemY - 6) in BytesToCheck) or
            (ReadObjectMap(LemX + 4, LemY - 6) in BytesToCheck) or
            (ReadObjectMap(LemX - 4, LemY - 2) in BytesToCheck) or
            (ReadObjectMap(LemX,     LemY - 2) in BytesToCheck) or
            (ReadObjectMap(LemX + 4, LemY - 2) in BytesToCheck) or
            (ReadObjectMap(LemX - 4, LemY + 2) in BytesToCheck) or
            (ReadObjectMap(LemX,     LemY + 2) in BytesToCheck) or
            (ReadObjectMap(LemX + 4, LemY + 2) in BytesToCheck);
end;


procedure TLemmingGame.Transition(L: TLemming; aAction: TBasicLemmingAction; DoTurn: Boolean = False);
{-------------------------------------------------------------------------------
  Handling of a transition and/or turnaround
-------------------------------------------------------------------------------}
var
  i: Integer;
begin

  with L do
  begin
    // check if any change
    if (LemAction = aAction) and not DoTurn then
      Exit;

    if DoTurn then
      LemDx := -LemDx;

    // *always* new animation
    i := AnimationIndices[aAction, LemRTL]; // watch out: here use the aAction parameter!
    LMA := Style.AnimationSet.MetaLemmingAnimations[i];
    LAB := Style.AnimationSet.LemmingAnimations.List^[i];
    LemMaxFrame := LMA.FrameCount - 1;
    LemAnimationType := LMA.AnimationType;
    FrameTopDy  := -LMA.FootY; // ccexplore compatible
    FrameLeftDx := -LMA.FootX; // ccexplore compatible

    // transition
    if LemAction = aAction then
      Exit;
    LemAction := aAction;
    LemFrame := 0;
    LemEndOfAnimation := False;
    LemFallen := 0;
    LemNumberOfBricksLeft := 0;

    // some things to do when entering state
    case LemAction of
      baSplatting:
        begin
          LemExplosionTimer := 0;
          LemDX := 0;
          CueSoundEffect(SFX_SPLAT)
        end;
      baBlocking:
        begin
          LemIsBlocking := True;
          SaveMap(L);
          SetBlockerField(L);
        end;
      baExiting    : CueSoundEffect(SFX_YIPPEE);
      baDigging    : LemIsNewDigger := True;
      baFalling    :
        begin
          // @Optional Game Mechanic
          if dgoFallerStartsWith3 in Options then
            LemFallen := 3;
        end;
      baBuilding   : LemNumberOfBricksLeft := 12;
      //baDrowning   : CueSoundEffect(SFX_DROWNING);
      //baVaporizing : CueSoundEffect(SFX_VAPORIZING);
      baOhnoing    : {if not UserSetNuking then} CueSoundEffect(SFX_OHNO);
      baExploding  : CueSoundEffect(SFX_EXPLOSION);
      baFloating   : LemFloatParametersTableIndex := 0;
      baMining     : Inc(LemY);
    end;
  end;
end;

procedure TLemmingGame.TurnAround(L: TLemming);
// we assume that the mirrored animations at least have the same
// framecount
var
  i: Integer;
begin
  with L do
  begin
    LemDX := -LemDX;
    i := AnimationIndices[LemAction, LemRTL];
    LMA := Style.AnimationSet.MetaLemmingAnimations[i];
    LAB := Style.AnimationSet.LemmingAnimations[i];
    LemMaxFrame := LMA.FrameCount - 1;
    LemAnimationType := LMA.AnimationType;
    FrameTopDy  := -LMA.FootY; // ccexplore compatible
    FrameLeftDx := -LMA.FootX; // ccexplore compatible
  end;
end;


function TLemmingGame.AssignSkill(Lemming1, Lemming2: TLemming; aSkill: TBasicLemmingAction): Boolean;
var
  Method: TSkillMethod;
begin
  Result := False;
  Method := Skillmethods[aSkill];
  if Assigned(Method) then
  begin
    Result := Method(Lemming1, Lemming2);
    if Result then
      {$ifdef flexi}if (fGameParams.SysDat.Options and 16 <> 0) or (aSkill <> baExploding) then{$endif}
      CueSoundEffect(SFX_ASSIGN_SKILL);
  end;
end;

function TLemmingGame.AssignClimber(Lemming1, Lemming2: TLemming): Boolean;
begin
  Result := False;

  if (CurrClimberCount > 0)
  and not Lemming1.LemIsClimber
  and not (Lemming1.LemAction in [baBlocking, baSplatting, baExploding]) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := Lemming1
    else
      begin
        Lemming1.LemIsClimber := True;
        Dec(CurrClimberCount);
        InfoPainter.DrawSkillCount(spbClimber, currClimberCount);
        { TODO : double check if this bug emulation is safe }
        // @Optional Game Mechanics
        if dgoAssignClimberShruggerActionBug in Options then
          if (Lemming1.LemAction = baShrugging) then
            Lemming1.LemAction := baWalking;
        Result := True;
        RecordSkillAssignment(Lemming1, baClimbing);
      end;
  end
end;

function TLemmingGame.AssignFloater(Lemming1, Lemming2: TLemming): Boolean;
begin
  Result := False;

  if (CurrFloaterCount > 0)
  and not Lemming1.LemIsFloater
  and not (Lemming1.LemAction in [baBlocking, baSplatting, baExploding]) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := Lemming1
    else
      begin
        Lemming1.LemIsFloater := True;
        Dec(currFloaterCount);
        InfoPainter.DrawSkillCount(spbUmbrella, currFloaterCount);
        Result := True;
        RecordSkillAssignment(Lemming1, baFloating);
      end;
  end
end;

function TLemmingGame.AssignBomber(Lemming1, Lemming2: TLemming): Boolean;
begin
  Result := False;

  if (currBomberCount > 0)
  and (Lemming1.LemExplosionTimer = 0)
  and not (lemming1.lemAction in [baOhnoing, baExploding, baVaporizing, baSplatting]) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := Lemming1
    else
      begin
        {$ifndef flexi}Lemming1.LemExplosionTimer := 79;{$else}
        if fGameParams.SysDat.Options and 16 = 0 then
          Lemming1.LemExplosionTimer := 1
          else
          Lemming1.LemExplosionTimer := 79;
        {$endif}
        Dec(currBomberCount);
        InfoPainter.DrawSkillCount(spbExplode, currBomberCount);
        Result := True;
        RecordSkillAssignment(Lemming1, baExploding);
      end;
  end
end;

function TLemmingGame.AssignBlocker(Lemming1, Lemming2: TLemming): Boolean;
begin
  Result := False;

  if (currBlockerCount > 0)
  and (lemming1.LemAction in [baWalking, baShrugging, baBuilding, baBashing, baMining, baDigging])
  and (CheckForOverlappingField(Lemming1) = FALSE) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := Lemming1
    else
      begin
        Dec(CurrBlockerCount);
        InfoPainter.DrawSkillCount(spbBlocker, currBlockerCount);
        Transition(Lemming1, baBlocking);
        Result := True;
        RecordSkillAssignment(Lemming1, baBlocking);
      end;
  end;
end;

function TLemmingGame.AssignBuilder(Lemming1, Lemming2: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baBashing, baMining, baDigging];
begin
  Result := False;

  if (currBuilderCount = 0) or (Lemming1.LemY + Lemming1.FrameTopdy < HEAD_MIN_Y) then
    Exit;

  if (Lemming1.LemAction in ActionSet) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := Lemming1
    else
      begin
        Transition(Lemming1, baBuilding);
        Dec(CurrBuilderCount);
        InfoPainter.DrawSkillCount(spbBuilder, currBuilderCount);
        Result := True;
        RecordSkillAssignment(Lemming1, baBuilding);
      end;
  end
  else if (Lemming2 <> nil) and (Lemming2.LemAction in ActionSet) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := Lemming2
    else
      begin
        Transition(Lemming2, baBuilding);
        Dec(CurrBuilderCount);
        InfoPainter.DrawSkillCount(spbBuilder, currBuilderCount);
        Result := True;
        RecordSkillAssignment(Lemming2, baBuilding);
      end;
  end;

end;

function TLemmingGame.AssignBasher(Lemming1, Lemming2: TLemming): Boolean;
var
  SelectedLemming: TLemming;
const
  ActionSet = [baWalking, baShrugging, baBuilding, baMining, baDigging];
begin
  Result := False;
  // selectedLemming := nil; not needed, this var is not used
  if (currBasherCount = 0) then
    Exit
  else if Lemming1.LemAction in ActionSet then
    SelectedLemming := Lemming1
  else if (Lemming2 <> nil) and (Lemming2.LemAction in ActionSet) then
    SelectedLemming := Lemming2
  else
    Exit;

  if (SelectedLemming.LemObjectInFront = DOM_STEEL) then
  begin
    if (not fCheckWhichLemmingOnly) then
      CueSoundEffect(SFX_HITS_STEEL);
    Exit;
  end
  else if ((SelectedLemming.LemObjectInFront = DOM_ONEWAYLEFT) and (SelectedLemming.LemDx <> -1)) or
          ((SelectedLemming.LemObjectInFront = DOM_ONEWAYRIGHT) and (SelectedLemming.LemDx <> 1)) then
    Exit
  else begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := SelectedLemming
    else
      begin
        Transition(SelectedLemming, baBashing);
        Dec(CurrBasherCount);
        InfoPainter.DrawSkillCount(spbBasher, CurrBasherCount);
        Result := True;
        RecordSkillAssignment(SelectedLemming, baBashing);
      end;
  end;
end;

function TLemmingGame.AssignMiner(Lemming1, Lemming2: TLemming): Boolean;
var
  SelectedLemming: TLemming;
const
  ActionSet = [baWalking, baShrugging, baBuilding, baBashing, baDigging];
begin
  Result := False;

  if (CurrMinerCOunt = 0) then
    Exit
  else if lemming1.LemAction in ActionSet then
    SelectedLemming := Lemming1
  else if Assigned(Lemming2) and (lemming2.LemAction in ActionSet) then
    SelectedLemming := Lemming2
  else
    Exit;

  with SelectedLemming do
  begin

    if (LemobjectInFront = DOM_STEEL) then
    begin
      if (not fCheckWhichLemmingOnly) then
        CueSoundEffect(SFX_HITS_STEEL);
      Exit;
    end
    else if (LemobjectBelow = DOM_STEEL)
    or ((LemobjectInFront = DOM_ONEWAYLEFT) and (LemDx <> -1))
    or ((LemobjectInFront = DOM_ONEWAYRIGHT) and (LemDx <> 1)) then
      Exit
    else begin
      if (fCheckWhichLemmingOnly) then
        WhichLemming := SelectedLemming
      else
        begin
          Transition(SelectedLemming, baMining);
          Dec(currMinerCount);
          InfoPainter.DrawSkillCount(spbMiner, currMinerCount);
          Result := True;
          RecordSkillAssignment(SelectedLemming, baMining);
        end;
    end;

  end;
end;

function TLemmingGame.AssignDigger(Lemming1, Lemming2: TLemming): Boolean;
begin
  Result := False;

  if (CurrDiggerCount = 0) or (lemming1.LemobjectBelow = DOM_STEEL) then
    Exit
  else if (lemming1.lemAction in [baWALKING, baSHRUGGING, baBUILDING, baBASHING, baMINING]) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := lemming1
    else begin
      Transition(lemming1, baDigging);
      Dec(currDiggerCount);
      InfoPainter.DrawSkillCount(spbDigger, currDiggerCount);
      Result := True;
      RecordSkillAssignment(Lemming1, baDigging);
    end
  end
  else if Assigned(lemming2) and (lemming2.lemAction in [baWALKING, baSHRUGGING, baBUILDING, baBASHING, baMINING]) then
  begin
    if (fCheckWhichLemmingOnly) then
      WhichLemming := lemming2
    else begin
      Transition(lemming2, baDigging);
      Dec(currDiggerCount);
      InfoPainter.DrawSkillCount(spbDigger, currDiggerCount);
      Result := True;
      RecordSkillAssignment(Lemming2, baDigging);
    end
  end;

end;

procedure TLemmingGame.SetSoundChannels(i: integer);
begin
  if i < 1 then i := 1;
  if i > 256 then i := 256;
  SoundMgr.AvailableChannels := i;
end;


function TLemmingGame.UpdateExplosionTimer(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    Dec(LemExplosionTimer);
    //deb([lemexplosiontimer, lemdigit]);
    if LemExplosionTimer > 0 then
      Exit
    else begin
      if LemAction in [baVaporizing, baDrowning, baFloating, baFalling] then
      begin
        Transition(L, baExploding);
        //call CueSoundEffect(EXPLOSION)
      end
      else begin
        Transition(L, baOhnoing);
        //call CueSoundEffect(OHNO)
      end;
      Result := True;
    end;
  end;
end;

procedure TLemmingGame.CheckForGameFinished;

    (*
    procedure doevent;
    begin

      fTargetIteration := 0;
      fHyperSpeedCounter := 0;
      if HyperSpeed then
        fLeavingHyperSpeed := True;

      SoundMgr.StopMusic(0);

      if assigned(fonfinish) then
        fonfinish(self);
    end; *)

begin

  if fGameFinished then
    Exit;

  if fParticleFinishTimer > 0 then
    Exit;

  if (Minutes <= 0) and (Seconds <= 0) then
  begin
    fGameFinished := True;
    GameResultRec.gTimeIsUp := True;
    Finish;
    Exit;
  end;

  if LemmingsIn >= MaxNumLemmings then
  begin
    Finish;
    Exit;
  end;

  if LemmingsRemoved >= MaxNumLemmings then
  begin
    Finish;
    Exit;
  end;

  if UserSetNuking and (LemmingsOut = 0) then
  begin
    Finish;
    Exit;
  end;

end;

procedure TLemmingGame.InitializeObjectMap;
{-------------------------------------------------------------------------------

  In one of the previous e-mails I said the DOS Lemmings object map has an
  x range from -16 to 1647 and a y range from 0 to 159.
  I think to provide better safety margins, let's extend the y range a bit,
  say from -16 to 175 (I added 16 in both directions).
  This is probably slightly on the excessive side but memory is cheap these days,
  and you can always reduce the x range since DOS Lemmings
  doesn't let you scroll to anywhere near x=1647
  (I think the max visible x range is like 1580 or something).

-------------------------------------------------------------------------------}
var
  x, y, i: Integer;
  Inf : TInteractiveObjectInfo;
  S: TSteel;
  Eff, V: Byte;
  MaxO: Integer;
begin

  ObjectMap.SetSize((1647 + OBJMAPOFFSET) div 4, (175 + OBJMAPOFFSET) div 4);
  ObjectMap.Clear(DOM_NONE);

  // map steel
  with Level.Steels, HackedList do
  begin
    for i := 0 to Count - 1 do
    begin
      S := List^[i];
      with S do
        for y := Top to Top + Height - 1 do //SteelY to SteelY + SteelHeight - 1 do
          for x := Left to Left + Width - 1 do //SteelX to SteelX + SteelWidth - 1 do
            WriteObjectMap(x, y, DOM_STEEL);
    end;
  end;

  with ObjectInfos do
  begin

{    // @Optional Game Mechanic
    if dgoDisableObjectsAfter15 in fOptions
    then MaxO := Min(Count - 1, 15)
    else MaxO := Count - 1;
}
    for i := 0 to {MaxO}Count - 1 do
    begin
      Inf := List^[i];
      with Inf, Obj, MetaObj do
      // @Optional Game Mechanic
      if not ((dgoDisableObjectsAfter15 in fOptions) and IsFake) then
      begin
        // 0..127   = triggered trap index ()
        // 128..255 = triggereffect (128 is DOM_NONE)
        Eff := TriggerEffect;
        if Eff = ote_TriggeredTrap then //TEX_TRIGGEREDTRAP then
          V := i
        else
          V := Eff + 128;
        for y := (Top and not 3) + TriggerTop to (Top and not 3) + TriggerTop + TriggerHeight - 1 do
          for x := (Left and not 3) + TriggerLeft to (Left and not 3) + TriggerLeft + TriggerWidth - 1 do
            WriteObjectMap(x, y, V) // traps --> object_id
      end;
    end; // for i

  end;
end;

function TLemmingGame.PrioritizedHitTest(out Lemming1, Lemming2: TLemming;
                                         MousePos: TPoint;
                                         CheckRightMouseButton: Boolean = True): Integer;
{-------------------------------------------------------------------------------
  meant for both prioritized processskillassignment and hittest.
  returns number of hits.
-------------------------------------------------------------------------------}
var
  L, PrioritizedLemming, NonPrioritizedLemming: TLemming;
  i, x, y: Integer;
  CP: TPoint;
const
  PrioActions =
    [baBlocking, baBuilding, baShrugging, baBashing, baMining, baDigging, baOhnoing];
const
  Sca = 0;
begin
  Result := 0;
  PrioritizedLemming := nil;
  NonPrioritizedLemming := nil;
  Lemming1 := nil;
  Lemming2 := nil;
  CP := MousePos;

  for i := 0 to LemmingList.Count - 1 do
  begin
    L := LemmingList.List^[i];
    with L do
    begin
      if LemRemoved then
        Continue;
      x := LemX + FrameLeftDx;
      y := LemY + FrameTopDy;
      if (x <= CP.X) and (CP.X <= x + 12) and (y <= CP.Y) and (CP.Y <= y + 12) then
      begin
        Inc(Result);
        if LemAction in PrioActions then
          PrioritizedLemming := L
        else
          NonPrioritizedLemming := L;
      end;
    end;
  end; // for

  if NonPrioritizedLemming <> nil then
    LastNPLemming := NonPrioritizedLemming;

  if (RightMouseButtonHeldDown and CheckRightMouseButton) or (PrioritizedLemming = nil) then
    Lemming1 := NonPrioritizedLemming
  else
    Lemming1 := PrioritizedLemming;

  Lemming2 := NonPrioritizedLemming;
end;


procedure TLemmingGame.InitializeMiniMap;
{-------------------------------------------------------------------------------
  Put the terrainpixels in the minimap. Copy them (scaled) from the worldbitmap.
  During the game the minimap will be updated like the world-bitmap gets updated.
  The lemming-pixels are not drawn in the minimap: these are drawn in the
  MiniMapBuffer.
------------------------------------------------------------------------------}
var
  OldCombine: TPixelCombineEvent;
  OldMode: TDrawMode;
  SrcRect, DstRect: TRect;
begin
  Minimap.SetSize(DOS_MINIMAP_WIDTH, DOS_MINIMAP_HEIGHT);
  Minimap.Clear(0);
  OldCombine := World.OnPixelCombine;
  OldMode := World.DrawMode;
  World.DrawMode := dmCustom;
  World.OnPixelCombine := CombineMinimapWorldPixels;
  SrcRect := World.BoundsRect;
  DstRect := Rect(0, 0, World.Width div 16, World.Height div 8);
//  OffsetRect(DstRect, 1, 0);
  World.DrawTo(Minimap, DstRect, SrcRect);
  World.OnPixelCombine := OldCombine;
  World.DrawMode := OldMode;
end;

function TLemmingGame.GetTrapSoundIndex(aDosSoundEffect: Integer): Integer;
begin
  case aDosSoundEffect of
    ose_RopeTrap             : Result := SFX_ROPETRAP;
    ose_SquishingTrap        : Result := SFX_SQUISHINGTRAP;
    ose_TenTonTrap           : Result := SFX_TENTON;
    ose_BearTrap             : Result := SFX_BEARTRAP;
    ose_ElectroTrap          : Result := SFX_ELECTROTRAP;
    ose_SpinningTrap         : Result := SFX_SPINNINGTRAP;
  else
    Result := -1;
  end;
end;

procedure TLemmingGame.CheckForInteractiveObjects(L: TLemming);
var
//  Dst: TRect;
  Inf: TInteractiveObjectInfo;
begin
  with L do
  begin
    LemObjectBelow := ReadObjectMap(LemX, LemY);
    LemObjectInFront := ReadObjectMap(LemX + 8 * LemDx, LemY - 8);

    case LemObjectBelow of
      // DOM_NONE = 128 = nothing
      DOM_NONE:
        Exit;
      // 0..127 triggered objects
      0..127:
        begin
          Inf := ObjectInfos[LemObjectBelow];
          if not Inf.Triggered then
          begin
            // trigger
            Inf.Triggered := True;

            if fGameParams.LemmixTrapBug then
            begin
              Inc(Inf.CurrentFrame);
              if Inf.CurrentFrame >= Inf.MetaObj.AnimationFrameCount then
                Inf.CurrentFrame := 0;
            end;

            RemoveLemming(L);
            CueSoundEffect(GetTrapSoundIndex(Inf.MetaObj.SoundEffect));
          end;
        end;

      // 128 + n (continuous objects, staticobjects, steel, oneway wall)
      DOM_EXIT:
        begin
          if LemAction <> baFalling then
          begin
            Transition(L, baExiting);
            CueSoundEffect(SFX_YIPPEE);
          end;
        end;
      DOM_FORCELEFT:
        if LemDx > 0 then
          TurnAround(L);
      DOM_FORCERIGHT:
        if LemDx < 0 then
          TurnAround(L);
      DOM_WATER:
        begin
          Transition(L, baDrowning);
          CueSoundEffect(SFX_DROWNING);
        end;
      DOM_FIRE:
        begin
          Transition(L, baVaporizing);
          CueSoundEffect(SFX_VAPORIZING);
        end;
    end;
  end;

end;

procedure TLemmingGame.ApplyExplosionMask(L: TLemming);
var
  X, Y: Integer;
begin
  // dos explosion mask 16 x 22

  ExplodeMaskBmp.DrawTo(World, L.LemX - 8, L.LemY -14);
  if not HyperSpeed then
    ExplodeMaskBmp.DrawTo(fTargetBitmap, L.LemX - 8, L.LemY -14);

  // fake draw mask in minimap
  // this clears 4 pixels as windows programmers should know!
  X := L.LemX div 16;
  Y := L.LemY div 8;
  Minimap.FillRectS(X - 1, Y - 1, X + 1, Y + 1, 0);
end;

procedure TLemmingGame.ApplyBashingMask(L: TLemming; MaskFrame: Integer);
var
  Bmp: TBitmap32;
  S, D: TRect;
  X, Y: Integer;
begin

  // dos bashing mask = 16 x 10

  if not L.LemRTL then
    Bmp := BashMasks
  else
    Bmp := BashMasksRTL;

  S := CalcFrameRect(Bmp, 4, MaskFrame);
  D.Left := L.LemX + L.FrameLeftDx;
  D.Top := L.LemY + L.FrameTopDy;
  D.Right := D.Left + 16;
  D.Bottom := D.Top + 10;

  Assert(CheckRectCopy(D, S), 'bash rect err');

  Bmp.DrawTo(World, D, S);
  if not HyperSpeed then
    Bmp.DrawTo(fTargetBitmap, D, S);

  // fake draw mask in minimap
  X := L.LemX div 16;
  Y := L.LemY div 8;
  MiniMap.PixelS[X, Y] := 0;
end;

procedure TLemmingGame.ApplyMinerMask(L: TLemming; MaskFrame, X, Y: Integer);
// x,y is topleft
var
  Bmp: TBitmap32;
  S, D: TRect;
//  C: TColor32;
  //iX, iY,
  aX, aY: Integer;
begin
  Assert((MaskFrame >=0) and (MaskFrame <= 1), 'miner mask error');

  if not L.LemRTL then
    Bmp := MineMasks
  else
    Bmp := MineMasksRTL;

  S := CalcFrameRect(Bmp, 2, MaskFrame);
//  D := S;
//  ZeroTopLeftRect(D);
//  RectMove(D, X, Y);

  D.Left := X;
  D.Top := Y;
  D.Right := X + RectWidth(S) - 1; // whoops: -1 is important to avoid stretching
  D.Bottom := Y + RectHeight(S) - 1; // whoops: -1 is important to avoid stretching

//  deb([rectwidth(d), rectwidth(s)]);
  Assert(CheckRectCopy(D, S), 'miner rect error');

  Bmp.DrawTo(World, D, S);
  if not HyperSpeed then
    Bmp.DrawTo(fTargetBitmap, D, S);

  // fake draw mask in minimap
  aX := L.LemX div 16;
  aY := L.LemY div 8;
  MiniMap.PixelS[aX, aY] := 0;

end;

procedure TLemmingGame.EraseParticles(L: TLemming);
{-------------------------------------------------------------------------------
  Erase the previously drawn particles of an exploded lemming
-------------------------------------------------------------------------------}
var
  i, X, Y: Integer;
  DstRect: TRect;
  Drawn: Boolean;
begin
  if not (moShowParticles in fGameParams.MiscOptions) then
    Exit;

  Drawn := False;

  with L do
    if LemParticleFrame <= 51 then
      for i := 0 to 79 do
      begin
        X := fParticles[LemParticleFrame][i].DX;
        Y := fParticles[LemParticleFrame][i].DY;
        if (X <> -128) and (Y <> -128) then
        begin
          X := LemX + X;
          Y := LemY + Y;
          fTargetBitmap.PixelS[X, Y] := World.PixelS[X, Y];
          Drawn := True;
          (*
          DstRect := Rect(X, Y, X + 1, Y + 1);
          if IntersectRect(DstRect, DstRect, World.BoundsRect) then
            World.DrawTo(fTargetBitmap, DstRect, DstRect);
          *)
        end;
      end;

  fExplodingGraphics := Drawn;


  (*
  with L do
  begin
    for i := 0 to 12 do
    begin
      X := LemX + i * 2;
      Y := LemY - LemParticleFrame - i * 3;
      DstRect := Rect(X, Y, X + 1, Y + 1);
      // important to intersect the rects!
      if IntersectRect(DstRect, DstRect, World.BoundsRect) then
        World.DrawTo(fTargetBitmap, DstRect, DstRect);
    end;
  end;
  *)
end;

procedure TLemmingGame.DrawParticles(L: TLemming);
var
  i, X, Y: Integer;
  DstRect: TRect;
  Drawn: Boolean;
const
  Colors: array[0..2] of TColor32 = (clYellow32, clRed32, clBlue32);
begin
  if not (moShowParticles in fGameParams.MiscOptions) then
    Exit;

  Drawn := False;

  with L do
    if LemParticleFrame <= 51 then
      for i := 0 to 79 do
      begin
        X := fParticles[LemParticleFrame][i].DX;
        Y := fParticles[LemParticleFrame][i].DY;
        if (X <> -128) and (Y <> -128) then
        begin
          X := LemX + X;
          Y := LemY + Y;
          Drawn := True;
          fTargetBitmap.PixelS[X, Y] := fParticleColors[i mod 16]//Colors[i mod 3]

          (*
          DstRect := Rect(X, Y, X + 1, Y + 1);
          //if IntersectRect(DstRect, DstRect, World.BoundsRect) then
          fTargetBitmap.FillRectS(X, Y, X + 1, Y + 1, Colors[i mod 3]{clYellow32});
            //World.DrawTo(fTargetBitmap, DstRect, DstRect);
          *)
        end;
      end;

  fExplodingGraphics := Drawn;

  (*
  with L do
  begin
    for i := 0 to 12 do
    begin
      X := LemX + i * 2;
      Y := LemY - LemParticleFrame - i * 3;
      fTargetBitmap.FillRectS(X, Y, X + 1, Y + 1, Colors[i mod 3]{clYellow32});
    end;
  end;
  *)

end;


procedure TLemmingGame.DrawAnimatedObjects;
var
  i: Integer;
  Inf : TInteractiveObjectInfo;
  //Cnt: Integer;
//  R: TRect;
begin
  if HyperSpeed then
    Exit;

(*  // we have to erase first
  // erase entries
  for i := 0 to Entries.Count - 1 do
  begin
    Inf := Entries.List^[i];
    Renderer.EraseObject(fTargetBitmap, Inf.Obj, World);
  end;
*)
  // erase other objects
  for i := 0 to ObjectInfos.Count - 1 do
  begin
    Inf := ObjectInfos.List^[i];
    Renderer.EraseObject(fTargetBitmap, Inf.Obj, World);
  end;

(*  // entrances
  // only on terrain
  for i := 0 to Entries.Count - 1 do
  begin
    Inf := Entries.List^[i];
    if odf_OnlyOnTerrain and Inf.Obj.DrawingFlags <> 0 then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil{World});
  end;
*)

  // other objects
  // only on terrain
  for i := 0 to ObjectInfos.Count - 1 do
  begin
    Inf := ObjectInfos.List^[i];
    if odf_OnlyOnTerrain and Inf.Obj.DrawingFlags <> 0 then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil{World});
  end;


(*  // entrances
  // rest
  for i := 0 to Entries.Count - 1 do
  begin
    Inf := Entries.List^[i];
    if odf_OnlyOnTerrain and Inf.Obj.DrawingFlags = 0 then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil{World});
  end;
*)
  // other objects
  // rest
  for i := 0 to ObjectInfos.Count - 1 do
  begin
    Inf := ObjectInfos.List^[i];
    if odf_OnlyOnTerrain and Inf.Obj.DrawingFlags = 0 then
      Renderer.DrawObject(fTargetBitmap, Inf.Obj, Inf.CurrentFrame, nil{World});
  end;

end;

procedure TLemmingGame.EraseLemmings;
{-------------------------------------------------------------------------------
  Erase the lemming from the targetbitmap by copying it's rect from the world
  bitmap.
-------------------------------------------------------------------------------}
var
  iLemming: Integer;
  CurrentLemming: TLemming;
  DstRect: TRect;
begin
  if HyperSpeed then
    Exit;

  with LemmingList do
    for iLemming := 0 to Count - 1 do
    begin
      CurrentLemming := List^[iLemming];
      with CurrentLemming do
        if not LemRemoved then
        begin
          DstRect := LemEraseRect;
          InflateRect(DstRect, 2, 2);
          // important to intersect the rects!
          if IntersectRect(DstRect, DstRect, World.BoundsRect) then
            World.DrawTo(fTargetBitmap, DstRect, DstRect);
        end
        // @particles (erase) if lem is removed
        else if LemParticleTimer > 0 then
        begin
          EraseParticles(CurrentLemming);
        end;
    end;
end;

procedure TLemmingGame.DrawLemmings;
var
  iLemming: Integer;
  CurrentLemming: TLemming;
  SrcRect, DstRect, DigRect: TRect;
  Digit: Integer;
  OldCombine: TPixelCombineEvent;
begin
  if HyperSpeed then
    Exit;

  fMinimapBuffer.Assign(Minimap);

  with LemmingList do
    for iLemming := 0 to Count - 1 do
    begin

      CurrentLemming := List^[iLemming];
      with CurrentLemming do
        if not LemRemoved then
        begin
          fCurrentlyDrawnLemming := CurrentLemming;
          SrcRect := GetFrameBounds;
          DstRect := GetLocationBounds;
          LemEraseRect := DstRect;

          fMinimapBuffer.PixelS[LemX div 16, LemY div 8] :=
            Color32(0, 255{176}, 000);

          if not LemHighlightReplay then
            LAB.DrawTo(fTargetBitmap, DstRect, SrcRect)
          else begin
            // replay assign job highlight fotoflash effect
            OldCombine := LAB.OnPixelCombine;
            LAB.OnPixelCombine := CombineLemmingHighlight;
            LAB.DrawTo(fTargetBitmap, DstRect, SrcRect);
            LAB.OnPixelCombine := OldCombine;
            LemHighlightReplay := False;
          end;

          if DrawLemmingPixel then
            fTargetBitmap.FillRectS(LemX, LemY, LemX + 1, LemY + 1, clRed32);

          if LemExplosionTimer > 0 then
          begin
            SrcRect := Rect(0, 0, 8, 8);
            DigRect := GetCountDownDigitBounds;
            LemEraseRect.Top := DigRect.Top;
            Assert(CheckRectCopy(SrcRect, DigRect), 'digit rect copy');

            case LemExplosionTimer of
              65..79  : Digit := 5;
              49..64  : Digit := 4;
              33..48  : Digit := 3;
              17..32  : Digit := 2;
              00..16  : Digit := 1;
            else Digit := 1;
            end;

            RectMove(SrcRect, 0, (5 - Digit) * 8); // get "frame"
            CntDownBmp.DrawTo(fTargetBitmap, DigRect, SrcRect);
          end;
        end // not LemmingRemoved
        // @particles, check explosiondrawing if the lemming is dead
        else if LemParticleTimer > 0 then begin
          DrawParticles(CurrentLemming);
        end;

    end; // for i...

  HitTest;

  if InfoPainter <> nil then
  begin
    InfoPainter.SetInfoLemmingsOut(LemmingsOut);
    //InfoPainter.DrawMinimap(fMinimapBuffer)
  end;

end;

procedure TLemmingGame.LayBrick(L: TLemming);
{-------------------------------------------------------------------------------
  bricks are in the lemming area so will automatically be copied to the screen
  during drawlemmings
-------------------------------------------------------------------------------}
var
  i, x: Integer;
  NumPixelsFilled: Integer;
  C: TColor32;
begin
  NumPixelsFilled := 0;

  with L do
  begin
    if (LemDx = 1) then
      x := LemX
    else
      x := LemX - 4;

    i := 12 - L.LemNumberOfBricksLeft;
    if i < 0 then i := 0;
    if i > 11 then i := 11;
    C := BrickPixelColors[i];
    C := C or ALPHA_TERRAIN;

//    C := BrickPixelColor or ALPHA_TERRAIN;


    repeat
      if World.PixelS[x, LemY - 1] = 0 then
        World.PixelS[x, LemY - 1] := C;
      Inc(NumPixelsFilled);
      Inc(X);
    until NumPixelsFilled = 6;
  end;

end;

function TLemmingGame.DigOneRow(L: TLemming; Y: Integer): Boolean;
var
  yy, N, X: Integer;
  mX, mY: Integer;
begin
  Result := FALSE;

  with L do
  begin

    n := 1;
    x := LemX - 4;
    yy := Y;
    if (yy < 0) then yy := 0;

    while (n <= 9) do
    begin
      if (HasPixelAt(x,yy) = TRUE) then
      begin
        RemovePixelAt(x,yy);
        Result := TRUE;
      end;
      inc(n);
      inc(x);
    end;// while

  end;

  // fake draw mask in minimap
  mX := L.LemX div 16;
  mY := L.LemY div 8;
  MiniMap.PixelS[mX, mY] := 0;


end;

function TLemmingGame.HandleLemming(L: TLemming): Boolean;
{-------------------------------------------------------------------------------
  This is the main lemming method, called by CheckLemmings().
  The return value should return true if the lemming has to be checked by
  interactive objects.
  o Increment lemming animationframe
  o Call specialized action-method
  o Do *not* call this method for a removed lemming
-------------------------------------------------------------------------------}
var
  Method: TLemmingMethod;
begin
  //Result := False;

  with L do
  begin
    // next frame (except floating and digging which are handled differently)
    if not (LemAction in [baFloating, baDigging]) then
    begin
      if LemFrame < LemMaxFrame then
      begin
        LemEndOfAnimation := False;
        Inc(LemFrame);
      end
      else begin
        LemEndOfAnimation := True;
        if LemAnimationType = lat_Loop then
          LemFrame := 0;
      end;
    end;
  end; // with

  Method := LemmingMethods[L.LemAction];
  Result := Method(L);
end;

function TLemmingGame.HandleWalking(L: TLemming): Boolean;
var
  dy, NewY: Integer;
begin
  Result := False;

  with L do
  begin
    Inc(LemX, LemDx);

    if (LemX >= LEMMING_MIN_X) and (LemX <= LEMMING_MAX_X) then
    begin
      if HasPixelAt_ClipY(LemX, LemY, 0) = TRUE then
      begin
        // walk, jump, climb, or turn around
        dy := 0;
        NewY := LemY;
        while (dy <= 6) and (HasPixelAt_ClipY(LemX, NewY - 1, -dy - 1) = TRUE) do
        begin
          Inc(dy);
          Dec(NewY);
        end;

        if dy > 6 then
        begin
          if (LemIsClimber) then
            Transition(L, baClimbing)
          else
            TurnAround(L);
          Result := True;
          Exit;
        end
        else begin
          if dy >= 3 then
          begin
            Transition(L, baJumping);
            NewY := LemY - 2;
          end;
          LemY := NewY;
          CheckForLevelTopBoundary(L);
          Result := True;
          Exit;
        end
      end
      else begin // no pixel at feet
        // walk or fall downwards
        dy := 1;
        while dy <= 3 do
        begin
          Inc(LemY);
          if HasPixelAt_ClipY(LemX, LemY, dy) = TRUE then
            Break;
          Inc(Dy);
        end;

        if dy > 3 then
        begin
          // in this case, lemming becomes a faller
          Inc(LemY);
          Transition(L, baFalling);
        end;

        if LemY > LEMMING_MAX_Y then
        begin
          RemoveLemming(L);
          Exit;
        end
        else begin
          Result := True;
          Exit;
        end;

      end;

    end
    else begin
      TurnAround(L);
      Result := True;
      Exit;
    end;
  end; // with L

end;

function TLemmingGame.HandleJumping(L: TLemming): Boolean;
var
  dy: Integer;
begin
  with L do
  begin
    dy := 0;
    while (dy < 2) and (HasPixelAt_ClipY(LemX, LemY - 1, -dy - 1) = TRUE) do
    begin
      Inc(Dy);
      Dec(LemY);
    end;

    if dy < 2 then
      Transition(L, baWalking);
    CheckForLevelTopBoundary(L);
    Result := True;
  end;
end;

function TLemmingGame.HandleDigging(L: TLemming): Boolean;
// returns FALSE if there are no terrain pixels to remove (??? did I write this?)
var
  Y: Integer;
begin
  Result := False;

  with L do
  begin

    if LemIsNewDigger then
    begin
      DigOneRow(L, LemY - 2);
      DigOneRow(L, LemY - 1);
      LemIsNewDigger := FALSE;
    end
    else begin
      Inc(lemFrame);
      if (lemFrame >= 16) then
        lemFrame := lemFrame - 16
    end;

    if LemFrame in [0, 8] then
    begin
      y := lemy;
      Inc(lemY);

      if (lemy > LEMMING_MAX_Y) then
      begin
        { TODO : Probably a small bug here: We should call RemoveLemming! }
        lemRemoved := TRUE;
        //Result := FALSE;
        Exit;
      end;

      if (DigOneRow(L, y) = FALSE) then
        Transition(L, baFalling)
      else if (ReadObjectMap(LemX,lemy) = DOM_STEEL) then
      begin
        CueSoundEffect(SFX_HITS_STEEL);
        Transition(L, baWalking);
      end;

      Result := TRUE;
    end
    else begin
      Result := FALSE
    end;

  end;
end;

function TLemmingGame.HandleClimbing(L: TLemming): Boolean;
begin
//  Result := False;

  with L do
  begin

    if (LemFrame <= 3) then
    begin
      // check if we approached the top
      if (HasPixelAt_ClipY(LemX, LemY - 7 - LemFrame, 0) = FALSE) then
      begin
        LemY := LemY - LemFrame + 2;
        Transition(L, baHoisting);
        CheckForLevelTopBoundary(L);
      end;
      Result := True;
      Exit;
    end
    else begin
      Dec(LemY);
      // check for overhang or level top boundary
      if (LemY + FrameTopDy < HEAD_MIN_Y)
      or (HasPixelAt_ClipY(LemX - LemDx, LemY - 8, -8) = TRUE) then
      begin
        Transition(L, baFalling, TRUE);
        //TurnAround(L);
        Inc(LemX, LemDx * 2);
      end;
      Result := True;
      Exit;
    end;

  end;
end;

function TLemmingGame.HandleDrowning(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    // here the use of HasPixelAt rather than HasPixelAt_ClipY
    // is correct

    if LemEndOfAnimation then
      RemoveLemming(L)
    else if HasPixelAt(LemX + 8 * LemDx, LemY) = FALSE then
      Inc(LemX, LemDx);
  end;
end;

function TLemmingGame.HandleHoisting(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    if LemFrame <= 4 then
    begin
      Dec(LemY, 2);
      CheckForLevelTopBoundary(L);
      Result := True;
      Exit;
    end
    else if (LemEndOfAnimation) then // LemFrame = 7
    begin
      Transition(L, baWalking);
      CheckForLevelTopBoundary(L);
      Result := True;
      Exit;
    end
//    else
  //    Result := False;
  end;
end;

function TLemmingGame.HandleBuilding(L: TLemming): Boolean;
begin
  Result := False;

  with L do
  begin
    // sound
    if (LemFrame = 10) and (LemNumberOfBricksLeft <= 3) then
      CueSoundEffect(SFX_BUILDER_WARNING);

    // lay brick
    if (LemFrame = 9)
    or ( (LemFrame = 10) and (LemNumberOfBricksLeft = 9) ) then
    begin
      LayBrick(L);
      //Result := False;
      Exit;
    end
    else if (LemFrame = 0) then
    begin

      Inc(LemX, LemDx);
      Dec(LemY);
      if (LemX <= LEMMING_MIN_X) or (LemX > LEMMING_MAX_X)
      or (HasPixelAt_ClipY(LemX, LemY - 1, -1) = TRUE) then
      begin
        //TurnAround(L);
        Transition(L, baWalking, TRUE);  // turn around as well
        CheckForLevelTopBoundary(L);
        Result := True;
        Exit;
      end;

      Inc(LemX, LemDx);
      if (HasPixelAt_ClipY(LemX, LemY - 1, -1) = TRUE) then
      begin
        //TurnAround(L);
        Transition(L, baWalking, TRUE);  // turn around as well
        CheckForLevelTopBoundary(L);
        Result := True;
        Exit;
      end;

      Dec(LemNumberOfBricksLeft);
      if (LemNumberOfBricksLeft = 0) then
      begin
        Transition(L, baShrugging);
        CheckForLevelTopBoundary(L);
        Result := True;
        Exit;
      end;

      if (HasPixelAt_ClipY(LemX + LemDx + LemDx, LemY - 9, -9) = TRUE)
      or (LemX <= LEMMING_MIN_X)
      or (LemX > LEMMING_MAX_X) then
      begin
        //TurnAround(L);
        Transition(L, baWalking, TRUE);  // turn around as well
        CheckForLevelTopBoundary(L);
        Result := True;
        Exit;
      end;

      {-------------------------------------------------------------------------------
        if builder too high he becomes a walker. will *not* turn around
        although it seems he should, but the CheckForLevelTop fails because
        of a changed FrameTopDy
      -------------------------------------------------------------------------------}
      if (LemY + FrameTopDy < HEAD_MIN_Y) then
      begin
        Transition(L, baWalking);
        CheckForLevelTopBoundary(L);
      end;

      Result := True;
      Exit;
    end
    else begin
      Result := True;
      Exit;
    end;

  end; // with L
end;

function TLemmingGame.HandleBashing(L: TLemming): Boolean;
var
  n, x, y, dy, Index: Integer;
  FrontObj: Byte;
begin
  Result := False;

  with L do
  begin
    index := lemFrame;
    if index >= 16 then
      Dec(index, 16);

    if (11 <= index) and (index <= 15) then
    begin
      Inc(LemX, LemDx);

      if (LemX < LEMMING_MIN_X) or (LemX > LEMMING_MAX_X) then
      begin
        // outside leftside or outside rightside?
        //TurnAround(L);
        Transition(L, baWalking, TRUE);  // turn around as well
      end
      else begin
        // check 3 pixels below the new position
        dy := 0;
        while (dy < 3) and (HasPixelAt_ClipY(LemX, LemY, dy) = FALSE) do
        begin
          Inc(dy);
          Inc(LemY);
        end;

        if dy = 3 then
          Transition(L, baFalling)
        else begin
          // check steel or one way digging
          FrontObj := ReadObjectMap(LemX + LemDx * 8, LemY - 8);

          if (FrontObj = DOM_STEEL) then
            CueSoundEffect(SFX_HITS_STEEL);

          if (FrontObj = DOM_STEEL)
          or ( (FrontObj = DOM_ONEWAYLEFT) and (LemDx <> -1) )
          or ( (FrontObj = DOM_ONEWAYRIGHT) and (LemDx <> 1) ) then
          begin
            //TurnAround(L);
            Transition(L, baWalking, TRUE);  // turn around as well
          end;
        end;

      end;
      Result := True;
      Exit;
    end
    else begin

      if (2 <= index) and (index <= 5) then
      begin
        // frame 2..5 and 18..21 or used for masking
        ApplyBashingMask(L, index - 2);

        // special treatment frame 5 (see txt)
        if LemFrame = 5 then
        begin
          n := 0;
          x := LemX + lemdx * 8;
          y := LemY - 6;

          // here the use of HasPixelAt rather than HasPixelAt_ClipY
          // is correct
          while (n < 4) and (HasPixelAt(x,y) = FALSE) do
          begin
            Inc(n);
            Inc(x, LemDx);
          end;

          if n = 4 then
            Transition(L, baWalking);
        end;
      end;
      //Result := FALSE;

    end;

  end; // with
end;

function TLemmingGame.HandleMining(L: TLemming): Boolean;
var
  BelowObj: Byte;
  Bug: Boolean;
begin
  Result := False;

  with L do
  begin

    if LemFrame = 1 then
    begin
      ApplyMinerMask(L, 0, LemX + frameLeftdx, LemY + frameTopdy);
      Exit;
    end
    else if lemFrame = 2 then
    begin
      ApplyMinerMask(L, 1, LemX + lemdx + frameLeftdx, LemY + 1 + frameTopdy);
      Exit;
    end
    else if LemFrame in [3, 15] then
    begin
      Inc(LemX, LemDx);
      if (LemX < LEMMING_MIN_X) or (LemX > LEMMING_MAX_X) then
      begin
        Transition(L, baWalking, TRUE); // turn around as well
        Result := True;
        Exit;
      end;

      Inc(LemX, lemdx);
      if (LemX < LEMMING_MIN_X) or (LemX > LEMMING_MAX_X) then
      begin
        Transition(L, baWalking, TRUE);  // turn around as well
        //TurnAround(L);
        Result := True;
        Exit;
      end;

      if (lemFrame = 3) then
      begin
        Inc(LemY);
        if (LemY > LEMMING_MAX_Y) then
        begin
          RemoveLemming(L);
          Result := False;
          Exit;
        end;
      end;

      if HasPixelAt_ClipY(LemX, LemY, 0) = FALSE then
      begin
        Transition(L, baFalling);
        Result := True;
        Exit;
      end;

      belowObj := ReadObjectMap(LemX, LemY);
      if (belowObj = DOM_STEEL) then
      begin
        CueSoundEffect(SFX_HITS_STEEL);
      end;

      {-------------------------------------------------------------------------------
        Emulating dos-bug onewayright:

        The check for steel and one-way walls, you see that the (belowObj == ONE_WAY_RIGHT)
        seems to be missing a check for the lemming's facing direction.
        That's correct as is, and it is simply emulating the same bug in DOS Lemmings,
        which results in a one-way wall pointing right being unminable in either direction.
      -------------------------------------------------------------------------------}
      // @Optional Game Mechanic
      Bug := dgoMinerOneWayRightBug in Options;
      {$ifdef flexi}Bug := (fGameParams.SysDat.Options2 and 1 = 0);{$endif}
      if Bug then
      begin
        if (belowObj = DOM_STEEL)
        or ( (belowObj = DOM_ONEWAYLEFT) and (LemDx <> -1) )
        or (belowObj = DOM_ONEWAYRIGHT) then // missing check
          Transition(L, baWalking, TRUE);  // turn around as well
      end
      else begin
        if (belowObj = DOM_STEEL)
        or ( (belowObj = DOM_ONEWAYLEFT) and (LemDx <> -1) )
        or ( (belowObj = DOM_ONEWAYRIGHT) and (LemDx <> 1) ) then // complete check
          Transition(L, baWalking, TRUE);  // turn around as well
      end;
      // until here the bug-issue

      Result := True;
      Exit;
    end

    else if (lemFrame = 0) then
    begin
      Inc(LemY);
      if (LemY > LEMMING_MAX_Y) then
      begin
        RemoveLemming(L);
        Exit;
      end
      else begin
        Result := True;
        Exit;
      end;
    end
    else
      Result := False

  end; // with
end;

function TLemmingGame.HandleFalling(L: TLemming): Boolean;
var
  dy: Integer;
begin
  Result := False;

  with L do
  begin

    if (LemFallen > 16) and LemIsFloater then
    begin
      Transition(L, baFloating);
      Result := True;
      Exit;
    end
    else begin
      dy := 0;
      while (dy < 3) and (HasPixelAt_ClipY(LemX,LemY,dy) = FALSE) do
      begin
        Inc(Dy);
        Inc(LemY);
        if (LemY > LEMMING_MAX_Y) then
        begin
          RemoveLemming(L); //LemRemoved := TRUE;
          //Result := False;
          Exit;
        end;
      end;// while

      if (dy = 3) then
      begin
        Inc(LemFallen, 3);
        Result := True;
        Exit;
      end
      else begin
        if (LemFallen >{=} MAX_FALLDISTANCE) then
        begin
          Transition(L, baSplatting);
          { ccexplore:
          However, the "return true" after call lemming.SetToSplattering()
          is actually correct.  It is in fact the bug in DOS Lemmings that
          I believe enables the "direct drop to exit":
          by returning TRUE, it gives a chance for lemming.CheckForInteractiveObjects()
          to be called immediately afterwards, which ultimately results in the
          lemming's action turning from SPLATTERING to EXITING.
          }
          Result := True;
          Exit;
        end
        else begin
          Transition(L, baWalking);
          Result := True;
          Exit;
        end;
      end;
    end

  end; // with

end;

function TLemmingGame.HandleFloating(L: TLemming): Boolean;
var
  dy, minY: Integer;
begin
  with L do
  begin

    LemFrame := FloatParametersTable[LemFloatParametersTableIndex].AnimationFrameIndex;
    dy := FloatParametersTable[LemFloatParametersTableIndex].dy;

    Inc(LemFloatParametersTableIndex);
    if LemFloatParametersTableIndex >= 16 then
      LemFloatParametersTableIndex := 8;

    if (dy <= 0) then
      Inc(LemY, dy)
    else begin
      minY := 0;
      while (dy > 0) do
      begin
        if (HasPixelAt_ClipY(LemX, LemY, minY) = TRUE) then
        begin
          Transition(L, baWalking);
          Result := True;
          Exit;
        end
        else begin
          Inc(LemY);
          Dec(dy);
          Inc(minY);
        end;
      end; // while
    end;

    if (LemY > LEMMING_MAX_Y) then
    begin
      RemoveLemming(L);
      Result := False;
      Exit;
    end
    else
      Result := True;

  end; // with
end;

function TLemmingGame.HandleSplatting(L: TLemming): Boolean;
begin
  Result := False;
  if L.LemEndOfAnimation then
    RemoveLemming(L);
end;

function TLemmingGame.HandleExiting(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    if LemEndOfAnimation then
    begin
      RemoveLemming(L);
      Inc(LemmingsIn);
      InfoPainter.SetInfoLemmingsOut(LemmingsOut);
      InfoPainter.SetInfoLemmingsIn(LemmingsIn, MaxNumLemmings);
    end;
  end;
end;

function TLemmingGame.HandleVaporizing(L: TLemming): Boolean;
begin
  Result := False;
  if L.LemEndOfAnimation then
    RemoveLemming(L);
end;

function TLemmingGame.HandleBlocking(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    if HasPixelAt_ClipY(LemX, LemY, 0) = FALSE then
    begin
      Transition(L, baWalking);
      LemIsBlocking := FALSE;
      RestoreMap(L);
    end;
  end;
end;

function TLemmingGame.HandleShrugging(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    if LemEndOfAnimation then
    begin
      Transition(L, baWalking);
      Result := True;
    end
  end;
end;

function TLemmingGame.HandleOhNoing(L: TLemming): Boolean;
var
  dy: Integer;
begin
  Result := False;
  with L do
  begin
    if LemEndOfAnimation then
    begin
      Transition(L, baExploding);
      Exit;
    end
    else begin
      dy := 0;
      while (dy < 3) and (HasPixelAt_ClipY(LemX, LemY, dy) = FALSE) do
      begin
        Inc(dy);
        Inc(LemY);
      end;

      if (LemY > LEMMING_MAX_Y) then
      begin
        RemoveLemming(L);
        Exit;
      end
      else
        Result := True;
    end;
  end; // with

end;

function TLemmingGame.HandleExploding(L: TLemming): Boolean;
begin
  Result := False;
  with L do
  begin
    if LemEndOfAnimation then
    begin
      if LemIsBlocking then
      begin
        LemIsBlocking := False;
        RestoreMap(L);
      end;
      if not (ReadObjectMap(LemX, LemY) in [DOM_STEEL, DOM_WATER]) then
      begin
        ApplyExplosionMask(L);
        //CreateParticles(L);
      end;
      RemoveLemming(L);
      LemExploded := True;
      LemParticleTimer := PARTICLE_FRAMECOUNT;
      if (moShowParticles in fGameParams.MiscOptions) then
        fParticleFinishTimer := PARTICLE_FINISH_FRAMECOUNT;
    end;
  end;
end;

function TLemmingGame.CheckForLevelTopBoundary(L: TLemming; LocalFrameTopDy: Integer = 0): Boolean;
var
  dy: Integer;
begin

  with L do
  begin
    Result := False;
    if LocalFrameTopDy = 0 then
      dy := FrameTopDy
    else
      dy := LocalFrameTopDy;
    if (LemY + dy < HEAD_MIN_Y) then
    begin
      Result := True;
      LemY := HEAD_MIN_Y - 2 - dy;
      TurnAround(L);
      if LemAction = baJumping then
        Transition(L, baWalking);
    end;
  end;
end;


procedure TLemmingGame.RemoveLemming(L: TLemming);
begin
  L.LemRemoved := True;
  Dec(LemmingsOut);
  Inc(LemmingsRemoved);
end;

procedure TLemmingGame.UpdateLemmings;
{-------------------------------------------------------------------------------
  The main method: handling a single frame of the game.
-------------------------------------------------------------------------------}
begin
  if fGameFinished then
    Exit;
  CheckForGameFinished;

  // do not move this!
  if not Paused then // paused is handled by the GUI
    CheckAdjustReleaseRate;

  // just as a warning: do *not* mess around with the order here
  IncrementIteration;
  EraseLemmings;
  CheckReleaseLemming;
  CheckLemmings;
  CheckUpdateNuking;
  UpdateInteractiveObjects;

  // when hyperspeed is terminated then copy world back into targetbitmap
  if fLeavingHyperSpeed then
  begin
    fHyperSpeed := False;
    fLeavingHyperSpeed := False;
    fTargetBitmap.Assign(World);
    if fPauseOnHyperSpeedExit and not Paused then
      SetSelectedSkill(spbPause);
  end;

  DrawAnimatedObjects;
  DrawLemmings;
  CheckForReplayAction;

  // force update if raw explosion pixels drawn
  if fExplodingGraphics then
    fTargetBitmap.Changed;

  CheckForPlaySoundEffect;
end;

procedure TLemmingGame.IncrementIteration;
var
  i: Integer;
const
  OID_ENTRY                 = 1;
begin
  Inc(fCurrentIteration);
  if fReplayGlitchIterations > 0 then
  begin
    Dec(fReplayGlitchIterations);
    Paused := fReplayGlitchIterations > 0;
  end;
  if not (Paused and (fCurrentIteration <= 34)) then Inc(fClockFrame);
  if (Paused and (fCurrentIteration <= 34)) then Inc(fGlitchIterations);

  if fParticleFinishTimer > 0 then
    Dec(fParticleFinishTimer);

  if fClockFrame = 17 then
  begin
    fClockFrame := 0;
    Dec(Seconds);
    if Seconds < 0 then
    begin
      Dec(Minutes);
      Seconds := 59;
    end;
  end
  else if fClockFrame = 1 then
    if InfoPainter <> nil then
    begin
      InfoPainter.SetInfoMinutes(Minutes);
      InfoPainter.SetInfoSeconds(Seconds);
    end;

  // hard coded dos frame numbers
  case CurrentIteration of
    15:
      begin
        CueSoundEffect(SFX_LETSGO);
      end;
    34:
      CueSoundEffect(SFX_ENTRANCE);
    35:
      begin
        EntriesOpened := True;

        {if DoTimePause then // time pause bug emulation
        begin
          Minutes := Level.Info.TimeLimit;
          Seconds := 0;
          fClockFrame := 3;
          if InfoPainter <> nil then
          begin
            InfoPainter.SetInfoMinutes(Minutes);
            InfoPainter.SetInfoSeconds(Seconds);
          end;
        end;}

        for i := 0 to ObjectInfos.Count - 1 do
          if ObjectInfos[i].Obj.Identifier = OID_ENTRY then
          begin
            ObjectInfos[i].Triggered := True;
            ObjectInfos[i].CurrentFrame := 1;
          end;
      end;
    55:
      begin
        if fStartupMusicAfterEntry then
        begin
          if gsoMusic in fSoundOpts then
            SoundMgr.PlayMusic(0);
          fStartupMusicAfterEntry := False;
        end;
      end;
  end;

end;

procedure TLemmingGame.DrawStatics;
begin
  if InfoPainter = nil then
    Exit;


  with InfoPainter, Level.Info do
  begin
    DrawSkillCount(spbSlower, ReleaseRate);
    DrawSkillCount(spbFaster, ReleaseRate);
    DrawSkillCount(spbClimber, ClimberCount);
    DrawSkillCount(spbUmbrella, FloaterCount);
    DrawSkillCount(spbExplode, BomberCount);
    DrawSkillCount(spbBlocker, BlockerCount);
    DrawSkillCount(spbBuilder, BuilderCount);
    DrawSkillCount(spbBasher, BasherCount);
    DrawSkillCount(spbMiner, MinerCount);
    DrawSkillCount(spbDigger, DiggerCount);
  end;
end;

procedure TLemmingGame.HitTest;
var
  HitCount: Integer;
  Lemming1, Lemming2: TLemming;
  S: string;
begin
  HitCount := PrioritizedHitTest(Lemming1, Lemming2, CursorPoint);
  if ((HitCount > 0) and (Lemming1 <> nil)) and not fHitTestAutoFail then
  begin
    S := LemmingActionStrings[Lemming1.lemAction];
    // get highlight text
    if Lemming1.LemIsClimber and Lemming1.LemIsFloater then
      S := SAthlete
    else if Lemming1.LemIsClimber then
      S := SClimber
    else if Lemming1.LemIsFloater then
      S := SFloater
    else
      S := LemmingActionStrings[Lemming1.LemAction];

    InfoPainter.SetInfoCursorLemming(S, HitCount);
    DrawDebugString(Lemming1);
    fCurrentCursor := 2;
  end
  else begin
    InfoPainter.SetInfoCursorLemming('', 0);
    fCurrentCursor := 1;
  end;
end;

function TLemmingGame.ProcessSkillAssignment: Boolean;
var
  Sel: TBasicLemmingAction;
  Lemming1, Lemming2: TLemming;
begin
  Result := False;
  // convert buttontype to skilltype
  Sel := SkillPanelButtonToAction[fSelectedSkill];
  Assert(Sel <> baNone);
  if PrioritizedHitTest(Lemming1, Lemming2, CursorPoint) > 0 then
  begin
    if Lemming1 = nil then
      Lemming1 := LastNPLemming;   // right-click bug emulation

    if Lemming1 <> nil then
    begin
      if (not Paused) or fPausedSkillAssignmentAllowed then
      begin
        fCheckWhichLemmingOnly := False;
        Result := AssignSkill(Lemming1, Lemming2, Sel);
        if Paused and (not (moStateControlEnabled in fGameParams.MiscOptions)) then
          fPausedSkillAssignmentAllowed := False;
      end;
    end;
  end;
end;

procedure TLemmingGame.ReplaySkillAssignment(aReplayItem: TReplayItem);
var
  L, Lemming1, Lemming2: TLemming;
  ReplayMousePos: TPoint;
//  Res: Boolean;
  ass: TBasicLemmingAction;
begin
  with aReplayItem do
  begin
    if (LemmingIndex < 0) or (LemmingIndex >= LemmingList.Count) then
    begin
      RegainControl;
//      ShowMessage('invalid replay, replay ended');
//          fRecorder.SaveToTxt(apppath+'inv.txt');
      infopainter.SetInfoCursorLemming('invalid', 0);

      Exit;
    end;
    L := LemmingList.List^[LemmingIndex];
    assert(assignedskill > 0);
    assert(assignedskill < 19);
    ass := TBasicLemmingAction(assignedskill{ - 1});
    if ass=bablocking then
    begin
      //windlg('');
    end;
//    assert(ass in AssignableSkills);
    if not (ass in AssignableSkills) then
      raise exception.create(i2s(integer(ass)) + ' ' + i2s(currentiteration));

    if ass in AssignableSkills then
    begin
      //Mouse.CursorPos := Point(Screen.Width div 2, Screen.Height div 2);
//      TestMouseOnLemming(L);
      {Res := }

      // for antiques but nice
      if ActionToSkillPanelButton[ass] <> fSelectedSkill then
        SetSelectedSkill(ActionToSkillPanelButton[ass], True);

      ReplayMousePos.X := CursorX;
      ReplayMousePos.Y := CursorY;

      if PrioritizedHitTest(Lemming1, Lemming2, ReplayMousePos, False) > 0 then
      begin
        if Lemming1 = nil then
          Lemming1 := LastNPLemming;  // right-click bug emulation

        if Lemming1 <> nil then
          begin
            fCheckWhichLemmingOnly := True;
            WhichLemming := nil;
            AssignSkill(Lemming1, Lemming2, ass);
            fCheckWhichLemmingOnly := False;
            if (WhichLemming = L) then
              AssignSkill(Lemming1, Lemming2, ass)
            else if (Lemming2 = L) then
              AssignSkill(Lemming2, Lemming2, ass)
          end;
      end;

//      AssignSkill(L, nil, ass);
//      if UseReplayPhotoFlashEffect then
      if not HyperSpeed then
        L.LemHighlightReplay := True;
{      if fReplayedLemmingIndex = -1 then
        fReplayedLemmingIndex := LemmingIndex; }
//      if
      if (LemmingX > 0) and (LemmingY > 0) then
      begin
        if (LemmingX <> l.lemx) or (LemmingY <> l.lemy) then
        begin
          infopainter.SetInfoCursorLemming('invalid', 0);
          RegainControl;
//          ShowMessage('invalid replay, replay ended');
        end;

      //  Assert(LemmingX=l.lemx, 'replay x error' + ','+i2s(lemmingX) + ','+i2s(l.LemX));
        //Assert(LemmingY=l.lemy, 'replay y error' + ','+i2s(LemmingY) + ','+i2s(l.Lemy));
      end
      else begin
        LemmingX := L.lemX;
        LemmingY := L.LemY;
      end;
//      Mouse.
//      Assert(Res, 'replay error 1');
//      if xxx<>lemx then deb(['err', xxx,LemX])
    end;

  end;
end;

procedure TLemmingGame.ReplaySkillSelection(aReplayItem: TReplayItem);
var
  bs: TSkillPanelButton;
begin
  bs := tSkillPanelButton(areplayitem.selectedbutton); // convert
    if bs in [
    spbClimber,
    spbUmbrella,
    spbExplode,
    spbBlocker,
    spbBuilder,
    spbBasher,
    spbMiner,
    spbDigger] then
  setselectedskill(bs, true);
//  case aReplayItem
end;


procedure TLemmingGame.SetSelectedSkill(Value: TSkillPanelButton; MakeActive: Boolean = True; RightClick: Boolean = false);
//var
  //OldSelectedSkill: TButtonSkill;
begin
  case Value of
    spbFaster:
      begin
        if fSpeedingUpReleaseRate <> MakeActive then
        case MakeActive of
          False: RecordReleaseRate(raf_StopChangingRR);
          True: if RightClick = false then RecordReleaseRate(raf_StartIncreaseRR);
        end;
        fSpeedingUpReleaseRate := MakeActive;
        if MakeActive then
          fSlowingDownReleaseRate := False;
        if RightClick and fSpeedingUpReleaseRate then InstReleaseRate := 1;
      end;
    spbSlower:
      begin
        if fSlowingDownReleaseRate <> MakeActive then
        case MakeActive of
          False: RecordReleaseRate(raf_StopChangingRR);
          True: if RightClick = false then RecordReleaseRate(raf_StartDecreaseRR);
        end;
        fSlowingDownReleaseRate := MakeActive;
        if MakeActive then
          fSpeedingUpReleaseRate := False;
        if RightClick and fSlowingDownReleaseRate then InstReleaseRate := -1;
      end;
    spbClimber:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := spbClimber;
        InfoPainter.DrawButtonSelector(spbClimber, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbUmbrella:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbExplode:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbBlocker:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbBuilder:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbBasher:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbMiner:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbDigger:
      begin
        if fSelectedSkill = Value then
          Exit;
        InfoPainter.DrawButtonSelector(fSelectedSkill, False);
        fSelectedSkill := Value;
        InfoPainter.DrawButtonSelector(fSelectedSkill, True);
        CueSoundEffect(SFX_SKILLBUTTON);
        RecordSkillSelection(Value);
      end;
    spbPause:
      //if not fHyperSpeed then
      begin
        case Paused of
          False:
            begin
              Paused := True;
              PausedSkillAssignmentAllowed := True;
              FastForward := False;
              RecordStartPause;
              //HyperSpeedEnd;
            end;
          True:
            begin
              Paused := False;
              FastForward := False;
              RecordEndPause;
            end;
        end;
      end;
    spbNuke:
      begin
        UserSetNuking := True;
        // next line of code is NUKE GLITCH
        // changing MaxNumLemmings also allows IN % to be calculated
        // and displayed in-game using the glitch calculation,
        // just like the actual game
        MaxNumLemmings := LemmingsReleased;

        ExploderAssignInProgress := True;
        RecordNuke;
      end;
  end;
end;

procedure TLemmingGame.CheckReleaseLemming;
var
  NewLemming: TLemming;
//  MustCreate: Boolean;
//  Entry: TInteractiveObject;
  ix, EntranceIndex: Integer;
//  L:
begin
  if not EntriesOpened then
    Exit;
  if UserSetNuking then
    Exit;

  // NextLemmingCountdown is initialized to 20 before start of a level
  Dec(NextLemmingCountdown);

  if NextLemmingCountdown = 0 then
  begin
    NextLemmingCountdown := CalculateNextLemmingCountdown;
    if LemmingsReleased < MaxNumLemmings then
    begin
      EntranceIndex := LemmingsReleased mod 4;
      ix := DosEntryTable[EntranceIndex];
      if ix >= 0 then
      begin
        NewLemming := TLemming.Create;
        with NewLemming do
        begin
          LemIndex := LemmingList.Add(NewLemming);
          LemBorn := CurrentIteration;
          Transition(NewLemming, baFalling);
          LemX := ObjectInfos[ix].Obj.Left + 24;
          // @Optional Game Mechanic
          if dgoEntranceX25 in Options then
            Inc(LemX);

          LemY := ObjectInfos[ix].Obj.Top + 14;
          LemDX := 1;

          // these must be initialized to nothing
          LemObjectInFront := DOM_NONE;
          LemObjectBelow := DOM_NONE;
        end;
        Inc(LemmingsReleased);
        Inc(LemmingsOut);
      end;
    end;
  end;

end;

procedure TLemmingGame.CheckUpdateNuking;
var
  CurrentLemming: TLemming;
begin

  if UserSetNuking and ExploderAssignInProgress then
  begin

    // find first following non removed lemming
    while (Index_LemmingToBeNuked <{=} LemmingsReleased)
    and (LemmingList[Index_LemmingToBeNuked].LemRemoved) do
      Inc(Index_LemmingToBeNuked);

    if (Index_LemmingToBeNuked > LemmingsReleased - 1) then // added - 1
      ExploderAssignInProgress := FALSE
    else begin
      CurrentLemming := LemmingList[Index_LemmingToBeNuked];
      with CurrentLemming do
      begin
        if (LemExplosionTimer = 0)
        and not (LemAction in [baSplatting, baExploding]) then
          LemExplosionTimer := 79;
      end;
      Inc(Index_LemmingToBeNuked);

    end;

  end;

end;

procedure TLemmingGame.CreateLemmingAtCursorPoint;
{-------------------------------------------------------------------------------
  debugging procedure: click and create lemming
-------------------------------------------------------------------------------}
var
  NewLemming: TLemming;
begin
  if not EntriesOpened then
    Exit;
  if UserSetNuking then
    Exit;
  if LemmingsReleased < MaxNumLemmings then
  begin
    NewLemming := TLemming.Create;
    with NewLemming do
    begin
      LemIndex := LemmingList.Add(NewLemming);
      LemBorn := CurrentIteration;
      Transition(NewLemming, baFalling);
//      Transition(NewLemming, baJumping);
      LemX := CursorPoint.X;
      LemY := CursorPoint.Y;
      LemDX := 1;

      // these must be initialized to nothing
      LemObjectInFront := DOM_NONE;
      LemObjectBelow := DOM_NONE;
    end;
    Inc(LemmingsReleased);
    Inc(LemmingsOut);
  end;

end;


function TLemmingGame.CalculateNextLemmingCountdown: Integer;
(* ccexplore:
  All I know is that in the DOS version, the formula is that for a given RR,
  the number of frames from one release to the next is:
  (99 - RR) / 2 + 4
  Where the division is the standard truncating integer division
  (so for example RR 99 and RR 98 acts identically).

  This means for example, at RR 99,
  it'd be release, wait, wait, wait, release, wait, wait, wait, release,

  I don't know what the frame rate is though on the DOS version,
  although to a large extent this mostly does not matter, since most aspects
  of the game mechanics is based off of number of frames rather than absolute time.
*)
begin
  Result := 99 - currReleaseRate;
  if (Result < 0) then
    Inc(Result, 256);
  Result := Result div 2 + 4
end;

procedure TLemmingGame.CueSoundEffect(aSoundId: Integer);
{-------------------------------------------------------------------------------
  Save last sound.
-------------------------------------------------------------------------------}
begin
  if HyperSpeed or not Playing or not (gsoSound in fSoundOpts) then
    Exit;

  if (fSoundToPlay = SFX_ASSIGN_SKILL) or (aSoundId < 0) then
    Exit;

  fSoundToPlay := aSoundId;

  if Paused then
    CheckForPlaySoundEffect;
end;

procedure TLemmingGame.DrawDebugString(L: TLemming);
begin
  if Assigned(fOnDebugLemming) then
    fOnDebugLemming(L);
end;
(*
procedure TLemmingGame.DrawError(const S: string; aCode: Integer);
{-------------------------------------------------------------------------------
  this procedure
  o sets the errorcountdown to 48
  o shows the error + code at the place of the hittest
-------------------------------------------------------------------------------}
begin
  if aCode > 0 then
    aCode := -aCode;
  if fInfoPainter <> nil then
    fInfoPainter.SetInfoCursorLemming(S, aCode);
end;
*)

procedure TLemmingGame.AdjustReleaseRate(Delta: Integer);
var
  N: Integer;
begin
  N := CurrReleaseRate + Delta;
  Restrict(N, Level.Info.ReleaseRate, 99);
  if N <> currReleaseRate then
  begin
    currReleaseRate := N;
    InfoPainter.DrawSkillCount(spbFaster, currReleaseRate);
  end;
end;

procedure TLemmingGame.RecordStartPause;
{-------------------------------------------------------------------------------
  Records the start of a pause session.
  Just in case: when the previous record is raf_Pausing or raf_StartPause
  we do *not* record it.
-------------------------------------------------------------------------------}

    function PrevOk: Boolean;
    var
      R: TReplayItem;
    begin
      Result := True;
      with fRecorder.List do
      begin
        if Count = 0 then
          Exit;
        R := TReplayItem(List^[Count - 1]);
        Result := R.ActionFlags and (raf_Pausing or raf_StartPause) = 0;
      end;
    end;

var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying or not PrevOk then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := raf_StartPause;
  R.ReleaseRate := currReleaseRate;
end;

procedure TLemmingGame.RecordEndPause;
{-------------------------------------------------------------------------------
  Recording the end of a pause.
  Just in case: this is only allowed if there is a startpause-counterpart.
  if there is a previous record then this previous record has to
  have a flag raf_Pausing or raf_StartPause.
  In all other cases EndPause is *not* recorded.
-------------------------------------------------------------------------------}

    function PrevOk: Boolean;
    var
      R: TReplayItem;
    begin
      Result := False;
      with fRecorder.List do
      begin
        if Count = 0 then
          Exit;
        R := TReplayItem(List^[Count - 1]);
        Result := R.ActionFlags and (raf_Pausing or raf_StartPause) <> 0;
      end;
    end;

var
  R: TReplayItem;

begin
  if not fPlaying or fReplaying or not PrevOk then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := raf_EndPause;
  R.ReleaseRate := currReleaseRate;
end;

procedure TLemmingGame.RecordNuke;
{-------------------------------------------------------------------------------
  Easy one: Record nuking. Always add new record.
-------------------------------------------------------------------------------}
var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying then
    Exit;
  R := fRecorder.Add;
  R.Iteration := CurrentIteration;
  R.ActionFlags := R.ActionFlags or raf_Nuke;
  // Just in case: nuking is normally not possible when pausing.
  // but it does no harm setting the raf_Pause flag
  if Paused then
    R.ActionFlags := R.ActionFlags or raf_Pausing;
  R.ReleaseRate := currReleaseRate;
end;

procedure TLemmingGame.RecordReleaseRate(aActionFlag: Byte);
{-------------------------------------------------------------------------------
  This is a tricky one. It can be done when pausing and when not pausing.
  Records a releaserate change command so the only valid parameters are:
    o raf_StartIncreaseRR,
    o raf_StartDecreaseRR,
    o raf_StopChangingRR
-------------------------------------------------------------------------------}
{ TODO : This whole "NewRec" thing can be deleted, we do not need the variable }
var
  R: TReplayItem;
  NewRec: Boolean;
begin
  if not fPlaying or fReplaying then
    Exit;

  Assert(aActionFlag in [raf_StartIncreaseRR, raf_StartDecreaseRR, raf_StopChangingRR]);

  NewRec := False;
  if Paused then
  begin
    Assert(Recorder.List.Count > 0);
    // get last record
    R := Recorder.List.List^[Recorder.List.Count - 1];

    // some records are not safe to overwrite
    // we must begin a new one then
    if (R.ActionFlags and raf_StartPause <> 0) or
       (R.Iteration <> CurrentIteration) or
       (R.ActionFlags and raf_SkillAssignment <> 0) then
    begin
      R := Recorder.add;
      NewRec := True;
    end;
  end
  else begin
    // not paused, always create new record
    R := Recorder.Add;
    NewRec := True;
  end;

  R.Iteration := CurrentIteration;
  R.ReleaseRate := CurrReleaseRate;

  { TODO : Just use the "or" statement }
  if NewRec then
  begin
    R.ActionFlags := aActionFlag;
  end
  else begin
    R.ActionFlags := R.ActionFlags or aActionFlag;
    if Paused then
      R.ActionFlags := R.ActionFlags or raf_Pausing;
  end;

end;

procedure TLemmingGame.RecordSkillAssignment(L: TLemming; aSkill: TBasicLemmingAction);
{-------------------------------------------------------------------------------
  Always add new record.
-------------------------------------------------------------------------------}
var
  R: TReplayItem;
begin
  if not fPlaying or fReplaying then
    Exit;

  R := Recorder.Add;

  R.Iteration := CurrentIteration;
  R.ActionFlags := raf_SkillAssignment;

  // this is possible in debugmode but don't know if I keep it that way
  if Paused then
    R.ActionFlags := R.ActionFlags or raf_Pausing;

  R.LemmingIndex := L.LemIndex;
  R.ReleaseRate := CurrReleaseRate;
  R.AssignedSkill := Byte(aSkill){ + 1}; // the byte is "compatible" for now
  R.LemmingX := L.LemX;
  R.LemmingY := L.LemY;
  R.CursorX := CursorPoint.X;
  R.CursorY := CursorPoint.Y;
end;

procedure TLemmingGame.RecordSkillSelection(aSkill: TSkillPanelButton);
var
  R: TReplayItem;
  NewRec: Boolean;
begin
//  Assert(aSkill)
  if not fPlaying then Exit;
  if fReplaying then
    Exit;
  assert(askill in [    spbClimber,
    spbUmbrella, spbExplode, spbBlocker, spbBuilder, spbBasher,
    spbMiner, spbDigger]);
//  if ReplayList.Count = 0 then

  NewRec := False;
  if Paused then
  begin
    Assert(Recorder.List.Count > 0);
    R := Recorder.List.List^[Recorder.List.Count - 1];

    // some records are not safe to overwrite
    // we must begin a new one then
    if (R.ActionFlags and raf_StartPause <> 0) or
       (R.Iteration <> CurrentIteration) or
       (R.ActionFlags and raf_SkillAssignment <> 0) then
    begin
      R := Recorder.Add;
      NewRec := True;
    end;


   // if R.Iteration = CurrentIteration then
     // if R.ActionFlags and raf}
  end
  else begin
    R := Recorder.Add;
    NewRec := True;
  end;

  R.Iteration := CurrentIteration;
  if NewRec then
    R.ActionFlags := raf_SkillSelection
  else
    R.ActionFlags := R.ActionFlags or raf_SkillSelection;

  if Paused then R.ActionFlags := R.ActionFlags or raf_Pausing;

  R.ReleaseRate := CurrReleaseRate;

  { TODO : make a table for this }
  case aSkill of
    spbClimber:r.SelectedButton := rsb_Climber;
    spbUmbrella:r.SelectedButton := rsb_Umbrella;
    spbExplode:r.SelectedButton := rsb_Explode;
    spbBlocker:r.SelectedButton := rsb_Stopper;
    spbBuilder:r.SelectedButton := rsb_Builder;
    spbBasher:r.SelectedButton := rsb_Basher;
    spbMiner:r.SelectedButton := rsb_Miner;
    spbDigger:r.SelectedButton := rsb_Digger;
  // make sure of nothing else
  end;


end;


procedure TLemmingGame.CheckForReplayAction;
// this is bad code but works for now

// all records with the same iterationnumber must be
// handled here in one atomic moment
var
  R: TReplayItem;
  Last: Integer;
begin
  if not fReplaying then
    Exit;

  Last := fRecorder.List.Count - 1;

  fReplayCommanding := True;

{  if CurrentIteration > 2366 then
  begin
  fReplayCommanding := True;

  end;
}
  try

  // although it may not be possible to have 2 replay-actions at one
  // iteration we use a while loop: it's the safest method
  while fReplayIndex <= Last do
  begin
    R := fRecorder.List.List^[fReplayIndex];


    // break if we go beyond the current iteration
    if R.Iteration <> CurrentIteration then
      Break;

    if raf_Nuke and r.actionflags <> 0 then
      SetSelectedSkill(spbNuke, True);
    {else }if raf_skillassignment and r.actionflags <> 0 then
    begin
//    deb([r.iteration, r.actionflags]);
      ReplaySkillAssignment(R);
    end;
    {else }if raf_skillselection and r.actionflags <> 0 then
      ReplaySkillSelection(R);
    {else }if raf_stopchangingRR and r.actionflags <> 0 then
    begin
      SetSelectedSkill(spbFaster, False);
      SetSelectedSkill(spbSlower, False);
      if R.ReleaseRate <> CurrReleaseRate then
        if R.ReleaseRate > 0 then
          AdjustReleaseRate(R.ReleaseRate - currReleaseRate);
    end
    else if raf_startincreaserr and r.actionflags <> 0 then
      SetSelectedSkill(spbFaster, True)
    else if raf_startdecreaserr and r.actionflags <> 0 then
      SetSelectedSkill(spbSlower, True);

    // check for changes (error)
    if not fReplaying then
      Exit;

    // double check
    if R.ReleaseRate <> CurrReleaseRate then
      if R.ReleaseRate > 0 then
        AdjustReleaseRate(R.ReleaseRate - currReleaseRate);

    Inc(fReplayIndex);



    if fReplayIndex >= fRecorder.List.Count then
      Break;
    if fReplayIndex < 0 then
      Break;
  end;

  finally
    fReplayCommanding := False;
  end;

end;

procedure TLemmingGame.CheckLemmings;
var
  i: Integer;
  CurrentLemming: TLemming;
  HandleInteractiveObjects: Boolean;
  CountDownReachedZero: Boolean;
begin
  for i := 0 to LemmingList.Count - 1 do
  begin
    CurrentLemming := LemmingList.List^[i];

    with CurrentLemming do
    begin
      CountDownReachedZero := False;
      // @particles
      if LemParticleTimer > 0 then
      begin
        Dec(LemParticleTimer);
        Inc(LemParticleFrame);
      end;
      if LemRemoved then
        Continue;
      if LemExplosionTimer <> 0 then
        CountDownReachedZero := UpdateExplosionTimer(CurrentLemming);
      if CountDownReachedZero then
        Continue;
      HandleInteractiveObjects := HandleLemming(CurrentLemming);
      if HandleInteractiveObjects then
        CheckForInteractiveObjects(CurrentLemming);
    end;

  end;
end;

procedure TLemmingGame.SetGameResult;
{-------------------------------------------------------------------------------
  We will not, I repeat *NOT* simulate the original Nuke-error.

  (ccexplore: sorry, code added to implement the nuke error by popular demand)
-------------------------------------------------------------------------------}
begin
  with GameResultRec do
  begin
    gCount              := MaxNumLemmings;   // NUKE GLITCH
    gToRescue           := Level.Info.RescueCount;
    gRescued            := LemmingsIn;

    if Level.Info.LemmingsCount = 0 then
      gTarget := 0
    else
      gTarget := (gToRescue * 100) div Level.Info.LemmingsCount;

    if gCount = 0 then
      gDone := 0
    else
      gDone := (gRescued * 100) div gCount;

    GameResult          := gDone >= gTarget;
    gSuccess            := GameResult;
    gCheated            := fGameCheated;

    if gCheated then
    begin
      gSuccess := True;
      gDone := 100;
    end;
  end;
end;

procedure TLemmingGame.CheckForPlaySoundEffect;
begin
  if HyperSpeed then
    Exit;
  if fSoundToPlay <> -1 then
  begin
    SoundMgr.PlaySound(fSoundToPlay);
//      PlaySound(Sounds[fSoundToPlay].Memory, HINSTANCE, SND_ASYNC + SND_MEMORY);
    fSoundToPlay := -1;
  end;
end;

procedure TLemmingGame.RegainControl;
{-------------------------------------------------------------------------------
  This is a very important routine. It jumps from replay into usercontrol.
-------------------------------------------------------------------------------}
//var
  //LastBeforeBeginPause: Integer;

    function FindBeforePause: Integer; //=findbeginpause
    var
      R: TReplayItem;
    begin

      Result := fReplayIndex;

      if fReplayIndex >= Recorder.List.Count then
      begin
        Result := Recorder.List.Count - 1;
        Exit;
      end;


      while Result >= 0 do
      begin
        R := Recorder.List.List^[Result];
        if R.ActionFlags and (raf_Pausing or raf_EndPause{ or raf_StartPause}) = 0 then
          Break;
        Dec(Result);
      end;
    end;

    (*
    function FindPoint: Integer; //=findbeginpause
    var
      R: TReplayItem;
    begin

      Result := fReplayIndex;

      if fReplayIndex >= Recorder.List.Count then
      begin
        Result := Recorder.List.Count - 1;
        Exit;
      end;


      while Result >= 0 do
      begin
        R := Recorder.List.List^[Result];
        if R.ActionFlags and (raf_Pausing or raf_EndPause{ or raf_StartPause}) = 0 then
          Break;
        Dec(Result);
      end;
    end;*)


begin
  if fReplaying then
  begin
    //cuesoundeffect()
    // there is a bug here when regaining...
    fReplaying := False;

    //LastBeforeBeginPause := FindBeforePause;
    //fRecorder.Truncate(Max(LastBeforeBeginPause, 0));

    fRecorder.Truncate(fReplayIndex);

    // special case: if the game is paused
    // and the control is regained we have to insert a
    // startpause record.
    if Paused then
      RecordStartPause;

    fReplayIndex := 0;//Recorder.List.Count - 1;
  end;
end;

procedure TLemmingGame.SetOptions(const Value: TDosGameOptions);
begin
  fOptions := Value;
  Include(fOptions, dgoObsolete);
end;

procedure TLemmingGame.HyperSpeedBegin(PauseWhenDone: Boolean = False);
begin
  Inc(fHyperSpeedCounter);
  fHyperSpeed := True;
  FastForward := False;
  if PauseWhenDone then
    fPauseOnHyperSpeedExit := True
  else
    fPauseOnHyperSpeedExit := False;
//  fSoundToPlay := -1;
end;

procedure TLemmingGame.HyperSpeedEnd;
begin
  if fHyperSpeedCounter > 0 then
  begin
    Dec(fHyperSpeedCounter);
    if fHyperSpeedCounter = 0 then
    begin
      fLeavingHyperSpeed := True;
      //fHyperSpeed := False;
    end;
  end;
end;


procedure TLemmingGame.UpdateInteractiveObjects;
{-------------------------------------------------------------------------------
  This method handles the updating of the moving interactive objects:
  o Entrances moving
  o Continuously moving objects like water
  o Triggered objects (traps)
  NB: It does not handle the drawing
-------------------------------------------------------------------------------}
var
  Inf: TInteractiveObjectInfo;
  i: Integer;
const
  OID_ENTRY                 = 1;
begin
(*  // moving entrances?
  if not fEntranceAnimationCompleted
  and (CurrentIteration >= 35) then
  begin
    for i := 0 to Entries.Count - 1 do
    begin
      Inf := Entries.List^[i];
      if Inf.Triggered then
      begin
        Inc(Inf.CurrentFrame);
        if Inf.CurrentFrame >= Inf.MetaObj.AnimationFrameCount then
        begin
          Inf.CurrentFrame := 0;
          Inf.Triggered := False;
          fEntranceAnimationCompleted := True;
        end;
      end;
    end;
  end;
*)
  // other objects
//  with ObjectInfos do

  if Paused and (CurrentIteration <= 34) then Exit;

  for i := ObjectInfos.Count - 1 downto 0 do
  begin
    Inf := ObjectInfos.List^[i];

    if Inf.Triggered or (Inf.MetaObj.AnimationType = oat_Continuous) then
      Inc(Inf.CurrentFrame);
    if Inf.CurrentFrame >= Inf.MetaObj.AnimationFrameCount then
    begin
      Inf.CurrentFrame := 0;
      Inf.Triggered := False;
      if (Inf.Obj.Identifier = OID_ENTRY) then
        fEntranceAnimationCompleted := True;
    end;

  end;

end;

procedure TLemmingGame.CheckAdjustReleaseRate;
begin
//exit;
  if SpeedingUpReleaseRate then
  begin
    //if not (Replaying and Paused) then
    AdjustReleaseRate(1)
  end
  else if SlowingDownReleaseRate then
  begin
    //if not (Replaying and Paused) then
    AdjustReleaseRate(-1)
  end;
  if InstReleaseRate = -1 then
    AdjustReleaseRate(-100)
  else if InstReleaseRate = 1 then
    AdjustReleaseRate(100);
  InstReleaseRate := 0;
end;

procedure TLemmingGame.SetSoundOpts(const Value: TGameSoundOptions);
begin
  if fSoundOpts = Value then
    Exit;
  fSoundOpts := Value;
  if not (gsoMusic in fSoundOpts) then
    SoundMgr.StopMusic(0)
  else
    SoundMgr.PlayMusic(0)
end;

procedure TLemmingGame.Finish;
begin
  fGameFinished := True;
  SoundMgr.StopMusic(0);
  SoundMgr.Musics.Clear;
  if Assigned(fOnFinish) then
    fOnFinish(Self);
end;

procedure TLemmingGame.Cheat;
begin
  if not fGameParams.CheatCodesEnabled then exit;
  fGameCheated := True;
  Finish;
end;

procedure TLemmingGame.SaveGameplayImage(Filename: String);
var
  OutImage: TPngObject;
  X, Y: Integer;
  C: TColor32;
  R, G, B: Byte;
begin
  OutImage := TPngObject.CreateBlank(COLOR_RGB, 8, fTargetBitmap.Width, fTargetBitmap.Height);
  for Y := 0 to fTargetBitmap.Height - 1 do
    for X := 0 to fTargetBitmap.Width - 1 do
    begin
      C := fTargetBitmap.Pixel[X, Y];
      R := C shr 16;
      G := C shr 8;
      B := C;
      C := ($FF shl 24) + (B shl 16) + (G shl 8) + (R);
      OutImage.Pixels[X, Y] := C;
    end;
  OutImage.CompressionLevel := 9;
  OutImage.SaveToFile(Filename);
  OutImage.Free;
end;

procedure TLemmingGame.Save(TestSave: Boolean = false);
var
  exn: String;
begin
  exn := '';
  if TestSave then exn := '_' + FormatDateTime('yymmdd"_"hhnn', Now);
  if not ForceDirectories(AppPath + 'Replay\') then
    Exit;
  Recorder.SaveToFile(AppPath + 'Replay\' +
    {$ifdef cust}{$ifndef flexi}Trim(fgameparams.Level.Info.Title) + exn{$else}
    fgameparams.Info.dSectionName + '_' + leadzerostr(fgameparams.info.dLevel + 1, 2){$endif}{$else}
    fgameparams.Info.dSectionName + '_' + leadzerostr(fgameparams.info.dLevel + 1, 2){$endif}
    + '.lrb');
  Recorder.SaveToTxt(AppPath + 'Replay\' +
    {$ifdef cust}{$ifndef flexi}Trim(fgameparams.Level.Info.Title) + exn{$else}
    fgameparams.Info.dSectionName + '_' + leadzerostr(fgameparams.info.dLevel + 1, 2){$endif}{$else}
    fgameparams.Info.dSectionName + '_' + leadzerostr(fgameparams.info.dLevel + 1, 2){$endif}
    + '.txt');

end;

{ TReplayItem }

procedure TLemmingGame.InitializeBrickColors(aBrickPixelColor: TColor32);
var
  i: Integer;
  aR, aG, aB: Integer;
  P: PColor32Entry;
begin
  BrickPixelColor := aBrickPixelColor;
//  FillChar(BrickPixelColors, sizeof(BrickPixelColors), 0);


(* testing
  for i := 0 to 11 do
    if Odd(i) then
    BrickPixelColors[i] := clyellow32
  else
    BrickPixelColors[i] := clred32;

    exit; *)


  for i := 0 to 11 do
    BrickPixelColors[i] := aBrickPixelColor;

  if not fUseGradientBridges then
    Exit;

  P := @BrickPixelColor;
//  with P^ do
    //deb([r,g,b]);
//  deb(['---------']);

  with p^ do
  begin
    ar:=r;
    ag:=g;
    ab:=b;
  end;

  // lighter
  for i := 7 to 11 do
  begin
    P := @BrickPixelColors[i];
    with P^ do
    begin
      if aR < 252  then inc(ar,4);
      if ag < 252 then inc(ag,4);
      if ab < 252 then inc(ab,4);
      r:=ar; g:=ag; b:=ab;
  //    deb([r,g,b]);
    end;
  end;


  P := @BrickPixelColor;

  with p^ do
  begin
    ar:=r;
    ag:=g;
    ab:=b;
  end;


  // darker
  for i := 5 downto 0 do
  begin
    P := @BrickPixelColors[i];
    with P^ do
    begin
      if aR > 3 then dec(ar,4);
      if ag > 3 then dec(ag,4);
      if ab > 3 then dec(ab,4);
      r:=ar; g:=ag; b:=ab;
//      deb([r,g,b]);
    end;
  end;

end;

{ TRecorder }

function TRecorder.Add: TReplayItem;
begin
  Result := TReplayItem.create;
  List.Add(Result);
end;

procedure TRecorder.Clear;
begin
  List.Clear;
end;

constructor TRecorder.Create(aGame: TLemmingGame);
begin
  fGame := aGame;
  List := TObjectlist.Create;
end;

destructor TRecorder.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TRecorder.LoadFromFile(const aFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmOpenRead{Write}); // needed for silent conversion
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TRecorder.LoadFromStream(S: TStream);
var
//  i: Integer;
  H: TReplayFileHeaderRec;
  R: TReplayRec;
  It: TReplayItem;
  Iter, RR, Parity: Integer;
begin
  S.ReadBuffer(H, SizeOf(H));

  if H.Signature <> 'LRB' then
    raise Exception.Create('invalid replay header signature, must be LRB');

  //if H.Version <> 1 then
    //raise Exception.Create('invalid replay header version, must be 1');

  if H.FileSize <> S.Size then
    raise Exception.Create('invalidate replay header filesize');

  if H.HeaderSize <> SizeOf(TReplayFileHeaderRec) then
    raise Exception.Create('invalid replay header headersize');

  if H.Mechanics - [dgoObsolete] <> fGame.Options - [dgoObsolete] then
    raise Exception.Create('invalid replay header different mechanics');

  if H.FirstRecordPos < H.HeaderSize then
    raise Exception.Create('invalid replay header first recordpos');

  if H.ReplayRecordSize <> SizeOf(TReplayFileHeaderRec) then
    raise Exception.Create('invalid replay header replay-record size');


  // we added this silent conversion in replay version 2, Lemmix 0.0.8.0
(*  if H.Version = 1 then
  begin
    H.Version := LEMMIX_REPLAY_VERSION;
    Include(H.Mechanics, dgoMinerOneWayRightBug);
    S.Seek(0, soFromBeginning);
    S.Write(H, H.HeaderSize);
  end; *)
{  if H.Version = 1 then
    ShowMessage('This replay file contains obsolete mechanics, please upgrade it');
 }


 
  if (H.Version <> LEMMIX_REPLAY_VERSION)
  and (H.Version <> LEMMIX_REPLAY_VERSION + 1) then
    raise Exception.Create('invalid replay header version (' + i2s(H.Version) + '). ' +
                           'This must be ' + i2s(LEMMIX_REPLAY_VERSION));


  Parity := 0;
  List.Clear;
  List.Capacity := H.ReplayRecordCount;
  S.Seek(H.FirstRecordPos, soFromBeginning);

  fGame.ReplayGlitchIterations := H.Reserved1 + 1;

  while True do
  begin
    if S.Read(R, SizeOf(TReplayRec)) <> SizeOf(TReplayRec) then
      Break;
    if R.Check <> 'R' then
      raise Exception.Create('replay record check error, must be "R"');
    It := Add;
    It.Iteration := R.Iteration;
    It.ActionFlags := R.ActionFlags;
    It.AssignedSkill := R.AssignedSkill;
    It.SelectedButton := R.SelectedButton;
    It.ReleaseRate := R.ReleaseRate;
    It.LemmingIndex := R.LemmingIndex;
    It.LemmingX := R.LemmingX;
    It.LemmingY := R.LemmingY;
    It.CursorX := R.CursorX;
    It.CursorY := R.CursorY;

    if R.ActionFlags = raf_StartPause then
      Inc(Parity)
    else if R.ActionFlags = raf_Endpause then
      Dec(Parity);

    RR := R.Releaserate;
    Iter := R.Iteration;

    // add fake paused record if unpaired startpuase
    // this happens when this file is saved in paused mode!
    if List.Count = H.ReplayRecordCount then
      if Parity = 1 then
//      if R.ActionFlags = raf_StartPause then
      begin
        It := Add;
        It.Iteration := Iter;
        It.ReleaseRate := RR;
        It.ActionFlags := raf_EndPause;
        Break;
      end;

    if List.Count >= H.ReplayRecordCount then
      Break;
  end;
end;

procedure TRecorder.SaveToFile(const aFileName: string);
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

procedure TRecorder.SaveToStream(S: TStream);
var
  i: Integer;
  H: TReplayFileHeaderRec;
  R: TReplayRec;
  It: TReplayItem;
//  RecCount: Integer;
  //Parity: Integer;

  (*
    // a little #$% selfprotection against an isolated startpause when saving
    function GetRecordableCount: Integer;
    var
      It: TReplayItem;
      Ix: Integer;
    begin
//      with fRecorder do
      begin
        Ix := List.Count - 1;
        Result := List.Count;
        while Ix >= 0 do
        begin
          It := List.List^[ix];
          if It.ActionFlags and raf_StartPause = 0 then
            Break;
          Dec(Result);
          Dec(Ix);
        end;

      end;
    end;
*)

begin

  FillChar(H, SizeOf(TReplayFileHeaderRec), 0);
//  Parity := 0;

  H.Signature := 'LRB';
  H.Version := LEMMIX_REPLAY_VERSION;
  H.FileSize := SizeOf(TReplayFileHeaderRec) +
                SizeOf(TReplayRec) * List.Count;
  H.HeaderSize := SizeOf(TReplayFileHeaderRec);
  H.Mechanics := fGame.Options; // compatible for now
  Include(H.Mechanics, dgoObsolete);
  H.FirstRecordPos := H.HeaderSize;
  H.ReplayRecordSize := SizeOf(TReplayFileHeaderRec);
  H.ReplayRecordCount := List.Count;
  H.Reserved1 := fGame.GlitchIterations;

  // bad programming
  {
  if fGameMechanics.Paused then
  begin
    Inc(H.ReplayRecordCount);
    parity := 1;
  end;
  }

  FillChar(H.LevelTitle, 32, ' ');
  with fGame.Level.Info do
    for i := 1 to Max(32, Length(Title)) do
      H.LevelTitle[i - 1] := Title[i];

  S.WriteBuffer(H, SizeOf(TReplayFileHeaderRec));

  for i := 0 to List.Count - 1 do
  begin
    It := List.List^[i];

    (*
    if It.ActionFlags = raf_Startpause then
      Inc(Parity)
    else if It.ActionFlags = raf_EndPause then
      Dec(Parity);

    if Parity > 1 then
      raise Exception.Create('too much start pauses');

    if Parity < - 1 then
      raise Exception.Create('too much end pauses'); *)

    R.Check := 'R';
    R.Iteration := It.fIteration;
    R.ActionFlags := It.fActionFlags;
    R.AssignedSkill := It.AssignedSkill;
    R.SelectedButton := It.fSelectedButton;
    R.ReleaseRate := It.fReleaseRate;
    R.LemmingIndex := It.fLemmingIndex;
    R.LemmingX := It.fLemmingX;
    R.LemmingY := It.fLemmingY;
    R.CursorX := It.CursorX;
    R.CursorY := It.CursorY;

    //It.WriteToRec(R);
    S.WriteBuffer(R, SizeOf(TReplayRec));
  end;
   (*

  //windlg(
  // this is the case when saving in paused mode, we add a pause record
  // to the stream
  if Parity = 1 then
  begin
    R.Check := 'R';
    R.Iteration := fGameMechanics.CurrentIteration;
    R.ActionFlags := raf_EndPause;
    S.WriteBuffer(R, SizeOf(TReplayRec));
  end;
//  else if Parity <> 0 then
  //  raise Exception.Create('invalid save');
  *)

end;

procedure TRecorder.SaveToTxt(const aFileName: string);

var
  i: Integer;
  l:tstringlist;
  m: TDosGameOptions;
  it:TReplayItem;

const skillstrings: array[rla_none..rla_exploding] of string =
(   '-',
    'Walk',
    'Jump (not allowed)',
    'Dig',
    'Climb',
    'Drown (not allowed)',
    'Hoist (not allowed)',
    'Build',
    'Bash',
    'Mine',
    'Fall (not allowed)',
    'Float',
    'Splat (not allowed)',
    'Exit (not allowed)',
    'Vaporize (not allowed)',
    'Block',
    'Shrug (not allowed)',
    'Ohno (not allowed)',
    'Explode'
);

const selstrings: array[rsb_none..rsb_nuke] of string = (
  '-',
  'Slower',
  'Faster',
  'Climber',
  'Umbrella',
  'Explode',
  'Stopper',
  'Builder',
  'Basher',
  'Miner',
  'Digger',
  'Pause',
  'Nuke');


    procedure ads(const s:string);
    begin
      l.add(s);
    end;

    function bs(b:boolean): string;
    begin
      if b then result := 'yes' else result := 'no';
    end;

    function lz(i,c:integer):string;
    begin
      result:=padl(i2s(i), c, ' ');
//      LeadZeroStr(i,c);
    end;

    function actionstr(af: word): string;
    begin
      {result := '?';
      if raf_StartPause and af <> 0  then
        result := 'Begin Pause'
      else if raf_EndPause and af <> 0  then
        result := 'End Pause'
      else if raf_StartIncreaseRR and af <> 0  then
        result := 'Start RR+'
      else if raf_StartDecreaseRR and af <> 0  then
        result := 'Start RR-'
      else if raf_StopChangingRR and af <> 0  then
        result := 'Stop RR'
      else if raf_SkillSelection and af <> 0  then
        result := 'Select'
      else if raf_SkillAssignment and af <> 0  then
        result := 'Assign'
      else if raf_Nuke and af <> 0  then
        result := 'Nuke';}

      Result := '........';
      if raf_StartPause and af <> 0  then
        result[1] := 'B';
      if raf_EndPause and af <> 0  then
        result[2] := 'E';
      if raf_StartIncreaseRR and af <> 0  then
        result[3] := '+';
      if raf_StartDecreaseRR and af <> 0  then
        result[4] := '-';
      if raf_StopChangingRR and af <> 0  then
        result[5] := '*';
      if raf_SkillSelection and af <> 0  then
        result[6] := 'S';
      if raf_SkillAssignment and af <> 0  then
        result[7] := 'A';
      if raf_Nuke and af <> 0  then
        result[8] := 'N';
    end;


begin
  l:=tstringlist.create;
  m:=fGame.Options;

  ads('Lemmix Replay Textfile recorded with CustLemmix');
  ads('------------------------------------------');
  ads('Title: ' + Trim(fgame.level.info.title));
  ads('Replay fileversion: ' + i2s(LEMMIX_REPLAY_VERSION));
  ads('Number of records: ' + i2s(List.count));
  ads('Pause glitch frames: ' + i2s(fGame.GlitchIterations));
  ads('------------------------------------------');
  ads('');
  ads('Mechanics');
  ads('------------------------------------------');
  ads('Disabled objects after #15          : ' + bs(dgoDisableObjectsAfter15 in m));
  ads('Miner One Way Right Bug             : ' + bs(dgoMinerOneWayRightBug in m));
  ads('Obsolete Mechanic (should be on)    : ' + bs(dgoObsolete in m));
  ads('Splatting Exits Bug                 : ' + bs(dgoSplattingExitsBug in m));
  ads('Old Entrance ABBA Order             : ' + bs(dgoOldEntranceABBAOrder in m));
  ads('Entrance of first lemming at X=25   : ' + bs(dgoEntranceX25 in m));
  ads('Faller Starts With 3 Falling pixels : ' + bs(dgoFallerStartsWith3 in m));
  ads('Max 4 Enabled Entrances             : ' + bs(dgoMax4EnabledEntrances in m));
  ads('Assign Climber Shrugger Action Bug  : ' + bs(dgoAssignClimberShruggerActionBug in m));
  ads('');

  ads(' Rec   Frame  Pausing Action        Skill     Button     RR   lem   x    y   mx   my');
  ads('--------------------------------------------------------------------------------------');

  for i:=0 to list.count-1 do
  begin
    it:=list.list^[i];
    ads(
        lz(i, 4) + '  ' +
        lz(it.iteration, 6) + '  ' +
        padr(bs(it.ActionFlags and raf_pausing <> 0), 7) + ' ' +
        padr(actionstr(it.actionflags), 12) + '  ' +
        padr(skillstrings[it.assignedskill], 8) + '  ' +
        padr(selstrings[it.selectedbutton], 8) + '  ' +
        lz(it.ReleaseRate, 3) + ' ' +
        lz(it.LemmingIndex,4) + ' ' +
        lz(it.lemmingx, 4) + ' ' +
        lz(it.lemmingy, 4) + ' ' +
        lz(it.CursorX, 4) + ' ' +
        lz(it.CursorY, 4)
    );
  end;

  l.savetofile(afilename);
  l.free;

end;

procedure TRecorder.Truncate(aCount: Integer);
begin
  List.Count := aCount;
end;

procedure TRecorder.LoadFromOldTxt(const aFileName: string);
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
  TYP: Word;
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
      TYP:=0;//rtNone;
      SKILL:=baWalking;
      LIX:=0;

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
                TYP := 0//rtNone
              else if comparetext(t, 'raSkillAssignment') = 0 then
                TYP := raf_SkillAssignment// rtAssignSkill
              else if comparetext(t, 'raNuke') = 0 then
                TYP := raf_Nuke //rtNuke
              else if comparetext(t, 'rrNone') = 0 then
                TYP := 0 //rtNone
              else if comparetext(t, 'rrStop') = 0 then
                TYP := raf_StopChangingRR //rtStopChangingRR
              else if comparetext(t, 'rrStartDecrease') = 0 then
                TYP := raf_StartDecreaseRR //rtStartDecreaseRR
              else if comparetext(t, 'rrStartIncrease') = 0 then
                TYP := raf_StartIncreaseRR; //rtStartIncreaseRR;
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

      if (ITER<>-1) and (TYP<>0) then
      begin
        //deb(['item:', iter]);
        It := Add;
         it.Iteration := ITER;
         it.actionflags := TYP;//RecTyp := TYP;
        if Skill > baWalking then
          it.AssignedSkill := Byte(Skill) + 1;
//        It.Skill := SKILL;
        It.LemmingIndex :=LIX;
        It.ReleaseRate  := RR;
      end;



    end;
  finally
    l.free;
  end;


end;


end.


(*


richting = 0 --> pi
snelheid = 10-100 (pixels)
gravitatie = -9.81

dit zijn x en y op een tijdstip t (kunnen negatief worden)
-------
x = x-lemming + snelheid*cos(richting)*t + 0.5*gravitatie*t^2
y = y-lemming + snelheid*sin(richting)*t + 0.5*gravitatie*t^2
--------
of dit is per stapje dt
Vx = snelheid*cos(richting)
Vy = snelheid*sin(richting)

----------
Vx = Vx+gravitatie*dt
Vy = Vy+gravitatie*dt

x = x + Vx*dt
y = y + Vy*dt
--------

En zodra y=0 dan is het stukje geland.


*)


(*
  TParticle = class
  private
  protected
  public
    X: Double;
    Y: Double;
    DX: Double;
    DY: Double;
    Color: TColor32;
    Age: Integer;
  end;

  TParticleList = class(TObjectList)
  private
    function GetItem(Index: Integer): TParticle;
  protected
  public
    function Add(Item: TParticle): Integer;
    procedure Insert(Index: Integer; Item: TParticle);
    property Items[Index: Integer]: TParticle read GetItem; default;
  published
  end;

  { TParticleList }

function TParticleList.Add(Item: TParticle): Integer;
begin
  Result := inherited Add(Item);
end;

function TParticleList.GetItem(Index: Integer): TParticle;
begin
  Result := inherited Get(Index);
end;

procedure TParticleList.Insert(Index: Integer; Item: TParticle);
begin
  inherited Insert(Index, Item);
end;

    procedure CreateParticles(L: TLemming);

    procedure TLemmingGame.CreateParticles(L: TLemming);
var
  i, NumParticles: Integer;
  Particle: TParticle;
begin
exit;
  NumParticles := 12;

  for i := 0 to NumParticles - 1 do
  begin
    Particle := TParticle.Create;
    { TODO : set capacity for speed }
    Particles.Add(Particle);
    with Particle do
    begin
      X := L.LemX + random(20) - random(20);
      Y := L.LemY + random(20) - random(20);
    end;
  end;



  (*
    xExp = x - expImg.getWidth()/2;
    yExp = y - expImg.getHeight()/2;
    maxCounter = 0;
    particles = new Particle[PARTICLE_NUM];
    for (int i=0; i<PARTICLE_NUM; i++) {
            double dx = (Math.random()*(MAX_DX-MIN_DX)+MIN_DX);
            double dy = (Math.random()*(MAX_DY-MIN_DY)+MIN_DY);
            int color = GameController.level.particleCol[(int)(Math.random()*GameController.level.particleCol.length)];
            int lifeCtr = LIFE_COUNTER+(int)(Math.random()*2*LIFE_VARIANCE)-LIFE_VARIANCE;
            if (lifeCtr > maxCounter)
                    maxCounter = lifeCtr;
            particles[i] = new Particle(x,y,dx,dy,color,lifeCtr);
    }
    counter = 0;
    finished = false;
    *)
end;

procedure TLemmingGame.UpdateParticles;
begin
(*
                for (int i=0; i<PARTICLE_NUM; i++) {
                        Particle p = particles[i];
                        if (p != null) {
                                //      calculate new position
                                p.x += p.dx;
                                p.y += p.dy + counter*GRAVITY;
                                // check life counter
                                if (p.lifeCtr >0)
                                        p.lifeCtr--;
                                else
                                        particles[i] = null;
                        }
                }
                if (++counter > maxCounter)
                        finished = true;
*)
end;

procedure TLemmingGame.EraseParticles;
var
  i: Integer;
  P: TParticle;
begin
  with Particles do
    for i := 0 to Count - 1 do
    begin
      P := List^[i];
      fTargetBitmap.PixelS[Trunc(P.X), Trunc(P.Y)] := World.Pixels[Trunc(P.X), Trunc(P.Y)];
    end;
end;

procedure TLemmingGame.DrawParticles;
var
  i: Integer;
  P: TParticle;
begin
  with Particles do
    for i := 0 to Count - 1 do
    begin
      P := List^[i];
      fTargetBitmap.PixelS[Trunc(P.X), Trunc(P.Y)] := clYellow32;
    end;
end;

(*
    richting = 0 --> pi
    snelheid = 10-100 (pixels)
    gravitatie = -9.81

    dit zijn x en y op een tijdstip t (kunnen negatief worden)
    -------
    x = x-lemming + snelheid*cos(richting)*t + 0.5*gravitatie*t^2
    y = y-lemming + snelheid*sin(richting)*t + 0.5*gravitatie*t^2
    --------
    of dit is per stapje dt
    Vx = snelheid*cos(richting)
    Vy = snelheid*sin(richting)

    ----------
    Vx = Vx+gravitatie*dt
    Vy = Vy+gravitatie*dt

    x = x + Vx*dt
    y = y + Vy*dt
    --------

    En zodra y=0 dan is het stukje geland.
*)

