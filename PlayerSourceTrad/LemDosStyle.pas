{$include lem_directives.inc}
unit LemDosStyle;

interface

uses
  Classes,
  UMisc,
  GR32,
  LemTypes, LemLevel, LemLVLLoader, LemGraphicSet, LemDosGraphicSet,
  LemMetaAnimation, LemAnimationSet, LemDosCmp, LemDosStructures, LemDosAnimationSet,
  LemStyle, LemLevelSystem, LemMusicSystem;

const
  ORIG_LEMMINGS_RANDSEED = 1;
  OHNO_LEMMINGS_RANDSEED = 2;
  H94_LEMMINGS_RANDSEED  = 3;
  XMAS_LEMMINGS_RANDSEED = 4;
  COVOX_LEMMINGS_RANDSEED = 5;
  CUST_LEMMINGS_RANDSEED = 6;
  PRIMA_LEMMINGS_RANDSEED = 7;
  EXTRA_LEMMINGS_RANDSEED = 8;

  //@styledef
  LEMMINGS_RANDSEED =
  {$ifndef flexi}
  {$ifdef orig} ORIG_LEMMINGS_RANDSEED; {$endif}
  {$ifdef ohno} OHNO_LEMMINGS_RANDSEED; {$endif}
  {$ifdef h94} H94_LEMMINGS_RANDSEED; {$endif}
  {$ifdef xmas} XMAS_LEMMINGS_RANDSEED; {$endif}
  {$ifdef covox} COVOX_LEMMINGS_RANDSEED; {$endif}
  {$ifdef prima} PRIMA_LEMMINGS_RANDSEED; {$endif}
  {$ifdef extra} EXTRA_LEMMINGS_RANDSEED; {$endif}
  {$ifdef cust} CUST_LEMMINGS_RANDSEED; {$endif}
  {$else}0;{$endif}


  { TODO : wow this is hard coded }
const
  DosMiniMapCorners: TRect = (
    Left: 208;   // width =about 100
    Top: 18;
    Right: 311;  // height =about 20
    Bottom: 37
  );

  // to draw
  DosMiniMapBounds: TRect = (
    Left: 208;   // width =about 100
    Top: 18;
    Right: 311 + 1;  // height =about 20
    Bottom: 37 + 1
  );

const
  DOS_MINIMAP_WIDTH  = 104;
  DOS_MINIMAP_HEIGHT = 20;

type
  TBaseDosLemmingStyle = class(TBaseLemmingStyle)
  private
    fOddTableFile: string;
    fMainDataFile: string;
    function GetAnimationSet: TBaseDosAnimationSet;
  protected
    function DoCreateAnimationSet: TBaseAnimationSet; override;
  public
    function CreateGraphicSet: TBaseGraphicSet; override;
    property AnimationSet: TBaseDosAnimationSet read GetAnimationSet; // get it typed
  published
    property OddTableFile: string read fOddTableFile write fOddTableFile; // only used in dos orig but for less code put here
    property MainDataFile: string read fMainDataFile write fMainDataFile; // default main.dat
  end;

  TDosOrigStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosOhNoStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosHoliday94Style = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosXmasStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosCovoxStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosPrimaStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosExtraStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  TDosCustStyle = class(TBaseDosLemmingStyle)
  protected
    function DoCreateLevelSystem: TBaseLevelSystem; override;
    function DoCreateMusicSystem: TBaseMusicSystem; override;
  end;

  {-------------------------------------------------------------------------------
    Basic levelloadingsystem for dos.
    We virtualized it until this class was able to load all levelinfo's for
    all dos styles, overriding only 3 methods.
  -------------------------------------------------------------------------------}
  TBaseDosLevelSystem = class(TBaseLevelSystem)
  protected
    //fDefaultSectionCount: Integer; // initialized at creation
    fDefaultLevelCount: Integer;
    fLookForLVL: Boolean; // looks for user-overridden lvl-files on disk
    fLevelPack: String;
  { overridden from base loader }
    procedure InternalLoadLevel(aInfo: TLevelInfo; aLevel: TLevel; OddLoad: Boolean = false); override;
    procedure InternalLoadSingleLevel(aPack, aSection, aLevelIndex: Integer; aLevel: TLevel; OddLoad: Boolean = false); override;
    procedure InternalPrepare; override;
    function EasyGetSectionName(aSection: Integer): string;
  private
    //procedure InternalLoadLevel(aInfo: TLevelInfo; aLevel: TLevel); // initialized at creation
  public
    fDefaultSectionCount: Integer;
    {$ifdef flexi}SysDat : TSysDatRec;{$endif}
    {$ifdef testmode}
    fTestMode: Boolean;
    fTestLevel: String;
    {$endif}
    constructor Create(aOwner: TPersistent);
  { these methods must be overridden by derived dos loaders }
    procedure DumpAllLevels;
    procedure GetSections(aSectionNames: TStrings); virtual;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); virtual;
    function GetLevelCount(aSection: Integer): Integer; virtual;
    function GetSectionCount: Integer; virtual;

    //For the time being it is not needed to virtualize this into a higher class.
    function FindFirstLevel(var Rec: TDosGamePlayInfoRec): Boolean; override;
    function FindNextLevel(var Rec : TDosGamePlayInfoRec): Boolean; override;
    function FindLevel(var Rec : TDosGamePlayInfoRec): Boolean; override;

    function GetLevelCode(const Rec : TDosGamePlayInfoRec): string; override;
    function FindLevelCode(const aCode: string; var Rec : TDosGamePlayInfoRec): Boolean; override;
    function FindCheatCode(const aCode: string; var Rec : TDosGamePlayInfoRec): Boolean; override;

    property LookForLVL: Boolean read fLookForLVL write fLookForLVL;
    property LevelPack: String read fLevelPack write fLevelPack;
  end;

  TDosOrigLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
//    function GetLevelCode(const Rec : TDosGamePlayInfoRec): string; override;
    { TODO : make this FindLevelCode general }
//    function FindLevelCode(const aCode: string; var Rec : TDosGamePlayInfoRec): Boolean; override;
  end;

  TDosOhNoLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
//    function GetLevelCode(const Rec : TDosGamePlayInfoRec): string; override;
//    function FindLevelCode(const aCode: string; var Rec : TDosGamePlayInfoRec): Boolean; override;
  end;


  TDosHoliday94LevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
  end;

  TDosXmasLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
  end;

  TDosCovoxLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
  end;

  TDosPrimaLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
  end;

  TDosExtraLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
  end;

  TDosCustLevelSystem = class(TBaseDosLevelSystem)
  public
    procedure GetSections(aSectionNames: TStrings); override;
    procedure GetEntry(aSection, aLevel: Integer; var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer); override;
    function GetLevelCount(aSection: Integer): Integer; override;
    function GetRankName(aSection: Byte): String;
//    function GetLevelCode(const Rec : TDosGamePlayInfoRec): string; override;
//    function FindLevelCode(const aCode: string; var Rec : TDosGamePlayInfoRec): Boolean; override;
  end;

  TDosOrigMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosOhNoMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosHoliday94MusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosXmasMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosCovoxMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosPrimaMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosExtraMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

  TDosCustMusicSystem = class(TBaseMusicSystem)
  private
  protected
  public
    MusicCount : Byte;
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; override;
  end;

function GenCode(aRandseed, aSection, aLevel: Integer): string;

function AutoCreateStyle(const aDirectory: string{$ifdef flexi}; aSysDat: TSysDatRec{$endif}): TBaseDosLemmingStyle;
function CreateDosOrigStyle(const aDirectory: string): TDosOrigStyle;
function CreateDosOhnoStyle(const aDirectory: string): TDosOhNoStyle;
function CreateDosCustStyle(const aDirectory: string{$ifdef flexi}; aSysDat: TSysDatRec{$endif}): TDosCustStyle;

implementation

uses
  SysUtils;

(* DOS table for loading levels in order see naamloos.txt or lemmings forum

There is an indirection table for levels. So when on level (say 4), it might tell you to load level 15 instead.

Below is the table I have. The numbers have to be divided by 2. If the bottom bit is set,
then its a duplicate, and it gets the new skills from an "odd table", which is loaded in from "ODDTABLE.DAT".

These are levels that have new skills to make them easier.
So if the indirection table has the bottom bit set, its a duplicate level,
with overidden skills. The PD version, doesnt use an indrection table, and while
the data disk does(Oh NO!), it doesnt use the odd table.

; Fun Levels
      db      147,155,157,149,151,153,159,14,22,54
      db      70,16,29,32,38,42,48,72,84,104
      db      138,23,68,96,98,116,78,100,108,134

; Tricky Levels
      db      1,30,36,50,52,56,58,80,102,120
      db      128,130,136,5,148,152,154,156,160,7
      db      11,13,15,17,19,21,25,27,33,31

; Taxing Levels
      db      37,39,41,43,45,47,49,51,53,55
      db      57,59,61,63,3,65,67,69,71,73
      db      75,77,79,81,83,85,87,89,35,111

; Mayhem Levels
      db      91,93,95,97,99,101,103,105,107,109
      db      112,113,115,117,119,121,123,125,127,150
      db      129,9,131,133,135,137,139,141,143,145
*)
// onebased entries
// fun 8 = entry 7 oddtable 7 (one based)
// fun 1 = 147/2 = 73 (0 based level009.dat)
// fun 2 = 155/2 = 77

type
  TDosOrigLevelOrderTable = array[1..4, 1..30] of Byte;  {fun..mayhem, 1..30} // encoded indices
  TDosOhNoLevelOrderTable = array[1..5, 1..20] of Byte;  {tame..havoc, 1..20} // raw order

const
  DosOrigSectionNames: array[1..4] of string = ('fun', 'tricky', 'taxing', 'mayhem');

  SectionTable: TDosOrigLevelOrderTable = (
    (
       147, 155, 157, 149, 151, 153, 159,  14,  22,  54,
        70,  16,  29,  32,  38,  42,  48,  72,  84, 104,
       138,  23,  68,  96,  98, 116,  78, 100, 108, 134
    ),
    (
         1,  30,  36,  50,  52,  56,  58,  80, 102, 120,
       128, 130, 136,   5, 148, 152, 154, 156, 160,   7,
        11,  13,  15,  17,  19,  21,  25,  27,  33,  31
    ),
    (
       37, 39, 41, 43, 45, 47, 49, 51, 53, 55,
       57, 59, 61, 63,  3, 65, 67, 69, 71, 73,
       75, 77, 79, 81, 83, 85, 87, 89, 35,111
    ),
    (
        91,  93,  95,  97,  99, 101, 103, 105, 107, 109,
       112, 113, 115, 117, 119, 121, 123, 125, 127, 150,
       129,   9, 131, 133, 135, 137, 139, 141, 143, 145
    )
  );

  {-------------------------------------------------------------------------------
    My own little system for OhNo: div 10 = file, mod 10 is index.
    So 117 means file 11, index 7.
    How did I find out this ??? I think I just compared and compared.
    Ohno original does about the same trick based on div 8 I think.
  -------------------------------------------------------------------------------}
  OhNoTable: TDosOhNoLevelOrderTable = (
    // tame 1..8     : 10.0, 10.1, 10.2, 10.3, 10.4, 10.5, 10.6, 10.7
    // tame 9..16    : 11.0, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7
    // tame 17..20   : 12.0, 12.1, 12.2, 12.3 (missing in windows)
    (
       100, 101, 102, 103, 104, 105, 106, 107,
       110, 111, 112, 113, 114, 115, 116, 117,
       120, 121, 122, 123
    ),

    // crazy 1..8    : 0.1, 1.0, 1.4, 2.0, 2.1, 3.0, 3.1, 3.5 (repaired typo crazy 1: 0.0 to the correct 0.1)
    // crazy 9..16   : 5.1, 5.4, 5.7, 6.7, 0.4, 1.5, 1.6, 2.6
    // crazy 17..20  : 3.4, 3.7, 4.3, 6,4
    (
         1,  10,  14,  20,  21,  30,  31, 35,
        51,  54,  57,  67,   4,  15,  16, 26,
        34,  37,  43,  64
    ),

    // wild 1..8     : 9.2, 7.0, 7.1, 7.2, 7.3, 7.5, 8.6, 0.7
    // wild 9..16    : 2.2, 2.5, 3.3, 3.6, 4.0, 4.2, 4.4, 5.0 (repaired typo wild 15: 4.5 to the correct 4.4)
    // wild 17..20   : 6.3, 6.5, 6.6, 4.7
    (
      92,70,71,72,73,75,86,7,
      22,25,33,36,40,42,44,50,
      63,65,66,47
    ),

    // wicked 1..8   : 9.6, 4.6, 9.0, 9.1, 0.5, 9.4, 6.1, 0.6
    // wicked 9..16  : 1.2, 8.7, 4.1, 5.5, 6.0, 6.2, 7.7, 8.1,
    // wicked 17..20 : 9.7, 9.3, 8.0, 7.6
    (
      96,46,90,91,5,94,61,6,
      12,87,41,55,60,62,77,81,
      97,93,80,76
    ),

    // havoc 1..8    : 7.4, 5.3, 3.2, 2.7, 2.4, 2.3, 5.2, 1.7
    // havoc 9..16   : 1.1, 0.3, 0.2, 0.0, 4.5, 8.5, 1.3, 8.2
    // havoc 17..20  : 8.3, 5.6, 8.4, 9.5
    (
      74,53,32,27,24,23,52,17,
      11,3,2,0,45,85,13,82,
      83,56,84,95
    )
  );

  // the following codes are randomly generated by me
const
  DosOrigLevelCodes: array[0..3, 0..29] of string =
  (
    (
    'AAWFHRIEJL', 'CMBVBHXJUI', 'SVSHEIMGVH', 'MDWHUZMXVA', 'DDNAPAUQUS',
    'OFRPYQZGRH', 'CUMWNOYRIA', 'SZUTFDXUPW', 'RHCQHWEHOY', 'EEGEOMHEPT',
    'TCBBCUUJPL', 'TMIVVDCBBF', 'RFOGJVIIRR', 'XIMHTTOSKO', 'PWRCMKCBOR',
    'CXVRYHSTMR', 'EFYJDZHBCW', 'OSCRAIXNIL', 'XJJMBWTEXM', 'DXKFUPCQYO',
    'ILFERZQROC', 'TDGWSLZEBE', 'AJKJCLNLRV', 'OGJPWPGSOW', 'JJQULFSVTX',
    'EUGHSLZEAF', 'XIWTZHKVAG', 'BNKADSIKOG', 'DHBSIUROXA', 'GXRAEEKKDA'
    ),
    (
    'NTOGWBMBJR', 'FUNKBZEBBC', 'UYCFTZMAPU', 'XNRZTSXZTU', 'AJIBUGWXBX',
    'YMRJKTSIQN', 'ZCZAQJNQVQ', 'YMMZGFLRTA', 'PUMEQBZUWL', 'AFPWBMYWWS',
    'ZFERVEAVOM', 'LDUWCPUFMK', 'NSUIAPHBMF', 'CRWIHMXSKC', 'VGAJXWLGKJ',
    'QLQTVLNQIG', 'KSREYAIXMN', 'WRKPDZMDIP', 'VMDMDIRVWE', 'ETHIGBXRCR',
    'AQPUEXVQMU', 'TEJXNWSHPC', 'IJKOXRTZJC', 'KHXGKCNNNF', 'FKZNPJJUXR',
    'ONYFAZMZFV', 'CLBZJSWVKR', 'HWEJKCZAEC', 'BQHUDSKMNC', 'YIYHUWKNHS'
    ),
    (
    'ILLZJPSMEI', 'KMJMBEZOLX', 'VPKHVPHQWG', 'QBMVWFZRSG', 'XGMJVPAKFM',
    'NGOPIOTSEY', 'LSOONTXOPV', 'LAYATXGQYU', 'BCJWFYPZUK', 'AAPUMNTYHE',
    'DZDIEKMCJH', 'HBHVGJPBGK', 'BTKASNLSAC', 'GNUQGVMLBR', 'RLGPJCWPVA',
    'RRWJVUXCGN', 'TULKEXMDYQ', 'PCLASDFCJQ', 'LOHSKCSZFU', 'LSZLATCPPX',
    'PUIQWHGBJA', 'IXEBWJEGMX', 'KDDQHKOWVE', 'TTREJHYKFG', 'AWVAENHQOO',
    'MUESGEWXCG', 'XRDKLLUXKX', 'OAWPKYXPQK', 'DCZNZHKRMB', 'CTDWFOEPQY'
    ),
    (
    'ZGAGFZYVDN', 'HPNIIUCRXB', 'KXMKUCRWHD', 'XVCGYKWUYC', 'AHSGKCRARZ',
    'SSOIROJIKC', 'HUNOARNIHO', 'FNXLQHOSFM', 'RIPNLDNUSF', 'LZTVKAYOMM',
    'OVCBQEASXV', 'BDWAYACZHY', 'ABBGHUKVTZ', 'JUFYJYHJYO', 'GKHSWNWBFM',
    'TETTURRRHI', 'GGKSLFGRGN', 'TVTZXTELEE', 'YBDXVXGCRW', 'VSATNHATKA',
    'ISZOJHYNZM', 'KIDCASIQJG', 'DDSXOBIYHQ', 'ZBPTKWIPHB', 'LNTACWPPVW',
    'EYCQQNBVDS', 'EUPCKBXTXM', 'VELOFGWLKR', 'BBBCMYEBRY', 'ACCCGRORQJ'
    )
  );

  // the following codes are randomly generated by me
  DosOhNoLevelCodes: array[0..4, 0..19] of string =
  (
    (
    'STSNZYUSZH', 'KVFLBJEMSC', 'NUMGUBQDDU', 'HCWYUNZZBW', 'UYGVNJFOYQ',
    'GJIKJVJHRM', 'OAKYVCKECN', 'QLCWMYNAJH', 'UOWHABSVFF', 'KOCMUSTJKV',
    'UEEELFTUHV', 'TCYHLRQARI', 'SMJIADXCQC', 'EICHWBTVZA', 'WRODQXQWSM',
    'VTTNGRSGCJ', 'GSZYPHFKEB', 'DGMUYNUTSS', 'IJHRKIYNSO', 'UUIXSQMKHR' ),
    (
    'IBOFJVXZGF', 'ROHSNBDNPU', 'LDFFGIZRMS', 'OTZOROVWOC', 'RXEIVGMCSJ',
    'MLQMSJJYAA', 'XFZQLVDAHG', 'VDMLRKUTPF', 'HWELYLLRVS', 'XCRWYREYWI',
    'GVSHYTHRQU', 'TCHNITDGPJ', 'OVITHLGMAN', 'WEHVBHHVUQ', 'LWGULPXJBP',
    'HKJQRLUQGX', 'EVLJAUUVJS', 'RDSIWNXXBI', 'RMPOVNCEVZ', 'RBDUCQETPG'
    ),
    (
    'ZUFSSMUTAY', 'BRZJOIKCKJ', 'QZTJUZMUOB', 'ISTJIIELUH', 'QWLJBCDZKC',
    'DBZZCLGONQ', 'BNRDDAPLKI', 'XETKAOCVHP', 'WKLRGGVGBL', 'GYPFGEJMJC',
    'JAUSUAQGIE', 'WZUZIGKWMY', 'ZCZPGTRHNO', 'RUNJLMNSNP', 'CESRWBKJYY',
    'EAEPFKCBPR', 'OEGOXDJIAG', 'YQTDRARVAA', 'AIGEESQGJL', 'LJENOAFXOI'
    ),
    (
    'PWNOVXLFRV', 'ETHVSKSXEA', 'HRSAAVRERD', 'IWBQYLVQTA', 'COPSAFYYZE',
    'YPFUVELLMP', 'BIOOKDFMLV', 'FCZTUMCASI', 'KFEXPJFCRY', 'MMURPIAPMU',
    'JMMXFGWDWT', 'IIUJLWXFEV', 'UAZIUJDJQY', 'CQKXCOOCSH', 'JTOZHPLJTR',
    'YERLYJPNLA', 'FSQDJWOAOJ', 'VCFINQHMCL', 'UKVSAEYEJB', 'EBFCWNHLIZ'
    ),
    (
    'PJMGMOXVOH', 'NASMATLIKZ', 'UVLLGWSBDD', 'VYWNQXZPVL', 'GYBDKJQKFD',
    'JKCPUJJNKZ', 'AXLBKJZSZH', 'TPUXESTVTO', 'QZPRDYGIXE', 'IOEZNKCQWX',
    'JMUQXLRWRF', 'MVRJNMKLID', 'ONAAUBHSPC', 'JEQWHFSGKN', 'GBTOEZKCVM',
    'XTHZHSGFZM', 'LVFUVDFFQW', 'PVPKPZWPAF', 'BILMLOCYZX', 'VBAWXNNSYW'
    )
  );

{-------------------------------------------------------------------------------
  BEGIN Random code gen.
  Original from Borland Delphi 5 system._randint.
-------------------------------------------------------------------------------}

var
  LemRandSeed: Integer = 0;

function LemRandom(Range: Integer): Integer;
asm
{     ->EAX     Range   }
{     <-EAX     Result  }
        IMUL    EDX, LemRandSeed, 08088405H
        INC     EDX
        MOV     LemRandSeed, EDX
        MUL     EDX
        MOV     EAX, EDX
end;

function LemRandomPAS(Range: Integer): Integer;
var
  D: Integer;
begin
  D := LemRandSeed * $08088405 + 1;
  LemRandSeed := D;


  //      IMUL    EDX, LemRandSeed, 08088405H
    //    INC     EDX
      //  MOV     LemRandSeed, EDX
        //MUL     EDX
//        MOV     EAX, EDX
end;

(*
function ChRandom64: U64;
var
  A, B: Integer;
type
  T64 = record
    IntA: Integer;
    IntB: Integer;
  end;
begin
  A := ChRandom(MaxInt);
  B := ChRandom(MaxInt);
  T64(Result).IntA := A;
  T64(Result).IntB := B;
end; *)

(*
procedure LemRandomize;
var
  systemTime :
  record
          wYear   : Word;
          wMonth  : Word;
          wDayOfWeek      : Word;
          wDay    : Word;
          wHour   : Word;
          wMinute : Word;
          wSecond : Word;
          wMilliSeconds: Word;
          reserved        : array [0..7] of char;
  end;
asm
        LEA     EAX,systemTime
        PUSH    EAX
        CALL    GetSystemTime
        MOVZX   EAX,systemTime.wHour
        IMUL    EAX,60
        ADD     AX,systemTime.wMinute   { sum = hours * 60 + minutes    }
        IMUL    EAX,60
        XOR     EDX,EDX
        MOV     DX,systemTime.wSecond
        ADD     EAX,EDX                 { sum = sum * 60 + seconds              }
        IMUL    EAX,1000
        MOV     DX,systemTime.wMilliSeconds
        ADD     EAX,EDX                 { sum = sum * 1000 + milliseconds       }
        MOV     ChRandSeed,EAX
end;
*)

{-------------------------------------------------------------------------------
  END random gen
-------------------------------------------------------------------------------}

function GenCode(aRandseed, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  generate access code for a lemming system. every lemming system should have
  its own randseed.
  aRandseed should be a low positive number
-------------------------------------------------------------------------------}
//var
  //r, i: Integer;
//  c: Char;
  (*
begin
  LemRandseed := aRandseed * 1000 + aSection * 100 + aLevel * 10;

  SetLength(Result, 10);
  for i := 1 to 10 do
  begin
    r := LemRandom(26);
    c := Chr(r + ord('A'));
    Result[i] := c;
  end;
*)

{-------------------------------------------------------------------------------
  generates random codes with alternating nouns non-nouns
-------------------------------------------------------------------------------}
const
  klinkers: array[0..4] of char = ('A','E','I','O','U');
  // Q is omitted on purpose
  medeklinkers: array[0..19] of char = ('B','C','D','F','G','H','J','K','L','M','N','P',{'Q',} 'R',
   'S','T','V','W','X','Y','Z');

  function RndChar(aMedeklinker: Boolean): Char;
  begin
    if aMedeklinker then
      Result := Medeklinkers[LemRandom(20)]
    else
      Result := Klinkers[LemRandom(5)];
  end;

var
  //L: TStringList;
  //Sec, Lev,
  i: Integer;
  r: Integer;
  c : Char;
//  S: string;
  DoMedeKlinker: Boolean;
begin
  // never change this
  LemRandseed := aRandseed * 10000 + (aSection + 1) * 100 + (aLevel + 1);

//  randseed := -1207816797; // so we do not need consts
  Result := StringOfChar(' ', 10);
  (*
  L := TStringlist.Create;
  L.Add('// randseed = ' + i2s(randseed));
  L.Add('var');
  L.Add(ArrayName + ': array[0..3, 0..29] of string = ');
  L.Add('  (');
  for Sec := 1 to SectionCount do
  begin
    L.Add ('    (');
    for Lev := 1 to LevelCount do
    begin
      DoMedeKlinker := Boolean(Random(2)); // init on random
      for i := 1 to 10 do
      begin
        r := Random(26);
        //c := Chr(r + ord('A'));
        C := RndChar(DoMedeKlinker);
        DoMedeKlinker := not DoMedeKlinker;
        S[i] := c;
      end;
      L.Add('    ' + '''' + S + '''' + IIF(Lev < LevelCount, ',', ''));
    end;
    L.Add ('    )' + IIF(Sec < SectionCount, ',', ''));
  end;
  L.Add('  );');

  L.SaveToFile('codes.txt');
  L.Free;
  *)

  DoMedeKlinker := Boolean(LemRandom(2)); // init on random
  for i := 1 to 10 do
  begin
    r := LemRandom(26);
    //c := Chr(r + ord('A'));
    C := RndChar(DoMedeKlinker);
    DoMedeKlinker := not DoMedeKlinker;
    Result[i] := c;
  end;


  //........
end;

function CreateDosOrigStyle(const aDirectory: string): TDosOrigStyle;
{var
  s,l:integer;
  sl:tstringlist;}
begin
  Result := TDosOrigStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.OddTableFile := Result.CommonPath + 'oddtable.dat';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;

  {

  sl:=tstringlist.create;
  for s := 0 to 4 do
    for l := 0 to 19 do
    begin
      sl.add(LeadZeroStr(s,3) + ',' + LeadZeroStr(l,3) + ': ' + gencode(LEMMINGS_RANDSEED, s, l));
    end;
  sl.savetofile('testcodes.txt');
  sl.free;}
end;

function CreateDosOhnoStyle(const aDirectory: string): TDosOhNoStyle;
{
var
  s,l:integer;
  sl:tstringlist;}
begin
  Result := TDosOhNoStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
  {
  sl:=tstringlist.create;
  for s := 0 to 4 do
    for l := 0 to 19 do
    begin
      sl.add(LeadZeroStr(s,3) + ',' + LeadZeroStr(l,3) + ': ' + gencode(LEMMINGS_RANDSEED, s, l));
    end;
  sl.savetofile('testcodes.txt');
  sl.free;
  }
end;

function CreateDosCustStyle(const aDirectory: string{$ifdef flexi}; aSysDat: TSysDatRec{$endif}): TDosCustStyle;
{
var
  s,l:integer;
  sl:tstringlist;}
begin
  Result := TDosCustStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
  {$ifdef flexi}Result.SysDat := aSysDat;{$endif}
  {
  sl:=tstringlist.create;
  for s := 0 to 4 do
    for l := 0 to 19 do
    begin
      sl.add(LeadZeroStr(s,3) + ',' + LeadZeroStr(l,3) + ': ' + gencode(LEMMINGS_RANDSEED, s, l));
    end;
  sl.savetofile('testcodes.txt');
  sl.free;
  }
end;

function CreateDosHoliday94Style(const aDirectory: string): TDosHoliday94Style;
begin
  Result := TDosHoliday94Style.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.OddTableFile := '';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
end;

function CreateDosXmasStyle(const aDirectory: string): TDosXmasStyle;
begin
  Result := TDosXmasStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.OddTableFile := '';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
end;

function CreateDosCovoxStyle(const aDirectory: string): TDosCovoxStyle;
begin
  Result := TDosCovoxStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.OddTableFile := '';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
end;

function CreateDosPrimaStyle(const aDirectory: string): TDosPrimaStyle;
begin
  Result := TDosPrimaStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.OddTableFile := '';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
end;

function CreateDosExtraStyle(const aDirectory: string): TDosExtraStyle;
begin
  Result := TDosExtraStyle.Create;
  Result.CommonPath := IncludeTrailingBackslash(aDirectory);
  Result.MainDataFile := Result.CommonPath + 'main.dat';
  Result.OddTableFile := '';
  Result.AnimationSet.MainDataFile := Result.CommonPath + 'main.dat';
  Result.AnimationSet.ReadMetaData;
end;

function AutoCreateStyle(const aDirectory: string{$ifdef flexi}; aSysDat: TSysDatRec{$endif}): TBaseDosLemmingStyle;
begin
//@styledef
{$ifdef orig}
  Result := CreateDosOrigStyle(aDirectory);
{$endif}
{$ifdef ohno}
  Result := CreateDosOhNoStyle(aDirectory);
{$endif}
{$ifdef h94}
  Result := CreateDosHoliday94Style(aDirectory);
{$endif}
{$ifdef xmas}
  Result := CreateDosXmasStyle(aDirectory);
{$endif}
{$ifdef covox}
  Result := CreateDosCovoxStyle(aDirectory);
{$endif}
{$ifdef prima}
  Result := CreateDosPrimaStyle(aDirectory);
{$endif}
{$ifdef extra}
  Result := CreateDosExtraStyle(aDirectory);
{$endif}
{$ifdef cust}
  Result := CreateDosCustStyle(aDirectory{$ifdef flexi}, aSysDat{$endif});
{$endif}
  Assert(Result <> nil);
end;

{ TBaseDosLemmingStyle }

function TBaseDosLemmingStyle.CreateGraphicSet: TBaseGraphicSet;
begin
  Result := TBaseDosGraphicSet.Create;
end;

function TBaseDosLemmingStyle.DoCreateAnimationSet: TBaseAnimationSet;
begin
  Result := TBaseDosAnimationSet.Create;
end;

function TBaseDosLemmingStyle.GetAnimationSet: TBaseDosAnimationSet;
begin
  Result := TBaseDosAnimationSet(fAnimationSet);
end;

{ TDosOrigStyle }

function TDosOrigStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosOrigLevelSystem.Create(Self);
end;


function TDosOrigStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosOrigMusicSystem.Create;
end;

{ TDosOhNoStyle }

function TDosOhNoStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosOhNoLevelSystem.Create(Self);
end;

function TDosOhNoStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosOhNoMusicSystem.Create;
end;

{ TDosHoliday94Style }

function TDosHoliday94Style.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosHoliday94LevelSystem.Create(Self);
end;

function TDosHoliday94Style.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosHoliday94MusicSystem.Create;
end;


{ TDosXmasStyle }

function TDosXmasStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosXmasLevelSystem.Create(Self);
end;

function TDosXmasStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosXmasMusicSystem.Create;
end;

{ TDosCovoxStyle }

function TDosCovoxStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosCovoxLevelSystem.Create(Self);
end;

function TDosCovoxStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosCovoxMusicSystem.Create;
end;

{ TDosPrimaStyle }

function TDosPrimaStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosPrimaLevelSystem.Create(Self);
end;

function TDosPrimaStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosPrimaMusicSystem.Create;
end;

{ TDosExtraStyle }

function TDosExtraStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosExtraLevelSystem.Create(Self);
end;

function TDosExtraStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosExtraMusicSystem.Create;
end;

{ TDosCustStyle }

function TDosCustStyle.DoCreateLevelSystem: TBaseLevelSystem;
begin
  Result := TDosCustLevelSystem.Create(Self);
end;

function TDosCustStyle.DoCreateMusicSystem: TBaseMusicSystem;
begin
  Result := TDosCustMusicSystem.Create;
end;

{ TBaseDosLevelSystem }

constructor TBaseDosLevelSystem.Create(aOwner: TPersistent);
begin
  inherited;
  fDefaultSectionCount := GetSectionCount;
  fDefaultLevelCount := GetLevelCount(0);
end;

function TBaseDosLevelSystem.EasyGetSectionName(aSection: Integer): string;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    GetSections(L);
    Result := L[aSection];
  finally
    L.Free;
  end;
end;

function TBaseDosLevelSystem.FindCheatCode(const aCode: string;
  var Rec: TDosGamePlayInfoRec): Boolean;
var
  Sec, Lev: Integer;
  P, i, L: Integer;
  Comp, Comp2: string;
  List: TStringList;
begin
  // cheat: "fun19" works too
  Comp := LowerCase(StringReplace(aCode, '.', '', [rfReplaceAll]));
  List := TStringList.Create;
  try
  GetSections(List);
  for i := 0 to List.Count - 1 do
  begin
    P := pos(lowercase(list[i]), Comp);
    if P <> 1 then
      begin
      Comp2 := Comp;
      P := pos(LeadZeroStr((i+1), 2), Comp);
      Comp := lowercase(list[i]) + copy(Comp, 3, 3);
      end;

    if P = 1 then
    begin
      L := StrToIntDef(copy(Comp, Length(List[i]) + 1, 2), 0);

      if L >= 1 then
      if L <= GetLevelCount(i) then
      begin
        with Rec do
        begin
          dValid := True;
          dPack := 0;
          dSection := i;
          dLevel := L - 1;
          dSectionName := List[i]
        end;
        Result := True;
        Exit;
      end;
    end else
      Comp := Comp2;
  end;
  finally
    List.Free;
  end;

end;

function TBaseDosLevelSystem.FindFirstLevel(var Rec: TDosGamePlayInfoRec): Boolean;
var
  L: TStringList;
begin
  Result := True;
  L := TStringList.Create;
  try
    GetSections(L);
    with Rec do
    begin
      dValid         := True;
      dPack          := 0;
      //if dSection >= 0
      //dSection       := 0;
      dLevel         := 0;
      dSectionName   := L[dSection]; //#EL watch out for the record-string-mem-leak
    end;
  finally
    L.Free;
  end;
end;

function TBaseDosLevelSystem.FindLevel(var Rec: TDosGamePlayInfoRec): Boolean;
begin
  with Rec do
  begin
    Result := (dPack = 0) and (dSection >= 0) and (dSection < fDefaultSectionCount) and
    (dLevel >= 0) and (dLevel < GetLevelCount(dSection));
    dValid := Result;
    if not Result then
      Exit;
    dSectionName := EasyGetSectionName(dSection);
  end;
end;

function TBaseDosLevelSystem.FindLevelCode(const aCode: string; var Rec: TDosGamePlayInfoRec): Boolean;
var
  Sec, Lev: Integer;
  P, i, L: Integer;
  Code: string;
  List: TStringList;
begin
  Result := False;

  if Length(aCode) <> 10 then
    Exit;

  for Sec := 0 to 4 do
    for Lev := 0 to 29 do
    begin
      {$ifndef flexi}Code := GenCode(LEMMINGS_RANDSEED, Sec, Lev);{$endif}
      {$ifdef flexi}Code := GenCode(SysDat.CodeSeed, Sec, Lev);{$endif}

      if CompareText(Code, aCode) = 0 then
      begin
        Result := True;
        Rec.dValid := True;
        Rec.dPack := 0;
        Rec.dSection := Sec;
        Rec.dLevel := Lev;
        Rec.dSectionName := EasyGetSectionName(Sec);
        Exit;
      end;
    end;
end;

function TBaseDosLevelSystem.FindNextLevel(var Rec: TDosGamePlayInfoRec): Boolean;
var
  L: TStringList;
begin
  {Result := (Rec.dLevel < GetLevelCount(Rec.dSection) - 1) or (Rec.dSection < fDefaultSectionCount - 1);

  Rec.dValid := False;
  if not Result then
    Exit;}

  L := TStringList.Create;
  try
    GetSections(L);
    with Rec do
    begin
      dValid         := True;
      dPack          := 0;

      Inc(dLevel); // this can lead to a overflow so...
      if dLevel >= GetLevelCount(dSection)  then
      begin
        dLevel := 0;
        Inc(dSection); // this can lead to a overflow so...
        if dSection >= GetSectionCount then
          dSection := 0;
      end;
      dSectionName   := L[dSection];
      //#EL watch out for the record-string-mem-leak
      Result := (dLevel <> 0) or (dSection <> 0);
    end;
  finally
    L.Free;
  end;
end;

procedure TBaseDosLevelSystem.GetEntry(aSection, aLevel: Integer; var aFileName: string;
  var aFileIndex: Integer; var IsOddTable: Boolean; var aOddIndex: Integer);
{-------------------------------------------------------------------------------
  This method must return information on where to get a level from
-------------------------------------------------------------------------------}
begin
  raise Exception.Create(ClassName + '.GetEntry is abstract');
end;

function TBaseDosLevelSystem.GetLevelCode(const Rec: TDosGamePlayInfoRec): string;
begin
  {$ifndef flexi}Result := GenCode(LEMMINGS_RANDSEED, Rec.dSection, Rec.dLevel);{$endif}
  {$ifdef flexi}Result := GenCode(SysDat.CodeSeed, Rec.dSection, Rec.dLevel);{$endif}
end;

function TBaseDosLevelSystem.GetLevelCount(aSection: Integer): Integer;
begin
  raise Exception.Create(ClassName + '.GetLevelCount is abstract');
end;

function TBaseDosLevelSystem.GetSectionCount: Integer;
// we could overwrite this function with a faster one but maybe we are
// to lazy :)
var
  Dummy: TStringList;
begin
//  Result := 0;
  Dummy := TStringList.Create;
  try
    GetSections(Dummy);
    Result := Dummy.Count;
  finally
    Dummy.Free;
  end;
end;

procedure TBaseDosLevelSystem.GetSections(aSectionNames: TStrings);
begin
  raise Exception.Create(ClassName + '.GetSections is abstract');
end;

procedure TBaseDosLevelSystem.InternalLoadLevel(aInfo: TLevelInfo; aLevel: TLevel; OddLoad: Boolean = false);
{-------------------------------------------------------------------------------

  NB: a little moving/messing around here with mem
-------------------------------------------------------------------------------}
var
  LVL: TLVLRec;
  OddTable: TDosOddTable;
  Ox: Integer;
  DataStream: TStream;
  Sections: TDosDatSectionList;
  Decompressor: TDosDatDecompressor;
  TheSection: TDosDatSection;
begin
  Assert(Owner is TBaseDosLemmingStyle);

  Sections := TDosDatSectionList.Create;
  Decompressor := TDosDatDecompressor.Create;
  try
    {$ifdef external}if FileExists(aInfo.DosLevelPackFileName) then
      DataStream := TFileStream.Create(aInfo.DosLevelPackFileName, fmOpenRead)
      else{$endif}
      DataStream := CreateDataStream(aInfo.DosLevelPackFileName, ldtLemmings);
    try
      Decompressor.LoadSectionList(DataStream, Sections, False);
    finally
      DataStream.Free;
    end;
    //Decompressor.LoadSectionListFromFile(aInfo.DosLevelPackFileName, Sections, False);
    TheSection := Sections[aInfo.DosLevelPackIndex];
    with TheSection do
    begin
      Decompressor.DecompressSection(CompressedData, DecompressedData);
      DecompressedData.Seek(0, soFromBeginning);
      DecompressedData.ReadBuffer(LVL, SizeOf(LVL));
      DecompressedData.Seek(0, soFromBeginning);
    end;

  // odd level?
  if aInfo.DosIsOddTableClone then
  begin
    Ox := aInfo.DosOddTableIndex;
    OddTable := TDosOddTable.Create;
    try
      OddTable.LoadFromFile(aInfo.DosOddTableFileName); // this one uses the resource if needed
      with OddTable do
      begin
        Move(Recs[Ox], LVL, SizeOf(TDosOddTableRec) - 32);
        Move(Recs[Ox].LevelName, LVL.LevelName, 32);
      end;
      TheSection.DecompressedData.WriteBuffer(LVL, SizeOf(LVL));

    finally
      OddTable.Free;
    end;
  end;

  TheSection.DecompressedData.Seek(0, soFromBeginning);

  TLVLLoader.LoadLevelFromStream(TheSection.DecompressedData, aLevel, OddLoad);


  finally
//    F.Free;
    Decompressor.Free;
    Sections.Free;
  end;

end;

procedure TBaseDosLevelSystem.InternalLoadSingleLevel(aPack, aSection, aLevelIndex: Integer; aLevel: TLevel; OddLoad: Boolean = false);
{-------------------------------------------------------------------------------
  Method for loading one level, without the preparing caching system.
-------------------------------------------------------------------------------}
var
  LocalSectionNames: TStringList;
//  iSection, iLevel: Integer;
  Fn: string;
  IsOdd: Boolean;
  OddIndex: Integer;
  FileIndex: Integer;
  Sty: TBaseDosLemmingStyle;

  OddTable: TDosOddTable;
//  Sections: TDosDatSectionList;
//  Decompressor: TDosDatDecompressor;
//  F: TFileStream;
  //Arc: TDosDatArchive;
  LocalLevelInfo: TLevelInfo;

    function GetOddTitle(Inf: TLevelInfo): string;
    var
      Ox: Integer;
    begin
      Result := StringOfChar(' ', 32);
      Ox := Inf.DosOddTableIndex;
      Move(OddTable.Recs[ox].LevelName, Result[1], 32);
    end;

var
  F: TFileStream;
  IsLoaded: Boolean;


begin
  Assert(Owner is TBaseDosLemmingStyle);


  // added override on demand (look for tricky21 = 221.lvl)
  IsLoaded := False;

  if fLookForLVL{$ifdef testmode} or fTestMode{$endif} then
  begin                     //umisc
    FN := GetApplicationPath + LeadZeroStr(aSection + 1, 2) + LeadZeroStr(aLevelIndex + 1, 2) + '.LVL';
    {$ifdef testmode}
    if fTestMode then FN := fTestLevel;
    {$endif}
    if FileExists(FN) then
    begin
      F := TFileStream.Create(FN, fmOpenRead);
      try
        TLVLLoader.LoadLevelFromStream(F, aLevel, OddLoad);
        if (aLevel.Info.OddTarget) <> $FFFF then
          InternalLoadSingleLevel(aPack, (aLevel.Info.Oddtarget shr 8), (aLevel.Info.Oddtarget mod 256), aLevel, true);
        IsLoaded := True;
      finally
        F.Free;
      end;
    end;
  end;

  if IsLoaded then
    Exit;

  // back to the normal procedure here

  // first try if it is cached in the nested collections
  if aPack <= LevelPacks.Count - 1 then
    if aSection <= LevelPacks[aPack].PackSections.Count - 1 then
      if aLevelIndex <= LevelPacks[aPack][aSection].LevelInfos.Count - 1 then
      begin
        InternalLoadLevel(LevelPacks[aPack].PackSections[aSection].LevelInfos[aLevelIndex], aLevel, OddLoad);
        if (aLevel.Info.OddTarget) <> $FFFF then
          InternalLoadSingleLevel(aPack, (aLevel.Info.Oddtarget shr 8), (aLevel.Info.Oddtarget mod 256), aLevel, true);
        Exit;
      end;

  // otherwise gather info and load it then
  OddTable := nil;
  LocalSectionNames := TStringList.Create;
  Sty := TBaseDosLemmingStyle(Owner);
  LocalLevelInfo := TLevelInfo.Create(nil);

  TRY

  GetSections(LocalSectionNames);

  if Sty.OddTableFile <> '' then
  begin
    OddTable := TDosOddTable.Create;
    OddTable.LoadFromFile(Sty.OddTableFile);
  end;

  IsOdd := False;
  OddIndex := -1;
  FileIndex := -1;
  Fn := '';
  GetEntry(aSection, aLevelIndex, Fn, FileIndex, IsOdd, OddIndex);
  Fn := Sty.CommonPath + Fn;//IncludeCommonPath(Fn);

  LocalLevelInfo.DosLevelPackFileName := Fn;
  LocalLevelInfo.DosLevelPackIndex := FileIndex;
  LocalLevelInfo.DosIsOddTableClone := IsOdd;
  LocalLevelInfo.DosOddTableIndex := OddIndex;
  if IsOdd then
    LocalLevelInfo.DosOddTableFileName := {Sty.IncludeCommonPath(}Sty.OddTableFile{)};

  {
  Arc := TDosDatArchive.Create;
  try
  Arc.LoadFromFile(Fn);
  if not IsOdd then
    LocalLevelInfo.TrimmedTitle := Trim(Arc.ExtractLevelTitle(FileIndex))
  else begin
    LocalLevelInfo.TrimmedTitle := Trim(GetOddTitle(LocalLevelInfo))
  end;
  finally
  Arc.Free;
  end;
  }

  InternalLoadLevel(LocalLevelInfo, aLevel, OddLoad);
  if (aLevel.Info.OddTarget) <> $FFFF then
          InternalLoadSingleLevel(aPack, (aLevel.Info.Oddtarget shr 8), (aLevel.Info.Oddtarget mod 256), aLevel, true);
  FINALLY

  if OddTable <> nil then
    OddTable.Free;
  LocalSectionNames.Free;
  LocalLevelInfo.Free;

  END;

end;

procedure TBaseDosLevelSystem.DumpAllLevels;
var
  aInfo: TLevelInfo;
  aLevel: TLevel;
  dS, dL: Integer;
  aFileName: String;
  aFileIndex, aOddIndex: Integer;
  IsOddTable: Boolean;
  OldLookForLvls: Boolean;
begin
  OldLookForLvls := fLookForLVL;
  fLookForLVL := false;
try
  aInfo := TLevelInfo.Create(nil);
  aLevel := TLevel.Create(nil);
  for dS := 0 to fDefaultSectionCount-1 do
    for DL := 0 to GetLevelCount(dS)-1 do
    begin
      IsOddTable := false;
      GetEntry(dS, dL, aFilename, aFileIndex, IsOddTable, aOddIndex);
      aInfo.DosLevelPackFileName := aFilename;
      aInfo.DosLevelPackIndex := aFileIndex;
      aInfo.DosIsOddTableClone := IsOddTable;
      aInfo.DosOddTableIndex := aOddIndex;
      if IsOddTable then
        aInfo.DosOddTableFileName := TBaseDosLemmingStyle(Owner).OddTableFile;
      LoadLevel(aInfo, aLevel);
      TLVLLoader.SaveLevelToFile(aLevel, LeadZeroStr(dS + 1, 2) + LeadZeroStr(dL + 1, 2) + '.lvl');
    end;
finally
  aInfo.Free;
  aLevel.Free;
end;
  fLookForLVL := OldLookForLvls;
end;

procedure TBaseDosLevelSystem.InternalPrepare;
{-------------------------------------------------------------------------------
  Load all level info
-------------------------------------------------------------------------------}
(*
var
  LocalSectionNames: TStringList;
  iSection, iLevel: Integer;
  Pack: TLevelPack;
  Section: TPackSection;
  LevelInfo: TLevelInfo;
  Fn1, Fn: string;
  IsOdd: Boolean;
  OddIndex: Integer;
  FileIndex: Integer;
  Sty: TBaseDosLemmingStyle;

  OddTable: TDosOddTable;
  Arc: TDosDatArchive;

    function GetOddTitle(Inf: TLevelInfo): string;
    var
      Ox: Integer;
    begin
      Result := StringOfChar(' ', 32);
      Ox := Inf.DosOddTableIndex;
      move(OddTable.recs[ox].LevelName, result[1], 32);
    end;
*)
begin

  raise Exception.Create('Internal Prepare not implemented');

(*

  OddTable := nil;
  if not (Owner is TBaseDosStyle) then
    raise Exception.Create('base dos level loader error');

  LocalSectionNames := TStringList.Create;
  Sty := TBaseDosStyle(Owner);
  Arc := TDosDatArchive.Create;

  TRY

  GetSections(LocalSectionNames);

  if Sty.OddTableFile <> '' then
  begin
    OddTable := TDosOddTable.Create;
    OddTable.LoadFromFile(Sty.IncludeCommonPath(Sty.OddTableFile));
  end;

  // default pack
  Pack := fLevelPacks.Add;
  Pack.LevelPackName := 'Default';
  // sections
  for iSection := 0 to LocalSectionNames.Count - 1 do
  begin
    Section := Pack.PackSections.Add;
    Section.SectionName := LocalSectionNames[iSection];
    //deb([section.sectionname + '_' + i2s(iSection)]);
    // levels
    for iLevel := 0 to GetLevelCount(iSection) - 1 do
    begin
      IsOdd := False;
      OddIndex := -1;
      FileIndex := -1;
      Fn := '';
      GetEntry(iSection{ + 1}, iLevel{ + 1}, Fn, FileIndex, IsOdd, OddIndex);
      Fn1 := Fn;
      Fn := Sty.IncludeCommonPath(Fn);
      LevelInfo := Section.LevelInfos.Add;
//      LevelInfo.iOwnerFile := '';
      LevelInfo.DosLevelPackFileName := Fn;
      LevelInfo.DosLevelPackIndex := FileIndex;
      LevelInfo.DosIsOddTableClone := IsOdd;
      LevelInfo.DosOddTableIndex := OddIndex;
      if IsOdd then
        LevelInfo.DosOddTableFileName := Sty.IncludeCommonPath(Sty.OddTableFile);
      {
      with LevelInfo do
        deb([section.sectionname + '_' + i2s(iLevel) + '=' + Fn1 + ',' + i2s(FileIndex) + ',' + i2s(OddIndex)]);
      }
      Arc.LoadFromFile(Fn);
//      DEB)(
      if not IsOdd then
        LevelInfo.TrimmedTitle := Trim(Arc.ExtractLevelTitle(FileIndex))
      else begin
        LevelInfo.TrimmedTitle := Trim(GetOddTitle(LevelInfo))
      end;
//      'level ' + i2s(iLevel + 1); // lemtools
//      LevelInfo.iOwnerIndex
      //raise exception.create('ERROR NOT FINISHED'); //kernel_resource
    end;
  end;

  FINALLY

  Arc.Free;
  if OddTable <> nil then
    OddTable.Free;
  LocalSectionNames.Free;

  END;

*)

end;

{ TDosOrigLevelLoader }

(*
function TDosOrigLevelSystem.FindLevelCode(const aCode: string; var Rec: TDosGamePlayInfoRec): Boolean;
var
  Sec, Lev: Integer;
  P, i, L: Integer;
  Comp: string;
  List: TStringList;
begin
  Result := inherited FindLevelCode(aCode, Rec);

{
  Result := False;
  for Sec := 0 to 3 do
    for Lev := 0 to 29 do
      if CompareText(DosOrigLevelCodes[Sec, Lev], aCode) = 0 then
      begin
        Result := True;
        Rec.dValid := True;
        Rec.dPack := 0;
        Rec.dSection := Sec;
        Rec.dLevel := Lev;
        Rec.dSectionName := EasyGetSectionName(Sec);
        Exit;
      end;
}
end;
*)

procedure TDosOrigLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
{-------------------------------------------------------------------------------
  I think the numbers in the table are subtracted by 1 before being used in the manner Mike described.
  For example, for Fun 1 the number is 147.
  Divide by 2 (dropping lowest bit first) and you get 73.  "Just Dig" is the 2nd
  level in set level009.dat.  Using 0-based counting, 73 would indeed be the correct number.
  On the other hand, for Fun 8 the number is 14.  Divide by 2 and you get 7.
  Yet "Turn Around Young Lemmings!" is the 7th
  level in set level000.dat.  The correct number should be 6 if 0-based counting is used.
  This inconsistency goes away if all the numbers are subtracted by 1 first.  For example, the 147 becomes 146.  Divide by 2
  and you still get 73.  But for Fun 9, subtract by 1 and you get 13.  Divide by 2 and now you get 6, the correct number when
  counting from 0.  Alternatively, you can add 1 to all numbers for 1-based counting, so that the 147 becomes 148 and 73
  becomes 74.  But either way, the parity (odd/even) changes when you add/subtract 1, and after that, Mike's description
  regarding the bottom bit works correctly.
-------------------------------------------------------------------------------}
var
  B: Byte;
  Fx: Integer; //lemstyles
begin
  B := SectionTable[aSection + 1, aLevel + 1];
  Dec(B);
  IsOddTable := {not} odd(B);// and Bit0 <> 0;
  B := B div 2;
  Fx := (B) div 8;
  aFileName := 'LEVEL' + LeadZeroStr(Fx, 3) + '.DAT';
  aFileIndex := B mod 8;
  aOddIndex := -1;
  if IsOddTable then
    aOddIndex := B{ - 1};
end;

(*
function TDosOrigLevelSystem.GetLevelCode(const Rec: TDosGamePlayInfoRec): string;
begin
  Result := inherited GetLevelCode(Rec);
  {
  Assert(Rec.dSection < 4);
  Assert(Rec.dLevel < 30);
  Result := DosOrigLevelCodes[Rec.dSection, Rec.dLevel];
  }
end;
*)

function TDosOrigLevelSystem.GetLevelCount(aSection: Integer): Integer;
begin
  Result := 30;
end;

procedure TDosOrigLevelSystem.GetSections(aSectionNames: TStrings);
begin
  aSectionNames.CommaText := 'Fun,Tricky,Taxing,Mayhem';
end;

(*procedure TDosOrigLevelLoader.InternalPrepare;
{-------------------------------------------------------------------------------
  Load all level info
-------------------------------------------------------------------------------}
const
  LocalSectionNames: array[0..3] of string = ('Fun', 'Tricky', 'Taxing', 'Mayhem');
var
  iSection, iLevel: Integer;
  Pack: TLevelPack;
  Section: TPackSection;
  LevelInfo: TLevelInfo;
  Fn1, Fn: string;
  IsOdd: Boolean;
  OddIndex: Integer;
  FileIndex: Integer;
  Sty: TDosOrigStyle;

  OddTable: TDosOddTable;
  Arc: TDosDatArchive;

    function GetOddTitle(Inf: TLevelInfo): string;
    var
      Ox: Integer;
    begin
      Result := StringOfChar(' ', 32);
      Ox := Inf.DosOddTableIndex;
      move(OddTable.recs[ox].LevelName, result[1], 32);
    end;

begin
  if not (Owner is TDosOrigStyle) then
    raise Exception.Create('dosorig loader error');
  Sty := TDosOrigStyle(Owner);
  Arc := TDosDatArchive.Create;
  OddTable := TDosOddTable.Create;
  OddTable.LoadFromFile(Sty.IncludeCommonPath(Sty.OddTableFile));
  // default pack
  Pack := fLevelPacks.Add;
  Pack.LevelPackName := 'Default';
  // sections
  for iSection := 0 to 3 do
  begin
    Section := Pack.PackSections.Add;
    Section.SectionName := LocalSectionNames[iSection];
    //deb([section.sectionname + '_' + i2s(iSection)]);
    // levels
    for iLevel := 0 to 29 do
    begin
      GetEntry(iSection + 1, iLevel + 1, Fn, FileIndex, IsOdd, OddIndex);
      Fn1 := Fn;
      Fn := Sty.IncludeCommonPath(Fn);
      LevelInfo := Section.LevelInfos.Add;
//      LevelInfo.iOwnerFile := '';
      LevelInfo.DosLevelPackFileName := Fn;
      LevelInfo.DosLevelPackIndex := FileIndex;
      LevelInfo.DosIsOddTableClone := IsOdd;
      LevelInfo.DosOddTableIndex := OddIndex;
      if IsOdd then
        LevelInfo.DosOddTableFileName := Sty.IncludeCommonPath(Sty.OddTableFile);
      {
      with LevelInfo do
        deb([section.sectionname + '_' + i2s(iLevel) + '=' + Fn1 + ',' + i2s(FileIndex) + ',' + i2s(OddIndex)]);
      }
      Arc.LoadFromFile(Fn);
      if not IsOdd then
        LevelInfo.TrimmedTitle := Trim(Arc.ExtractLevelTitle(FileIndex))
      else begin
        LevelInfo.TrimmedTitle := Trim(GetOddTitle(LevelInfo))
      end;
//      'level ' + i2s(iLevel + 1); // lemtools
//      LevelInfo.iOwnerIndex
      //raise exception.create('ERROR NOT FINISHED'); //kernel_resource
    end;
  end;

  Arc.Free;

end; *)

{ TDosOrigMusicSystem }

function TDosOrigMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  We use the amiga tracks. We use the amiga system for now: just alternate every 17
  in the archived resource we skipped into.mod so we have
  track_01.mod through track_20.mod.
  the special graphic levels have tracks 18..21
  fun22 abeast, tricky14 menacing, taxing15 awesome, mayhem 22 abeast II
-------------------------------------------------------------------------------}
begin
  Result := '';
  // locally use one-based levels
  Inc(aSection);
  Inc(aLevel);

  if (aSection = 1) and (aLevel = 22) then
    Result := 'track_18'
  else if (aSection = 2) and (aLevel = 14) then
    Result := 'track_19'
  else if (aSection = 3) and (aLevel = 15) then
    Result := 'track_20'
  else if (aSection = 4) and (aLevel = 22) then
    Result := 'track_21'
  else
    Result := 'track_' + LeadZeroStr((aLevel - 1) mod 17 + 1, 2);
  (*
  else case aLevel of
    1..17  : Result := 'track_' + LeadZeroStr(aLevel, 2) + '.mod';
    18..30 : Result := 'track_' + LeadZeroStr(aLevel - 17, 2) + '.mod';
  end; *)


end;

{ TDosOhNoMusicSystem }

function TDosOhNoMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
begin
  Result := 'track_' + LeadZeroStr((aLevel mod 6) + 1, 2);
end;


{ TDosOhNoLevelSystem }

procedure TDosOhNoLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
var
  H, Sec, Lev: Integer;
begin
  Assert((aSection >= 0) and (aSection <= 4));
  Assert((aLevel >= 0) and (aLevel <= 19));

  H := OhnoTable[aSection + 1, aLevel + 1];
  Sec := H div 10;
  Lev := H mod 10;

  aFileName := 'DLVEL' + LeadZeroStr(Sec, 3) + '.DAT';
  aFileIndex := Lev;

  IsOddTable := False;
  aOddIndex := -1;
end;

function TDosOhNoLevelSystem.GetLevelCount(aSection: Integer): Integer;
begin
  Result := 20;
end;

procedure TDosOhNoLevelSystem.GetSections(aSectionNames: TStrings);
var
  x : byte;
begin
  aSectionNames.CommaText := 'Tame,Crazy,Wild,Wicked,Havoc';
end;

{ TDosHoliday94MusicSystem }

function TDosHoliday94MusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
begin
  Result := 'track_' + LeadZeroStr((aLevel mod 3) + 1, 2);
end;

{ TDosHoliday94LevelSystem }

procedure TDosHoliday94LevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
begin

  aFileName := 'LEVEL' + LeadZeroStr((aSection * 2) + (aLevel div 8), 3) + '.DAT';
  aFileIndex := aLevel mod 8;

  {Inc(aSection);
  Inc(aLevel);

  case aSection of
    1:
      begin
        case aLevel of
          1..8:
            begin
              aFileName := 'LEVEL000.DAT';
              aFileIndex := aLevel - 1;
            end;
          9..16 :
            begin
              aFileName := 'LEVEL001.DAT';
              aFileIndex := aLevel - 9;
            end;
        end;
      end;
    2:
      begin
        case aLevel of
          1..8  :
            begin
              aFileName := 'LEVEL002.DAT';
              aFileIndex := aLevel - 1;
            end;
          9..16 :
            begin
              aFileName := 'LEVEL003.DAT';
              aFileIndex := aLevel - 9;
            end;
        end;
      end;
  end;}

end;

function TDosHoliday94LevelSystem.GetLevelCount( aSection: Integer): Integer;
begin
  Result := 16;
end;

procedure TDosHoliday94LevelSystem.GetSections(aSectionNames: TStrings);
begin
  aSectionNames.CommaText := 'Frost,Hail,Flurry,Blitz';
end;



{ TDosXmasMusicSystem }

function TDosXmasMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
begin
  Result := 'track_' + LeadZeroStr(((aLevel + (aSection * 2)) mod 3) + 1, 2);
end;

{ TDosXmasLevelSystem }

procedure TDosXmasLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
begin

  aFileName := 'LEVEL' + LeadZeroStr(aSection, 3) + '.DAT';
  aFileIndex := aLevel;

end;

function TDosXmasLevelSystem.GetLevelCount( aSection: Integer): Integer;
begin
  case aSection of
    0: Result := 2;
    1: Result := 4;
  end;
end;

procedure TDosXmasLevelSystem.GetSections(aSectionNames: TStrings);
begin
  aSectionNames.CommaText := 'Xmas91,Xmas92';
end;


{ TDosCovoxMusicSystem }

function TDosCovoxMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
begin
  Result := 'track_' + LeadZeroStr(((aLevel + (aSection * 2)) mod 17) + 1, 2);
end;

{ TDosCovoxLevelSystem }

procedure TDosCovoxLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
begin

  aFileName := 'LEVEL000.DAT';
  aFileIndex := (aSection * 2) + aLevel;

end;

function TDosCovoxLevelSystem.GetLevelCount( aSection: Integer): Integer;
begin
  Result := 2;
end;

procedure TDosCovoxLevelSystem.GetSections(aSectionNames: TStrings);
begin
  aSectionNames.CommaText := 'Fun,Tricky,Taxing,Mayhem';
end;


{ TDosPrimaMusicSystem }

function TDosPrimaMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
begin
  Result := 'track_' + LeadZeroStr(((aLevel + (aSection * 4)) mod 17) + 1, 2);
end;

{ TDosPrimaLevelSystem }

procedure TDosPrimaLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
begin

  aFileName := 'LEVEL' + LeadZeroStr((aSection div 2), 3) + '.DAT';
  aFileIndex := ((aSection mod 2) * 4) + aLevel;

end;

function TDosPrimaLevelSystem.GetLevelCount( aSection: Integer): Integer;
begin
  Result := 4;
end;

procedure TDosPrimaLevelSystem.GetSections(aSectionNames: TStrings);
begin
  aSectionNames.CommaText := 'Fun,Tricky,Taxing,Mayhem';
end;


{ TDosExtraMusicSystem }

function TDosExtraMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
var
  mult, mt: Integer;
begin
  case aSection of
    0: mult := 1;
    1: mult := 3;
    2: mult := 7;
    3: mult := 11;
    4: mult := 13;
  end;
  mt := (aLevel * mult) mod 23;
  if mt > 16 then mt := mt + 4;
  Result := 'track_' + LeadZeroStr(mt + 1, 2);
end;

{ TDosExtraLevelSystem }

procedure TDosExtraLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
begin

  aFileName := 'LEVEL' + LeadZeroStr(aSection, 3) + '.DAT';
  aFileIndex := aLevel;

end;

function TDosExtraLevelSystem.GetLevelCount( aSection: Integer): Integer;
begin
  case aSection of
    0: Result := 47;
    1: Result := 30;
    2: Result := 30;
    3: Result := 3;
    4: Result := 12;
    5: Result := 36;
    6: Result := 30;
    7: Result := 11;
  end;
end;

procedure TDosExtraLevelSystem.GetSections(aSectionNames: TStrings);
begin
  aSectionNames.CommaText := 'Genesis,Present,Sunsoft,Genesis2P,Sega,PSP,Amiga2P,Other';
end;



{ TDosCustMusicSystem }

function TDosCustMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
{-------------------------------------------------------------------------------
  amiga tracks 01..06
-------------------------------------------------------------------------------}
begin
  {$ifndef flexi}Randomize;
  Result := 'track_' + LeadZeroStr(random(23) + 1, 2);
  {$else}Result := 'track_' + LeadZeroStr((aLevel mod MusicCount) + 1, 2);{$endif}
end;


{ TDosCustLevelSystem }

procedure TDosCustLevelSystem.GetEntry(aSection, aLevel: Integer;
  var aFileName: string; var aFileIndex: Integer; var IsOddTable: Boolean;
  var aOddIndex: Integer);
var
  H, Sec, Lev: Integer;
begin
//  Assert((aSection >= 0) and (aSection <= 4));
//  Assert((aLevel >= 0) and (aLevel <= 19));

//  H := OhnoTable[aSection + 1, aLevel + 1];
//  Sec := H div 10;
//  Lev := H mod 10;

  {$ifndef flexi}
  aFilename := fLevelPack;
  aFileIndex := (aSection * 2) + (aLevel mod 2);
  {$else}
  aFilename := 'level' + LeadZeroStr(aSection, 3) + '.dat';
  aFileIndex := aLevel;
  {$endif}

  IsOddTable := False;
  aOddIndex := -1;
end;

function TDosCustLevelSystem.GetLevelCount(aSection: Integer): Integer;
{$ifdef flexi}
var
  Dcmp : TDosDatDecompressor;
  FSt : TStream;
  FSl : TDosDatSectionList;
{$endif}
begin
  {$ifdef flexi}
  Result := 1;
  try
    Dcmp := TDosDatDecompressor.Create;
    Fsl := TDosDatSectionList.Create;
    {$ifdef external}if FileExists(ExtractFilePath(ParamStr(0)) + 'LEVEL' + LeadZeroStr(aSection, 3) + '.dat') then
      Fst := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'LEVEL' + LeadZeroStr(aSection, 3) + '.dat', fmOpenRead)
      else{$endif}
      Fst := CreateDataStream('LEVEL' + LeadZeroStr(aSection, 3) + '.dat', ldtLemmings);
    Dcmp.LoadSectionList(Fst, Fsl, False);
    Result := Fsl.Count;
  finally
    Fst.Free;
    Dcmp.Free;
    Fsl.Free;
  end;
  {$else}
  Result := 2;
  {$endif}
end;

procedure TDosCustLevelSystem.GetSections(aSectionNames: TStrings);
var
  x : byte;
begin
  {$ifdef flexi}
  aSectionNames.CommaText := '';
  for x := 0 to (SysDat.RankCount - 1) do
  begin
    aSectionNames.CommaText := aSectionNames.CommaText + GetRankName(x);
    if x <> (SysDat.RankCount - 1) then aSectionNames.CommaText := aSectionNames.CommaText + ',';
  end;
  {$else}
  aSectionNames.CommaText := 'Step1,Step2,Step3,Step4,Step5';
  {$endif}
end;

function TDosCustLevelSystem.GetRankName(aSection: Byte): String;
var
  tstr : String;
  x : byte;
begin
  {$ifdef flexi}for x := 0 to 15 do
  begin
    if (tstr <> '') or (SysDat.RankNames[aSection][x] <> ' ') then
    begin
      tstr := tstr + SysDat.RankNames[aSection][x];
      if SysDat.RankNames[aSection][x] <> ' ' then Result := tstr;
    end;
  end;{$endif}
end;

end.

