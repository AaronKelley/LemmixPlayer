{$include lem_directives.inc}

unit GameInterfaces;

{-------------------------------------------------------------------------------
  Unit with shared interfaces between game and it's controls
-------------------------------------------------------------------------------}

interface

uses
  GR32,
  LemCore;

type
  // drawing of info and toolbarbuttons
  IGameToolbar = interface
    procedure DrawSkillCount(aButton: TSkillPanelButton; aNumber: Integer);
    procedure DrawButtonSelector(aButton: TSkillPanelButton; SelectorOn: Boolean);
    procedure DrawMinimap(Map: TBitmap32);
    procedure SetInfoCursorLemming(const Lem: string; HitCount: Integer);
    procedure SetInfoLemmingsOut(Num: Integer);
    procedure SetInfoLemmingsIn(Num, Max: Integer);
    procedure SetInfoMinutes(Num: Integer);
    procedure SetInfoSeconds(Num: Integer);
    procedure RefreshInfo;
  end;

implementation

end.

