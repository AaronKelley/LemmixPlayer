{$include lem_directives.inc}

unit LemCore;

interface

const
  GAME_BMPWIDTH = 1584;

(*
  { TODO : find good settings }
    we cannot get a nice minimapscale 1/16 so instead we chose the following:

    image width of game                 = 1584
    imagewidth of minimap               = 104
    width of white rectangle in minimap = 25





//  GAME_BMPWIDTH = 1664; // this is too far I think, but now it fits with the minimap!

  { the original dos lemmings show pixels 0 through 1583 (so including pixel 1583)
    so the imagewidth should be 1584, which means 80 pixels less then 1664


  MiniMapBounds: TRect = (
    Left: 208;   // width =about 100
    Top: 18;
    Right: 311;  // height =about 20
    Bottom: 37
  );
  *)





  clMask32  = $00FF00FF; // color used for "shape-only" masks

{•}

type
  TBasicLemmingAction = (
    baNone,
    baWalking,
    baJumping,
    baDigging,
    baClimbing,
    baDrowning,
    baHoisting,
    baBuilding,
    baBashing,
    baMining,
    baFalling,
    baFloating,
    baSplatting,
    baExiting,
    baVaporizing,
    baBlocking,
    baShrugging,
    baOhnoing,
    baExploding
  );

  TSkillPanelButton = (
    spbNone,
    spbSlower,
    spbFaster,
    spbClimber,
    spbUmbrella,
    spbExplode,
    spbBlocker,
    spbBuilder,
    spbBasher,
    spbMiner,
    spbDigger,
    spbPause,
    spbNuke
  );

const
  AssignableSkills = [
    baDigging,
    baClimbing,
    baBuilding,
    baBashing,
    baMining,
    baFloating,
    baBlocking,
    baExploding
  ];

const
  ActionToSkillPanelButton: array[TBasicLemmingAction] of TSkillPanelButton = (
    spbNone,
    spbNone,
    spbNone,
    spbDigger,
    spbClimber,
    spbNone,
    spbNone,
    spbBuilder,
    spbBasher,
    spbMiner,
    spbNone,
    spbUmbrella,
    spbNone,
    spbNone,
    spbNone,
    spbBlocker,
    spbNone,
    spbNone,
    spbExplode
  );

const
  SkillPanelButtonToAction: array[TSkillPanelButton] of TBasicLemmingAction = (
    baNone,
    baNone,
    baNone,
    baClimbing,
    baFloating,
    baExploding,
    baBlocking,
    baBuilding,
    baBashing,
    baMining,
    baDigging,
    baNone,
    baNone
  );

implementation

end.

