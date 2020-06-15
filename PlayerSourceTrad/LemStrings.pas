{$include lem_directives.inc}

unit LemStrings;

interface

uses
  LemCore;

// version 0.8.0.0 added optional overriding with LVL files
// version 0.8.0.1 added fixes to LemGame.pas:
//    1) fix incorrect entrance order for 3 entrances, non-original DOS
//    2) adds support for replaying the steel-digging glitch in replays
//    3) adds support for nuke glitch

const
  SVersion = 'DEV';

  //@styledef
  SCheatCode = 'cheatcodes';

resourcestring
  SProgramName = 'Lemmix Player';
  SDummyString = '';

  {-------------------------------------------------------------------------------
    Errors
  -------------------------------------------------------------------------------}
  SMetaPieceLoadError = 'MetaPiece LoadFromStream is not defined';
  SVgaSpecDimensionError_dd = 'Special Dos level graphics must be %d x %d pixels';

  {-------------------------------------------------------------------------------
    MenuScreen
  -------------------------------------------------------------------------------}
  //@styledef
  SProgramText =
    {$ifdef orig}
    'Lemmings Clone (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef ohno}
    'Oh No! More Lemmings! Clone (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef H94}
    'Holiday Lemmings 94 Clone (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef xmas}
    'Xmas Lemmings 91/92 Clone (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef covox}
    'Covox Lemmings Demo Clone (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef prima}
    'Prima Publishing Demo Clone (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef extra}
    'Extra Levels Lemmix Player (' + SVersion + ')' + #13 +
    {$endif}
    {$ifdef cust}
    {$ifndef flexi}'Customized Lemmings Clone (' + SVersion + ')' +{$endif} #13 +
    {$endif}
    {$ifndef flexi}'By Eric Langedijk, ccexplore, namida'{$else}'Flexible Lemmix Player'{$endif};

  // max size for string that fits in the scrolling reel = 34
  // 1234567890123456789012345678901234

  SCredits =
    {$ifdef flexi}'Flexi Player ' + SVersion + #13 +{$endif}
    'Thanks to...' + #13 +
    {$ifdef flexi}'EricLang for base Lemmix' + #13 +
    'ccexplore for Lemmix 0.8.0.1' + #13 +
    'namida for Flexi player and' + #13 +
    'general updates to Lemmix' + #13 +{$endif}
    'DMA for the original game' + #13 +
    'ccexplore for game-mechanics' + #13 +
    'Alex A. Denisov for Graphics32' + #13 +
    'Erik Turner for ZLibEx' + #13 +
    'Peter Morris for FastStrings' + #13 +
    'Un4seen Development for Bassmod at' + #13 +
    'http://www.un4seen.com' + #13 +
    'The Lemmings Forums at' + #13 +
    'https://www.lemmingsforums.net/' + #13 +
    'Volker Oth, ccexplore and Mindless for sharing sourcecode, resources ' +
    'and technical information about lemmings' + #13 +
    'Original credits...'  + #13 +
    'Lemmings By DMA Design'  + #13 +
    'Programming By Russell Kay'  + #13 +
    'Animation By Gary Timmons'  + #13 +
    'Graphics By Scott Johnston'  + #13 +
    'Music By Brian Johnston & Tim Wright'  + #13 +
    'PC Music By Tony Williams'  + #13 +
    'Copyright 1991 Psygnosis Ltd.';

  {-------------------------------------------------------------------------------
    LevelCodeScreen
  -------------------------------------------------------------------------------}
  SEnterCode =
    'Enter Code';

  SIncorrectCode =
    'INCORRECT CODE';

  SCodeForLevel_sd =
    'Code for %s' + #13 +
    'Level %d';

  {-------------------------------------------------------------------------------
    PreviewScreen
  -------------------------------------------------------------------------------}
  SPreviewString =
    'Level %d ' + '%s'                     + #13#13#13 +
    '          Number of Lemmings %d'      + #13#13 +
    '          %s To Be Saved'             + #13#13 +
    '          Release Rate %d'            + #13#13 +
    '          Time %d Minutes'            + #13#13 +
    '          Rating  %s'                 + #13#13#13 +
    '     Press mouse button to continue';


  {-------------------------------------------------------------------------------
    Game Screen Info Panel
  -------------------------------------------------------------------------------}
  SSkillPanelTemplate =
    '..............' + 'OUT_.....' + 'IN_.....' + 'TIME_.-..';

  SAthlete =
    'Athlete';

  SWalker =
    'Walker';

  SJumper =
    'Jumper';

  SDigger =
    'Digger';

  SClimber =
    'Climber';

  SDrowner =
    'Drowner';

  SHoister =
    'Hoister';

  SBuilder =
    'Builder';

  SBasher =
    'Basher';

  SMiner =
    'Miner';

  SFaller =
    'Faller';

  SFloater =
    'Floater';

  SSplatter =
    'Splatter';

  SExiter =
    'Exiter';

  SVaporizer =
    'Frier';

  SBlocker =
    'Blocker';

  SShrugger =
    'Shrugger';

  SOhnoer =
    'Ohnoer';

  SExploder =
    'Bomber';

  {-------------------------------------------------------------------------------
    Postview Screen
  -------------------------------------------------------------------------------}
  SYourTimeIsUp =
    'Your time is up!';

  SAllLemmingsAccountedFor =
    'All lemmings accounted for.';

  SYouRescuedYouNeeded_ss =
     'You rescued %s' + #13 +
     'You needed  %s';

  SResult0 =
    'ROCK BOTTOM! I hope for your sake'      + #13 +
    'that you nuked that level.';

  SResult1 =
    'Better rethink your strategy before'    + #13 +
    'you try this level again!';

  SResult2 =
    'A little more practice on this level'   + #13 +
    'is definitely recommended.';

  SResult3 =
    'You got pretty close that time.'        + #13 +
    'Now try again for that few % extra.';

  SResult4 =
    'OH NO, So near and yet so far (teehee)' + #13 +
    'Maybe this time.....';

  SResult5 =
    'RIGHT ON. You can''t get much closer'   + #13 +
    'than that. Let''s try the next...';

  SResult6 =
    'That level seemed no problem to you on' + #13 +
    'that attempt. Onto the next....';

  SResult7 =
    'You totally stormed that level!'        + #13 +
    'Let''s see if you can storm the next...';

  SResult8 =
    'Superb! You rescued every lemmings on' + #13 +
    'that level. Can you do it again....';

  SCongratulationOrig =
    #13 + #13 +
    'Congratulations!' +
    #13 + #13 + #13 + #13 + #13 +
    'Everybody here at DMA Design salutes you' + #13 +
    'as a MASTER Lemmings player. Not many' + #13 +
    'people will complete the Mayhem levels,' + #13 +
    'you are definitely one of the elite' + #13 +
    #13 + #13 + #13 + #13 + #13 +
    'Now hold your breath for the data disk';


  SResultOhNo0 =
    'Oh dear, not even one poor Lemming'   + #13 +
    'saved. Try a little harder next time.';

  SResultOhNo1 =
    'Yes, well, err, erm, maybe that is' + #13 +
    'NOT the way to do this level.';

  SResultOhNo2 =
    'We are not too impressed with your' + #13 +
    'attempt at that level!';

  SResultOhNo3 =
    'Getting close. You are either pretty' + #13 +
    'good, or simply lucky.';

  SResultOhNo4 =
    'Shame, You were short by a tiny amount.' + #13 +
    'Go for it this time.';

  SResultOhNo5 =
    'Just made it by the skin of your' + #13 +
    'teeth. Time to progress..';

  SResultOhNo6 =
    // This seems a typo but it really is enough+space + point in the exe
    'More than enough .You have the makings' + #13 +
    'of a master Lemmings player.';

  SResultOhNo7 =
    'What a fine display of Lemmings control.' + #13 +
    'Take a bow then carry on with the game.';

  SResultOhNo8 =
    'WOW! You saved every Lemming.' + #13 +
    'TOTALLY EXCELLENT!';

  SCongratulationOhNo =
    #13 + #13 +
    'Congratulations!' + #13 +
    #13 + #13 + #13 + #13 + #13 +
    'You are truly an Excellent' + #13 +
    'Lemmings player' + #13 +
    #13 +
    'The Lemmings Saga continues at a' + #13 +
    'later date, watch this space';


  SYourAccessCode_ds =
    'Your Access Code for Level %d' + #13 +
    'is %s';


  SPressLeftMouseForNextLevel =
    'Press left mouse button for next level';

  SPressLeftMouseToRetryLevel =
    'Press left mouse button to retry level';

  SPressRightMouseForMenu =
    'Press right mouse button for menu';

  SPressMouseToContinue =
    'Press mouse button to continue';

  {-------------------------------------------------------------------------------
    SkillPanel hints (not used yet)
  -------------------------------------------------------------------------------}
  SDigHint =
    'Dig';

  SClimbHint =
    'Climb';

  SBuildHint =
    'Build';

  SBashHint =
    'Bash';

  SMineHint =
    'Mine';

  SFloatHint =
    'Umbrella';

  SBlockHint =
    'Block';

  SExplodeHint =
    'Bomb';

const
  LemmingActionStrings: array[TBasicLemmingAction] of string = (
    SDummyString,
    SWalker,
    SJumper,
    SDigger,
    SClimber,
    SDrowner,
    SHoister,
    SBuilder,
    SBasher,
    SMiner,
    SFaller,
    SFloater,
    SSplatter,
    SExiter,
    SVaporizer,
    SBlocker,
    SShrugger,
    SOhnoer,
    SExploder
  );

  //@styledef
  {$ifdef orig}
  ResultStrings: array[0..8] of string = (
    SResult0,
    SResult1,
    SResult2,
    SResult3,
    SResult4,
    SResult5,
    SResult6,
    SResult7,
    SResult8
  );
  SCongrats: string = SCongratulationOrig;
  {$endif}

  {$ifdef ohno}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

  {$ifdef h94}
  {TODO: check this ones out}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

  {$ifdef xmas}
  {TODO: check this ones out}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

  {$ifdef covox}
  {TODO: check this ones out}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

  {$ifdef prima}
  {TODO: check this ones out}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

  {$ifdef extra}
  {TODO: check this ones out}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

  {$ifdef cust}
  ResultStrings: array[0..8] of string = (
    SResultOhNo0,
    SResultOhNo1,
    SResultOhNo2,
    SResultOhNo3,
    SResultOhNo4,
    SResultOhNo5,
    SResultOhNo6,
    SResultOhNo7,
    SResultOhNo8
  );
  SCongrats: string = SCongratulationOhNo;
  {$endif}

implementation

end.


