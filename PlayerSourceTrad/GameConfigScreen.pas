{$include lem_directives.inc}

unit GameConfigScreen;

interface

uses
  Windows, Classes, SysUtils, Controls,
  StrUtils, Dialogs,
  UMisc,
  Gr32, Gr32_Image, Gr32_Layers,
  LemCore,
  LemTypes,
  LemStrings,
  LemLevelSystem,
  LemGame,
  GameControl,
  {$ifdef flexi}LemDosStructures,{$endif}
  LemDosStyle,
  GameBaseScreen;
//  LemCore, LemGame, LemDosFiles, LemDosStyles, LemControls,
  //LemDosScreen;

const
  MAX_GIM_SETTING = 23;

{-------------------------------------------------------------------------------
   The dos postview screen, which shows you how you've done it.
-------------------------------------------------------------------------------}
type
  TGameConfigScreen = class(TGameBaseScreen)
  private
    fLookForLVLOption: Boolean;
    fExtraModeOption: Boolean;
    fCustOptions: Boolean;
    fTestModeOptions: Boolean;
    fScreenNumber: Integer;
    fGimmickSetting: Integer;
    fSkillsetSetting: Integer;
    fDoneFirstBuild: Boolean;
    function GetScreenText: string;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    {procedure SetScreenNumber(aValue: Integer);}
  protected
    procedure PrepareGameParams(Params: TDosGameParams); override;
    procedure BuildScreen; override;
    procedure CloseScreen(aNextScreen: TGameScreenType); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property ScreenNumber: Integer read fScreenNumber write fScreenNumber; //SetScreenNumber;
  published
  end;

implementation

uses Forms, LemStyle;

{ TDosGameConfig }

{procedure TGameConfigScreen.SetScreenNumber(aValue: Integer);
begin
  if aValue <> 1 then aValue := 0;
  if not (fCustOptions or fExtraModeOption) then aValue := 0;
  fScreenNumber := aValue;
end;}

procedure TGameConfigScreen.CloseScreen(aNextScreen: TGameScreenType);
begin
  inherited CloseScreen(aNextScreen);
end;

procedure TGameConfigScreen.PrepareGameParams(Params: TDosGameParams);
begin
  inherited;
  // First, set the options to the standard values
  fLookForLVLOption := {$ifdef nolookforlvls}false{$else}true{$endif};
  fExtraModeOption := {$ifdef noextramodes}false{$else}true{$endif};
  fCustOptions := {$ifdef cust}true{$else}false{$endif};
  fTestModeOptions := {$ifdef testmode}true{$else}false{$endif};

  //Next, adjust for Flexi option on LookForLVLs and ExtraModes
  {$ifdef flexi}
  if GameParams.SysDat.Options and 1 = 0 then fLookForLVLOption := false;
  if GameParams.SysDat.Options and 32 = 0 then fExtraModeOption := false;
  {$endif}
end;

procedure TGameConfigScreen.BuildScreen;
var
  Temp: TBitmap32;
//  DstRect: TRect;
begin
  //fSection := aSection;
  //fLevelNumber := aLevelNumber;
  //fGameResult := aResult;

  ScreenImg.BeginUpdate;
  Temp := TBitmap32.Create;
  try
    InitializeImageSizeAndPosition(640, 350);

    if not fDoneFirstBuild then
    begin
      ExtractBackGround;
      ExtractPurpleFont;
      fDoneFirstBuild := true;
    end;

    Temp.SetSize(640, 350);
    Temp.Clear(0);
    TileBackgroundBitmap(0, 0, Temp);
    DrawPurpleTextCentered(Temp, GetScreenText, 16);
    ScreenImg.Bitmap.Assign(Temp);
  finally
    ScreenImg.EndUpdate;
    Temp.Free;
  end;
end;

constructor TGameConfigScreen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Stretched := True;
  OnKeyDown := Form_KeyDown;
  OnKeyPress := Form_KeyPress; 
end;

destructor TGameConfigScreen.Destroy;
begin
  inherited Destroy;
end;

function TGameConfigScreen.GetScreenText: string;
var
  i: Integer;
  STarget: string;
  SDone: string;
  SScore: string;
  H: string;
  lfcount : integer;
  fEnabled : Boolean;
  fExtVal : LongWord;

    procedure Add(const S: string);
    begin
      Result := Result + S + #13;
      Inc(lfcount);
    end;

    procedure LF(aCount: Integer);
    begin
      Result := Result + StringOfChar(#13, aCount);
      Inc(lfcount, aCount);
    end;

    function OnOffText(aVal: Boolean): String;
    begin
      if aVal then
        Result := ' ON'
        else
        Result := 'OFF';
    end;

    procedure LPAdd(S: string; State: Boolean);
    begin
      while length(S) < 28 do
        S := S + ' ';
      S := S + ': ';
      Add(S + OnOffText(State));
    end;

    function PercentOptionText(aVal: Integer): String;
    begin
      case aVal of
        0: Result := 'OFF';
        1: Result := ' ON';
        2: Result := 'ADJ';
      end;
    end;

    function OnOffAltText(Op1, Op2: Boolean): String;
    begin
      if Op1 = false then
        Result := 'OFF'
        else begin
        if Op2 = false then
          Result := ' ON'
          else
          Result := 'ALT';
      end;
    end;

    function TestModeText(aVal: Integer): String;
    begin
      case aVal of
        0: Result := ' Pre/Postview';
        1: Result := '   No Preview';
        2: Result := 'Gameplay Only';
        3: Result := 'Game+AutoSave';
      end;
    end;

    function SteelModeText(aVal: Integer): String;
    begin
      case aVal of
        0: Result := '   No Override';
        1: Result := '     Autosteel';
        2: Result := 'Simple A.Steel';
      end;
    end;

    function MechText(aVal: Integer): String;
    begin
      case aVal of
        0: Result := 'Cust';
        1: Result := 'Orig';
        2: Result := 'OhNo';
      end;
    end;

    function GimmickName(aVal: Integer): String;
    begin
      case aVal of
        0: Result := 'Superlemming';
        1: Result := 'Frenzy';
        2: Result := 'Reverse Skill Count';
        3: Result := 'Karoshi';
        4: Result := 'Unalterable Terrain';
        5: Result := 'Skill Count Overflow';
        6: Result := 'No Gravity';
        7: Result := 'Hardworkers';
        8: Result := 'Backwards Walkers';
        9: Result := 'Lazy Lemmings';
       10: Result := 'Exhaustion';
       11: Result := 'Non-Fatal Bombers';
       12: Result := 'Invincibility';
       13: Result := 'One Skill Per Lemming';
       14: Result := 'Steel Inversion';
       15: Result := 'Solid Level Bottom';
       16: Result := 'Single-Use Permanents';
       17: Result := 'Disobedience';
       18: Result := 'Nuclear Bombers';
       19: Result := 'Turnaround On Assign';
       20: Result := 'Countdown Other Skills';
       21: Result := 'Assign To All';
       22: Result := 'Horizontal Wrap';
       23: Result := 'Vertical Wrap';
        //            1234567890123456789012345
      end;
    end;

    function SkillsetName(aVal: Integer): String;
    begin
      case aVal of
        0: Result := 'Walker';
        1: Result := 'Climber';
        2: Result := 'Swimmer';
        3: Result := 'Floater';
        4: Result := 'Glider';
        5: Result := 'Mechanic';
        6: Result := 'Bomber';
        7: Result := 'Stoner';
        8: Result := 'Blocker';
        9: Result := 'Platformer';
       10: Result := 'Builder';
       11: Result := 'Stacker';
       12: Result := 'Basher';
       13: Result := 'Miner';
       14: Result := 'Digger';
       15: Result := 'Cloner';
      end;
    end;

    function SoundOptionText(aMusic, aSound: Boolean): String;
    begin
      if aMusic then Result := ' ON'
      else if aSound then Result := ' FX'
      else Result := 'OFF';
    end;

var
  ts : String;
begin

  Add('Lemmix Configuration');
  LF(2);

  Add('1) Show Particles:            ' + OnOffText(GameParams.ShowParticles));
  Add('2) Lemmix Trap Bug:           ' + OnOffText(GameParams.LemmixTrapBug));
  Add('3) State Control:             ' + OnOffText(GameParams.StateControlEnabled));
  if fLookForLVLOption then
    Add('4) Look For LVL Files:        ' + OnOffText(GameParams.LookForLVLFiles));
  if fCustOptions then
    begin
      {$ifdef cust}{$ifndef flexi}
      Add('5) Mechanics:                ' + MechText(GameParams.Mechanics));
      ts := GameParams.LevelPack;
      while (Length(ts) < 18) do
        ts := ' ' + ts;
      if (Length(ts) > 18) then
        ts := LeftStr(ts, 15) + '...';
      Add('Q) Level Pack: ' + ts);
      ts := GameParams.fExternalPrefix;
      if ts = '' then ts := '(none)';
      while (Length(ts) < 18) do
        ts := ' ' + ts;
      if (Length(ts) > 18) then
        ts := LeftStr(ts, 15) + '...';
      Add('W) Prefix:     ' + ts);
      {$endif} {$endif}
    end;

    {$ifdef testmode}
  if fTestModeOptions then
    Add('E) Test Mode Style: ' + TestModeText(GameParams.QuickTestMode));
    {$endif}

  LF(19-lfcount);

  Add('Press Esc to return to main menu');

end;


procedure TGameConfigScreen.Form_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: CloseScreen(gstMenu);
    VK_RETURN: CloseScreen(gstMenu);
  end;
end;

procedure TGameConfigScreen.Form_KeyPress(Sender: TObject; var Key: Char);
var
  lChar : Char;

  procedure SwitchOption(aOpt: Integer);
  var
    tNum : LongWord;
    i: Integer;
    fDialog: TOpenDialog;
    ts, ts2: String;
  begin
    with GameParams do
      case aOpt of
        1: ShowParticles := not ShowParticles;
        2: begin
             LookForLVLFiles := not LookForLVLFiles;
             TBaseDosLevelSystem(Style.LevelSystem).LookForLVL := LookForLVLFiles;
           end;
        {$ifdef testmode}
        3: begin
              QuickTestMode := QuickTestMode + 1;
              if QuickTestMode > 3 then QuickTestMode := 0;
            end;
        {$endif}
        {$ifdef cust}{$ifndef flexi}
        4: begin
              fDialog := tOpenDialog.Create(self);
              fDialog.Title := 'Select level pack';
              fDialog.Filter := 'DAT files|*.dat';
              fDialog.Options := [ofFileMustExist];
              if fDialog.Execute then
              begin
                ts := fDialog.FileName;
                ts := ExtractFileName(ts);
                GameParams.LevelPack := ts;
                TDosCustLevelSystem(GameParams.Style.LevelSystem).LevelPack := ts;
              end;
            end;
        5: begin
              if fExternalPrefix = '' then
                MessageDlg('Choose any GROUNDxO.DAT, VGAGRx.DAT, VGASPECx.DAT' + #13 +
                           'or MAIN.DAT file with the desired prefix.', mtcustom, [mbok], 0)
                else begin
                i := MessageDlg('Do you want to choose a new prefix? If you choose' + #13 +
                                'No, the prefix will be cleared.', mtcustom, [mbYes, mbNo], 0);
                if i = mrNo then
                begin
                  fExternalPrefix := '';
                  BuildScreen;
                  Exit;
                end;
              end;
              fDialog := tOpenDialog.Create(self);
              fDialog.Title := 'Select a prefixed file';
              fDialog.Filter := 'DAT files|*.dat';
              fDialog.Options := [ofFileMustExist];
              if fDialog.Execute then
              begin
                ts := fDialog.FileName;
                ts := ExtractFileName(ts);
                ts2 := '';
                for i := 1 to Length(ts) do
                begin
                  if ts[i] = '_' then break;
                  ts2 := ts2 + ts[i];
                end;
                if i >= Length(ts) then
                begin
                  MessageDlg('This does not appear to be a prefixed file.', mtCustom, [mbok], 0);
                  Exit;
                end;
                GameParams.fExternalPrefix := ts2;
              end;
            end;
        6: begin
             Mechanics := Mechanics + 1;
             if Mechanics > 2 then Mechanics := 0;
           end;
            {$endif}{$endif}
        7: begin
             LemmixTrapBug := not LemmixTrapBug;
           end;
        8: begin
             StateControlEnabled := not StateControlEnabled;
           end;
      end;
    BuildScreen;
  end;

begin
  lChar := UpperCase(Key)[1];

    case lChar of
      '1' : SwitchOption(1);
      '2' : SwitchOption(7);
      '3' : SwitchOption(8);
      '4' : if fLookForLVLOption then SwitchOption(2);
      '5' : if fCustOptions then SwitchOption(6);
      'Q' : if fCustOptions then SwitchOption(4);
      'W' : if fCustOptions then SwitchOption(5);
      'E' : if fTestModeOptions then SwitchOption(3);
    end;
end;

end.
