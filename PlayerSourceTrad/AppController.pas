{$include lem_directives.inc}
unit AppController;

interface

uses
  Classes, SysUtils, UMisc, //typinfo,
  Forms,
  LemTypes, LemRendering, LemLevel, LemDosStyle, LemDosGraphicSet,
  {$ifdef flexi}LemDosMainDAT,{$endif}
  FBaseDosForm,
  GameControl;

type
  {-------------------------------------------------------------------------------
    The main application screen logic is handled by this class.
    it's a kind of simple statemachine, which shows the appropriate screens.
    These screens must change the GameParams.NextScreen property, when closing.
  -------------------------------------------------------------------------------}
  TAppController = class(TComponent)
  private
    //fCurrentScreen: TBaseDosForm;
    fGameParams: TDosGameParams; // instance
  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowMenuScreen;
    procedure ShowPreviewScreen;
    procedure ShowPlayScreen;
    procedure ShowPostviewScreen;
    procedure ShowLevelCodeScreen;
    procedure ShowOptionsScreen;
    procedure Execute;
  end;

implementation

uses
  GameMenuScreen,
  GameLevelCodeScreen,
  GamePreviewScreen,
  GamePostviewScreen,
  GameConfigScreen,
  GameWindow;

{ TAppController }

constructor TAppController.Create(aOwner: TComponent);
var
  Fn: string;
  {$ifdef flexi}fMainDatExtractor : TMainDatExtractor;{$endif}
  tsx: byte;
begin
  inherited;
  fGameParams := TDosGameParams.Create;
  fGameParams.Directory := LemmingsPath;
  fGameParams.MainDatFile := LemmingsPath + 'main.dat';
  fGameParams.Renderer := TRenderer.Create;
  fGameParams.Level := Tlevel.Create(nil);

  {$ifdef flexi}fMainDatExtractor := TMainDatExtractor.Create;
  fMainDatExtractor.FileName := LemmingsPath + 'main.dat';
  fGameParams.SysDat := fMainDatExtractor.GetSysData;
  fMainDatExtractor.free;
  {$endif}

  fGameParams.Style := AutoCreateStyle(fGameParams.Directory{$ifdef flexi}, fGameParams.SysDat{$endif});
//  fGameParams.Style.LevelSystem as TBaseDosLevelSystem
  fGameParams.GraphicSet := fGameParams.Style.CreateGraphicSet as TBaseDosGraphicSet;
  fGameParams.NextScreen := gstMenu;
  {$ifdef testmode}
  if ParamStr(1) = 'testmode' then
  begin
    fGameParams.fTestMode := true;
    fGameParams.fTestLevelFile := ExtractFilePath(Application.ExeName) + ParamStr(2);
    fGameParams.fTestGroundFile := ExtractFilePath(Application.ExeName) + ParamStr(3);
    fGameParams.fTestVgagrFile := ExtractFilePath(Application.ExeName) + ParamStr(4);
    fGameParams.fTestVgaspecFile := ExtractFilePath(Application.ExeName) + ParamStr(5);
    fGameParams.NextScreen := gstPreview;
  end;
  {$endif}
  fGameParams.SoundOptions := [gsoSound, gsoMusic];

  Fn := ReplaceFileExt(Application.Exename, '.ini');//' AppPath + 'LemmixPlayer.ini';
  if FileExists(Fn) then
    fGameParams.LoadFromIniFile(Fn);

  if fGameParams.Style.LevelSystem is TBaseDosLevelSystem then
    begin
    TBaseDosLevelSystem(fGameParams.Style.LevelSystem).LookForLVL := fGameParams.LookForLVLFiles;
    TBaseDosLevelSystem(fGameParams.Style.LevelSystem).LevelPack := fGameParams.LevelPack;
    {$ifdef testmode}TBaseDosLevelSystem(fGameParams.Style.LevelSystem).fTestMode := fGameParams.fTestMode;
    TBaseDosLevelSystem(fGameParams.Style.LevelSystem).fTestLevel := fGameParams.fTestLevelFile;{$endif}
    {$ifdef flexi}TBaseDosLevelSystem(fGameParams.Style.LevelSystem).SysDat := fGameParams.SysDat;
    TDosCustLevelSystem(fGameParams.Style.LevelSystem).SysDat := fGameParams.SysDat;
    TDosCustMusicSystem(fGameParams.Style.MusicSystem).MusicCount := fGameParams.SysDat.TrackCount;{$endif}
    end;

  {$ifdef flexi}
  tsx := fGameParams.SysDat.Options;
  if tsx and 1 = 0 then fGameParams.LookForLVLFiles := false;
  if tsx and 2 = 0 then fGameParams.CheatCodesEnabled := false;
  {if tsx and 32 = 0 then
  begin
    fGameParams.DebugSteel := false;
    fGameParams.ChallengeMode := false;
    fGameParams.TimerMode := false;
    fGameParams.GimmickMusic := true;
    fGameParams.ForceGimmick := '0000';
  end;}
  {$endif}

end;

destructor TAppController.Destroy;
var
  Fn: string;
begin
  Fn := ReplaceFileExt(Application.Exename, '.ini');//' AppPath + 'LemmixPlayer.ini';
//  Fn := AppPath + 'LemmixPlayer.ini';
  fGameParams.SaveToIniFile(Fn);

  fGameParams.Renderer.Free;
  fGameParams.Level.Free;
  fGameParams.Style.Free;
  fGameParams.GraphicSet.Free;
  fGameParams.Free;
  inherited;
end;

procedure TAppController.Execute;
{-------------------------------------------------------------------------------
  Main screen-loop. Every screen returns its nextscreen (if he knows) in the
  GameParams
-------------------------------------------------------------------------------}
var
  NewScreen: TGameScreenType;
begin
  while fGameParams.NextScreen <> gstExit do
  begin
    NewScreen := fGameParams.NextScreen; // save
    fGameParams.NextScreen := gstUnknown; // reset

    case NewScreen of
      gstMenu      : ShowMenuScreen;
      gstPreview   : ShowPreviewScreen;
      gstPlay      : ShowPlayScreen;
      gstPostview  : ShowPostviewScreen;
      gstNavigation: ShowOptionsScreen;
      gstLevelCode : ShowLevelCodeScreen;
    else
      Exit;
    end;
  end;
end;

procedure TAppController.ShowLevelCodeScreen;
var
  F: TGameLevelCodeScreen;
begin
  F := TGameLevelCodeScreen.Create(nil);
  try
    F.ShowScreen(fGameParams);
  finally
    F.Free;
  end;
end;

procedure TAppController.ShowMenuScreen;
var
  F: TGameMenuScreen;
begin
  F := TGameMenuScreen.Create(nil);
  try
    F.ShowScreen(fGameParams);
  finally
    F.Free;
  end;
end;

procedure TAppController.ShowOptionsScreen;
var
  F: TGameConfigScreen;
begin
  F := TGameConfigScreen.Create(nil);
  try
    F.ShowScreen(fGameParams);
  finally
    F.Free;
  end;
end;

procedure TAppController.ShowPlayScreen;
var
  F: TGameWindow;
begin
  F := TGameWindow.Create(nil);
  try
    F.ShowScreen(fGameParams);
  finally
    F.Free;
  end;
end;

procedure TAppController.ShowPostviewScreen;
var
  F: TGamePostviewScreen;
begin
  F := TGamePostviewScreen.Create(nil);
  try
    F.ShowScreen(fGameParams);
  finally
    F.Free;
  end;
end;

procedure TAppController.ShowPreviewScreen;
var
  F: TGamePreviewScreen;
  dS, dL: Integer;
begin

  F := TGamePreviewScreen.Create(nil);
  try
    {$ifdef testmode}
    if (fGameParams.fTestMode and (fGameParams.QuickTestMode <> 0)) then
    begin
      F.PrepareGameParams(fGameParams);
      F.BuildScreenInvis;
      fGameParams.NextScreen := gstPlay;
    end else{$endif}
    if fGameParams.DumpMode then
    begin
      for dS := 0 to TBaseDosLevelSystem(fGameParams.Style.LevelSystem).fDefaultSectionCount - 1 do
      for dL := 0 to TBaseDosLevelSystem(fGameParams.Style.LevelSystem).GetLevelCount(dS) - 1 {$ifdef cust}- 1{$endif} do
      begin
        fGameParams.WhichLevel := wlSame;
        fGameParams.Info.dSection := dS;
        fGameParams.Info.dLevel := dL;
        F.PrepareGameParams(fGameParams);
        F.BuildScreenInvis;
      end;
      fGameParams.WhichLevel := wlFirst;
      fGameParams.NextScreen := gstMenu;
      fGameParams.DumpMode := false;
    end else
    F.ShowScreen(fGameParams);
  finally
    F.Free;
  end;
end;

end.
                  (*
procedure TGameMenuScreen.ShowLevelCodeScreen;
var
  F: TGameLevelCodeScreen;
begin
  F := TGameLevelCodeScreen.Create(nil);
  try
//    F.MainDatFile := Self.MainDatFile;
    F.BuildScreen;
    F.ShowModal;
  finally
    F.Free;
  end;

end;

