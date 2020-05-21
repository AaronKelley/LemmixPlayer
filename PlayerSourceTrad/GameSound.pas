{$include lem_directives.inc}

unit GameSound;

interface

uses
  Windows, Classes, Contnrs, SysUtils,
  GameModPlay,
  Dialogs,
  LemTypes;

const
  DEFAULT_CHANNELS = 4;

type
  TAbstractSound = class
  private
    fStream: TMemoryStream;
    fResourceDataType: TLemDataType;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFileName(const aFileName: string);
    procedure LoadFromStream(aStream: TStream);
//    procedure Play; virtual; abstract;
//    procedure Stop; virtual; abstract;
  end;

  TSound = class(TAbstractSound)
  private
  protected
  public
    constructor Create;
//    procedure Play; override;
//    procedure Stop; override;
  end;

  TMusic = class(TAbstractSound)
  private
  protected
  public
    constructor Create;
//    procedure Play; override;
//    procedure Stop; override;
  end;

  TSoundList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSound;
  protected
  public
    function Add(Item: TSound): Integer;
    procedure Insert(Index: Integer; Item: TSound);
    property Items[Index: Integer]: TSound read GetItem; default;
  published
  end;


  TMusicList = class(TObjectList)
  private
    function GetItem(Index: Integer): TMusic;
  protected
  public
    function Add(Item: TMusic): Integer;
    procedure Insert(Index: Integer; Item: TMusic);
    property Items[Index: Integer]: TMusic read GetItem; default;
  published
  end;

  TSoundMgr = class
  private
    fMusics: TMusicList;
    fSounds: TList;

    fAvailableChannels : Integer;
    fBrickSound : Integer;
    fActiveSounds  : array[0..255] of Boolean;
    fPlayingSounds : array[0..255] of Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  { sounds }
    function AddSoundFromFileName(const aFileName: string): Integer;
    procedure PlaySound(Index: Integer);
    procedure StopSound(Index: Integer);
    //procedure MarkChannelFree(handle: HSYNC; channel, data: DWORD; user: POINTER); stdcall;
  { musics }
    function AddMusicFromFileName(const aFileName: string): Integer;
    procedure PlayMusic(Index: Integer);
    procedure StopMusic(Index: Integer);
    procedure CheckFreeChannels;

    property Musics: TMusicList read fMusics;
    property Sounds: TList read fSounds;

    property AvailableChannels: Integer read fAvailableChannels write fAvailableChannels;
    property BrickSound: Integer read fBrickSound write fBrickSound;
  end;

(*  TWavePlayerEx = class(TWavePlayer)
  private
//    Ix: Integer;
  protected
  public
  end;
  *)

  TSoundSystem = class
  private
    fMusicRef : HMUSIC;
    //fWinampPlugin : HPLUGIN;
    //fVgmPlugin : DWORD;

    //fWavePlayer: TWavePlayerEx;//array[0..2] of TWavePlayerEx;
//    procedure WavePlayer_Finished(Sender: TObject);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitWav;
    function PlayWav(S: TMemoryStream): Integer;
    procedure StopWav(S: TMemoryStream);

    procedure PlayMusic(S: TMemoryStream);
    procedure StopMusic(S: TMemoryStream);

  end;

implementation

uses
  MMSystem;

var
  _SoundSystem: TSoundSystem;

function SoundSystem: TSoundSystem;
begin
  if _SoundSystem = nil then
    _SoundSystem := TSoundSystem.Create;
  Result := _SoundSystem;
end;

{ TAbstractSound }

constructor TAbstractSound.Create;
begin
  inherited Create;
  fStream := TMemoryStream.Create;
end;

destructor TAbstractSound.Destroy;
begin
  fStream.Free;
  inherited;
end;

procedure TAbstractSound.LoadFromFileName(const aFileName: string);
{-------------------------------------------------------------------------------
  Loads from file or archive file in resource
-------------------------------------------------------------------------------}
var
  F: TStream;
begin
  {$ifdef external}
      if FileExists(aFileName + '.ogg') then
        F := TFileStream.Create(aFileName + '.ogg', fmOpenRead)
        else if FileExists(aFileName + '.it') then
        F := TFileStream.Create(aFileName + '.it', fmOpenRead)
        else
      {$endif}
  F := CreateDataStream(aFileName, fResourceDataType);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TAbstractSound.LoadFromStream(aStream: TStream);
begin
  fStream.LoadFromStream(aStream);
end;

{ TSound }

constructor TSound.Create;
begin
  inherited Create;
  fResourceDataType := ldtSound;
end;

{ TMusic }

constructor TMusic.Create;
begin
  inherited;
  fResourceDataType := ldtMusic;
end;

(*
procedure TSound.Play;
begin
  SoundSystem.PlayWav(fStream);
end;

procedure TSound.Stop;
begin
  SoundSystem.StopWav(fStream);
end;

procedure TMusic.Play;
begin
  SoundSystem.PlayMusic(fStream);
end;

procedure TMusic.Stop;
begin
  SoundSystem.StopMusic(fStream);
end;
*)

{ TSoundList }

function TSoundList.Add(Item: TSound): Integer;
begin
  Result := inherited Add(Item);
end;

function TSoundList.GetItem(Index: Integer): TSound;
begin
  Result := inherited Get(Index);
end;

procedure TSoundList.Insert(Index: Integer; Item: TSound);
begin
  inherited Insert(Index, Item);
end;

{ TMusicList }

function TMusicList.Add(Item: TMusic): Integer;
begin
  Result := inherited Add(Item);
end;

function TMusicList.GetItem(Index: Integer): TMusic;
begin
  Result := inherited Get(Index);
end;

procedure TMusicList.Insert(Index: Integer; Item: TMusic);
begin
  inherited Insert(Index, Item);
end;

{ TSoundMgr }

function TSoundMgr.AddMusicFromFileName(const aFileName: string): Integer;
var
  M: TMusic;
begin
  M := TMusic.Create;
  Result := Musics.Add(M);
  M.LoadFromFileName(aFileName);
end;

function TSoundMgr.AddSoundFromFileName(const aFileName: string): Integer;
var
  S: TMemoryStream;
begin
  S := TMemoryStream(CreateDataStream(aFileName, ldtSound));
  //S.SaveToFile('test.wav');
  Result := Sounds.Add(Pointer(BASS_SampleLoad(true, S.Memory, 0, S.Size, 65535, 0)));
  //S.LoadFromFileName(aFileName);
end;

constructor TSoundMgr.Create;
var
  i: Integer;
begin
  inherited Create;
  if not BASS_Init(-1, 44100, 0, 0, nil) then
    BASS_Free;
  fSounds := TList.Create;
  fMusics := TMusicList.Create;
  fAvailableChannels := DEFAULT_CHANNELS;
  for i := 0 to 255 do
    fPlayingSounds[i] := -1;
end;

destructor TSoundMgr.Destroy;
begin
  StopSound(0);
  StopMusic(0);
  fSounds.Free;
  fMusics.Free;
  inherited Destroy;
end;

procedure TSoundMgr.CheckFreeChannels;
var
  i : integer;
begin
  for i := 0 to fAvailableChannels-1 do
  begin
    //if fPlayingSounds[i] < 0 then Continue;
    if (BASS_ChannelIsActive(fPlayingSounds[i]) <> BASS_ACTIVE_PLAYING)
    or (BASS_ChannelGetPosition(fPlayingSounds[i], BASS_POS_BYTE) >= BASS_ChannelGetLength(fPlayingSounds[i], BASS_POS_BYTE)) then
    begin
      fActiveSounds[i] := false;
      fPlayingSounds[i] := -1;
      Continue;
    end;
    if BASS_ChannelGetPosition(fPlayingSounds[i], BASS_POS_BYTE) >= BASS_ChannelSeconds2Bytes(fPlayingSounds[i], 0.5) then
      fActiveSounds[i] := false;
  end;
end;

procedure TSoundMgr.PlaySound(Index: Integer);
var
  i{, i2} : Integer;
  c : HCHANNEL;
  LPosBytes : Integer;
begin

  CheckFreeChannels;

  if Index = fBrickSound then
  begin
    if fPlayingSounds[0] >= 0 then
      BASS_ChannelStop(fPlayingSounds[0]);
    c := BASS_SampleGetChannel(Integer(Sounds[Index]), true);
    BASS_ChannelPlay(c, true);
    {BASS_ChannelSetSync(c, BASS_SYNC_POS or BASS_SYNC_MIXTIME, BASS_ChannelSeconds2Bytes(c, 0.5), BASS_MarkChannelFree, @fActiveSounds[0]);
    BASS_ChannelSetSync(c, BASS_SYNC_END or BASS_SYNC_MIXTIME, 0, BASS_MarkChannelFree, @fActiveSounds[0]);
    BASS_ChannelSetSync(c, BASS_SYNC_END or BASS_SYNC_MIXTIME, 0, BASS_WipeChannel, @fPlayingSounds[0]);}
    fPlayingSounds[0] := c;
    fActiveSounds[0] := true;
    Exit;
  end;

  if Index >= 0 then
    if Index < Sounds.Count then
      //Sounds[Index].Play;
      for i := 1 to (fAvailableChannels - 1) do
      begin
        if not fActiveSounds[i] then
        begin
          //c := (Sounds[Index]);
          if fPlayingSounds[i] >= 0 then
            BASS_ChannelStop(fPlayingSounds[i]);
          c := BASS_SampleGetChannel(Integer(Sounds[Index]), true);
          {i2 := BASS_ErrorGetCode;
          if i2 <> 0 then messagedlg(inttostr(i2), mtcustom, [mbok], 0);}
          BASS_ChannelPlay(c, true);
          {BASS_ChannelSetSync(c, BASS_SYNC_POS or BASS_SYNC_MIXTIME, BASS_ChannelSeconds2Bytes(c, 0.5), BASS_MarkChannelFree, @fActiveSounds[i]);
          BASS_ChannelSetSync(c, BASS_SYNC_END or BASS_SYNC_MIXTIME, 0, BASS_MarkChannelFree, @fActiveSounds[i]);
          BASS_ChannelSetSync(c, BASS_SYNC_END or BASS_SYNC_MIXTIME, 0, BASS_WipeChannel, @fPlayingSounds[i]);}
          fPlayingSounds[i] := c;
          fActiveSounds[i] := true;
          Exit;
        end;
      end;
end;

procedure TSoundMgr.PlayMusic(Index: Integer);
begin
  if Index >= 0 then
    if Index < Musics.Count then
    //  Musics[Index].Play;
      SoundSystem.PlayMusic(Musics[Index].fStream)
end;

procedure TSoundMgr.StopSound(Index: Integer);
begin
//  if Index >= 0 then
  //  if Index <
  SoundSystem.StopWav(nil);
end;

procedure TSoundMgr.StopMusic(Index: Integer);
begin
  if Index >= 0 then
    if Index < Musics.Count then
      //Musics[Index].Stop;
      SoundSystem.StopMusic(nil);
end;

{ TSoundSystem }

constructor TSoundSystem.Create;
//var
  //i: Integer;
begin
  inherited Create;
  //if not BASS_Init(-1, 44100, 0, 0, nil) then
  //  BASS_Free;
  InitWav;
end;

destructor TSoundSystem.Destroy;
//var
  //i: Integer;
begin
  BASS_Free;
//  for i := 0 to 2 do
//    fWavePlayer.Free;
  inherited;
end;

procedure TSoundSystem.InitWav;
begin
  PlaySound(nil, HINSTANCE, SND_ASYNC + SND_MEMORY);
end;

procedure TSoundSystem.PlayMusic(S: TMemoryStream);
const
  MusicFlags = BASS_MUSIC_LOOP or BASS_MUSIC_RAMPS or BASS_MUSIC_SURROUND or
               BASS_MUSIC_POSRESET or BASS_SAMPLE_SOFTWARE;
var
  i, v : Integer;
begin
  BASS_StreamFree(fMusicRef);
  BASS_MusicFree(fMusicRef);
  fMusicRef := 0;
  if S.Size > 0 then
    fMusicRef := BASS_StreamCreateFile(true, S.Memory, 0, S.Size, MusicFlags);

  if fMusicRef = 0 then
  begin
    fMusicRef := BASS_MusicLoad(true, S.Memory, 0, S.Size, MusicFlags, 1);
    BASS_ChannelSetAttribute(fMusicRef, BASS_ATTRIB_MUSIC_AMPLIFY, 30);
  end else
    BASS_LoadLoopData(fMusicRef);

  if fMusicRef <> 0 then
    BASS_ChannelPlay(fMusicRef, false);

end;

function TSoundSystem.PlayWav(S: TMemoryStream): Integer;
//var
  //i: Integer;
begin
(*
//  for i := 0 to 2 do
  begin
    with fWavePlayer do
    begin
//      if State = wpPlaying then
  //      Stop;
//      if State <> wpPlaying then
      begin
        Source := S;
        Play;
  //      Exit;
      end;
    end;
  end; *)

  PlaySound(S.Memory, HINSTANCE, SND_ASYNC + SND_MEMORY);

end;

procedure TSoundSystem.StopMusic(S: TMemoryStream);
begin
  BASS_ChannelStop(fMusicRef);
  //BASS_StreamFree(fMusicRef);
end;

procedure TSoundSystem.StopWav(S: TMemoryStream);
begin
  PlaySound(nil, HINSTANCE, SND_ASYNC + SND_MEMORY);
end;

initialization
finalization
  _SoundSystem.Free;
  _SoundSystem := nil;
end.


