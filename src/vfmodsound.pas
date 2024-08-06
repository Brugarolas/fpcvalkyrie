{$INCLUDE valkyrie.inc}
// @abstract(FMOD Sound system for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(June 10, 2009)
//
// Implements an FMOD sound system for Valkyrie
//
// Default behaviour for music is stoping the previous song before
// playing a new one.

unit vfmodsound;

interface

uses Classes, SysUtils, vsound, vrltools, vluaconfig, vfmodlibrary;

// The basic sound class, published as the singleton @link(Sound).
// Should be initialized and disposed via TSystems.
type

{ TFMODSound }

TFMODSound = class(TSound)
       // Initializes the Sound system.
       constructor Create; override;
       // Initializes the Sound system.
       constructor Create( aConfig : TLuaConfig; const aPrefix : AnsiString = 'audio.' );
       // Deinitializes the Sound system.
       destructor Destroy; override;
     protected
       //Last used channel stored for StopSound
       FLastChannel: integer;
       // Open audio device with given parameters
       function OpenDevice( aFrequency : integer; aMaxChannels : Word; aFlags: Cardinal ) : Boolean;
       // Setup DSPs or other global FMOD parameters
       procedure Setup;
       // Implementation of Music Loading
       function LoadMusic( const aFileName : AnsiString; Streamed : Boolean ) : Pointer; override;
       // Implementation of Sound Loading
       function LoadSound( const aFileName : AnsiString ) : Pointer; override;
       // Implementation of Music Loading
       function LoadMusicStream( Stream : TStream; Size : DWord; Streamed : Boolean ) : Pointer; override;
       // Implementation of Sound Loading
       function LoadSoundStream( Stream : TStream; Size : DWord ) : Pointer; override;
       // Implementation of Music Freeing
       procedure FreeMusic( aData : Pointer; const aType : String ); override;
       // Implementation of Sound Freeing
       procedure FreeSound( aData : Pointer ); override;
       // Implementation of get error
       function GetError( ) : AnsiString; override;
       // Implementation of play Sound 3D
       procedure PlaySound3D( aData : Pointer; aRelative : TCoord2D ); override;
       // Implementation of play Sound
       procedure PlaySound( aData : Pointer; aVolume : Byte; aPan : Integer = -1 ); override;
       // Implementation of play Sound
       procedure PlayMusic( aData : Pointer; const aType : string; aRepeat : Boolean = True ); override;
       // Implementation of StopMusic
       procedure StopMusic( aData : Pointer; const aType : string ); override;
       // Implementation of SetLimitSoundVolume
       procedure SetLimitSoundVolume(Enable : Boolean); override;
       // Implementation of StopMusic
       procedure StopSound(); override;	   
       // Implementation of VolumeMusic
       procedure VolumeMusic( aData : Pointer; const aType : string; aVolume : Byte ); override;
     protected
       Limiter      : PFSoundDSPUnit;
       SoundMixer   : PFSoundDSPUnit;
       HighestValue : Array [0..1] of Single;
       Factor       : Single;
     end;

implementation

uses vutil;

{ TFMODSound }

constructor TFMODSound.Create;
begin
  inherited Create;
  LoadFMOD;

  if not OpenDevice(44100, 32, 0) then
  begin
    raise Exception.Create('FMODInit Failed -- '+GetError());
    FSOUND_Close();
  end
  else
    Setup;
end;

constructor TFMODSound.Create(aConfig: TLuaConfig; const aPrefix: AnsiString);
begin
  inherited Create;
  LoadFMOD;
  if not OpenDevice(
    aConfig.Configure( aPrefix + 'frequency', 44100 ),
    aConfig.Configure( aPrefix + 'fmod_mix_channels', 32 ),
    aConfig.Configure( aPrefix + 'fmod_flags', 0 )) then
  begin
    raise Exception.Create('FMODInit Failed -- '+GetError());
    FSOUND_Close();
  end
  else
    Setup;
end;

destructor TFMODSound.Destroy;
begin
  if Limiter <> nil then FSOUND_DSP_Free( Limiter );
  if SoundMixer <> nil then FSOUND_DSP_Free( SoundMixer );
  inherited Destroy;
  if FMOD <> nil then
    FSOUND_Close();
end;

function TFMODSound.OpenDevice(aFrequency: integer; aMaxChannels: Word; aFlags: Cardinal): Boolean;
begin
  Log( LOGINFO, 'Opening FMOD... ( frequency %d, max_channels %d, flags %d )', [ aFrequency, aMaxChannels, aFlags ] );
  if not FSOUND_Init(44100, 32, 0) then
  begin
    Log( LOGERROR, 'FSOUND_Init failed, error : ' + GetError() );
    Exit( False );
  end;
  Log( LOGINFO, 'FMOD Initialized ( %s ).', [ FSOUND_GetDriverName( FSOUND_GetDriver() ) ] );
  Exit( True );
end;

procedure limiter_32i(Sound: TFMODSound; NewBuffer: PInt32; Length: Integer);
var Index     : Integer;
    Component : Integer;
    Sample    : Int32;
    Highest   : Single;
    Threshold : Single;
begin
  Threshold := 96.0 * Sound.SoundVolume;
  
  for Component := 0 to 1 do
  begin
    Highest := Sound.HighestValue[Component];
    for Index := 0 to Length - 1 do
    begin
      Sample := NewBuffer[2 * Index + Component];
      if Abs(Sample) > Highest
        then Highest := Abs(Sample)
        else Highest := Highest * Sound.Factor;
      if Highest > Threshold then
      begin
        NewBuffer[2 * Index + Component] := Round( Sample * (Threshold / Highest) );
      end;
    end;
    Sound.HighestValue[Component] := Highest;
  end;
end;

procedure limiter_32f(Sound: TFMODSound; NewBuffer: PSingle; Length: Integer);
var Index     : Integer;
    Component : Integer;
    Sample    : Single;
    Highest   : Single;
    Threshold : Single;
begin
  Threshold := 96.0 * Sound.SoundVolume;
  
  for Component := 0 to 1 do
  begin
    Highest := Sound.HighestValue[Component];
    for Index := 0 to Length - 1 do
    begin
      Sample := NewBuffer[2 * Index + Component];
      if Abs(Sample) > Highest
        then Highest := Abs(Sample)
        else Highest := Highest * Sound.Factor;
      if Highest > Threshold then
      begin
        NewBuffer[2 * Index + Component] := Sample * (Threshold / Highest);
      end;
    end;
    Sound.HighestValue[Component] := Highest;
  end;
end;

procedure limiter_16i(Sound: TFMODSound; NewBuffer: PInt16; Length: Integer);
var Index     : Integer;
    Component : Integer;
    Sample    : Int16;
    Highest   : Single;
    Threshold : Single;
begin
  Threshold := 96.0 * Sound.SoundVolume;
  
  for Component := 0 to 1 do
  begin
    Highest := Sound.HighestValue[Component];
    for Index := 0 to Length - 1 do
    begin
      Sample := NewBuffer[2 * Index + Component];
      if Abs(Sample) > Highest
        then Highest := Abs(Sample)
        else Highest := Highest * Sound.Factor;
      if Highest > Threshold then
      begin
        NewBuffer[2 * Index + Component] := Round( Sample * (Threshold / Highest) );
      end;
    end;
    Sound.HighestValue[Component] := Highest;
  end;
end;

function limiter_callback(OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer; stdcall;
var PSound : ^TFMODSound;
    Sound  : TFMODSound;
begin
  PSound := Pointer(Param);
  Sound := PSound^;
  case FSOUND_GetMixer() of
    FSOUND_MIXER_BLENDMODE:
      limiter_32i(Sound, NewBuffer, Length);
    FSOUND_MIXER_QUALITY_FPU:
      limiter_32f(Sound, NewBuffer, Length);
    FSOUND_MIXER_MMXP5,
    FSOUND_MIXER_MMXP6,
    FSOUND_MIXER_QUALITY_MMXP5,
    FSOUND_MIXER_QUALITY_MMXP6:
      limiter_16i(Sound, NewBuffer, Length);
  end;
  Exit( NewBuffer );
end;

procedure TFMODSound.Setup;
begin
  Log( LOGINFO, 'FMOD Mixer Type %d', [ FSOUND_GetMixer() ] );
  Limiter := FSOUND_DSP_Create( @limiter_callback, FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT - 1, Integer(@Self) );
  if Limiter <> nil then
    FSOUND_DSP_SetActive( Limiter, True );
  HighestValue[0] := 0;
  HighestValue[1] := 0;
  Factor := 0.9999; // if different frequencies are allowed, this must be adjusted based on frequency
  SoundMixer := FSOUND_DSP_Create( nil, FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT - 2, 0 );
  if SoundMixer <> nil then
    FSOUND_DSP_SetActive( SoundMixer, True );
end;

function TFMODSound.LoadMusic(const aFileName: AnsiString; Streamed : Boolean): Pointer;
begin
  if Streamed then
    Exit( FSOUND_Stream_Open( PChar( aFileName ),FSOUND_LOOP_NORMAL or FSOUND_NORMAL,0,0) )
  else
    Exit( FMUSIC_LoadSong( PChar( aFileName ) ) );
end;

function TFMODSound.LoadSound(const aFileName: AnsiString): Pointer;
begin
  if FSurroundEnabled
    then Exit( FSOUND_Sample_Load( FSOUND_UNMANAGED, PChar(aFileName), FSOUND_HW3D or FSOUND_FORCEMONO, 0, 0) )
    else Exit( FSOUND_Sample_Load( FSOUND_UNMANAGED, PChar(aFileName), 0, 0, 0) );
end;

function TFMODSound.LoadMusicStream(Stream: TStream; Size : DWord; Streamed : Boolean ): Pointer;
var Data   : Pointer;
    Unused : Integer;
begin
  if Streamed then
  begin
    Data := GetCacheMem( Size );
    Stream.Read( Data^, Size );
    Exit( FSOUND_Stream_Open( PChar( Data ), FSOUND_LOADMEMORY or FSOUND_LOOP_NORMAL or FSOUND_NORMAL,0,Size) )
  end
  else
  begin
    Data := GetMem( Size );
    Stream.Read( Data^, Size );
    Unused := 0;
    LoadMusicStream := FMUSIC_LoadSongEx( PChar( Data ), 0, Size, FSOUND_2D or FSOUND_LOADMEMORY, Unused, 0 );
    FreeMem( Data, Size );
  end;
end;

function TFMODSound.LoadSoundStream(Stream: TStream; Size : DWord ): Pointer;
var Data : Pointer;
begin
  Data := GetMem( Size );
  Stream.Read( Data^, Size );
  LoadSoundStream := FSOUND_Sample_Load( FSOUND_UNMANAGED, PChar( Data ), FSOUND_2D or FSOUND_LOADMEMORY, 0, Size);
  FreeMem( Data, Size );
end;

procedure TFMODSound.FreeMusic( aData: Pointer; const aType : String );
begin
  if ( aType = '.mp3' ) or ( aType = '.ogg' ) or ( aType = '.wav' ) then
    FSOUND_Stream_Close(PFSoundStream(aData))
  else
    FMUSIC_FreeSong(PFMusicModule(aData));
end;

procedure TFMODSound.FreeSound(aData: Pointer);
begin
  FSOUND_Sample_Free(PFSoundSample(aData));
end;

function TFMODSound.GetError(): AnsiString;
var iError : AnsiString;
begin
  iError := FMOD_ErrorString(FSOUND_GetError());
  Exit( iError );
end;

procedure TFMODSound.PlaySound3D(aData: Pointer; aRelative: TCoord2D);
var iChannel   : Integer;
    iPos, iVel : TFSoundVector;
begin
  iVel.x := 0;           iVel.y := 0;           iVel.z := 0;
  FSOUND_3D_Listener_SetAttributes( @iVel, nil, -1, 0, 0, 0, 0, 1 );
  iChannel := FSOUND_PlaySoundEx( FSOUND_FREE, PFSoundSample(aData), SoundMixer, True );
  FLastChannel := iChannel;
  iPos.x := aRelative.X*0.1; iPos.y := aRelative.Y*0.1; iPos.z := 0;
  FSOUND_3D_SetMinMaxDistance( iChannel, 1, 100000 );
  FSOUND_SetVolume( iChannel, SoundVolume );
  FSOUND_3D_SetAttributes( iChannel, @iPos, nil );
  FSOUND_Update();
  FSOUND_SetPaused( iChannel, False );
end;

procedure TFMODSound.PlaySound(aData: Pointer; aVolume: Byte; aPan: Integer);
var iChannel : Integer;
begin
  iChannel := FSOUND_PlaySoundEx( FSOUND_FREE, PFSoundSample(aData), SoundMixer, False );
  fLastChannel := iChannel;
  FSOUND_SetVolume( iChannel, aVolume );
  if aPan = -1 then
    FSOUND_SetPan(iChannel,127)
  else
    FSOUND_SetPan(iChannel,aPan);
end;

procedure TFMODSound.PlayMusic(aData: Pointer; const aType : string; aRepeat: Boolean);
begin
  if ( aType = '.mp3' ) or ( aType = '.ogg' ) or ( aType = '.wav' ) then
  begin
    FSOUND_Stream_Play( 31, PFSoundStream(aData) );
    if not aRepeat then FSOUND_Stream_SetLoopCount(PFSoundStream(aData),0);
    FSOUND_SetVolume(31, MusicVolume);
  end
  else
  begin
    FMUSIC_SetLooping(PFMusicModule(aData),aRepeat);
    FMUSIC_PlaySong(PFMusicModule(aData));
    FMUSIC_SetMasterVolume(aData,MusicVolume);
  end;
end;

procedure TFMODSound.SetLimitSoundVolume(Enable: Boolean);
begin
  if Limiter <> nil then
    FSOUND_DSP_SetActive( Limiter, Enable );
end;

procedure TFMODSound.StopMusic(aData: Pointer; const aType : string );
begin
  if ( aType = '.mp3' ) or ( aType = '.ogg' ) or ( aType = '.wav' ) then
    FSOUND_Stream_Stop(PFSoundStream(aData))
  else
    FMUSIC_StopSong(PFMusicModule(aData));
end;

procedure TFMODSound.StopSound();
begin
  FSOUND_StopSound(fLastChannel);
 end;

procedure TFMODSound.VolumeMusic(aData: Pointer; const aType : string; aVolume: Byte );
begin
  if ( aType = '.mp3' ) or ( aType = '.ogg' ) or ( aType = '.wav' ) then
    FMUSIC_SetMasterVolume(aData, aVolume)
  else
    FSOUND_SetVolume(31, aVolume);
end;

end.
