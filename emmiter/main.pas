{
  Internet Radio emmiter
  Copyright (C) 2023  Ilya Medvedkov

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, fpjson, jsonparser,

  wccurlclient, WCCurlClientControls,

  OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes, OGLRIFFWaveWrapper,
  PlayerControls,

  OGLOpusWrapper, OGLFLACWrapper;

type

  { TMainForm }

  TMainForm = class(TForm)
    AuthToServerBtn : TToolButton;
    ImageList1 : TImageList;
    LogMemo : TMemo;
    OpenDialog1 : TOpenDialog;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel8 : TPanel;
    Splitter1 : TSplitter;
    Timer1 : TTimer;
    TaskTimer : TTimer;
    ToolBar1 : TToolBar;
    ToolBar2 : TToolBar;
    AddTrackBtn : TToolButton;
    DeleteTrackBtn : TToolButton;
    PlayButton : TToolButton;
    StopButton : TToolButton;
    procedure AddTrackBtnClick(Sender : TObject);
    procedure AuthToServerBtnClick(Sender : TObject);
    procedure DeleteTrackBtnClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure PlayButtonClick(Sender : TObject);
    procedure StopButtonClick(Sender : TObject);
    procedure TaskTimerTimer(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
  private
    CURLClient : TWCCURLClient;

    AppConfig  : TWCClientPropStorage;
    AuthOpts   : TWCClientConfigEditor;
    PLGrid     : TPlayListGrid;

    FOutStream     : ISoundStreamEncoder;
    FResampler     : ISoundResampler;
    FBuffer        : Pointer;
    FFrameSize     : ISoundFrameSize;
    FFrameDuration : Integer;
    FEncFrameSize  : ISoundFrameSize;
    FEncProps      : ISoundEncoderProps;
    FEncBuffer     : Pointer;

    FTracks   : TSLPlayList;
    FComments : IRIFFComment;

    FStartStamp,
      FCurStamp,
      FLstStamp : QWord;
    FSendedTime : QWord;
    FCommentsDelay : QWord;
    FDelay : Int64;
    FIsFirstFrame : Boolean;
    NeedNewFrame : Boolean;

    procedure OnInitCURL(Sender : TObject);
    procedure OnConnectedChanged(aValue : Boolean);
    procedure OnDisconnect(Sender : TObject);
    procedure AddLog(const aStr : String);
    procedure OnSetSID(const AValue : String);
    procedure OnSuccessAuth({%H-}aTask : THTTP2BackgroundTask; {%H-}aObj : TJSONObject);
    procedure OnAfterLaunchInOutStream(Sender : THTTP2BackgroundTask);
    procedure OnSynchroUpdateTask({%H-}aTask : THTTP2BackgroundTask);
    procedure OnSuccessIOStream(aTask : THTTP2BackgroundTask);

    procedure InitializeSoundFile;
    procedure GenNewAudioFrame;
    procedure FinalizeSoundFile;

    function IsTrackInvalid : Boolean;
    function IsTrackValid : Boolean;
  public
    procedure StartStreaming;
    function NextTrack : Boolean;
    procedure StopStreaming;
  end;

var
  MainForm : TMainForm;

implementation

uses LazUTF8, wcwebcamconsts;

{.$define USE_FLAC_CODEC}

const TIME_BUFFERING  = 3000; // 3 sec buffering
      COMMENT_FREQ    = 1000; // send comments every 1 second

      RESAMPLER_QUALITY     = 5;      // quality for speex resampler
      CODEC_LOSSY_QUALITY   = 0.5;    // codec quality value
      CODEC_LOSSY_BITRATE   = 96000;  // bitrate value for lossy codecs
      CODEC_LOSSY_MODE      = oemVBR; // encoder mode for lossy codecs
      CODEC_FLAC_COMPR      = fclLevel5; // compression mode for flac codec

      {$ifdef USE_FLAC_CODEC}
      STREAMING_CODEC = TSoundLite.CODEC_FLAC; // opus as alternative
      {$else}
      STREAMING_CODEC = TSoundLite.CODEC_OPUS; // flac as alternative
      {$endif}

      STREAMING_FREQ  = 48000;
      STREAMING_CHANNELS = 2;
      STREAMING_BITDEPTH = 16;

{$ifdef Windows}
const sLibsPath = '..\libs\';
{$endif}

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender : TObject);
begin
  {$ifdef Windows}
  TSoundLite.SetLibPath(sLibsPath, [slcOGG, slcFLAC, slcOpus, slcVorbis]);
  TSoundLite.SetLibNames(['libogg-0.dll'], true, slcOGG);
  TSoundLite.SetLibNames(['libFLAC-8.dll'], true, slcFLAC);
  TSoundLite.SetLibNames(['libopus-0.dll',
                          'libopusenc-0.dll',
                          'libopusfile-0.dll'], true, slcOpus);
  TSoundLite.SetLibNames(['libvorbis-0.dll',
                          'libvorbisenc-2.dll',
                          'libvorbisfile-3.dll'], true, slcVorbis);
  {$endif}
  if TSoundLite.InitSoundLite([slcOGG, slcVorbis, slcOpus, slcFLAC]) then
  begin
    CURLClient := TWCCURLClient.Create;

    CURLClient.OnInitCURL := @OnInitCURL;
    CURLClient.OnConnectedChanged := @OnConnectedChanged;
    CURLClient.OnDisconnect := @OnDisconnect;
    CURLClient.OnAddLog := @AddLog;
    CURLClient.OnSuccessAuth := @OnSuccessAuth;
    CURLClient.OnAfterLaunchInStream := @OnAfterLaunchInOutStream;
    CURLClient.OnAfterLaunchOutStream := @OnAfterLaunchInOutStream;
    CURLClient.OnSIDSetted := @OnSetSID;
    CURLClient.OnSynchroUpdateTask := @OnSynchroUpdateTask;
    CURLClient.OnSuccessIOStream := @OnSuccessIOStream;

    AppConfig := TWCClientPropStorage.Create(Self);
    AppConfig.JSONFileName := 'config.json';
    AppConfig.Defaults[AppConfig.DEVICE_POS] := 'radio_emitter_lite_sound';

    AuthOpts := TWCClientConfigEditor.Create(Panel8);
    AuthOpts.Parent := Panel8;
    AuthOpts.Top := 64;
    AuthOpts.Props := AppConfig;
    AuthOpts.CURLClient := CURLClient;
    AuthOpts.Align := alClient;
    AuthOpts.Apply;

    FTracks := TSLPlayList.Create;
    FTracks.AddFromFile('testing.flac');
    FTracks.PlayRepeat := true;

    PLGrid := TPlayListGrid.Create(Panel2);
    PLGrid.Parent := Panel2;
    PLGrid.PlayList := FTracks;
    PLGrid.ImageList := ImageList1;
    PLGrid.ImageIndex[slsPlaying] := 4;
    PLGrid.ImageIndex[slsPaused]  := 5;
    PLGrid.ImageIndex[slsStopped] := 6;
    PLGrid.Top := 64;
    PLGrid.Align := alClient;
    PLGrid.UpdatePlayList;

    AuthToServerBtn.Enabled := True;
    AuthToServerBtn.ImageIndex := 1;

    CURLClient.Start;
  end else
  begin
    AuthToServerBtn.Enabled := False;
    AuthOpts := nil;
    AppConfig := nil;
  end;
  PlayButton.Enabled := False;
  StopButton.Enabled := False;
  NeedNewFrame := true;
  FFrameDuration := ((Timer1.Interval * 2) div 120) * 120;

  with TSoundLite do
  FEncProps := TOGLSound.EncProps([PROP_CHANNELS, STREAMING_CHANNELS,
                                   PROP_FREQUENCY, STREAMING_FREQ,
                                   PROP_SAMPLE_SIZE, TOGLSound.BitdepthToSampleSize(STREAMING_BITDEPTH),
                                   // lossy encoder props
                                   ENC_PROP_QUALITY, CODEC_LOSSY_QUALITY,
                                   ENC_PROP_BITRATE, CODEC_LOSSY_BITRATE,
                                   ENC_PROP_MODE,    CODEC_LOSSY_MODE,
                                   // opus encoder props
                                   ENC_PROP_OPUS_HEADER_TYPE, ophSimple,
                                   ENC_PROP_OPUS_DECISION_DELAY, 0,
                                   // flac encoder props
                                   ENC_PROP_FLAC_COMPR_LEVEL, CODEC_FLAC_COMPR,
                                   ENC_PROP_FLAC_SUBSET, true]);
  FEncFrameSize := TOGLSound.FrameFromDuration(FEncProps.Frequency,
                                               FEncProps.Channels,
                                               FEncProps.SampleSize,
                                               FFrameDuration);
  FComments  := nil;
  FEncBuffer := GetMem(FEncFrameSize.AsBytes + 1024);
end;

procedure TMainForm.DeleteTrackBtnClick(Sender : TObject);
begin
  if PLGrid.RowTrack >= 0 then
    PLGrid.DeleteTrack(PLGrid.RowTrack);

  if IsTrackInvalid then
    StopStreaming;
end;

procedure TMainForm.AuthToServerBtnClick(Sender : TObject);
begin
  if CURLClient.Connected then
  begin
    CURLClient.Disconnect;
  end else
  begin
    CURLClient.Authorize(AuthOpts.UserName, AuthOpts.Password);
    TaskTimer.Enabled := True;
  end;
end;

procedure TMainForm.AddTrackBtnClick(Sender : TObject);
begin
  if OpenDialog1.Execute then
  begin
    FTracks.AddFromFile(OpenDialog1.FileName);
    PLGrid.UpdatePlayList;
  end;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  if assigned(AuthOpts) then
    AuthOpts.SaveProps;

  Freemem(FEncBuffer);

  Timer1.Enabled := false;
  TaskTimer.Enabled := false;

  CURLClient.Free;

  FinalizeSoundFile;

  FTracks.Free;

  TSoundLite.DoneSoundLite;
end;

procedure TMainForm.PlayButtonClick(Sender : TObject);
begin
  StartStreaming;
end;

procedure TMainForm.StopButtonClick(Sender : TObject);
begin
  StopStreaming;
end;

procedure TMainForm.TaskTimerTimer(Sender : TObject);
begin
  CURLClient.TasksProceed;

  if CURLClient.Connected then
  begin
    if CURLClient.IsStreaming then
    begin
      if NeedNewFrame then
        GenNewAudioFrame;

      if PLGrid.PlayState <> slsPlaying then
      begin
        PLGrid.PlayState := slsPlaying;
        PlayButton.Enabled := False;
        StopButton.Enabled := True;
        PLGrid.UpdatePlayList;
      end;
    end else
    begin
      if PLGrid.PlayState <> slsStopped then
      begin
        PLGrid.PlayState := slsStopped;
        PlayButton.Enabled := True;
        StopButton.Enabled := False;
        PLGrid.UpdatePlayList;
      end;
    end;
  end;
end;

procedure TMainForm.Timer1Timer(Sender : TObject);
begin
  if CURLClient.Connected then
    CURLClient.Proceed;
end;

procedure TMainForm.OnInitCURL(Sender : TObject);
begin
  NeedNewFrame := true;
  TaskTimer.Enabled := true;
end;

procedure TMainForm.OnConnectedChanged(aValue : Boolean);
begin
  if aValue then
  begin
    AuthToServerBtn.ImageIndex := 0;
  end else
  begin
    AuthToServerBtn.ImageIndex := 1;
    PlayButton.Enabled := False;
    StopButton.Enabled := False;
    CURLClient.IsStreaming := False;
    PLGrid.PlayState := slsInitial;

    PLGrid.UpdatePlayList;
  end;
end;

procedure TMainForm.OnDisconnect(Sender : TObject);
begin
  Timer1.Enabled := False;
  TaskTimer.Enabled := False;
  OnConnectedChanged(False);
end;

procedure TMainForm.AddLog(const aStr : String);
begin
  LogMemo.Lines.Add('['+DateTimeToStr(Now)+'] '+aStr);
  CURLClient.Log.Clear;
end;

procedure TMainForm.OnSetSID(const AValue : String);
begin
  AuthOpts.SID := AValue;
end;

procedure TMainForm.OnSuccessAuth(aTask : THTTP2BackgroundTask;
  aObj : TJSONObject);
begin
  Timer1.Enabled := True;
end;

procedure TMainForm.OnAfterLaunchInOutStream(Sender : THTTP2BackgroundTask);
var
  Tsk : THTTP2BackgroundTask;
begin
  Tsk := THTTP2BackgroundTask(Sender);
  Tsk.Data := Self;
end;

procedure TMainForm.OnSynchroUpdateTask(aTask : THTTP2BackgroundTask);
begin
  NeedNewFrame := True;
end;

procedure TMainForm.OnSuccessIOStream(aTask : THTTP2BackgroundTask);
begin
  FinalizeSoundFile;
end;

procedure TMainForm.InitializeSoundFile;
begin
  with FTracks.CurrentTrack do
  begin
    Activate;
    ResetToStart;
    FIsFirstFrame := true;
    FComments := TWAVE.NewComment(FileInfo.Comments) as IRIFFComment;
    FFrameSize := FileInfo.Codec.FrameFromDuration(FFrameDuration);
    if Assigned(FBuffer) then FreeMemAndNil(FBuffer);
    FBuffer := GetMem(FFrameSize.AsBytes);
    if (FFrameSize.Frequency <> STREAMING_FREQ) or
       (FFrameSize.Bitdepth  <> STREAMING_BITDEPTH) then
    begin
      FResampler := TSoundLite.NewSpeexResampler(FEncFrameSize,
                                                 FFrameSize.Frequency,
                                                 RESAMPLER_QUALITY);
    end else
      FResampler := nil;
  end;
end;

procedure TMainForm.GenNewAudioFrame;
var fmem, cmem : TCustomMemoryStream;
    sz : Cardinal;
    len, rlen : ISoundFrameSize;
    CHEAD : Cardinal;
    wbuf : Pointer;
    DeltaTime : Int64;
    sendComments : Boolean;
begin
  FLstStamp := FCurStamp;
  FCurStamp := GetTickCount64;

  DeltaTime := Int64(FCurStamp - FLstStamp);

  Inc(FCommentsDelay, DeltaTime);

  if FDelay > 0 then
  begin
    Dec(FDelay, DeltaTime);
    Exit;
  end;

  if (Int64(FSendedTime) - Int64(FCurStamp - FStartStamp)) > TIME_BUFFERING then
    FDelay := TIME_BUFFERING div 2;

  if IsTrackValid then
  begin
    len := FTracks.CurrentTrack.ReadData(FBuffer, FFrameSize, nil);
    if len.IsValid then
    begin
      NeedNewFrame := false;

      fmem := TMemoryStream.Create;
      fmem.Write(WEBCAM_FRAME_START_SEQ, sizeof(word));
      Sz := 0;
      fmem.Write(Sz, sizeof(Sz));

      sendComments := (FCommentsDelay > COMMENT_FREQ) or FIsFirstFrame;

      {
      Header is 4 bytes (32 bit)

       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      |N|F|C|1|2| BITDEPTH  |  NCH  |             FREQUENCY           |

      N - this frame is first for current track (1 bit = true/false)
      F - reserved (1 bit)
      C - this frame contains RIFF-style comments (1 bit = true/false)
      1 - reserver (1 bit)
      2 - reserver (1 bit)
      BITDEPTH  - the value of bitdepth for stream (6 bit)
      NCH       - the number of channels for stream (4 bit)
      FREQUENCY - the sample rate for stream in Hz (17 bit)
      }

      CHEAD := Cardinal(STREAMING_FREQ and $1FFFF) or
               Cardinal((Byte(STREAMING_CHANNELS) and $F) shl 17) or
               Cardinal((Byte(STREAMING_BITDEPTH) and $3F) shl 21) or
               Cardinal((Byte(sendComments) and $1) shl 29) or
               Cardinal((Byte(FIsFirstFrame) and $1) shl 31);

      fmem.Write(CHEAD, Sizeof(CHEAD));

      if sendComments then
      begin
        cmem := TMemoryStream.Create;
        try
          FComments.WriteToMemory(cmem);
          fmem.Write(cmem.Memory^, cmem.Size);
        finally
          cmem.Free;
        end;
        FCommentsDelay := 0;
      end;

      if Assigned(FOutStream) then
        FOutStream.SetStream(fmem) else
        FOutStream := TSoundLite.NewStreamEncoder(STREAMING_CODEC, fmem, FEncProps);

      if Assigned(FResampler) then
      begin
        rlen := FResampler.WriteInterleave(FBuffer, len);
        wbuf := FResampler.OutBuffer;
      end else
      begin
        wbuf := FBuffer;
        rlen := len;
      end;
      FOutStream.WriteData(wbuf, rlen, nil);
      FOutStream.Flush(nil);

      Sz := fmem.Size - WEBCAM_FRAME_HEADER_SIZE;
      fmem.Position := sizeof(word);
      fmem.Write(Sz, sizeof(Sz));
      fmem.Position := 0;

      Inc(FSendedTime, Trunc(rlen.AsDurationMs));

      CURLClient.Frame.NextFrame(fmem);

      FIsFirstFrame := false;
    end else
    begin
      if NextTrack then
        GenNewAudioFrame else
        StopStreaming;
    end;
  end;
end;

procedure TMainForm.FinalizeSoundFile;
begin
  FOutStream := nil;
  FFrameSize := nil;
  FComments  := nil;
  FResampler := nil;
  if Assigned(FBuffer) then
    FreeMemAndNil(FBuffer);
end;

function TMainForm.IsTrackInvalid : Boolean;
begin
  Result := not Assigned(FTracks.CurrentTrack);
end;

function TMainForm.IsTrackValid : Boolean;
begin
  Result := Assigned(FTracks.CurrentTrack);
end;

procedure TMainForm.StartStreaming;
begin
  FStartStamp := GetTickCount64;
  FCurStamp := FStartStamp;
  FSendedTime := 0;
  FCommentsDelay := COMMENT_FREQ;
  FTracks.PlayPosition := PLGrid.RowTrack;
  if IsTrackValid then
  begin
    InitializeSoundFile;
    CURLClient.LaunchOutStream(Format('RAW_%s_FRQ_%d_CHN_%d_BIT_%d',
                               [TSoundLite.CodecNameShrt(STREAMING_CODEC),
                                STREAMING_FREQ,
                                STREAMING_CHANNELS,
                                STREAMING_BITDEPTH]),
                               Timer1.Interval);
    PlayButton.Enabled := False;
  end;
end;

function TMainForm.NextTrack : Boolean;
begin
  if IsTrackValid then
    FTracks.CurrentTrack.Active := False;

  FTracks.Next;

  if IsTrackValid then
  begin
    InitializeSoundFile;
    Result := True;
  end else
    Result := False;

  PLGrid.UpdatePlayList;
end;

procedure TMainForm.StopStreaming;
begin
  StopButton.Enabled := False;
  CURLClient.IsStreaming := False;
end;

end.

