{
  Internet Radio listener
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
  ComCtrls, fpjson, jsonparser, wccurlclient, WCCurlClientControls,
  OGLSoundLite, OGLSoundUtils, OGLSoundUtilTypes,
  OGLRIFFWaveWrapper, OGLOpusWrapper,
  PlayerControls,
  ECommonObjs, LCLType, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    AuthToServerBtn : TToolButton;
    DeviceList : TListBox;
    ImageList1 : TImageList;
    Label1 : TLabel;
    Label2 : TLabel;
    FrameLoadedLabel : TLabel;
    Label4 : TLabel;
    FrameDecodedLabel : TLabel;
    Label6 : TLabel;
    TimeCachedLabel : TLabel;
    LogMemo : TMemo;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    Panel4 : TPanel;
    Panel5 : TPanel;
    Panel6 : TPanel;
    Panel7 : TPanel;
    Panel8 : TPanel;
    Panel9 : TPanel;
    PlayButton : TToolButton;
    Splitter1 : TSplitter;
    Splitter2 : TSplitter;
    TaskTimer : TTimer;
    Timer1 : TTimer;
    SlowTimer : TTimer;
    ToolBar1 : TToolBar;
    ToolBar2 : TToolBar;
    procedure AuthToServerBtnClick(Sender : TObject);
    procedure DeviceListDblClick(Sender : TObject);
    procedure DeviceListDrawItem(Control : TWinControl; Index : Integer;
      ARect : TRect; State : TOwnerDrawState);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure PlayButtonClick(Sender : TObject);
    procedure SlowTimerTimer(Sender : TObject);
    procedure TaskTimerTimer(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
  private
    CURLClient : TWCCURLClient;

    AppConfig  : TWCClientPropStorage;
    AuthOpts   : TWCClientConfigEditor;
    FMetaView  : TMetaView;

    FOutFile   : TSLOutputFile;
    FPlayer    : TSLFramedPlayer;
    FStreamingDevices : TStringList;
    FComments  : IRIFFComment;

    FTotalLoaded : Integer;

    procedure OnInitCURL(Sender : TObject);
    procedure OnConnectedChanged(aValue : Boolean);
    procedure OnDisconnect(Sender : TObject);
    procedure OnSetSID(const AValue : String);
    procedure AddLog(const aStr : String);

    procedure OnSuccessAuth({%H-}aTask : THTTP2BackgroundTask; {%H-}aObj : TJSONObject);
    procedure OnAfterLaunchInOutStream(Sender : THTTP2BackgroundTask);
    procedure OnSynchroUpdateTask({%H-}aTask : THTTP2BackgroundTask);
    procedure OnSuccessIOStream(aTask : THTTP2BackgroundTask);
    procedure OnSuccessUpdateStreams(aTask : THTTP2BackgroundTask;
      aArr : TJSONArray);

    procedure ListenCurrent;
    procedure InitializeListener(const aDevice : TJSONObject);
    procedure FinalizeListener;

    procedure UpdateFrame(aFrame : TCustomMemoryStream);
  public

  end;

var
  Form1 : TForm1;

implementation

uses wcwebcamconsts;

{$R *.lfm}

{ TForm1 }

procedure TForm1.AuthToServerBtnClick(Sender : TObject);
begin
  if CURLClient.Connected then
    CURLClient.Disconnect else
    CURLClient.Authorize(AuthOpts.UserName, AuthOpts.Password);
end;

procedure TForm1.DeviceListDblClick(Sender : TObject);
begin
  ListenCurrent;
end;

procedure TForm1.DeviceListDrawItem(Control : TWinControl; Index : Integer;
  ARect : TRect; State : TOwnerDrawState);
var
  B  : TBitmap;
  ts : TTextStyle;
  S  : String;
begin
  S := DeviceList.Items[Index];

  with DeviceList.Canvas do
  begin
    ts := TextStyle;
    ts.Alignment  := taLeftJustify;
    ts.Layout     := tlCenter;
    ts.Wordbreak  := false;
    ts.SingleLine := True;
    ts.Opaque     := false;

    if odSelected in State then
    begin
      brush.Color := clHighlight;
      font.Color := clWhite;
      pen.Color := clYellow;
      pen.Style := psDot;
    end else
    begin
      brush.Color := clWhite;
      font.Color  := clBlack;
      pen.Color   := brush.Color;
      pen.Style   := psSolid;
    end;

    Rectangle(aRect);

    B := TBitmap.Create;
    try
      B.Width := 16;
      B.Height := 16;
      ImageList1.Getbitmap(1, B);
      DeviceList.Canvas.Draw(aRect.Left, aRect.Top + aRect.Height div 2 - 8, B);
    finally
      B.Free;
    end;

    aRect.Left := aRect.Left + 16;

    DeviceList.Canvas.TextRect(aRect, aRect.Left+2, aRect.Top + 2, S, ts);
  end;
end;

procedure TForm1.FormCreate(Sender : TObject);
begin
  Randomize;
  {$ifdef Windows}
  TSoundLite.SetLibPath(sLibsPath, [slcOpenAL, slcOGG, slcFLAC, slcOpus]);
  TSoundLite.SetLibNames(['soft_oal.dll'], true, slcOpenAL);
  TSoundLite.SetLibNames(['libogg-0.dll'], true, slcOGG);
  TSoundLite.SetLibNames(['libFLAC-8.dll'], true, slcFLAC);
  TSoundLite.SetLibNames(['libopus-0.dll',
                          'libopusenc-0.dll',
                          'libopusfile-0.dll'], true, slcOpus);
  {$endif}
  if TSoundLite.InitSoundLite([slcOpenAL, slcOGG, slcOpus, slcFLAC]) then
  begin
    CURLClient := TWCCURLClient.Create;

    CURLClient.OnInitCURL := @OnInitCURL;
    CURLClient.OnConnectedChanged := @OnConnectedChanged;
    CURLClient.OnDisconnect := @OnDisconnect;
    CURLClient.OnAddLog := @AddLog;
    CURLClient.OnSuccessAuth := @OnSuccessAuth;
    CURLClient.OnSIDSetted := @OnSetSID;
    CURLClient.OnSuccessUpdateStreams := @OnSuccessUpdateStreams;
    CURLClient.OnAfterLaunchInStream := @OnAfterLaunchInOutStream;
    CURLClient.OnAfterLaunchOutStream := @OnAfterLaunchInOutStream;
    CURLClient.OnSynchroUpdateTask := @OnSynchroUpdateTask;
    CURLClient.OnSuccessIOStream := @OnSuccessIOStream;

    FStreamingDevices := TStringList.Create;
    FStreamingDevices.OwnsObjects := true;

    FMetaView := TMetaView.Create(Panel5);
    FMetaView.Parent := Panel5;
    FMetaView.Top := 64;
    FMetaView.Align := alClient;

    AppConfig := TWCClientPropStorage.Create(Self);
    AppConfig.JSONFileName := 'config.json';
    AppConfig.Defaults[AppConfig.DEVICE_POS] := 'radio_listener_' + IntToHex(Random($FFFFFF), 6);

    AuthOpts := TWCClientConfigEditor.Create(Panel2);
    AuthOpts.Parent := Panel2;
    AuthOpts.Top := 64;
    AuthOpts.Props := AppConfig;
    AuthOpts.CURLClient := CURLClient;
    AuthOpts.Align := alClient;
    AuthOpts.Apply;

    PlayButton.ImageIndex := 2;
    AuthToServerBtn.ImageIndex := 4;

    CURLClient.Start;

    FPlayer := nil;
  end else
  begin
    AuthToServerBtn.Enabled := False;
    AuthOpts := nil;
    AppConfig := nil;
  end;
  PlayButton.Enabled := false;
  DeviceList.Enabled := false;
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  if assigned(AuthOpts) then
    AuthOpts.SaveProps;

  FinalizeListener;

  if Assigned(FPlayer) then
    FreeAndNil(FPlayer);
  if Assigned(CURLClient) then
    FreeAndNil(CURLClient);
  if Assigned(FStreamingDevices) then
    FreeAndNil(FStreamingDevices);

  FComments := nil;

  TSoundLite.DoneSoundLite;
end;

procedure TForm1.PlayButtonClick(Sender : TObject);
begin
  if CURLClient.IsStreaming then
    FinalizeListener else
    ListenCurrent;
end;

procedure TForm1.SlowTimerTimer(Sender : TObject);
begin
  if CURLClient.Connected then
  begin
    CURLClient.UpdateStreams;
  end;
end;

procedure TForm1.TaskTimerTimer(Sender : TObject);
begin
  CURLClient.TasksProceed;

  if Assigned(FPlayer) then
  begin
    if FPlayer.Status <> slsInvalid then
      FPlayer.FramedSource.Process;

    if FPlayer.Status = slsPlaying then
      FPlayer.Proceed;

    FrameLoadedLabel.Caption := Inttostr(FTotalLoaded);
    if FPlayer.FramedSource.Decoder.TotalDuration.IsValid then
      FrameDecodedLabel.Caption := Inttostr(Round(FPlayer.FramedSource.Decoder.TotalDuration.AsDurationSec)) + 's';
    if assigned(FPlayer.FramedSource.AccumDuration) and FPlayer.FramedSource.AccumDuration.IsValid then
      TimeCachedLabel.Caption := Inttostr(Round(FPlayer.FramedSource.AccumDuration.AsDurationSec)) + 's';
  end;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
begin
  if CURLClient.Connected then
  begin
    CURLClient.Proceed;

    if Assigned(FPlayer) then
    begin
      if FPlayer.FramedSource.Starving then
      if FPlayer.Status = slsPlaying then
        FPlayer.Pause;

      if FPlayer.FramedSource.Cached then
      if FPlayer.Status in [slsInitial, slsStopped, slsPaused] then
        FPlayer.Play;
    end;
  end;
end;

procedure TForm1.OnInitCURL(Sender : TObject);
begin
  TaskTimer.Enabled := true;
end;

procedure TForm1.OnSetSID(const AValue : String);
begin
  AuthOpts.SID := AValue;
end;

procedure TForm1.OnConnectedChanged(aValue : Boolean);
begin
  PlayButton.Enabled := aValue;
  if aValue then
    AuthToServerBtn.ImageIndex := 0 else
  begin
    AuthToServerBtn.ImageIndex := 4;
    DeviceList.Items.Clear;
    FStreamingDevices.Clear;
    FrameDecodedLabel.Caption := '';
    FrameLoadedLabel.Caption := '';
    TimeCachedLabel.Caption := '';

    DeviceList.Enabled := false;
    SlowTimer.Enabled := false;
    Timer1.Enabled := false;
    TaskTimer.Enabled := false;
  end;
end;

procedure TForm1.OnDisconnect(Sender : TObject);
begin
  OnConnectedChanged(False);
end;

procedure TForm1.AddLog(const aStr : String);
begin
  LogMemo.Lines.Add('['+DateTimeToStr(Now)+'] '+aStr);
  CURLClient.Log.Clear;
end;

procedure TForm1.OnSuccessAuth(aTask : THTTP2BackgroundTask; aObj : TJSONObject
  );
begin
  DeviceList.Enabled := true;
  Timer1.Enabled := true;
  SlowTimer.Enabled := true;
end;

procedure TForm1.OnAfterLaunchInOutStream(Sender : THTTP2BackgroundTask);
var
  Tsk : THTTP2BackgroundTask;
begin
  Tsk := THTTP2BackgroundTask(Sender);
  Tsk.Data := Self;
  PlayButton.ImageIndex := 3;
end;

procedure TForm1.OnSuccessUpdateStreams(aTask : THTTP2BackgroundTask;
  aArr : TJSONArray);
var o : TJSONData;
    ind : Integer;
begin
  ind := DeviceList.ItemIndex;
  FStreamingDevices.BeginUpdate;
  FStreamingDevices.Clear;
  while aArr.Count > 0 do
  begin
    o := aArr.Extract(0);
    if not (o is TJSONObject) then FreeAndNil(o);
    FStreamingDevices.AddObject(CURLClient.ExtractDeviceName(o.AsJSON), o);
  end;
  FStreamingDevices.EndUpdate;
  DeviceList.Items.Assign(FStreamingDevices);
  if (FStreamingDevices.Count > ind) and (ind >= 0) then
  begin
    DeviceList.ItemIndex := ind;
  end;
end;

procedure TForm1.ListenCurrent;
begin
  FinalizeListener;
  if DeviceList.ItemIndex >= 0 then
  begin
    InitializeListener(TJSONObject(DeviceList.Items.Objects[DeviceList.ItemIndex]));
  end;
end;

procedure TForm1.OnSynchroUpdateTask(aTask : THTTP2BackgroundTask);
begin
  if aTask is THTTP2BackgroundInStreamTask then
    UpdateFrame(THTTP2BackgroundInStreamTask(aTask).PopFrame);
end;

procedure TForm1.OnSuccessIOStream(aTask : THTTP2BackgroundTask);
begin
  ATask.Lock;
  try
    FinalizeListener;
  finally
    ATask.UnLock;
  end;

  PlayButton.ImageIndex := 2;
end;

procedure TForm1.InitializeListener(const aDevice : TJSONObject);
var d : TJSONData;
    aProps : ISoundProps;
    aCodec : TSoundLiteCodecType;
    str, s, subs : String;

    parse_step, parse_step_root : byte;
    parsed_val : Integer;
    p : integer;
begin
  FOutFile := nil;

  // get subproto
  //
  d := aDevice.Find(cSUBPROTO);
  if Assigned(d) and (d.JSONType = jtString) then
  begin
    str := d.AsString;

    aProps := TOGLSound.Props([TSoundLite.PROP_CHUNK_SIZE, 120]);
    // Parse subproto str
    // to define decoder props
    // format of radio stream 'RAW_%s_FRQ_%d_CHN_%d_BIT_%d'
    parse_step := 0;
    parsed_val := 0;
    s := UpperCase(str);
    while true do
    begin
      case parse_step of
      0 : begin
          if s.StartsWith('RAW_') then
          begin
            Delete(S, 1, 4);
            parse_step := 1;
          end else
            parse_step := 32;
        end;
      1 : begin
          if s.StartsWith('FLAC_') then
          begin
            Delete(S, 1, 5);
            parse_step := 2;
            aCodec := TSoundLite.CODEC_FLAC;
          end else
          if s.StartsWith('OPUS_') then
          begin
            Delete(S, 1, 5);
            parse_step := 2;
            aCodec := TSoundLite.CODEC_OPUS;
            aProps.Add(TSoundLite.ENC_PROP_OPUS_HEADER_TYPE, ophSimple);
          end else
            parse_step := 32;
        end;
      2 : begin
          if Length(s) = 0 then
          begin
            parse_step := 6;
          end else
          if s.StartsWith('FRQ_') then
          begin
            Delete(S, 1, 4);
            parse_step_root := 3;
            parse_step := 16;
          end else
          if s.StartsWith('CHN_') then
          begin
            Delete(S, 1, 4);
            parse_step_root := 4;
            parse_step := 16;
          end else
          if s.StartsWith('BIT_') then
          begin
            Delete(S, 1, 4);
            parse_step_root := 5;
            parse_step := 16;
          end else
            parse_step := 64;
        end;
      3 : begin
        if (parsed_val > 0) then
        begin
          aProps.Add(TSoundLite.PROP_FREQUENCY, parsed_val);
          parse_step := 2;
        end else
          parse_step := 33;
      end;
      4 : begin
        if (parsed_val in [1, 2]) then
        begin
          aProps.Add(TSoundLite.PROP_CHANNELS, parsed_val);
          parse_step := 2;
        end else
          parse_step := 33;
      end;
      5 : begin
        if (parsed_val in [8, 16]) then
        begin
          aProps.Add(TSoundLite.PROP_SAMPLE_SIZE, TOGLSound.BitdepthToSampleSize(parsed_val));
          parse_step := 2;
        end else
          parse_step := 33;
      end;
      6 : begin
        if aProps.HasProp(TSoundLite.PROP_SAMPLE_SIZE) and
           aProps.HasProp(TSoundLite.PROP_CHANNELS) and
           aProps.HasProp(TSoundLite.PROP_FREQUENCY) then
        begin
          Break; // success!
        end else
          parse_step := 34;
      end;
      16 : begin
           parsed_val := 0;
           p := Pos('_', s) - 1;
           if p <= 0 then
             p := Length(s);

           subs := Copy(s, 1, p);
           if TryStrToInt(subs, parsed_val) then
           begin
             Delete(s, 1, p + 1);
             parse_step := parse_step_root;
           end else
             parse_step := 33;
        end;
      32 :
          raise Exception.CreateFmt('The subprotocol ''%s'' is not supported or malformed', [str]);
      33 :
          raise Exception.CreateFmt('Wrong argument value %s', [s]);
      34 :
          raise Exception.Create('Not enought data to start decoder');
      64 :
          raise Exception.CreateFmt('Unknow parameter detected at pos ''%s''', [s]);
     end;
    end;

    FTotalLoaded := 0;
    FPlayer := TSLFramedPlayer.Create;
    FPlayer.InitPlayer(aCodec, aProps, 6,
                 TOGLSound.FrameFromDuration(aProps.Get(TSoundLite.PROP_FREQUENCY),
                                             aProps.Get(TSoundLite.PROP_CHANNELS),
                                             aProps.Get(TSoundLite.PROP_SAMPLE_SIZE),
                                             120).AsBytes);
    CURLClient.LaunchInStream(aDevice.Get(cDEVICE));
  end else
  begin
    raise Exception.Create('No subprotocol detected');
  end;
end;

procedure TForm1.FinalizeListener;
begin
  CURLClient.StopStreaming;

  if Assigned(FOutFile) then
  begin
    FOutFile.StopStreaming;
    FreeAndNil(FOutFile);
  end;

  if Assigned(FPlayer) then
  begin
    FPlayer.Stop;
    FreeAndNil(FPlayer);
  end;
end;

procedure TForm1.UpdateFrame(aFrame : TCustomMemoryStream);
var
  CHEAD : Cardinal;
  F : Cardinal;
  C : Byte;
  S : TSoundSampleSize;

  firstFrame, hasComments : Boolean;
begin
  if not Assigned(aFrame) then Exit;

  try

    if not Assigned(FPlayer) then
      raise Exception.Create('Player is not assigned or stream was stopped');

    if aFrame.Size < (6 + Sizeof(CHEAD)) then
      raise Exception.Create('Wrong size of frame detected');

    CHEAD := 0;
    aFrame.Position := 6; // shift to frame body

    aFrame.Read(CHEAD, Sizeof(CHEAD));
    F := (CHEAD and $1FFFF);
    C := Byte(Cardinal((CHEAD and $001E0000) shr 17) and $F);
    S := TOGLSound.BitdepthToSampleSize(Byte(Cardinal((CHEAD and $7E00000) shr 21) and $3F));

    hasComments := Boolean(Byte(Cardinal(CHEAD shr 29) and $1));
    firstFrame  := Boolean(Byte(Cardinal(CHEAD shr 31) and $1));

    if (F = 0) or (C = 0) then
      raise Exception.Create('Header is malformed');

    if FPlayer.FramedSource.Channels > 0 then
    begin
      if (F <> FPlayer.FramedSource.Frequency) or
         (C <> FPlayer.FramedSource.Channels) or
         (S <> FPlayer.FramedSource.SampleSize) then
        raise Exception.Create('The audio stream parameters were suddenly changed');
    end;

    if hasComments then
    begin
      FComments := TWAVE.NewComment as IRIFFComment;
      FComments.ReadFromDataStream(TOGLSound.NewDataStream(aFrame, []));
      FMetaView.ShowComments(FComments, true);
    end;

    FPlayer.FramedSource.PushFrame(aFrame);
    Inc(FTotalLoaded);
  except
    on e : Exception do
    begin
      AddLog(e.ToString);
      aFrame.Free;
    end;
  end;
end;

end.

