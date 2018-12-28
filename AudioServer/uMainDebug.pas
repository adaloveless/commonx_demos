unit uMainDebug;
{$I DelphiDefs.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, stringx, IdSocketHandle,
  IdComponent, IdUDPBase, IdUDPServer, soundtools, systemx, Vcl.ExtCtrls, idglobal,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, IdIPMCastBase,
  IdIPMCastClient, sounddevice_portaudio, tickcount, consolelock,
  IdBaseComponent, managedthread;

type
  TForm1 = class(TForm)
    udps: TIdUDPServer;
    Timer1: TTimer;
    Timer2: TTimer;
    Timer3: TTimer;
    IdIPMCastClient1: TIdIPMCastClient;
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer3Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

    activated: boolean;
    lastsequence: int64;
    FBroadCast: boolean;
    Answered: string;
    LastPacketTime: ticker;
    spo: TSoundPacketOscillator;
{$IFNDEF USE_DSOUND}
    thr: TSoundDEvice_Portaudio;
{$ENDIF}
//    sin: TSineWaveOscillator;
    received: nativeint;
    missing: nativeint;

    { Private declarations }
    procedure udpsUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure PingEcho(reftime: cardinal);
    procedure MissingPacket(reftime: cardinal; missing_position: int64);

    procedure InitDSoundStuff;
    procedure DrawChart;
    procedure SyncChart;

    procedure CommonRead(data: PByte; iSize: nativeint; ABinding: TIdSocketHandle);
    procedure SendAppropriate(const b: TIDBytes);
    procedure SetBroadCast(const Value: boolean);
    property BroadCast:boolean read FBroadCAst write SetBroadCast;

  public
    { Public declarations }
  end;

// Poop fart burp. This comment serves no purpose.

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CommonRead(data: PByte; iSize: nativeint;
  ABinding: TIdSocketHandle);
var
  pack: TSoundPacket;
  iPredictedTIme: int64;
begin
  movemem32(@pack, data, iSize);
  LAstPacketTime := GetTicker;

  if pack.PingReferenceTime > 0 then begin
    PingEcho(pack.PingReferenceTime);
  end;




  //predicft the sample time of the next packet
  iPredictedTime := lastsequence + SOUND_PACKET_DATA_SIZE;

  //if this packet is in the future relative to our predicted time,
  //then we must have missed a packet....

  if (iPredictedTime) < pack.RemoteSampleTime then begin
    //however...

    //if we have this packet, then...
    if spo.FindPacket(iPredictedTime,true) = nil then begin

      //if our predicted time is within 1 second of our target tick time
      if abs(iPredictedTime- spo.LocalTick) < 44100 then begin
        //go looking for the missing packet by sending a message back to the
        //audio server requesting a retransmit
        MissingPacket(pack.PingReferenceTime, iPredictedTime);
        inc(Missing);
        if (missing mod 20)=0 then begin
          spo.reset;
          lastsequence := pack.remotesampletime;
        end;

      end else begin
        //else ignore it.. we must have just reset the stream or something
        lastsequence := pack.remotesampletime;
      end;
    end else begin
      //otherwise it must mean that our prediction was
      //incorrect and packets just came out of order or something
      //so ignore the crisis
      lastSequence := iPredictedTime;
    end;
  end else begin
    lastsequence := pack.remotesampletime;
  end;

  spo.AddPacket(pack);



  received := received + 1;
  if received > $FFFFFFF then begin
    received := 0;
  end;
end;

procedure TForm1.DrawChart;
var
  t: integer;
begin
  //SYncChart;

{$IFDEF CHARTS}
  chart1.series[0].clear;
  chart2.Series[0].Clear;
  if not assigned(spo) then
    exit;
  for t:= 0 to high(spo.StreamReadTimes) do begin
    chart1.series[0].AddXY(spo.StreamReadTimes[t].iPosition, spo.StreamReadTimes[t].REadTime);
//    chart1.Series[0].XValues[t] := spo.StreamReadTimes[t].iPosition;
//    chart1.Series[0].YValues[t] := spo.StreamReadTimes[t].REadTime;
  end;

  for t:= 0 to spo.EffectiveLagRecordCount-1  do begin
    chart2.series[0].AddXY(t, spo.LagRecords[t]);
//    chart1.Series[0].XValues[t] := spo.StreamReadTimes[t].iPosition;
//    chart1.Series[0].YValues[t] := spo.StreamReadTimes[t].REadTime;
  end;
{$ENDIF}

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  udps.active := false;
//  ShutDownAudio;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LockConsole;
  try
    Answered := '255.255.255.255';
  finally
    UnlockConsole;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  spo.Free;
  spo := nil;

{$IFNDEF USE_DSOUND}
  thr.Terminate;
  thr.WaitFor;
  thr.free;
{$ENDIF}



end;

procedure TForm1.InitDSoundStuff;
begin
{$IFNDEF USE_DSOUND}
  thr := TPM.NeedThread<TSoundDevice_portaudio>(self);
//  thr := TsoundDevice_portaudio.Create(self, nil);
  thr.Buffersize := 44100;
{$ENDIF}

  self.udps.OnUDPRead := self.udpsUDPRead;

  spo := TSoundPacketOscillator.Create;


  udps.active := true;

{$IFNDEF USE_DSOUND}
  thr.AddOscillator(spo);
  sleep(1000);
  thr.start;
{$ELSE}
  DXS.Renderer.AddOscillator(spo);
{$ENDIF}





end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  s: string;
begin
  if not activated then exit;
  s := '';
  if Broadcast then
    s := '??? ';

  caption := s+'Missing: '+inttostr(missing)+' ServiceLag: '+FloatPrecision(spo.avgservicelag,2)+' Lag: '+FloatPrecision(spo.Lag,2)+' ms. CorrectedLag: '+floatprecision(spo.Correctedlag,2)+' Received: '+inttostr(received)+' Resets:'+inttostr(spo.Resets);


end;

procedure TForm1.Timer2Timer(Sender: TObject);
var
  p: TRFAPacket;
  b: TidBytes;
begin
  if not activated then exit;

  if udps.Active then begin
    p.Init;
    p.ThreeCC[0] := ord(ansichar('R'));
    p.ThreeCC[1] := ord(ansichar('F'));
    p.ThreeCC[2] := ord(ansichar('A'));
    p.LastSequenceNumberProcessed := spo.LastSequenceNumberPlayed;
    p.LastSequenceNumberReceived := spo.LastSequenceNumberReceived;
    p.PingReferenceTime := 0;
    Setlength(b, sizeof(p));
    movemem32(@b[0], @p, sizeof(p));

    SendAppropriate(b);

  end;
end;

procedure TForm1.Timer3Timer(Sender: TObject);
begin
{$IFDEF CHARTS}
  DrawChart;
{$ENDIF}
  if activated then exit;

  InitDSoundStuff;

  ACtivated := true;

end;

procedure Tform1.SendAppropriate(const b: TIDBytes);
var
  s: string;
begin
    BroadCast := GetTimeSince(LAstPacketTime) > 4000;
    if broadcast then begin
      udps.SendBuffer('255.255.255.255', 798, b);
      LAstPacketTime := GetTicker;
    end
    else begin
      LockConsole;
      try
        s := Answered;
        UniqueString(s);
      finally
        UnlockConsole;
      end;
      udps.SendBuffer(s, 798, b)
    end;


end;
procedure TForm1.SetBroadCast(const Value: boolean);
begin
  if value <> FBroadcast then begin
    if value then begin
      Caption := 'Looking for Audio Senders.';
    end else begin
    end;
  end;

  FBroadCast := value;
end;

procedure Tform1.PingEcho(reftime: cardinal);
var
  p: TRFAPacket;
  b: TIdBytes;
begin
  if udps.Active then begin
    p.Init;
    p.ThreeCC[0] := ord(ansichar('R'));
    p.ThreeCC[1] := ord(ansichar('F'));
    p.ThreeCC[2] := ord(ansichar('A'));
    p.LastSequenceNumberProcessed := spo.LastSequenceNumberPlayed;
    p.LastSequenceNumberReceived := spo.LastSequenceNumberReceived;
    p.PingReferenceTime := reftime;
    Setlength(b, sizeof(p));
    movemem32(@b[0], @p, sizeof(p));
    SendAppropriate(b);

  end;
end;

procedure TForm1.SyncChart;
begin
{$IFDEF CHARTS}
  while chart1.Series[0].XValues.count < 100 do begin
    chart1.Series[0].AddXY(random(1000),random(1000));
  end;
{$ENDIF}
end;

procedure Tform1.MissingPacket(reftime: cardinal; missing_position: int64);
var
  p: TRFAPacket;
  b: TIdBytes;
begin
  if broadcast then exit;

  if udps.Active then begin
    p.Init;
    p.ThreeCC[0] := ord(ansichar('R'));
    p.ThreeCC[1] := ord(ansichar('F'));
    p.ThreeCC[2] := ord(ansichar('A'));
    p.LastSequenceNumberProcessed := spo.LastSequenceNumberPlayed;
    p.LastSequenceNumberReceived := spo.LastSequenceNumberReceived;
    p.PingReferenceTime := reftime;
    p.MissingPosition := missing_position;
    Setlength(b, sizeof(p));
    movemem32(@b[0], @p, sizeof(p));
    SendAppropriate(b);
  end;
end;


procedure TForm1.udpsUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  pack: TSoundPacket;
  iPredictedTIme: int64;
begin
  LockConsole;
  try
    Answered := ABinding.PeerIP;
  finally
    UnlockConsole;
  end;
  LastPacketTime := GetTicker;
  CommonRead(@AData[0], length(AData), ABinding);


end;

end.
