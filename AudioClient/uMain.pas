unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, herro, SoundTools_EasyWindows, sounddevice_mm, soundtools,
  Vcl.ExtCtrls, managedthread, soundinterfaces, math, soundsample, typex, sounddevice_portaudio, guihelpers, stringx;

type
  TSimpleSampleOscillator = class(TOscillatorObject)
  public
    procedure o(mt: ToscMessageType; out ss: TStereoSoundSample;
      iSampletime: Int64); override;
  end;

  TForm1 = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    tmLookForAudio: TTimer;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    lbDevices: TListBox;
    lblStatus: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbDevicesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    sd: TAbstractSoundDevice;
    sine: TsimpleSampleOscillator;
    audiomode: ni;
    procedure SwitchAudioMode(mode: ni);
    procedure CleanupAudio;
    procedure CleanupOscillators;
    procedure UpdateDeviceStatus;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if sd = nil then
    exit;
  if not sd.StartCalled then
    sd.Start;
  if sine = nil then begin
    sine := TSimpleSampleOscillator.Create;
    sd.AddOscillator(sine);
  end else begin
    sd.RemoveOscillator(sine);
    sine.free;
    sine := nil;
  end;
end;

procedure TForm1.CleanupAudio;
begin
  if sd <> nil then begin
    if not sd.StartCalled then
      sd.start;
  end;

  CleanupOscillators;
  if sd <> nil then begin
    sd.stop;
    sd.WaitForFinish;
    sd.free;
    sd := nil;
  end;

end;

procedure TForm1.CleanupOscillators;
begin
  if sine <> nil then begin
    sd.removeoscillator(sine);
    sine.free;
    sine := nil;
  end;


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SwitchAudioMode(-1);//initialize to -1 (no audio)
  RadioGroup1.ItemIndex := 0;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SwitchAudioMode(-1);//Cleans up stuff
end;

procedure TForm1.lbDevicesClick(Sender: TObject);
begin
  if sd = nil then
    exit;
//  SwitchAudioMode(-1);
//  SwitchAudioMode(radiogroup1.ItemIndex);


  var lb := sender as TListBox;
  if lb.ItemIndex < 0 then
    exit;

  var sDevice := lb.Items[lb.itemindex];
  sd.DeviceName := sDevice;
  UpdateDeviceStatus;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  SwitchAudioMode(RadioGroup1.ItemIndex);
  if RadioGroup1.ItemIndex >= 0 then begin
    sd.RefreshDevices;
    var h := sd.DeviceList;

    StringListToListBox(lbDevices, h.o);
    lbDevices.ItemIndex := 0;
  end else begin
    lbDevices.Clear;
  end;
  UpdateDeviceStatus;
end;

procedure TForm1.SwitchAudioMode(mode: ni);
begin
  if mode = self.audiomode then
    exit;

  CleanupAudio;
  audiomode := mode;
  case mode of
    0: sd := TPM.Needthread<TSoundDevice_MM>(self);
    1: sd := TPM.Needthread<TsoundDevice_PortAudio>(self);
  end;



end;

procedure TForm1.UpdateDeviceStatus;
var
  s: string;
begin
  if sd = nil then
    s := 'nil'
  else
    s := sd.ClassName+' '+sd.devicename;

  lblStatus.Caption := s;


end;

{ TSimpleSampleOscillator }

procedure TSimpleSampleOscillator.o(mt: ToscMessageType;
  out ss: TStereoSoundSample; iSampletime: Int64);
begin
  inherited;
  case mt of
//    TOscMessageType.mtBeginWindow://optional notifies when a new block has started
//    TOscMessageType.mtEndWindow://optional notifies when a block has ended
//    TOscMessageType.mtAttack://optional
//    TOscMessageType.mtRelease://optional
    TOscMessageType.mtGetSample: begin
      ss.Left := Sin(iSampleTime/50);
      ss.right := sin(iSampleTime/50);
    end;
  end;

end;

end.
