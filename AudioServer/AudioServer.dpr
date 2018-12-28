  program AudioServer;

uses
  BigBrainUltra,
  BrainWashUltra,
  Vcl.Forms,
  idstackwindows,
  uMainDebug in 'uMainDebug.pas' {Form1},
  soundinterfaces in '..\commonx\soundinterfaces.pas',
  SoundTools in '..\commonx\SoundTools.pas',
  CommonConstants in '..\commonx\CommonConstants.pas',
  DestructionQueue in '..\commonx\DestructionQueue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
