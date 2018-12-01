program ThreadsCommandsAndQueues;

uses
  bigbrainultra,
  brainwashultra,
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1},
  FrameTotalDebug in '..\commonx\FrameTotalDebug.pas' {framTotalDebug: TFrame},
  FastBitmap_OpenCL_Extension in '..\commonx\FastBitmap_OpenCL_Extension.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
