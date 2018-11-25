program ThreadsCommandsAndQueues;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1},
  FrameTotalDebug in '..\commonx\FrameTotalDebug.pas' {framTotalDebug: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
