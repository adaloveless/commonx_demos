unit uMain;

interface

uses
  systemx,typex, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, FrameTotalDebug, simplequeue, commandprocessor, anoncommand, globalmultiqueue, better_collections, linked_list;

type
  TQueueItem_IsPrime = class(TQueueItem)
  protected
    procedure DoExecute; override;
  public
    in_n: ni;
    out_isPrime: boolean;
  end;

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    panFrameHost: TPanel;
    Splitter1: TSplitter;
    Button1: TButton;
    lblResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  ac: TAnonymousCommand<ni>;
  res: ni;
const
  MIN_PRIME = 1000000000;
  MAX_PRIME = 1000010000;
begin
  //
  res := 0;
  lblResult.caption := 'You have '+Getnumberofprocessors().tostring+' cpus.';

  ac := TAnonymousCommand<ni>.create(
          function: ni
          var
            x: ni;
            qi: TQueueItem_IsPrime;
          begin
            res := 0;
            ac.Status := 'Creating Queue Items';
            ac.Step := 0;
            ac.StepCount := MAX_PRIME-MIN_PRIME;
            for x := MIN_PRIME to MAX_PRIME do begin
              if (x and 1)=0 then continue;//skip evens
              ac.Step := x-MIN_PRIME;
              qi := TQueueItem_IsPrime.create;
              qi.in_n := x;
              GMQ.AddItem(qi);
              qi.autodestroy := true;
              qi.onFinish_Anon := ( procedure (qilocal: TQueueItem)
                                var
                                  q: TQueueItem_IsPrime;
                                begin
                                  q := qilocal as TQueueItem_IsPrime;
                                  if q.out_isPrime then
                                    inc(res);
                                end
              );

            end;
            GMQ.WaitForAllQueues;
          end
          ,
          procedure(result: ni)
          begin
            GMQ.WaitForAllQueues;
            lblResult.caption := 'Found '+inttostr(res)+' primes.';

          end,
          procedure(e: Exception)
          begin
            raise e;

          end
        );
  ac.SynchronizeFinish := true;




end;

procedure TForm1.FormCreate(Sender: TObject);
var
  frm: TFramTotalDebug;
begin
  frm := TframTotalDebug.create(self);
  frm.parent := panFrameHost;
  frm.Align := alClient;

end;

{ TQueueItem_IsPrime }

procedure TQueueItem_IsPrime.DoExecute;
var
  x: ni;
  n: ni;
begin
  inherited;

  n := in_n;//move locally for optimization
  out_isPrime := true;
  for x := 2 to (n div 2) do begin
    //if no remainder from modulus operation
    if (n mod x) = 0 then begin
      //this is not prime
      out_isPrime := false;
      break;
    end;
  end;
end;

{ Tcmd_CreatePrimeTestsUsingQueues }


end.
