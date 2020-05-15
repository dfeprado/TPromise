unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uPromiseInterface;

type
  TForm2 = class(TForm)
    btnUpdateLabel: TButton;
    lblLabel: TLabel;
    btnUpdateLabel2: TButton;
    lblLabel2: TLabel;
    btnPromiseState: TButton;
    lblPromiseState: TLabel;
    btnPromiseRejection: TButton;
    btnCancelPromise: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    btnUpdateWithAnonPromise: TButton;
    lblAnonPromiseValue: TLabel;
    lblAnonPromiseTimer: TLabel;
    Timer2: TTimer;
    procedure btnUpdateLabelClick(Sender: TObject);
    procedure btnPromiseStateClick(Sender: TObject);
    procedure btnUpdateLabel2Click(Sender: TObject);
    procedure btnPromiseRejectionClick(Sender: TObject);
    procedure btnCancelPromiseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure btnUpdateWithAnonPromiseClick(Sender: TObject);
  private
    fPromise: IPromise<string>;
    fCounter,
    fCounter2: shortint;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  uPromise, uPromiseTypes, System.Threading;

{$R *.dfm}

procedure TForm2.btnUpdateLabel2Click(Sender: TObject);
begin
    if (Assigned(fPromise)) then
    begin
        lblLabel2.Caption := fPromise.getValue();
    end
    else
    begin
        lblLabel2.Caption := 'The promise does not exists yet.';
    end;
end;

procedure TForm2.btnUpdateLabelClick(Sender: TObject);
var
  xDate: TDateTime;
begin
  fPromise := TPromise<string>.Create(
    procedure (accept: AcceptProc<string>; reject: RejectProc)
    var
      xSecCount: shortint;
    begin
      xSecCount := 0;
      while (xSecCount < 10) do
      begin
        if (TTask.CurrentTask.Status = TTaskStatus.Canceled) then exit();
        sleep(1000);
        inc(xSecCount);
      end;
      accept('Hello world!');
    end
  );
  fPromise.then_(
    procedure (R: string)
    begin
      lblLabel.Caption := R;
    end
  );

  fCounter := 10;
  Timer1.Enabled := True;
end;

procedure TForm2.btnUpdateWithAnonPromiseClick(Sender: TObject);
begin
  fCounter2 := 10;
  TPromise<string>.Create(
    procedure (accept: AcceptProc<string>; reject: RejectProc)
    var
      xSecCount: shortint;
    begin
      xSecCount := 0;
      while (xSecCount < 10) do
      begin
        if (TTask.CurrentTask.Status = TTaskStatus.Canceled) then exit();
        sleep(1000);
        inc(xSecCount);
      end;
      accept('Hello world (with anonymous promise)!');
    end
  ).then_(
    procedure (value: string)
    begin
      self.lblAnonPromiseValue.Caption := value;
    end
  );
  Timer2.Enabled := true;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if (Assigned(fPromise)) then
    begin
        fPromise.cancel();
    end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
    dec(fCounter);
    Label1.Caption := fCounter.ToString();

    if (fCounter = 0) then
    begin
        Timer1.Enabled := false;
    end;
end;

procedure TForm2.Timer2Timer(Sender: TObject);
begin
  dec(fCounter2);
  lblAnonPromiseTimer.Caption := fCounter2.ToString();
  if (fCounter2 = 0) then Timer2.Enabled := false;
end;

procedure TForm2.btnCancelPromiseClick(Sender: TObject);
begin
  if (Assigned(fPromise)) then
  begin
      fPromise.cancel();
  end;
end;

procedure TForm2.btnPromiseRejectionClick(Sender: TObject);
//var
//  xFuture: IFuture<string>;
begin
//    xFuture := TTask.Future<string>(
//      function: string
//      begin
//          raise Exception.Create('TESTE DE EXCEPTION!');
//      end
//    );
//
//    fPromise := TPromise<string>.Create(xFuture);
//    fPromise.then_(
//      procedure (const R: string)
//      begin
//          lblLabel.Caption := R;
//      end
//    ).caught(
//      procedure
//      begin
//          ShowMessage(fPromise.getErrorStr());
//      end
//    );
end;

procedure TForm2.btnPromiseStateClick(Sender: TObject);
var
  state: String;
begin
    if (Assigned(fPromise)) then
    begin
        if (fPromise.isCanceled()) then state := 'Canceled'
        else if (fPromise.isFulfilled()) then state := 'Fulfilled'
        else if (fPromise.isPending()) then state := 'Pending'
        else state := 'Rejected';
    end
    else
    begin
        state := 'The promise does not exists yet.';
    end;
    lblPromiseState.Caption := state;
end;

end.
