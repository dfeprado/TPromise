unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uIPromise;

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
    procedure btnUpdateLabelClick(Sender: TObject);
    procedure btnPromiseStateClick(Sender: TObject);
    procedure btnUpdateLabel2Click(Sender: TObject);
    procedure btnPromiseRejectionClick(Sender: TObject);
    procedure btnCancelPromiseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    fPromise: IPromise<string>;
    fCounter: shortint;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  System.Threading, uPromise;

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
  xFuture: IFuture<string>;
  xDate: TDateTime;
begin
    xFuture := TTask.Future<string>(
      function: string
      var
        xSecCount: shortint;
      begin
          xSecCount := 0;
          while (xSecCount < 10) do
          begin
              TTask.CurrentTask.CheckCanceled();

              sleep(1000);

              inc(xSecCount);
          end;
          result := 'Hello world!';
      end
    );

    fPromise := TPromise<string>.Create(xFuture);
    fPromise.next(
      procedure (const R: string)
      begin
          lblLabel.Caption := R;
      end
    );

    fCounter := 10;
    Timer1.Enabled := True;
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

procedure TForm2.btnCancelPromiseClick(Sender: TObject);
begin
    if (Assigned(fPromise)) then
    begin
        fPromise.cancel();
    end;
end;

procedure TForm2.btnPromiseRejectionClick(Sender: TObject);
var
  xFuture: IFuture<string>;
begin
    xFuture := TTask.Future<string>(
      function: string
      begin
          raise Exception.Create('TESTE DE EXCEPTION!');
      end
    );

    fPromise := TPromise<string>.Create(xFuture);
    fPromise.next(
      procedure (const R: string)
      begin
          lblLabel.Caption := R;
      end
    ).caught(
      procedure
      begin
          ShowMessage(fPromise.getErrorStr());
      end
    );
end;

procedure TForm2.btnPromiseStateClick(Sender: TObject);
begin
    if (Assigned(fPromise)) then
    begin
        lblPromiseState.Caption := fPromise.getState();
    end
    else
    begin
        lblPromiseState.Caption := 'The promise does not exists yet.';
    end;
end;

end.
