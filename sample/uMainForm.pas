unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uIPromise;

type
  TForm2 = class(TForm)
    btnUpdateLabel: TButton;
    lblLabel: TLabel;
    btnUpdateLabel2: TButton;
    lblLabel2: TLabel;
    btnPromiseState: TButton;
    lblPromiseState: TLabel;
    btnPromiseRejection: TButton;
    procedure btnUpdateLabelClick(Sender: TObject);
    procedure btnPromiseStateClick(Sender: TObject);
    procedure btnUpdateLabel2Click(Sender: TObject);
    procedure btnPromiseRejectionClick(Sender: TObject);
  private
    fPromise: IPromise<string>;
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
        fPromise.so(
          procedure (const R: string)
          begin
              lblLabel2.Caption := R;
          end
        );
    end
    else
    begin
        lblLabel2.Caption := 'The promise does not exists yet.';
    end;
end;

procedure TForm2.btnUpdateLabelClick(Sender: TObject);
var
  xFuture: IFuture<string>;
begin
    xFuture := TTask.Future<string>(
      function: string
      var
        x: shortint;
      begin
          x := 10;
          sleep(3000);
          result := 'Hello world!';
      end
    );

    fPromise := TPromise<string>.Create(xFuture);
    fPromise.so(
      procedure (const R: string)
      begin
          lblLabel.Caption := R;
      end
    );
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
    fPromise.so(
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
