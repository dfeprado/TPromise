unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, uPromiseInterface;

type
  TForm1 = class(TForm)
    btnQueryApi: TButton;
    memoResult: TMemo;
    btnFakeLongProcess: TButton;
    procedure btnQueryApiClick(Sender: TObject);
    procedure btnFakeLongProcessClick(Sender: TObject);
  private
    querying: boolean;
    queryPromise: IPromise<String, String>;
    longProcessPromise: IPromise<String, String>;
    procedure setResultText(const value: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uApi;

{$R *.fmx}

procedure TForm1.btnFakeLongProcessClick(Sender: TObject);
begin
  if Assigned(longProcessPromise) then
  begin
    longProcessPromise.Cancel();
    longProcessPromise := nil;
    btnFakeLongProcess.Text := 'Fake a long process';
    exit();
  end;

  longProcessPromise := TApi.fakeLongFetch()
    .&Then(procedure (const value: String) begin
      btnFakeLongProcess.Text := 'Done with long process. '+value;
      longProcessPromise := nil;
    end)
    .Catch(procedure (const value: String) begin
      btnFakeLongProcess.Text := 'Erro on long process: '+value;
      longProcessPromise := nil;
    end);

  btnFakeLongProcess.Text := 'Processing... Click again to cancel.';
end;

procedure TForm1.btnQueryApiClick(Sender: TObject);
begin
  if querying then
  begin
    btnQueryApi.Text := 'Query API';
    queryPromise.Cancel();
    querying := false;
    Exit();
  end;

  memoResult.ClearContent();
  btnQueryApi.Text := 'Querying... Click again to cancel.';

  // TApi.fetch() returns a IPromise<String, String>.
  queryPromise := TApi.fetch()
    .&Then(procedure (const value: string) begin
      setResultText(value);
    end)
    .Catch(procedure (const value: string) begin
      setResultText(value);
    end);
  querying := true;
end;

procedure TForm1.setResultText(const value: string);
begin
  memoResult.Text := value;
  querying := false;
  btnQueryApi.Text := 'Query API';
end;

end.
