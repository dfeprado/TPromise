unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, promise,
  System.Generics.Collections;

type
  TForm2 = class(TForm)
    btnUpdateLabel: TButton;
    lblLabel: TLabel;
    Label1: TLabel;
    Timer1: TTimer;
    btnCatchPromise: TButton;
    procedure btnUpdateLabelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCatchPromiseClick(Sender: TObject);
  private
    Promises: TList<IPromiseBasic>;
    fCounter,
    fCounter2: shortint;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses promise.concret, System.Threading;

procedure TForm2.btnCatchPromiseClick(Sender: TObject);
begin
  // Promisses.Add is just used to automatic cancel promises
  // on form destruction
  Promises.Add(
    TPromise<String, Exception>.New(
      procedure (Resolve: TPromiseResolveProcedure<String>; Reject: TPromiseRejectProcedure<Exception>)
      var
        counter: Byte;
      begin
        // This task counts up to 20, within 2 seconds.
        // Then, its raise an exception to simulate the Catch().
        counter := 0;
        try
          while True do
          begin
          // !!!! IMPORTANT !!!
          // Always check for cancelation! If not, your program may freeze in unexpected moments
            TTask.CurrentTask.CheckCanceled();
            sleep(100);
            if counter = 20 then
              raise EArgumentException.Create('This is an exception raised from counter');
            Inc(counter);
          end;
          Resolve('OK');
        except
          // We are not interested in EOperationCancelled
          if ExceptObject() is EOperationCancelled then
            Exit();

          // Reject the promise with the Exception object
          Reject(AcquireExceptionObject() as Exception);
        end;
      end
    )
    .&Then(
      procedure (const Value: String)
      begin
        ShowMessage(Value);
      end
    )
    .Catch(
      procedure (const ErrorValue: Exception)
      begin
        // This procedure will run within main thread.
        // To change this, use TPromise.NewAsync() constructor.
        ShowMessage(ErrorValue.Message);
        ErrorValue.Free();
      end
    )
  );
end;

procedure TForm2.btnUpdateLabelClick(Sender: TObject);
begin
  // Promisses.Add is just used to automatic cancel promises
  // on form destruction
  Promises.Add(
    // Create a new promise parametirized for <String, String>
    TPromise<String, String>.New(
      procedure (Resolve: TPromiseResolveProcedure<String>; Reject: TPromiseRejectProcedure<String>)
      var
        elapsedSeconds: Byte;
      begin
        // This work task counts 10 seconds before resolve
        elapsedSeconds := 0;
        while elapsedSeconds < 10 do
        begin
          // !!!! IMPORTANT !!!
          // Always check for cancelation! If not, your program may freeze in unexpected moments
          TTask.CurrentTask.CheckCanceled();
          Sleep(1000);
          inc(elapsedSeconds);
        end;
        // Resolve the string "Hello, World!"
        Resolve('Hello, World!');
      end
    ).&Then(
      procedure (const Value: String)
      begin
        // Every &Then() procedure executes within main thread.
        // To change this, use TPromise.NewAsync() constructor.
        lblLabel.Caption := Value;
      end
    )
  );

  // Just start a counter to show the UI updating while task is counting.
  fCounter := 10;
  Timer1.Enabled := True;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Promises := TList<IPromiseBasic>.Create();
end;

procedure TForm2.FormDestroy(Sender: TObject);
var
  promise: IPromiseBasic;
begin
  for promise in Promises do
  begin
    promise.Cancel();
  end;
  Promises.Free();
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

end.
