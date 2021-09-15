program uPromiseSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  uPromise in '..\src\uPromise.pas',
  uPromiseInterface in '..\src\uPromiseInterface.pas',
  uApi in 'uApi.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
