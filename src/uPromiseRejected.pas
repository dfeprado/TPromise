unit uPromiseRejected;

interface

uses
  uPromiseStateInterface, uPromiseTypes;

type
  TPromiseRejected<T> = class (TInterfacedObject, IPromiseState<T>)
    private
      error: string;
      onReject: AnonRejectProc;
    protected
      constructor Create(const error: string; onReject: AnonRejectProc);
      function getErrStr(): string;
      function getStateStr(): string;
      function getValue(): T;
      procedure setAccept(accept: AnonAcceptProc<T>);
      procedure setReject(reject: AnonRejectProc);
      procedure cancel();

  end;

implementation

uses
  System.Classes;

{ TRejectedState<T> }

procedure TPromiseRejected<T>.cancel;
begin
end;

procedure TPromiseRejected<T>.setAccept(accept: AnonAcceptProc<T>);
begin
end;

procedure TPromiseRejected<T>.setReject(reject: AnonRejectProc);
begin
  onReject := reject;
  if (Assigned(onReject)) then
  begin
    if (TThread.CurrentThread.ThreadID <> MainThreadID) then
    begin
      TThread.Queue(
        TThread.CurrentThread,
        procedure
        begin
          self.onReject(self.error);
        end
      );
    end
    else onReject(error);
  end;
end;

constructor TPromiseRejected<T>.Create(const error: string; onReject: AnonRejectProc);
begin
  self.error := error;
  self.onReject := onReject;
end;

function TPromiseRejected<T>.getErrStr: string;
begin
  result := error;
end;

function TPromiseRejected<T>.getStateStr: string;
begin
  result := 'rejected';
end;

function TPromiseRejected<T>.getValue: T;
begin
end;

end.
