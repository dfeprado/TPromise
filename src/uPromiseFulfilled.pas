unit uPromiseFulfilled;

interface

uses
  uPromiseTypes, uPromiseStateInterface;

type
  TPromiseFulfilled<T> = class (TInterfacedObject, IPromiseState<T>)
    private
      fulfilledValue: T;
      acceptProc: AnonAcceptProc<T>;
    public
      constructor Create(value: T; acceptProc: AnonAcceptProc<T>);
      function getErrStr(): string;
      function getStateStr(): string;
      function getValue(): T;
      procedure setAccept(pProc: AnonAcceptProc<T>);
      procedure setReject(pProc: AnonRejectProc);
      procedure cancel();
  end;

implementation

uses
  System.Classes;

{ TResolvedState<T> }

procedure TPromiseFulfilled<T>.cancel;
begin
end;

procedure TPromiseFulfilled<T>.setReject(pProc: AnonRejectProc);
begin
end;

constructor TPromiseFulfilled<T>.Create(value: T; acceptProc: AnonAcceptProc<T>);
begin
  fulfilledValue := value;
  self.setAccept(acceptProc);
end;

function TPromiseFulfilled<T>.getErrStr: string;
begin
end;

function TPromiseFulfilled<T>.getStateStr: string;
begin
  result := 'resolved';
end;

function TPromiseFulfilled<T>.getValue: T;
begin
  result := fulfilledValue;
end;

procedure TPromiseFulfilled<T>.setAccept(pProc: AnonAcceptProc<T>);
begin
  self.acceptProc := pProc;
  if (Assigned(self.acceptProc)) then
  begin
    if (TThread.CurrentThread.ThreadID <> MainThreadID) then
    begin
      TThread.Queue(
        TThread.CurrentThread,
        procedure
        begin
          self.acceptProc(self.fulfilledValue);
        end
      );
    end
    else acceptProc(fulfilledValue);
  end;
end;

end.
