unit uPromiseCanceled;

interface

uses
  uPromiseStateInterface, uPromiseTypes;

type
  TPromiseCanceled<T> = class (TInterfacedObject, IPromiseState<T>)
    public
      function getErrStr(): string;
      function getStateStr(): string;
      function getValue(): T;
      procedure setAccept(accept: AnonAcceptProc<T>);
      procedure setReject(reject: AnonRejectProc);
      procedure cancel();
  end;

implementation

{ TCanceledState<T> }

procedure TPromiseCanceled<T>.cancel;
begin
end;

procedure TPromiseCanceled<T>.setReject(reject: AnonRejectProc);
begin
end;

function TPromiseCanceled<T>.getErrStr: string;
begin
  result := 'Promise canceled';
end;

function TPromiseCanceled<T>.getStateStr: string;
begin
  result := 'canceled';
end;

function TPromiseCanceled<T>.getValue: T;
begin
end;

procedure TPromiseCanceled<T>.setAccept(accept: AnonAcceptProc<T>);
begin
end;

end.
