unit uPromise.State.Canceled;

interface

uses
  uPromise.State.Base, uPromise.Types;

type
  TCanceledState<T> = class (TBaseState<T>)
    public
      function getErrorStr(): string; override;
      function getStateStr(): string; override;
      function getValue(): T; override;
      procedure then_(pProc: TAccept<T>); override;
      procedure caught(pProc: TReject); override;
      procedure cancel(); override;
  end;

implementation

{ TCanceledState<T> }

procedure TCanceledState<T>.cancel;
begin
end;

procedure TCanceledState<T>.caught(pProc: TReject);
begin
end;

function TCanceledState<T>.getErrorStr: string;
begin
end;

function TCanceledState<T>.getStateStr: string;
begin
    result := 'canceled';
end;

function TCanceledState<T>.getValue: T;
begin
end;

procedure TCanceledState<T>.then_(pProc: TAccept<T>);
begin
end;

end.
