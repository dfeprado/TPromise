unit uPromise.State.Rejected;

interface

uses
  uPromise.State.Base, uPromise.Types;

type
  TRejectedState<T> = class (TBaseState<T>)
    private
      fValue: string;

    protected
      constructor Create(const pErro: string; pProc: TReject);

      function getErrorStr(): string; override;
      function getStateStr(): string; override;
      function getValue(): T; override;
      procedure setNextState(); override;
      procedure next(pProc: TAccept<T>); override;
      procedure caught(pProc: TReject); override;
      procedure cancel(); override;

  end;

implementation

{ TRejectedState<T> }

procedure TRejectedState<T>.cancel;
begin
end;

procedure TRejectedState<T>.caught(pProc: TReject);
begin
    self.fRejectProc := pProc;
    self.syncReject();
end;

constructor TRejectedState<T>.Create(const pErro: string; pProc: TReject);
begin
    fValue := pErro;
    fRejectProc := pProc;
end;

function TRejectedState<T>.getErrorStr: string;
begin
    result := fValue;
end;

function TRejectedState<T>.getStateStr: string;
begin
    result := 'rejected';
end;

function TRejectedState<T>.getValue: T;
begin
end;

procedure TRejectedState<T>.setNextState;
begin
end;

procedure TRejectedState<T>.next(pProc: TAccept<T>);
begin
end;

end.
