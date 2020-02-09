unit uPromise.State.Resolved;

interface

uses
  uPromise.Types, uPromise.State.Base;

type
  TResolvedState<T> = class (TBaseState<T>)
    private
      fValue: T;

    protected
      procedure setNextState(); override;

    public
      constructor Create(pValue: T; pProc: TAccept<T>);

      function getErrorStr(): string; override;
      function getStateStr(): string; override;
      function getValue(): T; override;
      procedure next(pProc: TAccept<T>); override;
      procedure caught(pProc: TReject); override;
      procedure cancel(); override;
  end;

implementation

{ TResolvedState<T> }

procedure TResolvedState<T>.cancel;
begin
end;

procedure TResolvedState<T>.caught(pProc: TReject);
begin
end;

constructor TResolvedState<T>.Create(pValue: T; pProc: TAccept<T>);
begin
    fValue := pValue;
    fAcceptProc := pProc;
end;

function TResolvedState<T>.getErrorStr: string;
begin
end;

function TResolvedState<T>.getStateStr: string;
begin
    result := 'resolved';
end;

function TResolvedState<T>.getValue: T;
begin
    result := fValue;
end;

procedure TResolvedState<T>.setNextState;
begin
end;

procedure TResolvedState<T>.next(pProc: TAccept<T>);
begin
    self.fAcceptProc := pProc;

    self.syncAccept(fValue);
end;

end.
