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
      procedure so(pProc: TAccept<T>); override;
      procedure caught(pProc: TReject); override;
  end;

implementation

{ TResolvedState<T> }

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

procedure TResolvedState<T>.setNextState;
begin
end;

procedure TResolvedState<T>.so(pProc: TAccept<T>);
begin
    self.fAcceptProc := pProc;

    self.syncAccept(fValue);
end;

end.
