unit uPromise.bridge.state;

interface

uses
  uPromise.State;

type
  TPrChange<T> = procedure (pState: IPromiseState<T>) of object;

  TStateChangeBridge<T> = class (TObject)
    private
      fPrChange: TPrChange<T>;

    public
      constructor Create(pPrChange: TPrChange<T>);

      procedure change(pNewState: IPromiseState<T>);

  end;

implementation

{ TStateChangeBridge<T> }

procedure TStateChangeBridge<T>.change(pNewState: IPromiseState<T>);
begin
    fPrChange(pNewState);
end;

constructor TStateChangeBridge<T>.Create(pPrChange: TPrChange<T>);
begin
    fPrChange := pPrChange;
end;

end.
