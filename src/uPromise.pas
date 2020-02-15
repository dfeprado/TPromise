unit uPromise;

interface

uses uIPromise, uPromise.Types, System.Threading, uPromise.State;

type
  TPromise<T> = class (TInterfacedObject, IPromise<T>)
    private
      fSelfState: IPromiseState<T>;

    protected
      procedure changeState(pState: IPromiseState<T>);

    public
      constructor Create(pFuture: IFuture<T>);
      destructor Destroy; override;

      //-- from IPromise<T>
      function getErrorStr(): string;
      function getState(): string;
      function getValue(): T;
      function isResolved(): boolean;
      function isRejected(): boolean;
      function isUnresolved(): boolean;
      function isCanceled(): boolean;
      function then_(pProc: TAccept<T>): IPromise<T>;
      procedure caught(pProc: TReject);
      procedure cancel();

  end;

implementation

uses
  System.Classes, uPromise.State.Unresolved, uPromise.bridge.state;

{ TPromise<T> }

procedure TPromise<T>.cancel;
begin
    fSelfState.cancel();
end;

procedure TPromise<T>.caught(pProc: TReject);
begin
    fSelfState.caught(pProc);
end;

procedure TPromise<T>.changeState(pState: IPromiseState<T>);
begin
    fSelfState := pState;
end;

constructor TPromise<T>.Create(pFuture: IFuture<T>);
begin
    fSelfState := TUnresolvedState<T>.Create(self, pFuture, TStateChangeBridge<T>.Create(self.changeState));
end;

destructor TPromise<T>.Destroy;
begin
    self.cancel();
    fSelfState := nil;
    inherited;
end;

function TPromise<T>.getErrorStr: string;
begin
    result := fSelfState.getErrorStr();
end;

function TPromise<T>.getState: string;
begin
    result := fSelfState.getStateStr();
end;

function TPromise<T>.getValue: T;
begin
    result := fSelfState.getValue();
end;

function TPromise<T>.isCanceled: boolean;
begin
    result := fSelfState.getStateStr() = 'canceled';
end;

function TPromise<T>.isRejected: boolean;
begin
    result := fSelfState.getStateStr() = 'rejected';
end;

function TPromise<T>.isResolved: boolean;
begin
    result := fSelfState.getStateStr() = 'resolved';
end;

function TPromise<T>.isUnresolved: boolean;
begin
    result := fSelfState.getStateStr() = 'unresolved';
end;

function TPromise<T>.then_(pProc: TAccept<T>): IPromise<T>;
begin
    fSelfState.then_(pProc);
    result := self;
end;

end.
