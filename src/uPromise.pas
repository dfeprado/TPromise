unit uPromise;

interface

uses uPromiseInterface, uPromiseTypes, System.Threading, uPromiseStateInterface;

type
  TPromise<T> = class (TInterfacedObject, IPromise<T>)
    private
      fSelfState: IPromiseState<T>;
      autoRef: IPromise<T>;
      procedure changeState(newState: IPromiseState<T>);
      procedure undoAutoRef();
    public
      constructor Create(action: PromiseProc<T>);
      destructor Destroy; override;
      //-- from IPromise<T>
      function getErrStr(): string;
      function getValue(): T;
      function isFulfilled(): boolean;
      function isRejected(): boolean;
      function isPending(): boolean;
      function isCanceled(): boolean;
      function then_(accept: AnonAcceptProc<T>): IPromise<T>;
      procedure caught(reject: AnonRejectProc);
      procedure cancel();

  end;

implementation

uses
  System.Classes, uPromisePending;

{ TPromise<T> }

procedure TPromise<T>.cancel;
begin
  fSelfState.cancel();
end;

procedure TPromise<T>.caught(reject: AnonRejectProc);
begin
  fSelfState.setReject(reject);
end;

procedure TPromise<T>.changeState(newState: IPromiseState<T>);
begin
  fSelfState := newState;
end;

constructor TPromise<T>.Create(action: PromiseProc<T>);
begin
  fSelfState := TPromisePending<T>.Create(action, self.changeState);
  autoRef := self;
end;

destructor TPromise<T>.Destroy;
begin
  self.cancel();
  inherited;
end;

function TPromise<T>.getErrStr: string;
begin
  result := fSelfState.getErrStr();
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

function TPromise<T>.isFulfilled: boolean;
begin
  result := fSelfState.getStateStr() = 'resolved';
end;

function TPromise<T>.isPending: boolean;
begin
  result := fSelfState.getStateStr() = 'unresolved';
end;

function TPromise<T>.then_(accept: AnonAcceptProc<T>): IPromise<T>;
begin
  fSelfState.setAccept(accept);
  result := self;
end;

procedure TPromise<T>.undoAutoRef;
begin
  self.autoRef := nil;
end;

end.
