unit uPromisePending;

interface

uses
  System.SysUtils, uPromiseTypes, uPromiseStateInterface, System.Threading;

type
    TPromisePending<T> = class (TInterfacedObject, IPromiseState<T>)
      private type
        TStateChangerProc<T> = procedure (newState: IPromiseState<T>) of Object;
      private
        stateChanger: TStateChangerProc<T>;
        asyncTask: ITask;
        onAccept: AnonAcceptProc<T>;
        onReject: AnonRejectProc;
        undoAutoRef: TProc;
      protected
        procedure doAccept(value: T);
        procedure doReject(expl: string = '');
      public
        constructor Create(action: PromiseProc<T>; stateChanger: TStateChangerProc<T>; undoAutoRef: TProc);
        //-- From IPromiseState
        function getErrStr(): string;
        function getStateStr(): string;
        function getValue(): T;
        procedure setAccept(accept: AnonAcceptProc<T>);
        procedure setReject(reject: AnonRejectProc);
        procedure cancel();

    end;

implementation

uses
  uPromiseCanceled, uPromiseFulfilled, uPromiseRejected;

{ TUnresolvedState<T> }

procedure TPromisePending<T>.cancel;
begin
  self.asyncTask.Cancel();
  self.stateChanger(TPromiseCanceled<T>.Create(self.undoAutoRef));
end;

constructor TPromisePending<T>.Create(action: PromiseProc<T>; stateChanger: TStateChangerProc<T>; undoAutoRef: TProc);
begin
  self.stateChanger := stateChanger;
  self.undoAutoRef := undoAutoRef;
  asyncTask := TTask.Run(
    procedure
    begin
      action(self.doAccept, self.doReject);
    end
  );
end;

procedure TPromisePending<T>.doAccept(value: T);
begin
  self.stateChanger(TPromiseFulfilled<T>.Create(value, self.onAccept, self.undoAutoRef));
end;

procedure TPromisePending<T>.doReject(expl: string);
begin
  self.stateChanger(TPromiseRejected<T>.Create(expl, self.onReject, self.undoAutoRef));
end;

function TPromisePending<T>.getErrStr: string;
begin
  result := '';
end;

function TPromisePending<T>.getStateStr: string;
begin
  result := 'unresolved';
end;

function TPromisePending<T>.getValue: T;
begin
end;

procedure TPromisePending<T>.setAccept(accept: AnonAcceptProc<T>);
begin
  inherited;
  self.onAccept := accept;
end;

procedure TPromisePending<T>.setReject(reject: AnonRejectProc);
begin
  inherited;
  self.onReject := reject;
end;

end.
