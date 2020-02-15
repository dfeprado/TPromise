unit uPromise.State.Unresolved;

interface

uses uPromise.Types, System.Threading, uIPromise, uPromise.State.Base,
  uPromise.bridge.state;

type
    TUnresolvedState<T> = class (TBaseState<T>)
      private
        fFuture: IFuture<T>;
        fSoTask: ITask;
        fPromise: IPromise<T>;
        fCanceled: boolean;
        fStateBridge: TStateChangeBridge<T>;

      protected
        procedure setNextState(); override;

      public
        constructor Create(pPromise: IPromise<T>; pFuture: IFuture<T>; pChangeBridge: TStateChangeBridge<T>);

        function getErrorStr(): string; override;
        function getStateStr(): string; override;
        function getValue(): T; override;
        procedure then_(pProc: TAccept<T>); override;
        procedure caught(pProc: TReject); override;
        procedure cancel(); override;

    end;

implementation

uses
  uPromise.State.Resolved, uPromise.State.Rejected, System.SysUtils,
  uPromise.State, uPromise.State.Canceled;

{ TUnresolvedState<T> }

procedure TUnresolvedState<T>.cancel;
begin
    fCanceled := true;
    fFuture.Cancel();
end;

procedure TUnresolvedState<T>.caught(pProc: TReject);
begin
    self.fRejectProc := pProc;
end;

constructor TUnresolvedState<T>.Create(pPromise: IPromise<T>; pFuture: IFuture<T>; pChangeBridge: TStateChangeBridge<T>);
begin
    fPromise := pPromise;
    fFuture := pFuture;
    fStateBridge := pChangeBridge;
end;

function TUnresolvedState<T>.getErrorStr: string;
begin
    result := self.fErrorValue;
end;

function TUnresolvedState<T>.getStateStr: string;
begin
    result := 'unresolved';
end;

function TUnresolvedState<T>.getValue: T;
begin
end;

procedure TUnresolvedState<T>.setNextState;
begin
    if (fFuture.Status = TTaskStatus.Exception) then
    begin
        if (fCanceled) then
        begin
            fStateBridge.change(TCanceledState<T>.Create());
        end
        else
        begin
            fStateBridge.change(TRejectedState<T>.Create(fErrorValue, fRejectProc));
        end;
    end
    else
    begin
        fStateBridge.change(TResolvedState<T>.Create(fFuture.Value, fAcceptProc));
    end;

    fStateBridge.Free();
end;

procedure TUnresolvedState<T>.then_(pProc: TAccept<T>);
begin
    if (Assigned(fSoTask)) then
    begin
        Exit;
    end;

    fAcceptProc := pProc;

    if ((fFuture.Status = TTaskStatus.Running) or (fFuture.Status = TTaskStatus.WaitingToRun) )then
    begin
        fSoTask := TTask.Run(
          procedure
          var
            xResult: T;
            xValue: string;
            xFuture: IFuture<T>;
          begin
              xFuture := fFuture;

              try
                xFuture.Wait(INFINITE);

                self.syncAccept(xFuture.Value);

              except
                on e: EOperationCanceled do
                begin
                    self.syncCanceled();
                end;

                on e: EAggregateException do
                begin
                    fErrorValue := e.InnerExceptions[0].Message;
                    self.syncReject();
                end;
              end;
          end
        );
    end;
end;

end.
