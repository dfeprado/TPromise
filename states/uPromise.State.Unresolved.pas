unit uPromise.State.Unresolved;

interface

uses uPromise.Types, System.Threading, uIPromise, uPromise.State.Base;

type
    TUnresolvedState<T> = class (TBaseState<T>)
      private
        fFuture: IFuture<T>;
        fSoTask: ITask;
        fPromise: IPromise<T>;

      protected
        procedure setNextState(); override;

      public
        constructor Create(pPromise: IPromise<T>; pFuture: IFuture<T>);

        function getErrorStr(): string; override;
        function getStateStr(): string; override;
        procedure so(pProc: TAccept<T>); override;
        procedure caught(pProc: TReject); override;

    end;

implementation

uses
  uPromise.State.Resolved, uPromise.State.Rejected, System.SysUtils,
  uPromise.State;

{ TUnresolvedState<T> }

procedure TUnresolvedState<T>.caught(pProc: TReject);
begin
    self.fRejectProc := pProc;
end;

constructor TUnresolvedState<T>.Create(pPromise: IPromise<T>; pFuture: IFuture<T>);
begin
    fPromise := pPromise;
    fFuture := pFuture;
end;

function TUnresolvedState<T>.getErrorStr: string;
begin
    result := self.fErrorValue;
end;

function TUnresolvedState<T>.getStateStr: string;
begin
    result := 'unresolved';
end;

procedure TUnresolvedState<T>.setNextState;
begin
    if (fFuture.Status = TTaskStatus.Exception) then
    begin
        fPromise.changeState(TRejectedState<T>.Create(fErrorValue, fRejectProc));
    end
    else
    begin
        fPromise.changeState(TResolvedState<T>.Create(fFuture.Value, fAcceptProc));
    end;
end;

procedure TUnresolvedState<T>.so(pProc: TAccept<T>);
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
          begin
              try
                TTask.WaitForAll([fFuture]);
                self.syncAccept(fFuture.Value);
              except
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
