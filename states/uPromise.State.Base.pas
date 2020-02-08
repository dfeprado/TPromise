unit uPromise.State.Base;

interface

uses
  uPromise.State, uPromise.Types;

type
  TBaseState<T> = class abstract (TInterfacedObject, IPromiseState<T>)
    protected
      fAcceptProc: TAccept<T>;
      fRejectProc: TReject;
      fErrorValue: String;

      procedure syncAccept(pValue: T);
      procedure syncReject;
      procedure setNextState(); virtual; abstract;

      //--from IPromiseState<T>
      function getErrorStr(): string; virtual; abstract;
      function getStateStr(): string; virtual; abstract;
      procedure so(pProc: TAccept<T>); virtual; abstract;
      procedure caught(pProc: TReject); virtual; abstract;
  end;

implementation

uses
  System.Classes;

{ TBaseState<T> }

procedure TBaseState<T>.syncAccept(pValue: T);
begin
    TThread.Queue(
      nil,
      procedure
      begin
          if (Assigned(fAcceptProc)) then
          begin
              fAcceptProc(pValue);
          end;
          self.setNextState();
      end
    );
end;

procedure TBaseState<T>.syncReject;
begin
    TThread.Queue(
      nil,
      procedure
      begin
          if (Assigned(fRejectProc)) then
          begin
              fRejectProc();
          end;
          self.setNextState();
      end
    );
end;

end.
