unit promise.concret;

interface

uses
  promise, System.Threading, System.Generics.Collections, System.SyncObjs;

type

  TPromise<T, E> = class (TInterfacedObject, IPromise<T, E>)
    // static members
    public
      class function New(AsyncWork: TPromiseWorkProcedure<T, E>): IPromise<T, E>;
      class function NewAsync(AsyncWork: TPromiseWorkProcedure<T, E>): IPromise<T, E>;

    // instance members
    private
      Registration: IPromise<T, E>;
      IsAsync: Boolean;
      IsResolved: Boolean;
      IsRejected: Boolean;
      IsCanceled: Boolean;
      WorkTask: ITask;
      ResolveValue: T;
      RejectValue: E;
      OnResolve: TPromiseOnResolve<T>;
      OnReject: TPromiseOnReject<E>;
      constructor Create(AsyncWork: TPromiseWorkProcedure<T, E>);
      function SetAsync(): TPromise<T, E>;
      procedure Resolve(const Value: T);
      procedure Reject(const ErrorValue: E);
      procedure DoOnResolve(); inline;
      procedure DoOnReject(); inline;

    public
      // @IPromise implementation
      function &Then(OnResolve: TPromiseOnResolve<T>): IPromise<T, E>;
      function Catch(OnReject: TPromiseOnReject<E>): IPromise<T, E>;
      procedure Cancel();
  end;

implementation

uses
  System.Classes;

procedure TPromise<T, E>.Resolve(const Value: T);
begin
  if IsCanceled then
    Exit();

  ResolveValue := Value;
  IsResolved := True;
  DoOnResolve();
end;

function TPromise<T, E>.SetAsync: TPromise<T, E>;
begin
  Result := Self;
  IsAsync := True;
end;

procedure TPromise<T, E>.Reject(const ErrorValue: E);
begin
  if IsCanceled then
    Exit();

  RejectValue := ErrorValue;
  IsRejected := True;
  DoOnReject();
end;

function TPromise<T, E>.&Then(OnResolve: TPromiseOnResolve<T>): IPromise<T, E>;
begin
  Result := Self;

  if IsCanceled or ((WorkTask.Status <> TTaskStatus.Running) and Assigned(Self.OnResolve)) then
    Exit();

  if not IsResolved then
    Self.OnResolve := OnResolve
  else
    DoOnResolve();
end;

function TPromise<T, E>.Catch(OnReject: TPromiseOnReject<E>): IPromise<T, E>;
begin
  Result := Self;

  if IsCanceled or ((WorkTask.Status <> TTaskStatus.Running) and Assigned(Self.OnReject)) then
    Exit();

  if not IsRejected then
    Self.OnReject := OnReject
  else
    DoOnReject();
end;

procedure TPromise<T, E>.Cancel;
begin
  if WorkTask.Status = TTaskStatus.Running then
    WorkTask.Cancel();

  IsCanceled := True;
end;

constructor TPromise<T, E>.Create(AsyncWork: TPromiseWorkProcedure<T, E>);
begin
  Registration := Self;
  WorkTask := TTask.Create(
    procedure ()
    begin
      try
        AsyncWork(Resolve, Reject);
      finally
        Registration := Nil;
      end;
    end
  );
end;

procedure TPromise<T, E>.DoOnReject;
begin
  if not Assigned(OnReject) then
    Exit();

  if not IsAsync then
  begin
    TThread.Queue(
      Nil,
      procedure ()
      begin
        OnReject(RejectValue);
      end
    );
  end
  else
    OnReject(RejectValue);
end;

procedure TPromise<T, E>.DoOnResolve;
begin
  if not Assigned(OnResolve) then
    Exit();

  if not IsAsync then
  begin
    TThread.Queue(
      Nil,
      procedure ()
      begin
        OnResolve(ResolveValue);
      end
    );
  end
  else
    OnResolve(ResolveValue);
end;

class function TPromise<T, E>.New(AsyncWork: TPromiseWorkProcedure<T, E>): IPromise<T, E>;
var
  promiseInstance: TPromise<T, E>;
begin
  Result := Nil;
  if not Assigned(AsyncWork) then
    Exit();

  promiseInstance := TPromise<T, E>.Create(AsyncWork);
  promiseInstance.WorkTask.Start();

  Result := promiseInstance;
end;

class function TPromise<T, E>.NewAsync(AsyncWork: TPromiseWorkProcedure<T, E>): IPromise<T, E>;
var
  promiseInstance: TPromise<T, E>;
begin
  Result := Nil;
  if not Assigned(AsyncWork) then
    Exit();

  promiseInstance := TPromise<T, E>.Create(AsyncWork).SetAsync();
  promiseInstance.WorkTask.Start();

  Result := promiseInstance;
end;

end.
