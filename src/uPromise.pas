unit uPromise;

interface

uses
  uPromiseInterface, System.Threading, System.SyncObjs;

type
  /// <summary>
  ///   Defines a classe that implements IPromise&lt;R, E&gt; (from uPromiseInterface).
  /// </summary>
  ///  <remarks>
  ///  The <c>TPromise.New()</c> method returns a IPromise that runs a given work on background and, when it fulfills, synchronizes with
  ///  the main thread. The <c>TPromise.NewAsync()</c> method returns a IPromise that, when fulfilled, does not synchronize. So, you should
  ///  use synchronization mechanisms to avoid race conditions. The <c>NewAsync()</c> method is intended to use inside of a thread other than the main one.
  ///  If you're using just one thread (ie main thread), <c>New()</c> should be what yout want.
  ///  </remarks>
  TPromise<R, E> = class (TInterfacedObject, IPromise<R, E>)
  private type
    TPromiseState = (psPending, psFulfilled, psRejected, psCancelled);

  public
    /// <summary>
    ///   Returns a new IPromise instance that runs a work on backgroud. When fulfilled/rejected, the result
    ///  will be synchronize with main thread.
    /// </summary>
    ///  <param name="asyncWork">A work that must run on background.</param>
    class function New(asyncWork: TAsyncWork<R, E>): IPromise<R, E>;
    /// <summary>
    ///   Returns a new IPromise instance that runs a work on backgroud. Does not synchronize with main thread when fulfilled/rejected. So
    ///  be aware to avoid race condition; use synchronization mechanisms (e.g. Mutex, CriticalSection, SpinLock...)
    /// </summary>
    ///  <param name="asyncWork">A work that must run on background.</param>
    class function NewAsync(asyncWork: TAsyncWork<R, E>): IPromise<R, E>;

  private
    utils: TPromiseUtils;
    selfPromise: IPromise<R, E>;
    isSync: Boolean;
    syncer: TCriticalSection;
    promiseTask: ITask;
    state: TPromiseState;
    asyncWork: TAsyncWork<R, E>;
    fulfilledValue: R;
    rejectedValue: E;
    onResCallback: TOnResolveCallback<R>;
    onRejCallback: TOnRejectCallback<E>;
    constructor Create(asyncWork: TAsyncWork<R, E>; sync: Boolean);
    /// <summary>
    ///   Updates the promise state. Returns <c>true</c> if the state was updated.
    /// </summary>
    function updateState(const toState: TPromiseState): Boolean;
    procedure internalResolve(const value: R);
    procedure internalReject(const err: E);

  protected
    destructor Destroy(); override;

  public
    //@IMPLEMENTING IPromise<R, E>
    function &Then(callback: TOnResolveCallback<R>): IPromise<R, E>;
    function Catch(callback: TOnRejectCallback<E>): IPromise<R, E>;
    procedure Cancel();
  end;

implementation

uses
  System.Classes, System.SysUtils;

{ TPromise<R, E> }

procedure TPromise<R, E>.Cancel;
begin
  // Just cancel pending promises.
  syncer.Enter();
  if state = psPending then
  begin
    state := psCancelled;
    promiseTask.Cancel();
  end;
  syncer.Leave();
end;

constructor TPromise<R, E>.Create(asyncWork: TAsyncWork<R, E>; sync: Boolean);
begin
  // When a new promise is created, it puts the background work, aka asyncWork, on a TTask.
  // The asyncWork receives 3 arguments:
  //  resolve: an anonymous function that should be called when promise fulfills
  //  reject: an anonymous function that should be called when promise rejects
  //  utils: the TPromiseUtils record.
  //
  //  The task watch about expections. Exceptions should be captured inside asyncWork.
  // Otherwise, they will be ignored.
  //
  // internalResolve() fulfills the promise. internalReject reject the promise.
  //
  // The promise keeps a self reference to preserve the interfaced object. It is released
  // when the promise fulfills, reject or fail (with uncaptured exception). So, you do not
  // need to maintain a promise reference.
  syncer := TCriticalSection.Create();
  selfPromise := Self;
  isSync := sync;
  self.asyncWork := asyncWork;
  promiseTask := TTask.Run(procedure () begin
    try
      asyncWork(
        procedure (const value: R) begin
          self.internalResolve(value);
        end,
        procedure (const err: E) begin
          self.internalReject(err);
        end,
        utils
      )
    except
      //IGNORED
    end;
    selfPromise := nil;
  end);
end;

destructor TPromise<R, E>.Destroy;
begin
  syncer.Free();
  inherited;
end;

procedure TPromise<R, E>.internalReject(const err: E);
begin
  if not updateState(psRejected) then
    exit();

  self.rejectedValue := err;

  // If the promise has onRejCallback when it fulfills, setted by Catch(),
  // then call it in sync/async mode.
  if Assigned(onRejCallback) then
  begin
    if isSync then
      TThread.Queue(nil, procedure () begin
        onRejCallback(err);
        selfPromise := nil;
      end)
    else
    begin
      onRejCallback(err);
      selfPromise := nil;
    end;
  end;
end;

procedure TPromise<R, E>.internalResolve(const value: R);
begin
  if not updateState(psFulfilled) then
    Exit();

  // If the promise has onResCallback when it fulfills, setted by &Then(),
  // then call it in sync/async mode.
  fulfilledValue := value;
  if Assigned(self.onResCallback) then
  begin
    if isSync then
      TThread.Queue(nil, procedure () begin
        onResCallback(value);
        selfPromise := nil;
      end)
    else
    begin
      self.onResCallback(value);
      selfPromise := nil;
    end;
  end;
end;

class function TPromise<R, E>.New(asyncWork: TAsyncWork<R, E>): IPromise<R, E>;
begin
  result := TPromise<R, E>.Create(asyncWork, True);
end;

class function TPromise<R, E>.NewAsync(asyncWork: TAsyncWork<R, E>): IPromise<R, E>;
begin
  result := TPromise<R, E>.Create(asyncWork, False);
end;

function TPromise<R, E>.Catch(callback: TOnRejectCallback<E>): IPromise<R, E>;
var
  currState: TPromiseState;
begin
  // If promise's already rejected, Catch() immediatly calls callback() with result.
  // Else, it keeps the callback reference for future use.
  // If the promise's already rejected, and a callback() was already called, or the promise's cancelled, then Catch() does nothing.
  Result := self;
  syncer.Enter();
  try
    if Assigned(onRejCallback) and (state = psRejected) then
      Exit();

    currState := state;
    self.onRejCallback := callback;
  finally
    syncer.Leave();
  end;

  if currState = psRejected then
    onRejCallback(rejectedValue);
end;

function TPromise<R, E>.&Then(callback: TOnResolveCallback<R>): IPromise<R, E>;
var
  currState: TPromiseState;
begin
  // If promise's already fulfilled, &Then() immediatly calls callback() with result.
  // Else, it keeps the callback reference for future use.
  // If the promise's already rejected, and a callback() was already called, or the promise's cancelled, then &Then() does nothing.
  Result := self;
  syncer.Enter();
  try
    if Assigned(onResCallback) and (state = psFulfilled) then
      Exit();

    currState := state;
    self.onResCallback := callback;
  finally
    syncer.Leave();
  end;

  if currState = psFulfilled then
    onResCallback(fulfilledValue);
end;

function TPromise<R, E>.updateState(const toState: TPromiseState): Boolean;
begin
  Result := false;
  syncer.Enter();
  try
    // Do not update state if Cancel() was called.
    if state = psCancelled then
    begin
      selfPromise := nil;
      Exit();
    end;

    state := toState;
    result := true;
  finally
    syncer.Leave();
  end;
end;

end.
