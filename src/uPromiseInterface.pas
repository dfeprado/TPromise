unit uPromiseInterface;

interface

type
  /// <summary>
  ///   Defines useful methods/fields to help background works
  /// </summary>
  TPromiseUtils = record
    /// <summary>
    ///   Sleep the background work for <c>ms</c> milliseconds. The difference between <c>System.Sleep()</c> is that
    ///  <c>Delay()</c> is aware of work cancelation, making it cancellable.
    /// </summary>
    procedure Delay(const ms: Cardinal);
  end;

  /// <summary>
  ///   Defines the fulfill anonymous function signature.
  /// </summary>
  TOnResolveCallback<R> = reference to procedure (const value: R);
  /// <summary>
  ///   Defines the reject anonymous function signature.
  /// </summary>
  TOnRejectCallback<E> = reference to procedure (const value: E);
  /// <summary>
  ///   Defines the background work anonymous function signature.
  ///  <para>Note that, besides resolve and reject anonymous functions args there's a new argument: <c>utils: TPromiseUtils</c>. This argument
  ///  is a record with useful functions, like <c>Delay()</c></para>
  /// </summary>
  TAsyncWork<R, E> = reference to procedure (resolve: TOnResolveCallback<R>; reject: TOnRejectCallback<E>; const utils: TPromiseUtils);

  /// <summary>
  ///   Defines the interface for a mechanism that mimics the JS's promise.
  /// </summary>
  IPromise<R, E> = interface ['{AB34AAD8-4DE0-44D6-B7B4-3EDEB7AC39F4}']
    /// <summary>
    ///   Defines the callback to be called when the promise fulfills.
    /// </summary>
    ///  <param name="callback">The anonymous function to be used when the promise fulfills</param>
    ///  <returns>The promise's instance. So, you can "chain" methods calls.</returns>
    function &Then(callback: TOnResolveCallback<R>): IPromise<R, E>;
    /// <summary>
    ///   Defines the callback to be called when the promise rejects.
    /// </summary>
    ///  <param name="callback">The anonymous function to be used when the promise rejects</param>
    ///  <returns>The promise's instance. So, you can "chain" methods calls.</returns>
    function Catch(callback: TOnRejectCallback<E>): IPromise<R, E>;
    /// <summary>
    ///   Calls cancel() on background work. The work should implements status checking to be cancellable.
    /// </summary>
    procedure Cancel();
  end;

implementation

uses
  System.Threading, System.Classes, System.SysUtils;

{ TPromiseUtils }

procedure TPromiseUtils.Delay(const ms: Cardinal);
var
  elapsedMs: Cardinal;
begin
  elapsedMs := 0;
  while elapsedMs < ms do
  begin
    TTask.CurrentTask.CheckCanceled();
    Inc(elapsedMs, 100);
    TThread.Yield();
    Sleep(100);
  end;
end;

end.
