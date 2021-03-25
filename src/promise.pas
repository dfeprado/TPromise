unit promise;

interface

type
  /// <summary>This is the procedure passed to the work task to Resolve() it</summary>
  TPromiseResolveProcedure<T> = procedure (const Value: T) of object;
  /// <summary>This is the procedure passed to work task to Reject() it</summary>
  TPromiseRejectProcedure<E> = procedure (const ErrorValue: E) of object;
  /// <summary>This is the procedure to execute when promise is resolved.
  ///  <para>It's setted on <code>IPromise.Then()</code></para></summary>
  TPromiseOnResolve<T> = reference to procedure (const Value: T);
  /// <summary>This is the procedure to execute when promise is rejected.
  ///  <para>It's setted on <code>IPromise.Catch()</code></para></summary>
  TPromiseOnReject<E> = reference to procedure (const ErrorValue: E);
  /// <summary>This is the work task procedure. It's the promise work's definition; what it will do in async</summary>
  TPromiseWorkProcedure<T, E> = reference to procedure (Resolve: TPromiseResolveProcedure<T>; Reject: TPromiseRejectProcedure<E>);

  /// <summary>Defines the basic operations for promises, independently of parameter type</summary>
  IPromiseBasic = interface ['{E61DCF70-4E82-45EE-A962-C8FA1D5BC024}']
    /// <summary>Cancels the promise.
    ///  <para>In DEBUG it will produce an EOperationCancelled exception. It not affects release builds</para>
    /// </summary>
    procedure Cancel();
  end;

  /// <summary>The promise with parameterized types
  ///  <para>The parameter T is the resolve type</para>
  ///  <para>The parameter E is the reject type</para>
  ///  </summary>
  IPromise<T, E> = interface (IPromiseBasic) ['{CED8D0BD-5814-442B-A8EC-D9F195005D43}']
    /// <summary>Set what procedure to call when promise is resolved</summary>
    function &Then(OnResolve: TPromiseOnResolve<T>): IPromise<T, E>;
    /// <summary>Set what procedure to call when promise is rejected</summary>
    function Catch(OnReject: TPromiseOnReject<E>): IPromise<T, E>;
  end;

implementation

end.
