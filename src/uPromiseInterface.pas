unit uPromiseInterface;

interface

uses
  uPromiseTypes;

type
  IPromise<T> = interface ['{4D1D721B-B3F1-46DD-BE76-C3E44A429529}']
    function getErrStr(): string;
    function getValue(): T;
    function isFulfilled(): boolean;
    function isRejected(): boolean;
    function isPending(): boolean;
    function isCanceled(): boolean;
    function then_(pProc: AnonAcceptProc<T>): IPromise<T>;
    procedure caught(pProc: AnonRejectProc);
    procedure cancel();
  end;

implementation

end.
