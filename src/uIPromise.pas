unit uIPromise;

interface

uses
  uPromise.State, uPromise.Types;

type
  IPromise<T> = interface ['{4D1D721B-B3F1-46DD-BE76-C3E44A429529}']
    function getState(): string;
    function getErrorStr(): string;
    function getValue(): T;
    function isResolved(): boolean;
    function isRejected(): boolean;
    function isUnresolved(): boolean;
    function then_(pProc: TAccept<T>): IPromise<T>;
    procedure caught(pProc: TReject);
    procedure cancel();
  end;

implementation

end.
