unit uPromise.State;

interface

uses
  uPromise.Types;

type
    IPromiseState<T> = interface ['{4F20B1E6-BF9A-4C6B-9F58-120D2F19A553}']
      function getErrorStr(): string;
      function getStateStr(): string;
      procedure so(pProc: TAccept<T>);
      procedure caught(pProc: TReject);
    end;

implementation

end.
