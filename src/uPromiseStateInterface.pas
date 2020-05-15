unit uPromiseStateInterface;

interface

uses
  uPromiseTypes;

type
    IPromiseState<T> = interface ['{4F20B1E6-BF9A-4C6B-9F58-120D2F19A553}']
      function getErrStr(): string;
      function getStateStr(): string;
      function getValue(): T;
      procedure setAccept(accept: AnonAcceptProc<T>);
      procedure setReject(reject: AnonRejectProc);
      procedure cancel();
    end;

implementation

end.
