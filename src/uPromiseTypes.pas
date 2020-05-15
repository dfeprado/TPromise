unit uPromiseTypes;

interface

type
  AcceptProc<T> = procedure (value: T) of object;
  RejectProc = procedure (expl: string = '') of object;
  PromiseProc<T> = reference to procedure (accept: AcceptProc<T>; reject: RejectProc);
  AnonAcceptProc<T> = reference to procedure(value: T);
  AnonRejectProc = reference to procedure (expl: string = '');

implementation

end.

