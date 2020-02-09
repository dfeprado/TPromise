unit uPromise.Types;

interface

type
  TAccept<T> = reference to procedure (const pArg: T);
  TReject = reference to procedure ();

implementation

end.

