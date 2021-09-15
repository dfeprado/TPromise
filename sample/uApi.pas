unit uApi;

interface

uses
  uPromiseInterface;

type
  TApi = class abstract
    // this show you a real fetch example using TPromise
    class function fetch(): IPromise<String, String>;
    // this show you a example of a process that needs to "sleep()" and how to implement it.
    class function fakeLongFetch(): IPromise<String, String>;
  end;

implementation

{ TApi }

uses
  System.Net.HttpClient, uPromise, System.Classes, System.SysUtils;

class function TApi.fakeLongFetch: IPromise<String, String>;
begin
  result := TPromise<String, String>.New(
    procedure (resolve: TOnResolveCallback<String>; reject: TOnRejectCallback<String>; const utils: TPromiseUtils)
    begin
      // Use utils.Delay() instead of Sleep(). This makes the background work cancellable
      utils.Delay(10000); // waits here for 10 seconds...
      resolve('Click to run again!');
    end
  );
end;

class function TApi.fetch: IPromise<String, String>;
begin
  result := TPromise<String, String>.New(
    procedure (resolve: TOnResolveCallback<String>; reject: TOnRejectCallback<String>; const utils: TPromiseUtils)
    var
      client: THttpClient;
      resContent: TStringStream;
      httpRes: IHTTPResponse;
    begin
      resContent := TStringStream.Create();
      client := THttpClient.Create();
      try
        try
          httpRes := client.Get('https://jsonplaceholder.typicode.com/comments', resContent);
          if httpRes.StatusCode = 200 then
            resolve(resContent.DataString)
          else
            reject('HTTP GET Error: '+httpRes.StatusCode.ToString());
        except
          on E:Exception do
            reject(E.Message);
        end;
      finally
        resContent.Free();
        client.Free();
      end;
    end
  );
end;

end.
