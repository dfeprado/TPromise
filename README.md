# TPromise
## The JS Promise concept on Delphi!
v2.0.0

### News
 - Fix issues with FMX and enable use on Android, IOs and Linux. Special thanks for [rzhghost](https://github.com/rzhghost) who finds it and helped me with tests.

> Important: Version 2 introduces break changes. If you're already using TPromise v1, you should do some changes to your code. If you do not want to use v2, keep using v1 by setting the head to tag 1.1.0. See [**Break changes introduced by V2**](#break-changes-introduced-by-v2) to see what needs to change from v1 to v2.

## Usage
All units are inside src/ folder.

uPromiseInterface defines IPromise interface and anonymous functions. You're supposed to use it more than the another unit.

The another unit is uPromise, that defines de TPromise class that implements IPromise interface.

The golden usage rule is:
 - If you just need to get a IPromise referece just use uPromiseInterface.
 - If you just need to create a IPromise, you should use both.

**Example:**
```pascal
unit uBrowserFetch;
// This units creates and returns a IPromise. So, it uses uPromiseInterface and uPromise units.

interface

uses uPromiseInterface;
// Implements a browser fetch() like function
function fetch(const url: string): IPromise<String, String>;
	
implementation

uses uPromise;
function fetch(const url: string): IPromise<String, String>;
begin
	result := TPromise.New(
		procedure (resolve: TOnResolveCallback<String>; reject: TOnRejectCallback<String>; const utils: TPromiseUtils)
		begin
			// Do some network stuff
			resolve(fetchResultStr);
		end
	);
end;
```

```pascal
unit uMainForm;

// This unit just need a reference to IPromise<T, E>, and in implementation. So it just use uPromiseInterface

interface
	...
	
implementation

uses uPromiseInterface, uBrowserFetch;

...

procedure TMainForm.SomeButtonClick(Sender: TObject);
begin
	fetch('https://my.login.page.com/?user=foo&pwd=bar')
		.&then(
			procedure (const result: string)
			begin
				// Do something with fetch result
			end
		);
end;

...
```

## Examples
See the samples/ folder. There's a network fetch sample using TPromise.

## Break changes introduced by v2
### Renamed codes
 - unit promise.pas -> uPromiseInterface
 	* TPromiseOnResolve<T> -> TOnResolveCallback<T>
	* TPromiseOnReject<E> -> TOnRejectCallback<E>
	* TPromiseWorkProcedure<T, E> -> TAsyncWork<T, E>
 - unit uPromiseConcret.pas -> uPromise.pas
	
### New features
 - Introduces IPromise<R, E>.Cancel() method. See samples notes about **Cancellable async works**
