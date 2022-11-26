program SimpleBleScanExample;

{$mode objfpc}{$H+}

{ Lazarus / Free Pascal BLE scan example for SimpleBLE library.

  This project is Copyright (c) 2022 Erik Lins and released under the MIT License.
    https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library

  This example is a port of the C scan example in SimpleBLE to Lazarus/FreePascal.
    https://github.com/OpenBluetoothToolbox/SimpleBLE/tree/main/examples/simpleble/c/scan

  The SimpleBLE library is Copyright (c) 2021-2022 Kevin Dewald and released under the MIT License.
    https://github.com/OpenBluetoothToolbox/SimpleBLE
}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, SimpleBle;

type

  { TSimpleBleScanExample }

  TSimpleBleScanExample = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


{ Callback functions for SimpleBLE }

procedure AdapterOnScanStart(Adapter: TSimplebleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  WriteLn('Adapter ' + Identifier + ' started scanning.');
  SimpleBleFree(Identifier);
end;

procedure AdapterOnScanStop(Adapter: TSimplebleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  WriteLn('Adapter ' + Identifier + ' stopped scanning.');
  SimpleBleFree(Identifier);
end;

procedure AdapterOnScanFound(Adapter: TSimplebleAdapter; Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
var
  AdapterIdentifier: PChar;
  PeripheralIdentifier: PChar;
  PeripheralAddress: PChar;
begin
  AdapterIdentifier := SimpleBleAdapterIdentifier(Adapter);
  PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
  PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
  if (AdapterIdentifier = '') or (PeripheralAddress = '') then
    Exit;
  WriteLn('Adapter ' + AdapterIdentifier + ' found device: ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
  SimpleBlePeripheralReleaseHandle(Peripheral);
  SimpleBleFree(PeripheralIdentifier);
  SimpleBleFree(PeripheralAddress);
end;

procedure AdapterOnScanUpdated(Adapter: TSimplebleAdapter; Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
var
  AdapterIdentifier: PChar;
  PeripheralIdentifier: PChar;
  PeripheralAddress: PChar;
begin
  AdapterIdentifier := SimpleBleAdapterIdentifier(Adapter);
  PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
  PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
  if (AdapterIdentifier = '') or (PeripheralAddress = '') then
    Exit;
  WriteLn('Adapter ' + AdapterIdentifier + ' updated device: ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
  SimpleBlePeripheralReleaseHandle(Peripheral);
  SimpleBleFree(PeripheralIdentifier);
  SimpleBleFree(PeripheralAddress);
end;

{ -------------------------------- }


procedure TSimpleBleScanExample.DoRun;
var
  ErrorMsg: String;
  Adapter: TSimplebleAdapter;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // look for BLE adapters
  if SimpleBleAdapterGetCount() = 0 then
  begin
    WriteLn('No BLE adapter was found.');
    Terminate;
    Exit;
  end;

  // get a handle for the BLE Adapter
  Adapter := SimpleBleAdapterGetHandle(0);
  if Adapter = 0 then
  begin
    WriteLn('Could not get handle for BLE adapter.');
    Terminate;
    Exit
  end;
  WriteLn('Found BLE adapter and got handle.');

  // register SimpleBLE scan callback functions
  SimpleBleAdapterSetCallbackOnScanStart(Adapter, @AdapterOnScanStart, Nil);
  SimpleBleAdapterSetCallbackOnScanStop(Adapter, @AdapterOnScanStop, Nil);
  SimpleBleAdapterSetCallbackOnScanFound(Adapter, @AdapterOnScanFound, Nil);
  SimpleBleAdapterSetCallbackOnScanUpdated(Adapter, @AdapterOnScanUpdated, Nil);

  // start BLE scanning for 5 seconds
  SimpleBleAdapterScanFor(Adapter, 5000);

  // wait for enter key
  ReadLn();

  // release the BLE handle
  SimpleBleAdapterReleaseHandle(Adapter);

  // stop program loop
  Terminate;
end;

constructor TSimpleBleScanExample.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSimpleBleScanExample.Destroy;
begin
  inherited Destroy;
end;

procedure TSimpleBleScanExample.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
end;


var
  Application: TSimpleBleScanExample;
begin
  Application:=TSimpleBleScanExample.Create(nil);
  Application.Title:='SimpleBleScanTest';
  Application.Run;
  Application.Free;
end.

