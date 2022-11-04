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
  Classes, SysUtils, CustApp, SimpleBle
  { you can add units after this };

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

procedure adapter_on_scan_start(adapter: simpleble_adapter_t; userdata: PPointer);
var
  identifier: PChar;
begin
  identifier := simpleble_adapter_identifier(adapter);
  if identifier = '' then
    Exit;
  writeln('Adapter ' + identifier + ' started scanning.');
  simpleble_free(@identifier);
end;

procedure adapter_on_scan_stop(adapter: simpleble_adapter_t; userdata: PPointer);
var
  identifier: PChar;
begin
  identifier := simpleble_adapter_identifier(adapter);
  if identifier = '' then
    Exit;
  writeln('Adapter ' + identifier + ' stopped scanning.');
  simpleble_free(@identifier);
end;

procedure adapter_on_scan_found(adapter: simpleble_adapter_t; peripheral: simpleble_peripheral_t; userdata: PPointer);
var
  adapter_identifier: PChar;
  peripheral_identifier: PChar;
  peripheral_address: PChar;
begin
  adapter_identifier := simpleble_adapter_identifier(adapter);
  peripheral_identifier := simpleble_adapter_identifier(peripheral);
  peripheral_address := simpleble_peripheral_address(peripheral);
  if (adapter_identifier = '') or (peripheral_address = '') then
    Exit;
  writeln('Adapter ' + adapter_identifier + ' found device: ' + peripheral_identifier + ' [' + peripheral_address + ']');
  simpleble_peripheral_release_handle(peripheral);
  simpleble_free(@peripheral_address);
  simpleble_free(@peripheral_identifier);
end;

procedure adapter_on_scan_updated(adapter: simpleble_adapter_t; peripheral: simpleble_peripheral_t; userdata: PPointer);
var
  adapter_identifier: PChar;
  peripheral_identifier: PChar;
  peripheral_address: PChar;
begin
  adapter_identifier := simpleble_adapter_identifier(adapter);
  peripheral_identifier := simpleble_adapter_identifier(peripheral);
  peripheral_address := simpleble_peripheral_address(peripheral);
  if (adapter_identifier = '') or (peripheral_address = '') then
    Exit;
  writeln('Adapter ' + adapter_identifier + ' updated device: ' + peripheral_identifier + ' [' + peripheral_address + ']');
  simpleble_peripheral_release_handle(peripheral);
  simpleble_free(@peripheral_address);
  simpleble_free(@peripheral_identifier);
end;

{ -------------------------------- }


procedure TSimpleBleScanExample.DoRun;
var
  ErrorMsg: String;
  adapter: simpleble_adapter_t;

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
  if simpleble_adapter_get_count() = 0 then
  begin
    writeln('No BLE adapter was found.');
    Terminate;
    Exit;
  end;

  // get a handle for the BLE adapter
  adapter := simpleble_adapter_get_handle(0);
  if adapter = 0 then
  begin
    writeln('Could not get handle for BLE adapter.');
    Terminate;
    Exit
  end;
  writeln('Found BLE adapter and got handle.');

  // register SimpleBLE scan callback functions
  simpleble_adapter_set_callback_on_scan_start(adapter, @adapter_on_scan_start, Nil);
  simpleble_adapter_set_callback_on_scan_stop(adapter, @adapter_on_scan_stop, Nil);
  simpleble_adapter_set_callback_on_scan_found(adapter, @adapter_on_scan_found, Nil);
  simpleble_adapter_set_callback_on_scan_updated(adapter, @adapter_on_scan_updated, Nil);

  // start BLE scanning for 5 seconds
  simpleble_adapter_scan_for(adapter, 5000);

  // wait for enter key
  readln();

  // release the BLE handle
  simpleble_adapter_release_handle(adapter);

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
  writeln('Usage: ', ExeName, ' -h');
end;


var
  Application: TSimpleBleScanExample;
begin
  Application:=TSimpleBleScanExample.Create(nil);
  Application.Title:='SimpleBleScanTest';
  Application.Run;
  Application.Free;
end.

