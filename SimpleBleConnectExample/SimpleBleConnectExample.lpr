program SimpleBleConnectExample;

{$mode objfpc}{$H+}

{ Lazarus / Free Pascal BLE connect example for SimpleBLE library.

  This project is Copyright (c) 2022 Erik Lins and released under the MIT License.
    https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library

  This example is a port of the C connect example in SimpleBLE to Lazarus/FreePascal.
    https://github.com/OpenBluetoothToolbox/SimpleBLE/tree/main/examples/simpleble/c/connect

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

  { TSimpleBleConnectExample }

  TSimpleBleConnectExample = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

const
  PERIPHERAL_LIST_SIZE = 10;

var
  peripheral_list: array [0..PERIPHERAL_LIST_SIZE-1] of simpleble_peripheral_t;
  peripheral_list_len: size_t = 0;
  adapter: simpleble_adapter_t = 0;


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
  writeln('Adapter ' + identifier + ' started scanning.');
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
  if peripheral_list_len < PERIPHERAL_LIST_SIZE then
  begin
    // Save the peripheral
    peripheral_list[peripheral_list_len] := peripheral;
    Inc(peripheral_list_len)
  end
  else
  begin
    // As there was no space left for this peripheral, release the associated handle.
    simpleble_peripheral_release_handle(peripheral);
  end;
  simpleble_free(@peripheral_address);
  simpleble_free(@peripheral_identifier);
end;

{ -------------------------------- }


procedure TSimpleBleConnectExample.DoRun;
var
  ErrorMsg: String;
  adapter: simpleble_adapter_t;
  err_code: simpleble_err_t = SIMPLEBLE_SUCCESS;
  i, j, k, selection, services_count: Integer;
  peripheral: simpleble_peripheral_t;
  peripheral_identifier: PChar;
  peripheral_address: PChar;
  service: simpleble_service_t;
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

  // start BLE scanning for 5 seconds
  simpleble_adapter_scan_for(adapter, 5000);

  // show list of found devices
  writeln('The following devices were found:');
  for i := 0 to (peripheral_list_len - 1) do
  begin
    peripheral := peripheral_list[i];
    peripheral_identifier := simpleble_peripheral_identifier(peripheral);
    peripheral_address := simpleble_peripheral_address(peripheral);
    writeln('[' + IntToStr(i) + '] ' + peripheral_identifier + ' [' + peripheral_address + ']');
    simpleble_free(@peripheral_identifier);
    simpleble_free(@peripheral_address);
  end;

  // select a device to connect to
  selection := -1;
  write('Please select a device to connect to: ');
  readln(selection);
  if (selection < 0) or (selection >= peripheral_list_len) then
  begin
    writeln('Invalid selection.');
    Terminate;
  end;

  // connect to device
  peripheral := peripheral_list[selection];
  peripheral_identifier := simpleble_peripheral_identifier(peripheral);
  peripheral_address := simpleble_peripheral_address(peripheral);
  writeln('Connecting to ' + peripheral_identifier + ' [' + peripheral_address + ']');
  simpleble_free(@peripheral_identifier);
  simpleble_free(@peripheral_address);
  err_code := simpleble_peripheral_connect(peripheral);
  if err_code <> SIMPLEBLE_SUCCESS then
  begin
    writeln('Failed to connect.');
    Terminate;
  end;
  services_count := simpleble_peripheral_services_count(peripheral);
  writeln('Successfully connected, listing ' + IntToStr(services_count) + ' services.');

  // show gatt table with services and characteristics
  for i := 0 to (services_count - 1) do
  begin
    err_code := simpleble_peripheral_services_get(peripheral, i, service);
    if err_code <> SIMPLEBLE_SUCCESS then
    begin
      writeln('Failed to get service.');
      Terminate;
    end;
    writeln('Service: ' + service.uuid.value + ' - (' + IntToStr(service.characteristic_count) + ')');
    for j := 0 to (service.characteristic_count - 1) do
    begin
      writeln('  Characteristic: ' + service.characteristics[j].uuid.value + ' - (' + IntToStr(service.characteristics[j].descriptor_count) + ')');
      for k := 0 to (service.characteristics[j].descriptor_count - 1) do
        writeln('    Descriptor: ' + service.characteristics[j].descriptors[k].uuid.value);
    end;
  end;

  // wait for enter
  ReadLn();

  // and disconnect again from device
  simpleble_peripheral_disconnect(peripheral);

  // release the BLE handle
  simpleble_adapter_release_handle(adapter);

  // stop program loop
  Terminate;
end;

constructor TSimpleBleConnectExample.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSimpleBleConnectExample.Destroy;
var
  i: size_t;
begin
  inherited Destroy;
  writeln('Releasing allocated resources.');
  // Release all saved peripherals
  for i := 0 to (peripheral_list_len - 1) do
    simpleble_peripheral_release_handle(peripheral_list[i]);
  // Let's not forget to release the associated handle.
  simpleble_adapter_release_handle(adapter);
end;

procedure TSimpleBleConnectExample.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;


var
  Application: TSimpleBleConnectExample;
begin
  Application:=TSimpleBleConnectExample.Create(nil);
  Application.Title:='SimpleBleScanTest';
  Application.Run;
  Application.Free;
end.

