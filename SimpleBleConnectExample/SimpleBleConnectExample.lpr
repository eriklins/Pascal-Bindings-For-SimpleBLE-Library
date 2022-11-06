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
  Classes, SysUtils, CustApp, SimpleBle;

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
  PeripheralList: array [0..PERIPHERAL_LIST_SIZE-1] of TSimpleBlePeripheral;
  PeripheralListLen: Integer = 0;
  Adapter: TSimpleBleAdapter = 0;


{ Callback functions for SimpleBLE }

procedure AdapterOnScanStart(Adapter: TSimpleBleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := simpleble_adapter_identifier(Adapter);
  if Identifier = '' then
    Exit;
  WriteLn('Adapter ' + Identifier + ' started scanning.');
end;

procedure AdapterOnScanStop(Adapter: TSimpleBleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := simpleble_adapter_identifier(Adapter);
  if Identifier = '' then
    Exit;
  WriteLn('Adapter ' + Identifier + ' started scanning.');
end;

procedure AdapterOnScanFound(Adapter: TSimpleBleAdapter; Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
var
  AdapterIdentifier: PChar;
  PeripheralIdentifier: PChar;
  PeripheralAddress: PChar;
begin
  AdapterIdentifier := simpleble_adapter_identifier(Adapter);
  PeripheralIdentifier := simpleble_peripheral_identifier(Peripheral);
  PeripheralAddress := simpleble_peripheral_address(Peripheral);
  if (AdapterIdentifier = '') or (PeripheralAddress = '') then
    Exit;
  WriteLn('Adapter ' + AdapterIdentifier + ' found device: ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
  if PeripheralListLen < PERIPHERAL_LIST_SIZE then
  begin
    // Save the Peripheral
    PeripheralList[PeripheralListLen] := Peripheral;
    Inc(PeripheralListLen)
  end
  else
  begin
    // As there was no space left for this Peripheral, release the associated handle.
    simpleble_peripheral_release_handle(Peripheral);
  end;
end;

{ -------------------------------- }


procedure TSimpleBleConnectExample.DoRun;
var
  ErrorMsg: String;
  Adapter: TSimpleBleAdapter;
  ErrCode: TSimpleBleErr = SIMPLEBLE_SUCCESS;
  i, j, k, Selection, ServicesCount: Integer;
  Peripheral: TSimpleBlePeripheral;
  PeripheralIdentifier: PChar;
  PeripheralAddress: PChar;
  Service: TSimpleBleService;
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
    WriteLn('No BLE adapter was found.');
    Terminate;
    Exit;
  end;

  // get a handle for the BLE Adapter
  Adapter := simpleble_adapter_get_handle(0);
  if Adapter = 0 then
  begin
    WriteLn('Could not get handle for BLE adapter.');
    Terminate;
    Exit
  end;
  WriteLn('Found BLE adapter and got handle.');

  // register SimpleBLE scan callback functions
  simpleble_adapter_set_callback_on_scan_start(Adapter, @AdapterOnScanStart, Nil);
  simpleble_adapter_set_callback_on_scan_stop(Adapter, @AdapterOnScanStop, Nil);
  simpleble_adapter_set_callback_on_scan_found(Adapter, @AdapterOnScanFound, Nil);

  // start BLE scanning for 5 seconds
  simpleble_adapter_scan_for(Adapter, 5000);

  // show list of found devices
  WriteLn('The following devices were found:');
  for i := 0 to (PeripheralListLen - 1) do
  begin
    Peripheral := PeripheralList[i];
    PeripheralIdentifier := simpleble_peripheral_identifier(Peripheral);
    PeripheralAddress := simpleble_peripheral_address(Peripheral);
    WriteLn('[' + IntToStr(i) + '] ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
    simpleble_free(@PeripheralIdentifier);
    simpleble_free(@PeripheralAddress);
  end;

  // select a device to connect to
  Selection := -1;
  write('Please select a device to connect to: ');
  ReadLn(Selection);
  if (Selection < 0) or (Selection >= PeripheralListLen) then
  begin
    WriteLn('Invalid selection.');
    Terminate;
  end;

  // connect to device
  Peripheral := PeripheralList[Selection];
  PeripheralIdentifier := simpleble_peripheral_identifier(Peripheral);
  PeripheralAddress := simpleble_peripheral_address(Peripheral);
  WriteLn('Connecting to ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
  simpleble_free(@PeripheralIdentifier);
  simpleble_free(@PeripheralAddress);
  ErrCode := simpleble_peripheral_connect(Peripheral);
  if ErrCode <> SIMPLEBLE_SUCCESS then
  begin
    WriteLn('Failed to connect.');
    Terminate;
  end;
  ServicesCount := simpleble_peripheral_services_count(Peripheral);
  WriteLn('Successfully connected, listing ' + IntToStr(ServicesCount) + ' services.');

  // show gatt table with services and characteristics
  for i := 0 to (ServicesCount - 1) do
  begin
    ErrCode := simpleble_peripheral_services_get(Peripheral, i, Service);
    if ErrCode <> SIMPLEBLE_SUCCESS then
    begin
      WriteLn('Failed to get service.');
      Terminate;
    end;
    WriteLn('Service: ' + Service.Uuid.Value + ' - (' + IntToStr(Service.CharacteristicCount) + ')');
    for j := 0 to (Service.CharacteristicCount-1) do
    begin
      WriteLn('  Characteristic: ' + Service.Characteristics[j].Uuid.Value + ' - (' + IntToStr(Service.Characteristics[j].DescriptorCount) + ')');
      for k := 0 to (Service.Characteristics[j].DescriptorCount - 1) do
        WriteLn('    Descriptor: ' + Service.Characteristics[j].Descriptors[k].Uuid.Value);
    end;
  end;

  // wait for enter
  ReadLn();

  // and disconnect again from device
  simpleble_peripheral_disconnect(Peripheral);

  // release the BLE handle
  simpleble_adapter_release_handle(Adapter);

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
  i: Integer;
begin
  inherited Destroy;
  WriteLn('Releasing allocated resources.');
  // Release all saved peripherals
  for i := 0 to (PeripheralListLen - 1) do
    simpleble_peripheral_release_handle(PeripheralList[i]);
  // Let's not forget to release the associated handle.
  simpleble_adapter_release_handle(Adapter);
end;

procedure TSimpleBleConnectExample.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
end;


var
  Application: TSimpleBleConnectExample;
begin
  Application:=TSimpleBleConnectExample.Create(nil);
  Application.Title:='SimpleBleScanTest';
  Application.Run;
  Application.Free;
end.

