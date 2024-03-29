program SimpleBleNotifyExample;

{$mode objfpc}{$H+}

{ Lazarus / Free Pascal BLE notify example for SimpleBLE library.

  This project is Copyright (c) 2022 Erik Lins and released under the MIT License.
    https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library

  This example is a port of the C notify example in SimpleBLE to Lazarus/FreePascal.
    https://github.com/OpenBluetoothToolbox/SimpleBLE/tree/main/examples/simpleble/c/notify

  The SimpleBLE library is Copyright (c) 2021-2022 Kevin Dewald and released under the MIT License.
    https://github.com/OpenBluetoothToolbox/SimpleBLE
}

{$UNDEF DYNAMIC_LOADING}
{$IFDEF WINDOWS}
  //{$DEFINE DYNAMIC_LOADING}    { UNCOMMENT IF YOU WANT DYNAMIC LOADING }
{$ENDIF}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, SimpleBle;

type

  { TSimpleBleNotifyExample }

  TSimpleBleNotifyExample = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  TServiceCharacteristic = record
    Service: TSimpleBleUuid;
    Characteristic: TSimpleBleUuid;
  end;


const
  PERIPHERAL_LIST_SIZE = 10;
  SERVICES_LIST_SIZE = 32;

var
  CharacteristicList: array [0..SERVICES_LIST_SIZE-1] of TServiceCharacteristic;
  PeripheralList: array [0..PERIPHERAL_LIST_SIZE-1] of TSimpleBlePeripheral;
  PeripheralListLen: NativeUInt = 0;
  Adapter: TSimpleBleAdapter = 0;


{ Callback functions for SimpleBLE }

procedure AdapterOnScanStart(Adapter: TSimpleBleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  WriteLn('Adapter ' + Identifier + ' started scanning.');
  SimpleBleFree(Identifier);
end;

procedure AdapterOnScanStop(Adapter: TSimpleBleAdapter; Userdata: PPointer);
var
  Identifier: PChar;
begin
  Identifier := SimpleBleAdapterIdentifier(Adapter);
  if Identifier = '' then
    Exit;
  WriteLn('Adapter ' + Identifier + ' started scanning.');
  SimpleBleFree(Identifier);
end;

procedure AdapterOnScanFound(Adapter: TSimpleBleAdapter; Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
var
  AdapterIdentifier: PChar;
  PeripheralIdentifier: PChar;
  PeripheralAddress: PChar;
begin
  AdapterIdentifier := SimpleBleAdapterIdentifier(adapter);
  PeripheralIdentifier := SimpleBlePeripheralIdentifier(peripheral);
  PeripheralAddress := SimpleBlePeripheralAddress(peripheral);
  if (AdapterIdentifier = '') or (PeripheralAddress = '') then
    Exit;
  WriteLn('Adapter ' + AdapterIdentifier + ' found device: ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
  if PeripheralListLen < PERIPHERAL_LIST_SIZE then
  begin
    // Save the peripheral
    PeripheralList[PeripheralListLen] := peripheral;
    Inc(PeripheralListLen)
  end
  else
  begin
    // As there was no space left for this peripheral, release the associated handle.
    SimpleBlePeripheralReleaseHandle(peripheral);
  end;
  SimpleBleFree(PeripheralIdentifier);
  SimpleBleFree(PeripheralAddress);
end;

procedure PeripheralOnNotify(Service: TSimpleBleUuid; Characteristic: TSimpleBleUuid; Data: PByte; DataLength: NativeUInt; Userdata: PPointer);
var
  i: Integer;
begin
  write('Received[' + IntToStr(DataLength) + ']: ');
  for i := 0 to (DataLength-1) do
    write(IntToStr(data[i]) + ' ');
  WriteLn();
end;

{ -------------------------------- }


procedure TSimpleBleNotifyExample.DoRun;
var
  ErrorMsg: String;
  Adapter: TSimpleBleAdapter;
  ErrCode: TSimpleBleErr = SIMPLEBLE_SUCCESS;
  i, j, k, Selection, CharacteristicCount: Integer;
  Peripheral: TSimpleBlePeripheral;
  PeripheralIdentifier: PChar;
  PeripheralAddress: PChar;
  Service: TSimpleBleService;
begin

  {$IFDEF DYNAMIC_LOADING}
  if not SimpleBleLoadLibrary() then begin
    writeln('Failed to load library');
    readln;
    exit;
  end;
  {$ENDIF}

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

  // start BLE scanning for 5 seconds
  SimpleBleAdapterScanFor(Adapter, 5000);

  // list found Peripheral devices
  WriteLn('The following devices were found:');
  for i := 0 to (PeripheralListLen - 1) do
  begin
    Peripheral := PeripheralList[i];
    PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
    PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
    WriteLn('[' + IntToStr(i) + '] ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
    SimpleBleFree(PeripheralIdentifier);
    SimpleBleFree(PeripheralAddress);
  end;

  // select device to connect
  Selection := -1;
  write('Please select a device to connect to: ');
  ReadLn(Selection);
  if (Selection < 0) or (Selection >= PeripheralListLen) then
  begin
    WriteLn('Invalid selection.');
    Terminate;
  end;

  // connect to selected device
  Peripheral := PeripheralList[Selection];
  PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
  PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
  WriteLn('Connecting to ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
  SimpleBleFree(PeripheralIdentifier);
  SimpleBleFree(PeripheralAddress);
  ErrCode := SimpleBlePeripheralConnect(Peripheral);
  if ErrCode <> SIMPLEBLE_SUCCESS then
  begin
    WriteLn('Failed to connect.');
    Terminate;
  end;
  WriteLn('Successfully connected, listing services and characteristics.');

  // show list of characteristics to select one to subscribe to notifications
  CharacteristicCount := 0;
  for i := 0 to (SimpleBlePeripheralServicesCount(Peripheral)-1) do
  begin
    ErrCode := SimpleBlePeripheralServicesGet(Peripheral, i, Service);
    if ErrCode <> SIMPLEBLE_SUCCESS then
    begin
      WriteLn('Failed to get service.');
      Terminate;
    end;
    for j := 0 to (Service.CharacteristicCount-1) do
    begin
      if CharacteristicCount >= SERVICES_LIST_SIZE then
        break;
      WriteLn('[' + IntToStr(CharacteristicCount) + '] ' + Service.Uuid.Value + ' ' + Service.Characteristics[j].Uuid.Value);
      CharacteristicList[CharacteristicCount].Service := Service.Uuid;
      CharacteristicList[CharacteristicCount].Characteristic := Service.Characteristics[j].Uuid;
      Inc(CharacteristicCount);
    end;
  end;

  // select characteristic to subsribe notifications
  Selection := -1;
  write('Please select characteristic to read from: ');
  ReadLn(Selection);
  if (Selection < 0) or (Selection >= CharacteristicCount) then
  begin
    WriteLn('Invalid selection.');
    Terminate;
  end;

  // subscribe to notification and register callback function
  SimpleBlePeripheralNotify(Peripheral, CharacteristicList[Selection].Service, CharacteristicList[Selection].Characteristic, @PeripheralOnNotify, Nil);

  // sleep 5 sec, during these 5 secs the Peripheral needs to update the characteristic value
  Sleep(5000);

  // unsubscribe notifications
  SimpleBlePeripheralUnsubscribe(Peripheral, CharacteristicList[Selection].Service, CharacteristicList[Selection].Characteristic);

  // disconnect from Peripheral
  SimpleBlePeripheralDisconnect(Peripheral);

  // wait for enter
  ReadLn();

  // release the BLE handle
  SimpleBleAdapterReleaseHandle(Adapter);

  {$IFDEF DYNAMIC_LOADING}
  SimpleBleUnloadLibrary();
  {$ENDIF}

  // stop program loop
  Terminate;
end;

constructor TSimpleBleNotifyExample.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSimpleBleNotifyExample.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  WriteLn('Releasing allocated resources.');
  // Release all saved peripherals
  for i := 0 to (PeripheralListLen - 1) do
    SimpleBlePeripheralReleaseHandle(PeripheralList[i]);
  // Let's not forget to release the associated handle.
  SimpleBleAdapterReleaseHandle(Adapter);
end;

procedure TSimpleBleNotifyExample.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
end;


var
  Application: TSimpleBleNotifyExample;
begin
  Application:=TSimpleBleNotifyExample.Create(nil);
  Application.Title:='SimpleBleScanTest';
  Application.Run;
  Application.Free;
end.

