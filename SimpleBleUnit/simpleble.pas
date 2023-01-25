unit SimpleBle;

{$mode ObjFPC}{$H+}

{ Lazarus / Free Pascal bindings for the cross-platform SimpleBLE library.

  Pascal bindings are Copyright (c) 2022-2023 Erik Lins and released under the MIT License.
    https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library

  The SimpleBLE library is Copyright (c) 2021-2022 Kevin Dewald and released under the MIT License.
    https://github.com/OpenBluetoothToolbox/SimpleBLE
}


interface

uses
  {$IFDEF UNIX}
  ctypes,
  {$ENDIF}
  Classes, SysUtils;

const
  {$ifdef windows}
    SimpleBleExtLibrary = 'simpleble-c.dll';
  {$else}
    {$ifdef darwin}
      SimpleBleExtLibrary = 'simpleble-c.dylib';
    {$else}
      SimpleBleExtLibrary = 'simpleble-c.so';
    {$endif}
  {$endif}

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

  //#define SIMPLEBLE_UUID_STR_LEN 37  // 36 characters + null terminator
  //#define SIMPLEBLE_CHARACTERISTIC_MAX_COUNT 16
  //#define SIMPLEBLE_DESCRIPTOR_MAX_COUNT 16
  //Note: in C array declaration the above is the number of elements,
  //hence in Pascal we need to subtract 1 in the array declaration
  //like array[0..SIMPLEBLE_UUID_STR_LEN-1]
  SIMPLEBLE_UUID_STR_LEN = 37;
  SIMPLEBLE_CHARACTERISTIC_MAX_COUNT = 16;
  SIMPLEBLE_DESCRIPTOR_MAX_COUNT = 16;


{ types from SimpleBLE types.h }

type
  //typedef enum {
  //    SIMPLEBLE_SUCCESS = 0,
  //    SIMPLEBLE_FAILURE = 1,
  //} simpleble_err_t;
  TSimpleBleErr = (SIMPLEBLE_SUCCESS = 0, SIMPLEBLE_FAILURE = 1);

  //typedef struct {
  //  char value[SIMPLEBLE_UUID_STR_LEN];
  //} simpleble_uuid_t;
  TSimpleBleUuid = record
    Value: array[0..SIMPLEBLE_UUID_STR_LEN-1] of Char;
  end;

  //typedef struct {
  //    simpleble_uuid_t uuid;
  //} simpleble_descriptor_t;
  TSimpleBleDescriptor = record
    Uuid: TSimpleBleUuid;
  end;

  //typedef struct {
  //    simpleble_uuid_t uuid;
  //    size_t descriptor_count;
  //    simpleble_descriptor_t descriptors[SIMPLEBLE_DESCRIPTOR_MAX_COUNT];
  //} simpleble_characteristic_t;
  TSimpleBleCharacteristic = record
    Uuid: TSimpleBleUuid;
    CanRead: Boolean;
    CanWriteRequest: Boolean;
    CanWriteCommand: Boolean;
    CanNotify: Boolean;
    CanIndicate: Boolean;
    DescriptorCount: NativeUInt;
    Descriptors: array[0..SIMPLEBLE_DESCRIPTOR_MAX_COUNT-1] of TSimpleBleDescriptor;
  end;

  //typedef struct {
  //  simpleble_uuid_t uuid;
  //  size_t characteristic_count;
  //  simpleble_characteristic_t characteristics[SIMPLEBLE_CHARACTERISTIC_MAX_COUNT];
  //} simpleble_service_t;
  TSimpleBleService = record
    Uuid: TSimpleBleUuid;
    CharacteristicCount: NativeUInt;
    Characteristics: array[0..SIMPLEBLE_CHARACTERISTIC_MAX_COUNT-1] of TSimpleBleCharacteristic;
  end;

  //typedef struct {
  //    uint16_t manufacturer_id;
  //    size_t data_length;
  //    uint8_t data[27];
  //    // Note: The maximum length of a BLE advertisement is 31 bytes.
  //    // The first byte will be the length of the field,
  //    // the second byte will be the type of the field (0xFF for manufacturer data),
  //    // the next two bytes will be the manufacturer ID,
  //    // and the remaining 27 bytes are the manufacturer data.
  //} simpleble_manufacturer_data_t;
  TSimpleBleManufacturerData = record
    ManufacturerId: UInt16;
    DataLength: NativeUInt;
    Data: array[0..27-1] of Byte
  end;

  //typedef void* simpleble_adapter_t;
  //typedef void* simpleble_peripheral_t;
  TSimpleBleAdapter = NativeUInt;
  TSimpleBlePeripheral = NativeUInt;

  //typedef enum {
  //  SIMPLEBLE_OS_WINDOWS = 0,
  //  SIMPLEBLE_OS_MACOS = 1,
  //  SIMPLEBLE_OS_LINUX = 2,
  //} simpleble_os_t;
  TSimpleBleOs = (SIMPLEBLE_OS_WINDOWS = 0, SIMPLEBLE_OS_MACOS = 1, SIMPLEBLE_OS_LINUX = 2);

  //typedef enum {
  //    SIMPLEBLE_ADDRESS_TYPE_PUBLIC = 0,
  //    SIMPLEBLE_ADDRESS_TYPE_RANDOM = 1,
  //    SIMPLEBLE_ADDRESS_TYPE_UNSPECIFIED = 2,
  //} simpleble_address_type_t;
  TSimpleBleAddressType = (SIMPLEBLE_ADDRESS_TYPE_PUBLIC = 0, SIMPLEBLE_ADDRESS_TYPE_RANDOM = 1, SIMPLEBLE_ADDRESS_TYPE_UNSPECIFIED = 2);


{ functions from SimpleBLE adapter.h }

// new types for callback functions
type
  TSimpleBleCallbackScanStart = procedure(adapter: TSimpleBleAdapter; userdata: PPointer);
  TSimpleBleCallbackScanStop = procedure(adapter: TSimpleBleAdapter; userdata: PPointer);
  TSimpleBleCallbackScanUpdated = procedure(adapter: TSimpleBleAdapter; peripheral: TSimpleBleAdapter; userdata: PPointer);
  TSimpleBleCallbackScanFound = procedure(adapter: TSimpleBleAdapter; peripheral: TSimpleBleAdapter; userdata: PPointer);

//SIMPLEBLE_EXPORT bool simpleble_adapter_is_bluetooth_enabled(void);
function SimpleBleAdapterIsBluetoothEnabled(): Boolean; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_is_bluetooth_enabled';

//SIMPLEBLE_EXPORT size_t simpleble_adapter_get_count(void);
function SimpleBleAdapterGetCount(): NativeUInt; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_get_count';

//SIMPLEBLE_EXPORT simpleble_adapter_t simpleble_adapter_get_handle(size_t index);
function SimpleBleAdapterGetHandle(index: NativeUInt): TSimpleBleAdapter; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_get_handle';

//SIMPLEBLE_EXPORT void simpleble_adapter_release_handle(simpleble_adapter_t handle);
procedure SimpleBleAdapterReleaseHandle(handle: TSimpleBleAdapter); cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_release_handle';

//SIMPLEBLE_EXPORT char* simpleble_adapter_identifier(simpleble_adapter_t handle);
function SimpleBleAdapterIdentifier(handle: TSimpleBleAdapter): PChar; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_identifier';

//SIMPLEBLE_EXPORT char* simpleble_adapter_address(simpleble_adapter_t handle);
function SimpleBleAdapterAddress(handle: TSimpleBleAdapter): PChar; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_address';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_start(simpleble_adapter_t handle);
function SimpleBleAdapterScanStart(handle: TSimpleBleAdapter): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_scan_start';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_stop(simpleble_adapter_t handle);
function SimpleBleAdapterScanStop(handle: TSimpleBleAdapter): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_scan_stop';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_is_active(simpleble_adapter_t handle, bool* active);
function SimpleBleAdapterScanIsActive(handle: TSimpleBleAdapter; var active: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_scan_is_active';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_for(simpleble_adapter_t handle, int timeout_ms);
function SimpleBleAdapterScanFor(handle: TSimpleBleAdapter; timeout_ms: Integer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_scan_for';

//SIMPLEBLE_EXPORT size_t simpleble_adapter_scan_get_results_count(simpleble_adapter_t handle);
function SimpleBleAdapterScanGetResultsCount(handle: TSimpleBleAdapter): NativeUInt; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_scan_get_results_count';

//SIMPLEBLE_EXPORT simpleble_peripheral_t simpleble_adapter_scan_get_results_handle(simpleble_adapter_t handle, size_t index);
function SimpleBleAdapterScanGetResultsHandle(handle: TSimpleBleAdapter; index: NativeUInt): TSimpleBlePeripheral; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_scan_get_results_handle';

//SIMPLEBLE_EXPORT size_t simpleble_adapter_get_paired_peripherals_count(simpleble_adapter_t handle);
function SimpleBleAdapterGetPairedPeripheralsCount(handle: TSimpleBleAdapter): NativeUInt; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_get_paired_peripherals_count';

//SIMPLEBLE_EXPORT simpleble_peripheral_t simpleble_adapter_get_paired_peripherals_handle(simpleble_adapter_t handle, size_t index);
function SimpleBleAdapterGetPairedPeripheralsHandle(handle: TSimpleBleAdapter; index: NativeUInt): TSimpleBlePeripheral; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_get_paired_peripherals_handle';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_start(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, void* userdata), void* userdata);
function SimpleBleAdapterSetCallbackOnScanStart(handle: TSimpleBleAdapter; callback: TSimpleBleCallbackScanStart; userdata: PPointer): TSimpleBleErr;  cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_set_callback_on_scan_start';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_stop(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, void* userdata), void* userdata);
function SimpleBleAdapterSetCallbackOnScanStop(handle: TSimpleBleAdapter; callback: TSimpleBleCallbackScanStop; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_set_callback_on_scan_stop';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_updated(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function SimpleBleAdapterSetCallbackOnScanUpdated(handle: TSimpleBleAdapter; callback: TSimpleBleCallbackScanUpdated; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_set_callback_on_scan_updated';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_found(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function SimpleBleAdapterSetCallbackOnScanFound(handle: TSimpleBleAdapter; callback: TSimpleBleCallbackScanFound; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_adapter_set_callback_on_scan_found';


{ functions from SimpleBLE peripheral.h }

// new types for callback functions
type
  TSimpleBleCallbackOnConnected = procedure(peripheral: TSimpleBlePeripheral; userdata: PPointer);
  TSimpleBleCallbackOnDisconnected = procedure(peripheral: TSimpleBlePeripheral; userdata: PPointer);
  TSimpleBleCallbackNotify = procedure(service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt; userdata: PPointer);
  TSimpleBleCallbackIndicate = procedure(service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt; userdata: PPointer);

//SIMPLEBLE_EXPORT void simpleble_peripheral_release_handle(simpleble_peripheral_t handle);
procedure SimpleBlePeripheralReleaseHandle(handle: TSimpleBlePeripheral); cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_release_handle';

//SIMPLEBLE_EXPORT char* simpleble_peripheral_identifier(simpleble_peripheral_t handle);
function SimpleBlePeripheralIdentifier(handle: TSimpleBlePeripheral): PChar; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_identifier';

//SIMPLEBLE_EXPORT char* simpleble_peripheral_address(simpleble_peripheral_t handle);
function SimpleBlePeripheralAddress(handle: TSimpleBlePeripheral): PChar; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_address';

//SIMPLEBLE_EXPORT simpleble_address_type_t simpleble_peripheral_address_type(simpleble_peripheral_t handle);
function SimpleBlePeripheralAddressType(handle: TSimpleBlePeripheral): TSimpleBleAddressType; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_address_type';

//SIMPLEBLE_EXPORT int16_t simpleble_peripheral_rssi(simpleble_peripheral_t handle);
function SimpleBlePeripheralRssi(handle: TSimpleBlePeripheral): Int16; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_rssi';

//SIMPLEBLE_EXPORT int16_t simpleble_peripheral_tx_power(simpleble_peripheral_t handle);
function SimpleBlePeripheralTxPower(handle: TSimpleBlePeripheral): Int16; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_tx_power';

//SIMPLEBLE_EXPORT uint16_t simpleble_peripheral_mtu(simpleble_peripheral_t handle);
function SimpleBlePeripheralMtu(handle: TSimpleBlePeripheral): UInt16; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_mtu';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_connect(simpleble_peripheral_t handle);
function SimpleBlePeripheralConnect(handle: TSimpleBlePeripheral): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_connect';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_disconnect(simpleble_peripheral_t handle);
function SimpleBlePeripheralDisconnect(handle: TSimpleBlePeripheral): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_disconnect';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_is_connected(simpleble_peripheral_t handle, bool* connected);
function SimpleBlePeripheralIsConnected(handle: TSimpleBlePeripheral; var connected: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_is_connected';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_is_connectable(simpleble_peripheral_t handle, bool* connectable);
function SimpleBlePeripheralIsConnectable(handle: TSimpleBlePeripheral; var connectable: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_is_connectable';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_is_paired(simpleble_peripheral_t handle, bool* paired);
function SimpleBlePeripheralIsPaired(handle: TSimpleBlePeripheral; var paired: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_is_paired';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_unpair(simpleble_peripheral_t handle);
function SimpleBlePeripheralUnpair(handle: TSimpleBlePeripheral): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_unpair';

//SIMPLEBLE_EXPORT size_t simpleble_peripheral_services_count(simpleble_peripheral_t handle);
function SimpleBlePeripheralServicesCount(handle: TSimpleBlePeripheral): NativeUInt; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_services_count';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_services_get(simpleble_peripheral_t handle, size_t index, simpleble_service_t* services);
function SimpleBlePeripheralServicesGet(handle: TSimpleBlePeripheral; index: NativeUInt; var services: TSimpleBleService): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_services_get';

//SIMPLEBLE_EXPORT size_t simpleble_peripheral_manufacturer_data_count(simpleble_peripheral_t handle);
function SimpleBlePeripheralManufacturerDataCount(handle: TSimpleBlePeripheral): NativeUInt; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_manufacturer_data_count';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_manufacturer_data_get(simpleble_peripheral_t handle, size_t index, simpleble_manufacturer_data_t* manufacturer_data);
function SimpleBlePeripheralManufacturerDataGet(handle: TSimpleBlePeripheral; index: NativeUInt; var manufacturer_data: TSimpleBleManufacturerData): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_manufacturer_data_get';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_read(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, uint8_t** data, size_t* data_length);
function SimpleBlePeripheralRead(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; var data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_read';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_write_request(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length);
function SimpleBlePeripheralWriteRequest(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_write_request';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_write_command(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length);
function SimpleBlePeripheralWriteCommand(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_write_command';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_notify(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, void (*callback)(simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length, void* userdata), void* userdata);
function SimpleBlePeripheralNotify(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; callback: TSimpleBleCallbackNotify; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_notify';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_indicate(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, void (*callback)(simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length, void* userdata), void* userdata);
function SimpleBlePeripheralIndicate(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; callback: TSimpleBleCallbackIndicate; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_indicate';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_unsubscribe(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic);
function SimpleBlePeripheralUnsubscribe(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid):TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_unsubscribe';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_read_descriptor(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, simpleble_uuid_t descriptor, uint8_t** data, size_t* data_length);
function SimpleBlePeripheralReadDescriptor(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; descriptor: TSimpleBleUuid; data: PByte; var data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_read_descriptor';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_write_descriptor(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, simpleble_uuid_t descriptor, const uint8_t* data, size_t data_length);
function SimpleBlePeripheralWriteDescriptor(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; descriptor: TSimpleBleUuid; data: PByte; data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_write_descriptor';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_set_callback_on_connected(simpleble_peripheral_t handle, void (*callback)(simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function SimpleBlePeripheralSetCallbackOnConnected(handle: TSimpleBlePeripheral; callback: TSimpleBleCallbackOnConnected; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_set_callback_on_connected';

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_set_callback_on_disconnected(simpleble_peripheral_t handle, void (*callback)(simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function SimpleBlePeripheralSetCallbackOnDisconnected(handle: TSimpleBlePeripheral; callback: TSimpleBleCallbackOnDisconnected; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary name 'simpleble_peripheral_set_callback_on_disconnected';


{ functions from SimpleBLE simpleble.h }

//SIMPLEBLE_EXPORT void simpleble_free(void* handle);
procedure SimpleBleFree(var handle); cdecl; external SimpleBleExtLibrary name 'simpleble_free';


{ functions from SimpleBLE logging.h }

type
  //typedef enum {
  //  SIMPLEBLE_LOG_LEVEL_NONE = 0,
  //  SIMPLEBLE_LOG_LEVEL_FATAL,
  //  SIMPLEBLE_LOG_LEVEL_ERROR,
  //  SIMPLEBLE_LOG_LEVEL_WARN,
  //  SIMPLEBLE_LOG_LEVEL_INFO,
  //  SIMPLEBLE_LOG_LEVEL_DEBUG,
  //  SIMPLEBLE_LOG_LEVEL_VERBOSE
  //} simpleble_log_level_t;
  TSimpleBleLogLevel = (SIMPLEBLE_LOG_LEVEL_NONE    = 0,
                        SIMPLEBLE_LOG_LEVEL_FATAL   = 1,
                        SIMPLEBLE_LOG_LEVEL_ERROR   = 2,
                        SIMPLEBLE_LOG_LEVEL_WARN    = 3,
                        SIMPLEBLE_LOG_LEVEL_INFO    = 4,
                        SIMPLEBLE_LOG_LEVEL_DEBUG   = 5,
                        SIMPLEBLE_LOG_LEVEL_VERBOSE = 6);

  //typedef void (*simpleble_log_callback_t)(
  //    simpleble_log_level_t level,
  //    const char* module,
  //    const char* file,
  //    uint32_t line,
  //    const char* function,
  //    const char* message
  //);
  TCallbackLog = procedure(level: TSimpleBleLogLevel; module: PChar; lfile: PChar; line: DWord; lfunction: PChar; lmessage: PChar);

//SIMPLEBLE_EXPORT void simpleble_logging_set_level(simpleble_log_level_t level);
procedure SimpleBleLoggingSetLevel(level: TSimpleBleLogLevel); cdecl; external SimpleBleExtLibrary name 'simpleble_logging_set_level';

//SIMPLEBLE_EXPORT void simpleble_logging_set_callback(simpleble_log_callback_t callback);
procedure SimpleBlLloggingSetCallback(callback: TCallbackLog); cdecl; external SimpleBleExtLibrary name 'simpleble_logging_set_callback';


{ functions from SimpleBLE utils.h }

//simpleble_os_t get_operating_system(void);
function SimpleBleGetOperatingSystem(): TSimpleBleOs; cdecl; external SimpleBleExtLibrary name 'get_operating_system';


implementation

end.

