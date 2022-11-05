unit SimpleBle;

{$mode ObjFPC}{$H+}

{ Lazarus / Free Pascal bindings for the cross-platform SimpleBLE library.

  Pascal bindings are Copyright (c) 2022 Erik Lins and released under the MIT License.
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
  //Note: in C array declaration the below is the number of elements,
  //hence in Pascal we need to subtract 1 in the array declaration
  //like array[0..cSIMPLEBLE_UUID_STR_LEN-1]
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
    ManufacturerId: Integer;
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


{ functions from SimpleBLE adapter.h }

// new types for callback functions
type
  TCallbackScanStart = procedure(adapter: TSimpleBleAdapter; userdata: PPointer);
  TCallbackScanStop = procedure(adapter: TSimpleBleAdapter; userdata: PPointer);
  TCallbackScanUpdated = procedure(adapter: TSimpleBleAdapter; peripheral: TSimpleBleAdapter; userdata: PPointer);
  TCallbackScanFound = procedure(adapter: TSimpleBleAdapter; peripheral: TSimpleBleAdapter; userdata: PPointer);

//SIMPLEBLE_EXPORT bool simpleble_adapter_is_bluetooth_enabled(void);
function simpleble_adapter_is_bluetooth_enabled(): Boolean; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT size_t simpleble_adapter_get_count(void);
function simpleble_adapter_get_count(): NativeUInt; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_adapter_t simpleble_adapter_get_handle(size_t index);
function simpleble_adapter_get_handle(index: NativeUInt): TSimpleBleAdapter; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT void simpleble_adapter_release_handle(simpleble_adapter_t handle);
procedure simpleble_adapter_release_handle(handle: TSimpleBleAdapter); cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT char* simpleble_adapter_identifier(simpleble_adapter_t handle);
function simpleble_adapter_identifier(handle: TSimpleBleAdapter): PChar; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT char* simpleble_adapter_address(simpleble_adapter_t handle);
function simpleble_adapter_address(handle: TSimpleBleAdapter): PChar; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_start(simpleble_adapter_t handle);
function simpleble_adapter_scan_start(handle: TSimpleBleAdapter): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_stop(simpleble_adapter_t handle);
function simpleble_adapter_scan_stop(handle: TSimpleBleAdapter): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_is_active(simpleble_adapter_t handle, bool* active);
function simpleble_adapter_scan_is_active(handle: TSimpleBleAdapter; var active: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_scan_for(simpleble_adapter_t handle, int timeout_ms);
function simpleble_adapter_scan_for(handle: TSimpleBleAdapter; timeout_ms: Integer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT size_t simpleble_adapter_scan_get_results_count(simpleble_adapter_t handle);
function simpleble_adapter_scan_get_results_count(handle: TSimpleBleAdapter): NativeUInt; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_peripheral_t simpleble_adapter_scan_get_results_handle(simpleble_adapter_t handle, size_t index);
function simpleble_adapter_scan_get_results_handle(handle: TSimpleBleAdapter; index: NativeUInt): TSimpleBlePeripheral; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT size_t simpleble_adapter_get_paired_peripherals_count(simpleble_adapter_t handle);
function simpleble_adapter_get_paired_peripherals_count(handle: TSimpleBleAdapter): NativeUInt; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_peripheral_t simpleble_adapter_get_paired_peripherals_handle(simpleble_adapter_t handle, size_t index);
function simpleble_adapter_get_paired_peripherals_handle(handle: TSimpleBleAdapter; index: NativeUInt): TSimpleBlePeripheral; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_start(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, void* userdata), void* userdata);
function simpleble_adapter_set_callback_on_scan_start(handle: TSimpleBleAdapter; callback: TCallbackScanStart; userdata: PPointer): TSimpleBleErr;  cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_stop(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, void* userdata), void* userdata);
function simpleble_adapter_set_callback_on_scan_stop(handle: TSimpleBleAdapter; callback: TCallbackScanStop; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_updated(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function simpleble_adapter_set_callback_on_scan_updated(handle: TSimpleBleAdapter; callback: TCallbackScanUpdated; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_adapter_set_callback_on_scan_found(simpleble_adapter_t handle, void (*callback)(simpleble_adapter_t adapter, simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function simpleble_adapter_set_callback_on_scan_found(handle: TSimpleBleAdapter; callback: TCallbackScanFound; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;


{ functions from SimpleBLE peripheral.h }

// new types for callback functions
type
  TCallbackOnConnected = procedure(peripheral: TSimpleBlePeripheral; userdata: PPointer);
  TCallbackOnDisconnected = procedure(peripheral: TSimpleBlePeripheral; userdata: PPointer);
  TCallbackNotify = procedure(service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt; userdata: PPointer);
  TCallbackIndicate = procedure(service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt; userdata: PPointer);

//SIMPLEBLE_EXPORT void simpleble_peripheral_release_handle(simpleble_peripheral_t handle);
procedure simpleble_peripheral_release_handle(handle: TSimpleBlePeripheral); cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT char* simpleble_peripheral_identifier(simpleble_peripheral_t handle);
function simpleble_peripheral_identifier(handle: TSimpleBlePeripheral): PChar; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT char* simpleble_peripheral_address(simpleble_peripheral_t handle);
function simpleble_peripheral_address(handle: TSimpleBlePeripheral): PChar; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT int16_t simpleble_peripheral_rssi(simpleble_peripheral_t handle);
function simpleble_peripheral_rssi(handle: TSimpleBlePeripheral): Word; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_connect(simpleble_peripheral_t handle);
function simpleble_peripheral_connect(handle: TSimpleBlePeripheral): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_disconnect(simpleble_peripheral_t handle);
function simpleble_peripheral_disconnect(handle: TSimpleBlePeripheral): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_is_connected(simpleble_peripheral_t handle, bool* connected);
function simpleble_peripheral_is_connected(handle: TSimpleBlePeripheral; var connected: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_is_connectable(simpleble_peripheral_t handle, bool* connectable);
function simpleble_peripheral_is_connectable(handle: TSimpleBlePeripheral; var connectable: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_is_paired(simpleble_peripheral_t handle, bool* paired);
function simpleble_peripheral_is_paired(handle: TSimpleBlePeripheral; var paired: Boolean): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_unpair(simpleble_peripheral_t handle);
function simpleble_peripheral_unpair(handle: TSimpleBlePeripheral): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT size_t simpleble_peripheral_services_count(simpleble_peripheral_t handle);
function simpleble_peripheral_services_count(handle: TSimpleBlePeripheral): NativeUInt; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_services_get(simpleble_peripheral_t handle, size_t index, simpleble_service_t* services);
function simpleble_peripheral_services_get(handle: TSimpleBlePeripheral; index: NativeUInt; services: TSimpleBleService): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT size_t simpleble_peripheral_manufacturer_data_count(simpleble_peripheral_t handle);
function simpleble_peripheral_manufacturer_data_count(handle: TSimpleBlePeripheral): NativeUInt; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_manufacturer_data_get(simpleble_peripheral_t handle, size_t index, simpleble_manufacturer_data_t* manufacturer_data);
function simpleble_peripheral_manufacturer_data_get(handle: TSimpleBlePeripheral; index: NativeUInt; manufacturer_data: TSimpleBleManufacturerData): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_read(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, uint8_t** data, size_t* data_length);
function simpleble_peripheral_read(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; var data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_write_request(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length);
function simpleble_peripheral_write_request(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_write_command(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length);
function simpleble_peripheral_write_command(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; data: PByte; data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_notify(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, void (*callback)(simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length, void* userdata), void* userdata);
function simpleble_peripheral_notify(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; callback: TCallbackNotify; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_indicate(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, void (*callback)(simpleble_uuid_t service, simpleble_uuid_t characteristic, const uint8_t* data, size_t data_length, void* userdata), void* userdata);
function simpleble_peripheral_indicate(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; callback: TCallbackIndicate; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_unsubscribe(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic);
function simpleble_peripheral_unsubscribe(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid):TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_read_descriptor(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, simpleble_uuid_t descriptor, uint8_t** data, size_t* data_length);
function simpleble_peripheral_read_descriptor(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; descriptor: TSimpleBleUuid; data: PByte; var data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_write_descriptor(simpleble_peripheral_t handle, simpleble_uuid_t service, simpleble_uuid_t characteristic, simpleble_uuid_t descriptor, const uint8_t* data, size_t data_length);
function simpleble_peripheral_write_descriptor(handle: TSimpleBlePeripheral; service: TSimpleBleUuid; characteristic: TSimpleBleUuid; descriptor: TSimpleBleUuid; data: PByte; data_length: NativeUInt): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_set_callback_on_connected(simpleble_peripheral_t handle, void (*callback)(simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function simpleble_peripheral_set_callback_on_connected(handle: TSimpleBlePeripheral; callback: TCallbackOnConnected; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT simpleble_err_t simpleble_peripheral_set_callback_on_disconnected(simpleble_peripheral_t handle, void (*callback)(simpleble_peripheral_t peripheral, void* userdata), void* userdata);
function simpleble_peripheral_set_callback_on_disconnected(handle: TSimpleBlePeripheral; callback: TCallbackOnDisconnected; userdata: PPointer): TSimpleBleErr; cdecl; external SimpleBleExtLibrary;


{ functions from SimpleBLE simpleble.h }

//SIMPLEBLE_EXPORT void simpleble_free(void* handle);
procedure simpleble_free(handle: PPointer); cdecl; external SimpleBleExtLibrary;


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
  TSimpleBleLogLevel = (SIMPLEBLE_LOG_LEVEL_NONE = 0, SIMPLEBLE_LOG_LEVEL_FATAL = 1, SIMPLEBLE_LOG_LEVEL_ERROR = 2, SIMPLEBLE_LOG_LEVEL_WARN = 3, SIMPLEBLE_LOG_LEVEL_INFO = 4, SIMPLEBLE_LOG_LEVEL_DEBUG = 5, SIMPLEBLE_LOG_LEVEL_VERBOSE = 6);

  //typedef void (*simpleble_log_callback_t)(
  //    simpleble_log_level_t level,
  //    const char* module,
  //    const char* file,
  //    uint32_t line,
  //    const char* function,
  //    const char* message
  //);
  // ???
  TCallbackLog = procedure(level: TSimpleBleLogLevel; module: PChar; lfile: PChar; line: DWord; lfunction: PChar; lmessage: PChar);

//SIMPLEBLE_EXPORT void simpleble_logging_set_level(simpleble_log_level_t level);
procedure simpleble_logging_set_level(level: TSimpleBleLogLevel); cdecl; external SimpleBleExtLibrary;

//SIMPLEBLE_EXPORT void simpleble_logging_set_callback(simpleble_log_callback_t callback);
procedure simpleble_logging_set_callback(callback: TCallbackLog); cdecl; external SimpleBleExtLibrary;


{ functions from SimpleBLE utils.h }

//simpleble_os_t get_operating_system(void);
function get_operating_system(): TSimpleBleOs; cdecl; external SimpleBleExtLibrary;


implementation

end.

