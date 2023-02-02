# Pascal Bindings For SimpleBLE Library
These are Lazarus/FreePascal bindings for the SimpleBLE cross-platform Bluetooth LE (BLE) library.

## SimpleBLE
SimpleBLE is a fully-fledged cross-platform library and bindings for Bluetooth Low Energy (BLE).

> "The SimpleBLE project aims to provide fully cross-platform BLE libraries and bindings for Python and C++, designed for simplicity and ease of use with a licencing scheme chosen to be friendly towards commercial use. All specific operating system quirks are handled internally to provide a consistent behavior across all platforms. The libraries also provide first-class support for vendorization of all third-party dependencies, allowing for easy integration into existing projects."

The project lives on github (https://github.com/OpenBluetoothToolbox/SimpleBLE) and documentation is available through ReadTheDocs (https://simpleble.readthedocs.io/en/latest/).

It can be easily compiled into a shared library (.dll, .so, .dylib) and so far comes with C/C++ and Python bindings for the library functions. This project here adds Pascal bindings based on the C API in form of a unit to leverage the library functionality in [Lazarus](https://www.lazarus-ide.org/) / [FreePascal](https://www.freepascal.org/) projects.

## Usage
The SimpleBLE bindings come as a single-file Pascal unit and this is in the folder "Pascal\_Unit". To use it in a Lazarus/FreePascal project just copy it to the folder of your Lazarus project and add it to your project files list.

To actually use the SimpleBLE functions in the compiled application, the corresponding shared libraries need to be copied into the same path as the compiled application. (Alternatively other location could be used, provided it's on the PATH or it's some system wide location like Windows\System32 - however, at least the latter is usually not recommended.)

Here is a list of required files:
* **simpleble.pas**: The Pascal unit with SimpleBLE bindings.
* **simpleble-c.dll, simpleble.dll, fmt.dll**: The three shared libraries with all the SimpleBLE functionality in. You need all three of them.

On Windows systems shared libraries use the extension ".dll", but on other systems this is different (like .so on Linux/Unix).

Currently the Pascal bindings have been implemented and tested with Lazarus version 2.3.0 and Free Pascal version 3.3.1 (installed 'trunk' with fpdupdeluxe) on Windows 10 / 64 bit and Ubuntu 20.04LTS / 64 bit.

## Examples
The original SimpleBLE project comes with three C examples, which have been ported to Lazarus:

* **SimpleBleScanExample**: A console application based on scan.c from SimpleBLE and demonstrates scanning for BLE advertisements from peripherals. The output shows a list of devices with BLE MAC address, device name (if present), RSSI value and manufacturer data (if present).
* **SimpleBleConnectExample**: A console application based on connect.c from SimpleBLE and demonstrates
  * Scanning for BLE advertisements from peripherals like above.
  * Selecting a peripheral to connect to.
  * Fetch BLE services, characteristics and descriptors from the peripherals's GATT table and shows as a list.
* **SimpleBleNotifyExample**: A console application based on notify.c from SimpleBLE and demonstrates
  * Scanning for BLE advertisements from peripherals like above.
  * Selecting a peripheral to connect to.
  * Fetch BLE services, characteristics and descriptors from the peripherals's GATT table and shows as a list.
  * Selecting a characteristic and subscribe to notifications.
  * If characteristic value changes on the peripheral, the new values are shown.

There are some more examples, but those are C++ and weren't (yet...) ported to Pascal.

### Building the Examples
On Windows the example projects build without issues.

On Linux (tested on Ubuntu 20.04LTS) it's necessary to copy the shared libraries into the root folder of each example project.

## Building the SimpleBLE Shared Libraries
The SimpleBLE project comes with batch files / shell scripts for Windows / Linux which automagically compile the shared libraries. These are located in the "utils" folder of the SimpleBLE repo. However, for me it did not work under Linux/Ubuntu properly.

### Windows
When running the batch file, the compiled libraries will go into "build\_simpleble/bin/Release". A suitable compiler needs to be installed, I used Visual Studio Community edition 2022 (64 bit), version 17.3.6 on Windows 10 / 64 bit.

### Linux
On Linux please follow the instructions given at https://github.com/OpenBluetoothToolbox/SimpleBLE/blob/main/docs/simpleble/usage.rst.

## Releases
Pre-built SimpleBLE shared libraries are available on the [releases tab](https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library/releases). Currently there are versions for Windows 64 bit / 32 bit as well as Linux 64 bit available.

## Contributing/Feedback
I'm far from being an expert in Pascal programming, but liked and used Lazarus/FreePascal for some projects. So, feedback / improvements / pull-requests / etc. are welcome!

## License
Copyright (C) 2022 Erik Lins

This project is released under the MIT License.
