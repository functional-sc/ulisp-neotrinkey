# uLisp 4.5 - AdaFruit NeoTrinkey Edition

### LINUX INSTALLATION

1. Buy an [AdaFruit Neo Trinkey](https://www.adafruit.com/product/4870) and optionally a [USB-A to USB-C adapter](https://www.adafruit.com/product/5030)
2. Install the [Arduino CLI](https://arduino.github.io/arduino-cli/)
3. Download the latest [uLisp AdaFruit NeoTrinkey binary relase](./releases/)
4. UnZip and upload the release to the Trinkey with the Arduino CLI using the [upload.sh script](./unix-utils/upload.sh). Your port may be different than `/dev/ttyACM0`.

It shoud look something like this:
```sh
    $ ./upload.sh ulisp45.ino.neotrinkey_m0.bin 
    Platform adafruit:samd@1.7.13 already installed
    Device       : ATSAMD21x18
    Version      : v1.1 [Arduino:XYZ] Jan 12 2021 01:06:46
    Address      : 0x0
    Pages        : 4096
    Page Size    : 64 bytes
    Total Size   : 256KB
    Planes       : 1
    Lock Regions : 16
    Locked       : none
    Security     : false
    BOD          : true
    BOR          : true
    Write 154200 bytes to flash (2410 pages)
    [==============================] 100% (2410/2410 pages)
    Done in 3.499 seconds
    Verify 154200 bytes of flash
    [==============================] 100% (2410/2410 pages)
    Verify successful
    Done in 1.586 seconds
    New upload port: /dev/ttyACM0 (serial)
```

When finished, your AdaFruit NeoTrinkey will blink a single blue pixel, once.
On the next boot it will start running the demo and be colorful.

### CONNECTING - ANDROID

You may need a [USB-A to USB-C adapter](https://www.adafruit.com/product/5030), 
just plug it in connect:

![Adroid Example](docs/android-example.png?raw=true)

You'll want to adjus the font to monospace and perhaps the size.  Do this in 
*Settings -> Terminal*

![Adroid Example](docs/android-settings.png?raw=true)

### CONNECTING - PC

First install [Arduino IDE](https://www.arduino.cc/en/software), plug in your
NeoTrinkey, then bring up the serial monitor via *Tools -> Serial Monitor*

![Arduino Example](docs/arduino-ide-example.png?raw=true)

### CONNECTING - CHROMEBOOK

On Chromebooks you'll need to reconnect the device.  Reinsert and when 
*"USB device detected"* pops up in the notifications click on 
*"connect to Linux"* about 3 times.

### USEAGE

### BUILDING

Building the image is pretty complicated because to use both the buttons AND
the neo-pixel lights you need to download the Arduino Libraries, modify them
by hand and only then you can create a fully-functional image to upload onto
your NeoTrinkey board.

* [Building Instructions](docs/uLisp-build.md)

