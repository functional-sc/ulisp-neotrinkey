#!/bin/sh

#
# get arduino-cli from
#   https://arduino.github.io/arduino-cli/0.35/installation/
#

#./arduino-cli board list
./arduino-cli core install adafruit:samd --additional-urls "https://adafruit.github.io/arduino-board-index/package_adafruit_index.json" 

./arduino-cli upload \
            -b adafruit:samd:adafruit_trinket_m0 \
            -i ulisp45/ulisp45.ino.neotrinkey_m0.bin \
            -p /dev/ttyACM0
