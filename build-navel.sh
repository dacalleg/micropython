#!/bin/bash

git submodule update --init

make -C mpy-cross
git submodule init lib/berkeley-db-1.xx
git submodule update

git clone https://github.com/micropython/micropython-lib.git

cd /micropython/ports/esp32 && make BORAD=NAVEL

#echo "esptool.py -p /dev/ttyUSB0 -b 460800 --before default_reset --after hard_reset --chip esp32  write_flash --flash_mode dio --flash_size detect --flash_freq 40m 0x1000 ./build/bin/bootloader/bootloader.bin 0x8000 ./build/bin/partition_table/partition-table.bin 0xd000 ./build/bin/ota_data_initial.bin 0x10000 ./build/bin/micropython.bin"