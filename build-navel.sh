#!/bin/bash

git submodule update --init

make -C mpy-cross
git submodule init lib/berkeley-db-1.xx
git submodule update

git clone https://github.com/micropython/micropython-lib.git

cd /micropython/ports/esp32 && make BORAD=NAVEL