#!/bin/bash
pasmo --tap main.asm pirstc.tap
if [ "$?" = "0" ]; then
    ../bas2tap -a10 -sPIRST loader.bas loader.tap
    cat loader.tap pirstc.tap > pirst.tap
    fuse pirst.tap
fi
exit 0
