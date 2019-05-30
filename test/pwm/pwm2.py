#!/usr/bin/python3
from serial import Serial
from time import sleep

ser = Serial('/dev/ttyUSB0', baudrate=115200)

# open music.raw unsigned 8-bit PCM audio
with open('music.raw','rb') as f:
    b = f.read(16)
    while(len(b)>0):
        ser.write(b)
        ser.flush()
        b = f.read(16)

print("done")
