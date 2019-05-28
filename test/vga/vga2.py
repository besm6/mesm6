#!/usr/bin/python3
from PIL import Image
from serial import Serial
import random

ser = Serial('/dev/ttyUSB0', baudrate=115200)

s = ':017FD800'

def send_byte(port, addr, data):
    s = ':01{:04X}00{:02X}'.format(addr, data & 0xFF)
    print(s)
    port.write(bytes(s, 'utf8'))
    port.flush()

def send_word(port, addr, data):
    s = ':02{:04X}00{:04X}'.format(addr, data & 0xFFFF)
    print(s)
    port.write(bytes(s, 'utf8'))
    port.flush()

im = Image.open('lebedev-sa2.png')
pix = im.load()
sz =  im.size

send_byte(ser, 0o77736, 0)
send_word(ser, 0o77737, 0)


for y in range(sz[1]):
    for x in range(0,sz[0],8):
        bits = 0
        for i in range(8):
            bits += pix[x+i,y]<<(7-i)
        send_byte(ser, 0o77730, 255 ^ bits)

