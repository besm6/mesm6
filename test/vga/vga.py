#!/usr/bin/python3
from serial import Serial
from time import sleep
import random

ser = Serial('/dev/ttyUSB0', baudrate = 115200)

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


for i in range(400000):
    bits = random.getrandbits(2)
    send_byte(ser, 0o77736, bits)
    bits = random.getrandbits(15)
    send_word(ser, 0o77737, bits)
    bits = random.getrandbits(8)
    send_word(ser, 0o77730, bits)

ser.close()
