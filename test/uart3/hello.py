#!/usr/bin/python3
from serial import Serial
from time import sleep

ser = Serial('/dev/ttyUSB0')

t0 = -10

hello = '00767938383F'
spin  = '240912'
spin1 = '010204081020'

def get_anim(pos, t):
    global hello, spin, spin1
    if t<0:
        return '00'
    if t<20:
        return spin1[t*2 % 12] + spin1[t*2 % 12 + 1]
    if t<50:
        return spin[t*2 % 6] + spin[t*2 % 6 + 1]
    return hello[pos*2] + hello[pos*2+1]

reverse = 0

while True:
    s = ':'
    for i in range(6):
        s += get_anim(i, t0-i*13)
    ser.write(bytes(s, 'utf8'))
    ser.flush()
    sleep(0.08)
    if reverse == 0:
        t0 = t0 + 1
    else:
        t0 = t0 - 1
    if t0 > 120:
        reverse = 1
    if t0 < -10:
        reverse = 0

ser.close()