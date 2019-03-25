#!/usr/bin/python3
# -*- coding: utf-8 -*-

# jumptab16.v
with open('entry16.hex','w') as out:
    out.write("v2.0 raw\n")
    with open('jumptab16.v') as f:
        for line in f.readlines():
            addr, entry = line.split(':')
            out.write(hex(int(entry.strip().split(',')[0][3:]))[2:])
            out.write('\n')

with open('entry64.hex','w') as out:
    out.write("v2.0 raw\n")
    with open('jumptab64.v') as f:
        for line in f.readlines():
            addr, entry = line.split(':')
            out.write(hex(int(entry.strip().split(',')[0][3:]))[2:])
            out.write('\n')

mc = open('mc.hex', 'w')
mc.write("v2.0 raw\n")

with open('microcode.v') as f:
    for line in f.readlines():
        l = line.split(':')[1].strip()[4:-1]
        mccode = hex(int(l,8) )
        # mc_high = hex((int(l,8) >> 32) & 0xffffffff)
        # print(mc_high, mc_low)
        mc.write(mccode[2:])
        mc.write('\n')

