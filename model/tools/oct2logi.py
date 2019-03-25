#!/usr/bin/python3

import sys, array

if len(sys.argv) != 2:
    print("Usage: oct2logi.py filename.oct")
    sys.exit(1)


irom = array.array('Q', [0]*32768)
dram = array.array('Q', [0]*32768)

with open(sys.argv[1]) as f:
    for line in f.readlines():
        # print(line.split())
        # i 00001 02 24 00000 01 010 0007
        try:
            key, i, lm, lop, laddr, rm, rop, raddr = line.split()
            word = 0
            word += (int(lm, 8) << 44)
            if lop[0] != '0':
                word += (int(lop,   8) << 39)
                word += (int(laddr, 8) << 24)
            else:
                word += (int(lop,   8) << 36)
                word += (int(laddr, 8) << 24)
            word += (int(rm, 8) << 20)
            if rop[0] != '0':
                word += (int(rop,   8) << 15)
                word +=  int(raddr, 8)
            else:
                word += (int(rop,   8)   << 12)
                word +=  int(raddr, 8)
            print('IROM: ',i, oct(word >> 24), oct(word & 0o77777777))
            irom[int(i,8)] = word
        except:
            pass
        try:
            key, i, lm, laddr, rm, raddr = line.split()
            word = 0
            word += (int(lm, 8) << 36)
            word += (int(laddr, 8) << 24)
            word += (int(rm, 8) << 12)
            word += (int(raddr, 8))
            dram[int(i,8)] = word
            print('DRAM: ',i,hex(word))
        except:
            pass

with open('irom.hex', 'w') as f:
  f.write('v2.0 raw\n')
  for i in range(32767):
    f.write(hex(irom[i])[2:] + '\n')

with open('dram.hex', 'w') as f:
  f.write('v2.0 raw\n')
  for i in range(32767):
    f.write(hex(dram[i])[2:] + '\n')