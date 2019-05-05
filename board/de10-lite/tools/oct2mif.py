#!/usr/bin/python3
import sys

""" Generates Memory initialization files """

if len(sys.argv) != 2:
    print("Usage: oct2mif.py filename.oct")
    sys.exit(1)

header = "DEPTH = 4096;\nWIDTH = 48;\nADDRESS_RADIX = OCT;\nDATA_RADIX = OCT;\nCONTENT BEGIN\n"

irom = open("irom.mif","wt")
dram = open("dram.mif","wt")

irom.write(header)
dram.write(header)

try:
    with open(sys.argv[1],"rt") as f:
        for line in f.readlines():
            fields = line.strip().split()
            if fields[0].startswith('#'):
                continue

            if fields[0].startswith('i'):
                if len(fields) != 8:
                    print("Bad line in OCT: {}".format(line))
                    break
                key, i, lm, lop, laddr, rm, rop, raddr = fields
                word = 0
                word += (int(lm, 8) << 44)
                if lop[0] == '2' or lop[0] == '3':
                    word += (int(lop,    8) << 39)
                    word += (int(laddr,  8) << 24)
                else:
                    word += (int(lop,    8) << 36)
                    word += ((int(laddr, 8) & 0o07777) << 24)
                word += (int(rm, 8) << 20)
                if rop[0] == '2' or rop[0] == '3':
                    word += (int(rop,    8) << 15)
                    word += (int(raddr,  8))
                else:
                    word += (int(rop,   8) << 12)
                    word += (int(raddr, 8) & 0o07777)
                irom.write("{} : {:016o};\n".format(i, word))

            if fields[0].startswith('d'):
                key, i, lm, laddr, rm, raddr = fields
                if len(fields) != 6:
                    print("Bad line in OCT: {}".format(line))
                    break
                word = 0
                word += (int(lm, 8)    << 36)
                word += (int(laddr, 8) << 24)
                word += (int(rm, 8)    << 12)
                word += (int(raddr, 8))

                dram.write("{} : {:016o};\n".format(i, word))
except:
    print("Something bad happened:")
    print(sys.exc_info())

irom.write("END;\n")
dram.write("END;\n")
irom.close()
dram.close()
