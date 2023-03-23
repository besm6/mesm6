#!/usr/bin/env python3
# -*- encoding: utf-8 -*-
#
# Convert punchcard output of DISPAK simulator into object file.
#
import sys, os, string, subprocess, struct

#
# Parse command line.
#
if len(sys.argv) < 2:
    print("Usage: punch-to-obj.py filename.punch")
    sys.exit(1)
input_name = sys.argv[1]
basename = os.path.splitext(input_name)[0]
#print("basename =", basename)

#
# Open input file.
#
try:
    input_file = open(input_name)
except:
    print("%s: Cannot open input file" % input_name)
    sys.exit(1)

def get12bits(card, x):
    word = 0
    for y in range(12):
        bit = card[y][x]
        if bit == 'O':
            word |= 1 << y
    return word

#
# Read input file and generate an object file.
#
obj_file = open(basename + ".obj", "wb")
obj_file.write(b"BESM6\0")
for cardno in range(1024):
    card = {}
    card[0] = input_file.readline()
    if not card[0]:
        break
    for i in range(1,13):
        card[i] = input_file.readline()
    if not card[12]:
        print("%s: Bad file format" % input_name)
        sys.exit(1)
    #print(card[0], end='')
    if cardno == 0:
        # Skip first card.
        continue

    for x in [4, 8, 13, 17, 22, 26, 31, 35, 40, 44, 49, 53, 58, 62, 67, 71]:
        a = get12bits(card, x)
        b = get12bits(card, x+1)
        c = get12bits(card, x+2)
        d = get12bits(card, x+3)
        print("%04o %04o %04o %04o" % (a, b, c, d))

        f = d & 0xff
        e = (d >> 8) | (c << 4 & 0xff)
        d = c >> 4
        c = b & 0xff
        b = (b >> 8) | (a << 4 & 0xff)
        a = a >> 4
        #print("%02x %02x %02x %02x %02x %02x" % (a, b, c, d, e, f))
        obj_file.write(struct.pack("BBBBBB", a, b, c, d, e, f))

    if card[0][3] == 'O':
        # Last card.
        break

obj_file.close()

print("File %s succesfully converted into %s.obj" % (input_name, basename))
