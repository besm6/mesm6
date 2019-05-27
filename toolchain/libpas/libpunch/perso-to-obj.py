#!/usr/bin/env python
# -*- encoding: utf-8 -*-
#
# Extract object file from library.
# Invoke dispak simulator to run Libpunch utility.
# Produce an output image in binary format:
#   filename.obj
#
# Leave twree imtermediate files:
#   filename.b6     -- task file for dispak simulator
#   filename.punch  -- punchcards of resulting object file
#
import sys, os, string, subprocess, struct

#
# Parse command line.
#
if len(sys.argv) != 4:
    print "Usage: perso-to-obj.py filename location progname"
    sys.exit(1)
input_name = sys.argv[1]
location = sys.argv[2]
progname = sys.argv[3]
basename = os.path.splitext(os.path.basename(input_name))[0]
#print "basename =", basename

#
# Read input file and generate a task file.
#
task_name = basename + ".b6"
task_file = open(task_name, "w")
task_file.write("""шифр 419999 зс5^
лен 40(2048)^
лен 41(2148)^
лен 42(2248)^
eeв1а3
*name %s
*perso: %s
*table: libpunch(%s)
*libpunch
*end file
``````
еконец
""" % (progname, location, progname))
task_file.close()

#
# Run dispak simulator.
#
dispak = subprocess.Popen('dispak --punch=%s.punch %s.b6' % (basename, basename),
    shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

nerrors = 0
for line in dispak.stdout.readlines():
    # Find status: number of errors.
    line = line.decode('utf-8')
    if line[:30] == u"ТНIS NАМЕ IS АВSЕNТ IN САТАLОG":
        nerrors += 1
        #print "nerrors =", nerrors

retval = dispak.wait()
#print "retval =", retval
if retval == 127:
    print "dispak: Command not found"
    sys.exit(1)
if retval != 0:
    print "dispak: Failed to invoke Libpunch utility"
    sys.exit(1)
if nerrors != 0:
    print "dispak: Libpunch errors detected! Try 'dispak %s.b6'" % basename
    sys.exit(1)

#
# Open the punch file.
#
try:
    punch_file = open(basename + ".punch")
except:
    print "%s.punch: Cannot open punch file" % basename
    sys.exit(1)

#
# Extract one column of 12 bits from the punchcard.
#
def get12bits(card, x):
    word = 0
    for y in range(12):
        bit = card[y][x]
        if bit == 'O':
            word |= 1 << y
    return word

#
# Generate obj file.
#
obj_file = open(basename + ".obj", "w")
obj_file.write("BESM6\0")
for cardno in range(1024):
    card = {}
    card[0] = punch_file.readline()
    if not card[0]:
        break
    for i in range(1,13):
        card[i] = punch_file.readline()
    if not card[12]:
        print "%s.obj: Bad file format" % basename
        sys.exit(1)
    #print card[0],
    if cardno == 0:
        # Skip first card.
        continue

    for x in [4, 8, 13, 17, 22, 26, 31, 35, 40, 44, 49, 53, 58, 62, 67, 71]:
        a = get12bits(card, x)
        b = get12bits(card, x+1)
        c = get12bits(card, x+2)
        d = get12bits(card, x+3)
        #print "%04o %04o %04o %04o" % (a, b, c, d)

        f = d & 0xff
        e = (d >> 8) | (c << 4 & 0xff)
        d = c >> 4
        c = b & 0xff
        b = (b >> 8) | (a << 4 & 0xff)
        a = a >> 4
        #print "%02x %02x %02x %02x %02x %02x" % (a, b, c, d, e, f)
        obj_file.write(struct.pack("BBBBBB", a, b, c, d, e, f))

    if card[0][3] == 'O':
        # Last card.
        break

obj_file.close()

print "File %s succesfully extracted into %s.obj" % (basename, basename)
