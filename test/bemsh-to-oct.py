#!/usr/bin/env python
# -*- encoding: utf-8 -*-
#
# Compile BEMSH assembler source into binary.
# Invoke dispak simulator to run BEMSH.
# Produce an output image in octal format:
#   filename.oct
#
# Leave two imtermediate files:
#   filename.b6     -- task file for dispak simulator
#   filename.lst    -- listing generated by bemsh
#   filename.dump   -- dump of resulting binary
#
import sys, os, string, subprocess

#
# Parse command line.
#
if len(sys.argv) != 2:
    print "Usage: bemsh-to-oct.py filename.bemsh"
    sys.exit(1)
input_name = sys.argv[1]
basename = os.path.splitext(input_name)[0]
#print "basename =", basename

#
# Open input file.
#
try:
    input_file = open(input_name)
except:
    print "%s: Cannot open input file" % input_name
    sys.exit(1)

#
# Read input file and generate a task file.
#
task_file = open(basename + ".b6", "w")
task_file.write("""шифр 419999^
трак 64^
лент 30(2048-6200)^
ацп 40^
росп 0^
врем 240^
лист 0-37^
вход 4000^
е
в 4000
к 00 010 4003
к 15 24 04000
к 00 066 0001 00 000 0100
с 3000 67
в 14000
а1
ввд$$$^
""")
for line in input_file.readlines():
    task_file.write(line.rstrip() + "^\n")
task_file.write("""квч$$$^
трн$$$^
0-0^
печмак^
бтмалф^
зонгп 270000^
наз   000001^
кнц$$$^
_$
еконец
""")
task_file.close()

#
# Run dispak simulator.
#
dispak = subprocess.Popen('dispak --drum-dump=%s.dump %s.b6' % (basename, basename),
    shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

lst_file = open(basename + ".lst", "w")
nerrors = -1
for line in dispak.stdout.readlines():
    lst_file.write(line.rstrip() + "\n")

    # Find status: number of errors.
    n = line.find("=")
    if n > 0:
        if line[:n] == "ЧИСЛО ОШИБОК":
            nerrors = int(line[n+1:n+5])
            #print "nerrors =", nerrors
lst_file.close()

retval = dispak.wait()
#print "retval =", retval
if retval == 127:
    print "dispak: Command not found"
    lst_file.close()
    os.remove(basename + ".lst")
    sys.exit(1)
if retval != 0:
    print "dispak: Failed to invoke BEMSH assembler"
    sys.exit(1)
if nerrors != 0:
    print "dispak: BEMSH errors detected: see %s.lst for details" % basename
    sys.exit(1)

#
# Check whether the address belongs to the instruction memory space,
# versus data space.
#
def instruction_space(addr):
    return addr < 02000

    # For ALU test:
    #if addr >= 000001 and addr <= 000001: return 1
    #if addr >= 032000 and addr <= 032007: return 1
    #if addr >= 032012 and addr <= 034407: return 1
    #if addr >= 035052 and addr <= 035156: return 1
    #if addr >= 035207 and addr <= 035760: return 1
    #if addr >= 036022 and addr <= 036121: return 1
    #if addr >= 036125 and addr <= 036427: return 1
    #return 0

#
# Open the dump file.
#
try:
    dump_file = open(basename + ".dump")
except:
    print "%s.dump: Cannot open dump file" % basename
    sys.exit(1)

#
# Generate oct file.
#
oct_file = open(basename + ".oct", "w")
for line in dump_file.readlines():
    #print line,
    n = line.find(" ")
    if n > 0:
        cmd = line[:n]
        if cmd == "к":
            # Word value, for example:
            # к 02 24 00000, 01 31 00002 ; с 1240 0000 0710 0002 ; 00001
            word = line.split()
            addr = int(word[14], 8)
            #print addr, word
            if instruction_space(addr):
                # Addresses 0...01777: instructions
                oct_file.write("i %05o %s %s %s %s %s %s\n" %
                    (addr, word[1], word[2], word[3][:-1], word[4], word[5], word[6]))
            else:
                # Addresses 02000...77777: data
                oct_file.write("d %05o %s %s %s %s\n" %
                    (addr, word[9], word[10], word[11], word[12]))
oct_file.close()

print "File %s succesfully compiled into %s.oct" % (input_name, basename)
