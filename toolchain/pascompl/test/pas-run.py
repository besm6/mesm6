#!/usr/bin/env python3
# -*- encoding: utf-8 -*-
#
# Translate Pascal source into assembler (Madlen).
# Invoke dispak simulator to compile and run the program.
# Output a listing in text format.
#
# Leave two intermediate files:
#   filename.madlen -- assembler sources
#   filename.obj    -- object file
#   filename.b6     -- task file for dispak simulator
#
import sys, os, string, subprocess

#
# Parse command line.
#
if len(sys.argv) != 2:
    print("Usage: pas-run.py filename.pas")
    sys.exit(1)
input_name = sys.argv[1]
basename = os.path.splitext(input_name)[0]
#print("basename =", basename)

#
# Translate to object file
#
obj_name = basename + ".obj"
if subprocess.call(["besm6-pascompl", input_name, obj_name]) != 0:
    print("%s: pascompl failed" % input_name)
    sys.exit(1)

#
# Disassemble to Madlen sources
#
asm_name = basename + ".madlen"
asm_file = open(asm_name, "w")
if subprocess.call(["besm6-dtran", "-e", obj_name], stdout=asm_file) != 0:
    print("%s: dtran failed" % obj_name)
    sys.exit(1)
asm_file.close()

#
# Open input file.
#
try:
    asm_file = open(asm_name)
except:
    print("%s: Cannot open input file" % asm_name)
    sys.exit(1)

#
# Read assembler file and generate a task file.
#
task_name = basename + ".b6"
task_file = open(task_name, "w")
task_file.write("""шифр 419999 зс5^
лен 67(2148)^
eeв1а3
*name %s
*no list
*assem
""" % basename)
for line in asm_file.readlines():
    task_file.write(line.rstrip() + "\n")
task_file.write("""*perso:670440,cont
*no load list
*main %s
*execute
*end file
``````
еконец
""" % basename)
task_file.close()

#
# Run dispak simulator.
#
if subprocess.call(["dispak", task_name]) != 0:
    print("%s: dispak failed" % task_name)
    sys.exit(1)
