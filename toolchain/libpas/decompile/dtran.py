#!/usr/bin/env python3
# -*- encoding: utf-8 -*-
#
# De-translate an object module into Madlen assembler.
# Invoke dispak simulator to run DTRAN.
#
# Leave two imtermediate files:
#   filename.b6     -- task file for dispak simulator
#   filename.lst    -- listing generated by DTRAN
#
import sys, os, string, subprocess

#
# Parse command line.
#
if len(sys.argv) != 2:
    print("Usage: dtran.py progname")
    sys.exit(1)
progname = sys.argv[1]
basename = progname.replace('/', '_')
#print("progname =", progname)

#
# Read input file and generate a task file.
#
task_file = open(basename + ".b6", "w")
task_file.write("""шифр 419999 зс5^
лен 40(2048)^
лен 41(2148)^
eeв1а3
*name %s
*perso: 401350, cont
*call dtran:410440(%s)
*call putflag*
40
*assem
*read: 1
*end file
``````
еконец
""" % (progname, progname))
task_file.close()

#
# Run dispak simulator.
#
dispak = subprocess.Popen('dispak -l %s.b6' % (basename),
    shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

lst_file = open(basename + ".lst", "w")
for line in dispak.stdout.readlines():
    line = line.decode('utf-8')
    lst_file.write(line.rstrip() + "\n")
lst_file.close()

retval = dispak.wait()
#print("retval =", retval)
if retval == 127:
    print("dispak: Command not found")
    lst_file.close()
    os.remove(basename + ".lst")
    sys.exit(1)
if retval != 0:
    print("dispak: Failed to invoke DTRAN")
    sys.exit(1)
